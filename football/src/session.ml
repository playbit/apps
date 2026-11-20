(* Match authority follows live Playbit membership. A successor imports
   the last useful match snapshot and preserves the occupied slots. *)

module Channel = Playbit.Session
open Wire

type status = Connecting | Offline | Online | Full

let find_slot slots peer = Array.find_index ((=) (Some peer)) slots

(* Presence supplies a sorted, unique member list, so every host seats newcomers
   in the same order and keeps players already on the pitch in their shirts. *)
let assign seats members =
    let seats =
        Array.map (function Some peer when List.mem peer members -> Some peer | _ -> None) seats
    in
    let count first =
        let n = ref 0 in
        for slot = first to first + Match.players_per_team - 1 do
            if seats.(slot) <> None then incr n
        done;
        !n
    in
    (* Fill the outfield first. Slot 6 takes kickoff and slot 0 keeps goal. *)
    let order = [6; 4; 5; 3; 1; 2; 0] in
    List.iter (fun peer ->
        if find_slot seats peer = None then (
            let away = Match.players_per_team in
            let first = if count 0 <= count away then 0 else away in
            List.find_opt (fun role -> seats.(first + role) = None) order
            |> Option.iter (fun role -> seats.(first + role) <- Some peer)
        )
    ) members;
    seats

type channel = { handle : Channel.channel; identity : string }
type operation = Subscribing | Sending_state | Sending_message

type t = {
    mutable local_game : Match.t;
    mutable practice_slot : int;

    mutable channel : channel option;
    mutable self : string option;
    mutable pending : (Channel.operation * operation) option;
    mutable subscribed : bool;
    mutable failed : bool;

    (* Presence elects a leader. It becomes authoritative after recovering
       a match from the previous leader or the remaining participants. *)
    mutable members : string list;
    mutable joins : int;
    mutable leader : string option;
    mutable live : snapshot option;
    mutable snapshot_owner : string option;
    mutable confirmed : bool;
    mutable offer_to : string option;
    mutable best_offer : snapshot option;
    mutable no_state : string list;

    mutable rematch : int option;
    mutable dirty : bool;
    mutable change_time : float;
    mutable next_request : float;
    mutable send_delay : float;
    mutable accumulator : float;
    mutable sent_tick : int;

    mutable input : Match.input;
    mutable sent_input : Match.input;
    mutable input_age : float;
    mutable prediction : (Match.player * float) option;
    inputs : Match.input option array;
    input_ttl : float array;

    mutable wake_armed : bool;
    mutable last_frame : int64;
}

let create () = {
    local_game = Match.create (); practice_slot = 6;
    channel = None; self = None;
    pending = None; subscribed = false; failed = false;
    members = []; joins = 0; leader = None; live = None; snapshot_owner = None; confirmed = false;
    offer_to = None; best_offer = None; no_state = []; rematch = None;
    dirty = false; change_time = 0.; next_request = 0.;
    send_delay = 0.;
    accumulator = 0.; sent_tick = -1;
    input = Match.idle; sent_input = Match.idle; input_age = 0.; prediction = None;
    inputs = Array.make Match.player_count None;
    input_ttl = Array.make Match.player_count 0.;
    wake_armed = false; last_frame = 0L;
}

let connected t = t.subscribed && t.leader <> None
let leading t = t.leader <> None && t.leader = t.self
let is_authority t = leading t && t.confirmed

(* Keep the last snapshot across disconnects for handoff. Offline play has
   its own field and never mutates that snapshot. *)
let live_match t = if connected t then t.live else None

let game t =
    match live_match t with Some state -> state.game | None -> t.local_game

let local_slot t =
    match live_match t, t.self with
    | Some state, Some me -> find_slot state.slots me
    | _ -> Some t.practice_slot

let has_ball t =
    match local_slot t with
    | Some slot -> (game t).ball.owner = Some slot
    | None -> false

let display_position t slot =
    let player = match t.prediction with
        | Some (player, _) when player.slot = slot -> player
        | _ -> (game t).players.(slot)
    in
    (player.x, player.y)

let human_count t =
    match live_match t with
    | Some state ->
        Array.fold_left (fun n peer -> if Option.is_some peer then n + 1 else n) 0 state.slots
    | None -> 1

let status t =
    if connected t && t.confirmed then
        match local_slot t with
        | Some _ -> Online
        | None when human_count t = Match.player_count -> Full
        | None -> Connecting
    else if t.failed || t.channel = None then Offline
    else Connecting

let can_restart t = not (connected t) || (t.confirmed && local_slot t <> None)

let without_actions (input : Match.input) = { input with pass = false; shoot = false }

let install t state =
    let previous = t.live in
    let new_round = Option.fold previous ~none:true
        ~some:(fun current -> current.round <> state.round) in
    Array.iteri (fun slot peer ->
        let was = Option.bind previous (fun state -> state.slots.(slot)) in
        if was <> peer || new_round || not t.confirmed then (
            t.inputs.(slot) <- Option.map (fun _ -> Match.idle) peer;
            t.input_ttl.(slot) <- 0.
        )
    ) state.slots;
    if new_round then (
        t.sent_input <- Match.idle;
        t.input <- Match.idle;
        t.input_age <- 0.
    ) else (
        let old_slot = Option.bind previous (fun old ->
            Option.bind t.self (find_slot old.slots)) in
        let new_slot = Option.bind t.self (find_slot state.slots) in
        if old_slot <> new_slot then t.input <- without_actions t.input
    );
    t.prediction <- None;
    if t.rematch <> Some state.round then t.rematch <- None;
    t.live <- Some state

let promote t =
    let chosen = match t.best_offer with Some _ as offer -> offer | None -> t.live in
    t.best_offer <- None;
    let state = match chosen with
        | Some state -> state
        | None ->
            (* The initial solo player's field can become the live field. *)
            let slots = vacant () in
            let game =
                if t.members = Option.to_list t.self then (
                    slots.(t.practice_slot) <- t.self;
                    let game = t.local_game in
                    t.local_game <- Match.create ();
                    game
                )
                else Match.create ()
            in
            { round = 1; game; slots }
    in
    install t { state with slots = assign state.slots t.members };
    t.snapshot_owner <- t.self;
    t.confirmed <- true;
    t.dirty <- true;
    t.sent_tick <- -1;
    t.accumulator <- 0.

let identify_local t members =
    match t.channel with
    | None -> None
    | Some channel ->
        match List.filter (Channel.same_instance channel.identity) members with
        | [me] -> Some me
        | _ -> None

let take_joins t =
    let joins = t.joins in
    t.joins <- 0;
    joins

module Players = Set.Make (String)

(* Playbit presence tuples are account, device and instance (16 bytes each).
   Reconnecting through another device record still identifies the same player. *)
let players_in members =
    List.fold_left (fun players peer ->
        let account = String.sub peer 0 16 and instance = String.sub peer 32 16 in
        Players.add (account ^ instance) players
    ) Players.empty members

let count_joins previous members =
    Players.cardinal (Players.diff (players_in members) (players_in previous))

let presence t members =
    let members = List.sort_uniq String.compare members in
    let was_connected = connected t in
    let had_self = t.self <> None in
    t.self <- identify_local t members;
    if t.subscribed && had_self && t.self = None then t.failed <- true;
    let leader =
        if t.subscribed && t.self <> None then List.nth_opt members 0
        else None
    in
    if leader <> t.leader then (
        t.leader <- leader;
        t.confirmed <- false;
        t.dirty <- false;
        t.offer_to <- None;
        t.best_offer <- None;
        t.no_state <- [];
        t.next_request <- 0.;
        t.change_time <- 0.;
        t.send_delay <- 0.;
        t.accumulator <- 0.;
        t.sent_input <- Match.idle;
        t.input_age <- 0.;
        t.prediction <- None
    );
    if members <> t.members then (
        (* Both snapshots include us. The first after joining or reconnecting
           establishes a baseline, so the players already there stay quiet. *)
        if was_connected && connected t then
            t.joins <- t.joins + count_joins t.members members;
        t.members <- members;
        t.no_state <- List.filter (fun peer -> List.mem peer members) t.no_state;
        if is_authority t then
            Option.iter (fun state ->
                install t { state with slots = assign state.slots members };
                t.dirty <- true
            ) t.live
    );
    if leading t && not t.confirmed && members = Option.to_list t.self then promote t

let version state = (state.round, state.game.tick)

let can_offer t sender =
    match t.snapshot_owner with
    | Some leader when Some leader <> t.self && List.mem leader t.members -> leader = sender
    | _ -> Option.fold t.live ~none:true
        ~some:(fun state -> find_slot state.slots sender <> None)

let kick_off t state =
    if state.game.phase = Match.Full_time && state.round < next_round_limit then (
        install t { state with round = state.round + 1; game = Match.create () };
        t.dirty <- true;
        t.sent_tick <- -1;
        t.accumulator <- 0.
    )

let receive t sender packet =
    (* A packet may arrive after its sender has left or lost the election. *)
    if connected t && Some sender <> t.self && List.mem sender t.members then
        match packet, t.live with
        | Request takeover, _ ->
            if is_authority t then t.dirty <- true
            else if takeover && t.leader = Some sender then t.offer_to <- Some sender
        | Offer (recipient, state), _ when
            t.self = Some recipient && leading t && not t.confirmed && can_offer t sender ->
            let older = function
                | None -> true
                | Some current -> version current < version state
            in
            t.no_state <- List.filter ((<>) sender) t.no_state;
            if older t.live && older t.best_offer then t.best_offer <- Some state
        | No_state recipient, _ when t.self = Some recipient && leading t && not t.confirmed ->
            if not (List.mem sender t.no_state) then t.no_state <- sender :: t.no_state
        | State state, previous when t.leader = Some sender ->
            let up_to_date = match previous with
                | Some last when t.snapshot_owner = Some sender -> version state >= version last
                | _ -> true
            in
            if up_to_date then (
                install t state;
                t.snapshot_owner <- Some sender;
                t.confirmed <- true
            )
        | Input (round, shirt, input), Some state when is_authority t && round = state.round ->
            (match find_slot state.slots sender with
             | None -> ()
             | Some slot ->
                 let old = Option.value t.inputs.(slot) ~default:Match.idle in
                 (* Motion follows the participant. A late button press belongs
                    to the shirt they saw when they pressed it. *)
                 t.inputs.(slot) <- Some {
                     input with pass = (shirt = slot && input.pass) || old.pass;
                                shoot = (shirt = slot && input.shoot) || old.shoot;
                 };
                 t.input_ttl.(slot) <- 0.75)
        | Rematch round, Some state when is_authority t && round = state.round ->
            if find_slot state.slots sender <> None then kick_off t state
        | _ -> ()

let subscribe t =
    match t.channel, t.pending with
    | Some channel, None when not t.subscribed ->
        (match Channel.subscribe channel.handle with
         | op -> t.pending <- Some (op, Subscribing); t.failed <- false
         | exception Playbit.Host_error _ -> t.failed <- true)
    | _ -> ()

let refresh_presence t =
    match Channel.presence () with
    | members ->
        presence t members;
        if Option.is_some t.self then (
            if t.subscribed then t.failed <- false
            else if t.failed then subscribe t
        )
    | exception Playbit.Host_error _ -> ()

let start t =
    if t.channel = None then (
        try
            let identity = Channel.identity () in
            let handle = Channel.open_channel "football-v1" in
            t.channel <- Some { handle; identity }
        with Playbit.Host_error _ -> t.failed <- true
    );
    refresh_presence t;
    subscribe t

let publish_failed t error =
    t.send_delay <- 0.1;
    if error = Int32.of_int Playbit.Error.unavailable then (
        refresh_presence t;
        if t.self = None then t.failed <- true
    )

let publish t packet =
    match t.channel, t.pending with
    | Some channel, None when connected t ->
        (match Channel.publish channel.handle (encode packet) with
         | op ->
             let operation = match packet with State _ -> Sending_state | _ -> Sending_message in
             t.pending <- Some (op, operation);
             true
         | exception Playbit.Host_error error ->
             publish_failed t error;
             false)
    | _ -> false

let send_controls t state slot =
    let input = t.input in
    let changed = input.dx <> t.sent_input.dx || input.dy <> t.sent_input.dy in
    let moving = input.dx <> 0. || input.dy <> 0. in
    if t.rematch = Some state.round then (
        if publish t (Rematch state.round) then t.rematch <- None
    ) else if t.input_age >= 0.05
        && (input.pass || input.shoot || changed || (moving && t.input_age >= 0.25))
        && publish t (Input (state.round, slot, input)) then (
        t.sent_input <- input;
        t.input_age <- 0.;
        t.input <- without_actions input
    )

let pump t =
    let has_peers = match t.members with _ :: _ :: _ -> true | _ -> false in
    if connected t && has_peers && t.pending = None && t.send_delay <= 0. then
        match t.offer_to, t.live with
        | Some peer, state ->
            let reply = match state with
                | Some state -> Offer (peer, state)
                | None -> No_state peer
            in
            if publish t reply then t.offer_to <- None
        | None, Some state when is_authority t ->
            if (t.dirty || state.game.tick - t.sent_tick >= 3) && publish t (State state) then (
                t.dirty <- false;
                t.sent_tick <- state.game.tick
            )
        | None, _ when not t.confirmed ->
            (* Presence and messages can arrive in either order. Stop requesting
               a snapshot as soon as a usable one arrives. *)
            if t.change_time >= t.next_request && publish t (Request (leading t)) then
                t.next_request <- t.change_time +. (if t.change_time < 0.4 then 0.2 else 1.)
        | None, Some state -> Option.iter (send_controls t state) (local_slot t)
        | None, None -> ()

let step_time = 1. /. float Match.ticks_per_second

let predict t elapsed =
    let game = game t in
    if connected t && t.confirmed && not (leading t) && game.phase = Match.Playing
        && (t.input.dx <> 0. || t.input.dy <> 0.) then
        Option.iter (fun slot ->
            let player, predicted = match t.prediction with
                | Some prediction -> prediction
                | None -> ({ game.players.(slot) with slot }, 0.)
            in
            let elapsed = min elapsed (0.17 -. predicted) in
            if elapsed > 0. then (
                Match.move_human ~ticks:(elapsed /. step_time)
                    player t.input (game.ball.owner = Some slot);
                t.prediction <- Some (player, predicted +. elapsed)
            )
        ) (local_slot t)

let clear_actions t =
    Array.iteri (fun slot ->
        Option.iter (fun input ->
            if input.Match.pass || input.shoot then
                t.inputs.(slot) <- Some (without_actions input)
        )
    ) t.inputs;
    t.input <- without_actions t.input

(* Transfers only ever take over a teammate currently driven by the AI. The
   roster in each snapshot tells every participant which shirt they control. *)
let switch t ~live from_slot to_slot =
    match t.inputs.(from_slot), t.inputs.(to_slot) with
    | Some input, None when Match.team_of_slot from_slot = Match.team_of_slot to_slot ->
        (match live with
         | None -> t.practice_slot <- to_slot
         | Some state ->
             state.slots.(to_slot) <- state.slots.(from_slot);
             state.slots.(from_slot) <- None;
             t.dirty <- true);
        t.inputs.(to_slot) <- Some (without_actions input);
        t.inputs.(from_slot) <- None;
        t.input_ttl.(to_slot) <- t.input_ttl.(from_slot);
        t.input_ttl.(from_slot) <- 0.
    | _ -> ()

let defend t ~live (game : Match.t) =
    Array.iteri (fun slot -> function
        | Some input when input.Match.pass && game.ball.owner <> Some slot
                          && not (Match.in_challenge_range game slot) ->
            let team = Match.team_of_slot slot in
            (match game.ball.owner with
             | Some owner when Match.team_of_slot owner = team -> switch t ~live slot owner
             | _ ->
                 let available i = i = slot || t.inputs.(i) = None in
                 let nearest = Match.closest_outfield ~available game team game.ball.x game.ball.y in
                 if nearest >= 0 then switch t ~live slot nearest)
        | _ -> ()) t.inputs

let follow_possession t ~live (game : Match.t) passer receiver =
    match game.ball.owner with
    | Some owner when t.inputs.(owner) = None ->
        let team = Match.team_of_slot owner in
        (match passer with
         | Some from_slot when receiver = Some owner && t.inputs.(from_slot) <> None
                               && Match.team_of_slot from_slot = team ->
             switch t ~live from_slot owner
         | _ when not (Match.keeper owner) ->
             (* A team's only human follows its first touch. Teammates
                keep their assignments unless following their own pass. *)
             let first = Match.first_slot team in
             let count = ref 0 and teammate = ref first in
             for slot = first to first + Match.players_per_team - 1 do
                 if t.inputs.(slot) <> None then (incr count; teammate := slot)
             done;
             if !count = 1 then switch t ~live !teammate owner
         | _ -> ())
    | _ -> ()

let simulate t elapsed =
    let live = live_match t in
    if Option.is_none live || is_authority t then (
        let game = game t in
        t.accumulator <- min 1. (t.accumulator +. elapsed);
        while game.phase <> Match.Full_time && t.accumulator >= step_time do
            t.accumulator <- t.accumulator -. step_time;
            if Option.is_none live then Array.fill t.inputs 0 Match.player_count None
            else Array.iteri (fun slot ttl ->
                if ttl > 0. then (
                    let ttl = max 0. (ttl -. step_time) in
                    t.input_ttl.(slot) <- ttl;
                    if ttl = 0. then t.inputs.(slot) <- Some Match.idle
                )
            ) t.input_ttl;
            Option.iter (fun slot -> t.inputs.(slot) <- Some t.input) (local_slot t);
            let before = game.phase in
            if before = Match.Playing then defend t ~live game;
            let owner = game.ball.owner in
            let passer = game.ball.last_touch and receiver = game.ball.receiver in
            Match.step game t.inputs;
            let receiving_in_play = match before with
                | Match.Playing -> true
                | Match.Kickoff _ -> game.phase = Match.Playing
                | _ -> false in
            if owner <> game.ball.owner && receiving_in_play then
                follow_possession t ~live game passer receiver;
            if Option.is_some live && game.phase = Match.Full_time then t.dirty <- true;
            clear_actions t
        done
    )

let advance t elapsed =
    t.change_time <- t.change_time +. elapsed;
    t.input_age <- t.input_age +. elapsed;
    t.send_delay <- max 0. (t.send_delay -. elapsed);
    if leading t && not t.confirmed && t.change_time >= 0.6 &&
       (t.live <> None || t.best_offer <> None ||
        List.for_all (fun peer -> Some peer = t.self || List.mem peer t.no_state) t.members)
    then promote t;
    simulate t elapsed;
    pump t

let needs_wake t =
    connected t && (not t.confirmed ||
        (leading t && ((game t).phase <> Match.Full_time || t.send_delay > 0.)))

let arm_wake t =
    if needs_wake t && not t.wake_armed then
        match Playbit.wake_after 1000 with
        | () -> t.wake_armed <- true
        | exception Playbit.Host_error _ -> ()

let advance_if_hidden t =
    let now = Playbit.monotonic_ns () in
    let elapsed =
        if t.last_frame = 0L then 1.
        else Int64.to_float (Int64.sub now t.last_frame) /. 1e9
    in
    if elapsed > 0.1 then (
        t.last_frame <- now;
        advance t (min 1. elapsed)
    )

let frame t elapsed (input : Match.input) =
    t.input <- {
        input with pass = input.pass || t.input.pass; shoot = input.shoot || t.input.shoot;
    };
    if needs_wake t then t.last_frame <- Playbit.monotonic_ns ();
    advance t elapsed;
    predict t elapsed;
    arm_wake t

(* Only one channel operation is in flight. A ready event identifies its
   handle; the saved operation kind determines what a completion means. *)
let complete t completed =
    match t.pending with
    | Some (op, operation) when op = completed ->
        (match Channel.take op with
         | Channel.Pending -> ()
         | Channel.Ready ->
             t.pending <- None;
             if operation = Subscribing then (
                 t.subscribed <- true;
                 t.failed <- false;
                 refresh_presence t
             )
         | exception Playbit.Host_error error ->
             t.pending <- None;
             (match operation with
              | Subscribing ->
                  t.failed <- true;
                  presence t []
              | Sending_state | Sending_message ->
                  publish_failed t error;
                  if operation = Sending_state then t.dirty <- true))
    | _ -> ()

let channel_status t error =
    if error = Int32.of_int Playbit.Error.unavailable then (
        t.failed <- true;
        presence t []
    ) else if error = Int32.of_int Playbit.Error.limit_exceeded then
        if leading t then t.dirty <- true
        else (
            t.confirmed <- false;
            t.next_request <- 0.;
            t.change_time <- 0.;
            t.prediction <- None
        )

let channel_message t (handle, sender, bytes) =
    match t.channel with
    | Some channel when handle = channel.handle ->
        (match decode bytes with
         | None -> ()
         | Some packet ->
             (match packet with
              | Input _ when is_authority t && (game t).phase <> Match.Full_time ->
                  advance_if_hidden t
              | _ -> ());
             receive t sender packet)
    | _ -> ()

let event t (event : Playbit.event) =
    if event.kind = Int32.of_int Playbit.EventKind.presence_changed then
        refresh_presence t
    else if event.kind = Int32.of_int Playbit.EventKind.wake then (
        t.wake_armed <- false;
        if needs_wake t then advance_if_hidden t
    ) else (
        match Channel.ready event, Channel.status event, Channel.message event with
        | Some completed, _, _ -> complete t completed
        | _, Some error, _ -> channel_status t error
        | _, _, Some message -> channel_message t message
        | None, None, None -> ()
    );
    pump t;
    arm_wake t

let restart t =
    if not (connected t) && (game t).phase = Match.Full_time then (
        t.local_game <- Match.create ();
        t.practice_slot <- 6;
        t.accumulator <- 0.
    ) else if can_restart t then (
        Option.iter (fun state ->
            if is_authority t then kick_off t state
            else if state.game.phase = Match.Full_time then t.rematch <- Some state.round
        ) t.live;
        pump t;
        arm_wake t
    )
