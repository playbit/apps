module Input = Playbit.Input
module View = Playbit.View

(* Playbit delivers menu selections as clicks on these stable target IDs. *)
module Target = struct
  let canvas = 1l
  let pass = 2l
  let shoot = 3l
  let rematch = 4l
  let reconnect = 5l
end

type join_notice = { count : int; expires_at : int64 }

type layout_key = {
  surface : Input.surface;
  footer : Match_view.footer;
  full_time : Match_view.full_time option;
  joined : int option;
}

type t = {
  mutable surface : Input.surface;
  mutable layout : (layout_key * Match_view.t) option;
  mutable join_notice : join_notice option;
  positions : (float * float) array;
  mutable atmosphere : int;
  mutable atmosphere_elapsed : float;
}

let create () = {
  surface = { width = 720; height = 545; pixel_scale = 1.; inputs = 3 };
  layout = None;
  join_notice = None;
  positions = Array.make Match.player_count (0., 0.);
  atmosphere = 0;
  atmosphere_elapsed = 0.;
}

let visible screen = screen.surface.width > 0 && screen.surface.height > 0
let has_keyboard screen = screen.surface.inputs land 2 <> 0

(* A hidden window may go several seconds without drawing a frame. *)
let joined_players screen =
  match screen.join_notice with
  | None -> None
  | Some notice when Playbit.monotonic_ns () < notice.expires_at -> Some notice.count
  | Some _ -> screen.join_notice <- None; None

let announce_joins screen count =
  let count = count + Option.value (joined_players screen) ~default:0 in
  let expires_at = Int64.add (Playbit.monotonic_ns ()) 4_000_000_000L in
  screen.join_notice <- Some { count; expires_at }

(* The harbour animates at eight frames a second, independently of match time. *)
let animate screen elapsed =
  screen.atmosphere_elapsed <- screen.atmosphere_elapsed +. elapsed;
  if screen.atmosphere_elapsed < 0.125 then false
  else (
    screen.atmosphere_elapsed <- screen.atmosphere_elapsed -. 0.125;
    screen.atmosphere <- (screen.atmosphere + 1) mod Art.atmosphere_frames;
    true)

type hit = Hint | Result | Pitch of float * float | Outside

let hit_test screen ~x ~y =
  match screen.layout with
  | None -> Outside
  | Some (_, view) ->
      match view.action with
      | Some (bounds, action) when Pixel_layout.contains bounds x y ->
          (match action with Play -> Hint | Rematch -> Result)
      | _ ->
          match view.stadium with
          | Some fit when Pixel_layout.contains fit.box x y ->
              let x, y = Pixel_layout.to_source fit x y in
              let x, y = Art.unproject ~x ~y in
              if x >= 0. && x <= Match.width && y >= 0. && y <= Match.height
              then Pitch (x, y) else Outside
          | _ -> Outside

let shirts = [| 1; 2; 3; 7; 8; 11; 9 |]
let club = function Match.Home -> "Portside FC" | Match.Away -> "Rovers"
let restart_name = function
  | Match.Throw_in -> "Throw-in"
  | Match.Corner -> "Corner"
  | Match.Goal_kick -> "Goal kick"

let sprites screen room (game : Match.t) =
  let slot = Session.local_slot room in
  Array.map (fun (p : Match.player) ->
    let x, y = screen.positions.(p.slot) in
    let px, py = Session.display_position room p.slot in
    let moving = abs_float (px -. x) +. abs_float (py -. y) > 0.05 in
    let restart_pose = match game.phase with
      | Match.Set_piece (restart, slot)
          when p.slot = slot && game.pause > 0 && game.pause <= Match.restart_windup restart ->
          Some (if restart = Match.Throw_in then Art.Throw_windup game.pause else Art.Kick_windup game.pause)
      | Match.Playing when game.ball.last_touch = Some p.slot && game.ball.air_ticks >= Match.throw_flight - 4 ->
          Some (Art.Throw_release (Match.throw_flight - game.ball.air_ticks))
      | _ -> None in
    screen.positions.(p.slot) <- (px, py);
    Art.{ x = px; y = py; home = p.team = Match.Home; keeper = Match.keeper p.slot;
          selected = slot = Some p.slot; moving; facing_x = p.facing_x;
          facing_y = p.facing_y; number = shirts.(Match.role p.slot); restart_pose }
  ) game.players

(* The ball stays at the local dribbler's feet between network snapshots. *)
let ball_position room (game : Match.t) =
  match game.ball.owner with
  | Some slot when Session.local_slot room = Some slot ->
      let x, y = Session.display_position room slot in
      let player = game.players.(slot) in
      game.ball.x +. (x -. player.x), game.ball.y +. (y -. player.y)
  | _ -> game.ball.x, game.ball.y

let pitch screen room game () =
  let ball_x, ball_y = ball_position room game in
  let ball_y, ball_lift, ball_held = match game.phase with
    | Match.Set_piece (Match.Throw_in, slot) when game.pause > 0 && game.pause <= Match.throw_windup ->
        (ball_y +. Art.throw_reach game.pause *. game.players.(slot).facing_y,
         Art.throw_height game.pause, true)
    | _ ->
        let t = float game.ball.air_ticks /. float Match.throw_flight in
        let height = Art.nearest (27. *. t +. 48. *. t *. (1. -. t)) in
        (ball_y, height, false) in
  let celebration = match game.phase with
    | Match.Goal team -> Some (team, (Match.goal_pause - game.pause) / 3)
    | _ -> None in
  Art.draw ~players:(sprites screen room game) ~ball_x ~ball_y
    ~ball_dx:game.ball.vx ~ball_dy:game.ball.vy ~ball_lift ~ball_held ~ticks:game.tick
    ~atmosphere:screen.atmosphere ~celebration

type play_action = Attack | Defend | Play_again | Wait

let play_action room (game : Match.t) status =
  match status, game.phase with
  | Session.Full, _ -> Wait
  | _, Match.Full_time -> if Session.can_restart room then Play_again else Wait
  | _, (Match.Goal _ | Match.Set_piece _) -> Wait
  | _, Match.Kickoff _ when game.ball.owner = None -> Wait
  | _ -> if Session.has_ball room then Attack else Defend

let footer room (game : Match.t) status action ~keyboard ~notice =
  let club_label = match Session.local_slot room with
    | Some slot -> "You play for " ^ club (Match.team_of_slot slot)
    | None -> "Spectating" in
  let count = Session.human_count room in
  let participants = if count = 1 then "1 participant" else string_of_int count ^ " participants" in
  let hint, hint_action = match action with
    | Attack ->
        (if keyboard then "SPACE - tap to pass, hold to shoot" else "Tap to pass, hold to shoot"),
        Some Match_view.Play
    | Defend ->
        (if keyboard then "SPACE - tackle or switch player" else "Tap to tackle or switch player"),
        Some Match_view.Play
    | Play_again | Wait -> "", None in
  let message = match status, notice, game.phase with
    | Session.Full, _, _ -> Some "Game is full. Start a new game session"
    | _, (Some _ as notice), _ -> notice
    | Session.Connecting, _, _ -> Some "Joining session..."
    | _, _, Match.Set_piece (restart, slot) ->
        Some (restart_name restart ^ " for " ^ club (Match.team_of_slot slot))
    | Session.Offline, _, _ -> Some "Offline practice"
    | _, _, Match.Goal team -> Some ("Goal! " ^ club team)
    | _, _, Match.Kickoff _ when game.ball.owner = None -> Some "Returning for kickoff"
    | _ -> None in
  Match_view.{ club = club_label; participants; hint; action = hint_action; message }

let menu status action =
  let items = match action with
    | Attack -> [Target.pass, "Pass"; Target.shoot, "Shoot"]
    | Defend -> [Target.pass, "Tackle or switch player"]
    | Play_again -> [Target.rematch, "Kick off again"]
    | Wait -> [] in
  if status = Session.Offline then (Target.reconnect, "Reconnect") :: items else items

let render screen room ~notice =
  let game = Session.game room and status = Session.status room in
  let keyboard = has_keyboard screen in
  let action = play_action room game status in
  let footer = footer room game status action ~keyboard ~notice in
  let full_time = match game.phase with
    | Match.Full_time ->
        let replay = if not (Session.can_restart room) then Match_view.Waiting
          else if keyboard then Keyboard else Touch in
        Some Match_view.{ home_score = game.home_score; away_score = game.away_score; replay }
    | _ -> None in
  let surface = screen.surface in
  let joined = joined_players screen in
  let key = { surface; footer; full_time; joined } in
  let view = match screen.layout with
    | Some (previous, view) when previous = key -> view
    | _ ->
        let view = Match_view.create ?full_time ?joined ~width:surface.width ~height:surface.height
          ~density:surface.pixel_scale footer in
        screen.layout <- Some (key, view);
        view in
  let board () =
    let seconds = (game.ticks_left + Match.ticks_per_second - 1) / Match.ticks_per_second in
    Match_view.draw_scorebug ~home_score:game.home_score ~away_score:game.away_score ~seconds in
  let scene = View.create () in
  let canvas () =
    View.viewport_canvas scene Target.canvas ~width:view.width ~height:view.height
      ~continuous:true (Match_view.draw view ~pitch:(pitch screen room game) ~board) in
  (match menu status action with
   | [] -> canvas ()
   | items -> View.context_menu scene items canvas);
  Playbit.submit (View.finish scene)
