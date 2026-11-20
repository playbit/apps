module Input = Playbit.Input
module Event_kind = Playbit.EventKind
module Target = Screen.Target

let room = Session.create ()
let controls = Controls.create ()
let book = Scorebook.create ()
let screen = Screen.create ()
let needs_redraw = ref true
let pressing_hint = ref false

let join () = Session.start room
let connect () = Scorebook.open_session book join

let reset_controls () =
  pressing_hint := false;
  Controls.clear controls

let restart () =
  if (Session.game room).phase = Match.Full_time && Session.can_restart room then (
    reset_controls ();
    Session.restart room)

let click target =
  if target = Target.reconnect then connect ()
  else if target = Target.rematch then restart ()
  else if Session.local_slot room <> None then (
    if target = Target.pass then controls.pass <- true
    else if target = Target.shoot then controls.shoot <- true)

let key (event : Input.key) =
  match event.key with
  | 1 | 2 | 3 | 4 -> Controls.arrow controls event.key ~pressed:event.pressed
  | 5 when (Session.game room).phase = Match.Full_time -> if event.pressed then restart ()
  | 5 when event.pressed -> Controls.press controls ~has_ball:(Session.has_ball room)
  | 5 -> Controls.release controls
  | _ -> ()

let pointer (event : Input.pointer) =
  let hit = Screen.hit_test screen ~x:event.x ~y:event.y in
  match event.phase, hit with
  | 1, Screen.Result -> restart ()
  | 1, Hint ->
      pressing_hint := true;
      Controls.press controls ~has_ball:(Session.has_ball room)
  | (2 | 3), _ when !pressing_hint ->
      pressing_hint := false;
      if event.phase = 2 && hit = Hint then Controls.release controls
      else controls.button <- Up
  | (0 | 1), Pitch (x, y) when event.buttons = 1 && not !pressing_hint
                               && (Session.game room).phase <> Match.Full_time ->
      controls.destination <- Some (x, y)
  | _ -> ()

let frame elapsed =
  let game = Session.game room in
  let player = Option.map (fun slot -> game.players.(slot)) (Session.local_slot room) in
  let input = Controls.frame controls elapsed player in
  Session.frame room elapsed input;
  if Screen.animate screen elapsed || input.dx <> 0. || input.dy <> 0. then
    needs_redraw := true

let update (event : Playbit.event) =
  let is kind = event.kind = Int32.of_int kind in
  (* A frame can advance the current game in place. A network event can replace it. *)
  let previous = Session.game room in
  let tick = previous.tick and phase = previous.phase in
  let status = Session.status room and slot = Session.local_slot room in
  let notice = book.notice in
  Session.event room event;
  let joined = Session.take_joins room in
  if joined > 0 then (
    Screen.announce_joins screen joined;
    needs_redraw := true);
  let elapsed =
    if is Event_kind.frame && event.target = Target.canvas then Input.frame event.data else 0. in
  if is Event_kind.started then connect ()
  else if is Event_kind.surface_changed then (
    screen.surface <- Input.surface event.data;
    if not (Screen.visible screen) then reset_controls ();
    needs_redraw := true)
  else if is Event_kind.data_ready then (
    Match_audio.complete ();
    Scorebook.complete book join)
  else if is Event_kind.click then (click event.target; needs_redraw := true)
  else if event.target = Target.canvas then (
    if is Event_kind.key then key (Input.key event.data)
    else if is Event_kind.pointer then pointer (Input.pointer event.data)
    else if is Event_kind.frame then frame elapsed);

  let game = Session.game room in
  let visible = Screen.visible screen in
  Match_audio.update ~elapsed ~active:(visible && game.phase <> Match.Full_time);
  if visible && game.tick > tick then
    Array.iteri (fun i at ->
      if at > tick && game.tick - at <= 15 then Match_audio.play Match.cues.(i)
    ) game.cue_ticks;
  (* The authority can be a reader. Each seated writer saves the same final score. *)
  let next_status = Session.status room and next_slot = Session.local_slot room in
  if next_status = Session.Online && phase <> Match.Full_time && game.phase = Match.Full_time then
    Scorebook.full_time book game;
  if Option.map Match.team_of_slot slot <> Option.map Match.team_of_slot next_slot then
    reset_controls ();
  if game != previous || game.tick <> tick || status <> next_status || slot <> next_slot
     || is Event_kind.presence_changed || notice <> book.notice then
    needs_redraw := true

let turn () =
  let rec drain remaining =
    if remaining > 0 then
      match Playbit.poll () with
      | None -> ()
      | Some event -> update event; drain (remaining - 1)
  in
  drain Playbit.max_queued_events;
  if !needs_redraw then (
    Screen.render screen room ~notice:book.notice;
    needs_redraw := false)

let () =
  Playbit.quit_on_last_window true;
  Playbit.run turn
