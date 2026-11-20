open Pixel_layout

type action = Play | Rematch
type footer = {
  club : string;
  participants : string;
  hint : string;
  action : action option;
  message : string option;
}
type replay = Keyboard | Touch | Waiting
type full_time = { home_score : int; away_score : int; replay : replay }
type t = {
  width : int;
  height : int;
  stadium : fit option;
  scorebug : fit option;
  labels : Playbit.View.item list;
  result : Playbit.View.item option;
  action : (box * action) option;
}

let item fit image =
  let b = fit.box in
  Playbit.View.Pixels
    (Playbit.View.{x = b.x; y = b.y; width = b.w; height = b.h; color = 0l},
     fit.texels_w, fit.texels_h, pixels fit image)

let blank ~w ~h = { w; h; rgba = Bytes.make (4 * w * h) '\000' }

let paint (image : image) ~x ~y ~w ~h color =
  for row = max 0 y to min image.h (y + h) - 1 do
    for col = max 0 x to min image.w (x + w) - 1 do
      Bytes.set_int32_be image.rgba (4 * (row * image.w + col)) color
    done
  done

let text image ~x ~y ~scale color label =
  Pixel_font.draw (fun col row ->
    paint image ~x:(x + col * scale) ~y:(y + row * scale) ~w:scale ~h:scale color
  ) ~x:0 ~y:0 label

let lettering label =
  let w = Pixel_font.measure label and h = Pixel_font.height in
  let image = blank ~w ~h in
  text image ~x:0 ~y:0 ~scale:1 0xffeac3ffl label;
  image

let scorebug_width = 168
let scorebug_height = 36

let scorebug_label image center ~scale color label =
  let x = center - Pixel_font.measure label * scale / 2 in
  let y = 19 - Pixel_font.height * scale / 2 in
  text image ~x ~y ~scale color label

let scorebug_base = lazy (
  let image = blank ~w:scorebug_width ~h:scorebug_height in
  let rect x y w h = paint image ~x ~y ~w ~h in
  let label center = scorebug_label image center ~scale:1 in
  rect 3 3 165 33 0x132624ffl;
  rect 1 0 164 34 0xa98757ffl;
  rect 0 1 166 32 0xa98757ffl;
  rect 2 2 162 30 0x263b34ffl;
  rect 2 0 162 1 0xe8cb8dffl;
  rect 1 33 164 1 0x77613fffl;
  rect 5 6 23 22 0x82493effl;
  rect 5 27 23 1 0xeb7357ffl;
  label 16 0xfff7daffl "POR";
  rect 84 6 23 22 0x3c5577ffl;
  rect 84 27 23 1 0x6789dcffl;
  label 95 0xfff7daffl "ROV";
  rect 109 6 54 22 0x30493bffl;
  rect 109 27 54 1 0x58715dffl;
  label 56 0x9eae91ffl "-";
  image)

let last_scorebug = ref None

let draw_scorebug ~home_score ~away_score ~seconds =
  let key = (home_score, away_score, seconds) in
  match !last_scorebug with
  | Some (previous, pixels) when previous = key -> pixels
  | _ ->
      let base = Lazy.force scorebug_base in
      let image = { base with rgba = Bytes.copy base.rgba } in
      let score center n =
        let label = string_of_int n in
        let scale = if Pixel_font.measure label <= 11 then 2 else 1 in
        scorebug_label image center ~scale 0xfff7daffl label
      in
      score 42 home_score;
      score 70 away_score;
      let two_digits n = (if n < 10 then "0" else "") ^ string_of_int n in
      let clock = two_digits (seconds / 60) ^ ":" ^ two_digits (seconds mod 60) in
      (* Proportional glyphs sit in fixed cells so the clock never shifts. *)
      Array.iteri (fun i offset ->
        let glyph = String.make 1 clock.[i] in
        let cell_width = if i = 2 then 1 else 5 in
        let x = 111 + 2 * offset + cell_width - Pixel_font.measure glyph in
        text image ~x ~y:(19 - Pixel_font.height) ~scale:2 0xffedbbffl glyph
      ) [|0; 6; 12; 14; 20|];
      last_scorebug := Some (key, image.rgba);
      image.rgba

let result_panel full_time =
  let title, accent =
    if full_time.home_score > full_time.away_score then "Portside FC won", 0xf27958ffl
    else if full_time.home_score < full_time.away_score then "Rovers won", 0x91acedffl
    else "Tie", 0xefda95ffl in
  let prompt = match full_time.replay with
    | Keyboard -> "Press SPACE to play again"
    | Touch -> "Tap to play again"
    | Waiting -> "Waiting for the next kickoff" in
  let title_w = 2 * Pixel_font.measure title and prompt_w = Pixel_font.measure prompt in
  let w = max 152 (max title_w (prompt_w + 10) + 24) and h = 59 in
  let image = blank ~w ~h in
  let panel_w = w - 3 in
  paint image ~x:3 ~y:3 ~w:panel_w ~h:(h - 3) 0x163a2bffl;
  paint image ~x:1 ~y:0 ~w:(panel_w - 2) ~h:(h - 3) 0x748f7dffl;
  paint image ~x:0 ~y:1 ~w:panel_w ~h:(h - 5) 0x1b2d28ffl;
  paint image ~x:1 ~y:(h - 4) ~w:(panel_w - 2) ~h:1 0x121f1affl;
  paint image ~x:(panel_w / 2 - 8) ~y:8 ~w:16 ~h:2 accent;
  text image ~x:((panel_w - title_w) / 2) ~y:14 ~scale:2 0xfff7daffl title;
  let prompt_x = (panel_w - prompt_w) / 2 in
  if full_time.replay <> Waiting then
    paint image ~x:(prompt_x - 5) ~y:37 ~w:(prompt_w + 10) ~h:14 0x33493effl;
  text image ~x:prompt_x ~y:38 ~scale:1 0xdce6cbffl prompt;
  image

let result_layer ~density ~font_zoom (pitch : fit) full_time =
  let image = result_panel full_time in
  let b = pitch.box in
  let inset = min 8 (min b.w b.h / 8) in
  let area = { x = b.x + inset; y = b.y + inset; w = b.w - 2 * inset; h = b.h - 2 * inset } in
  let max_zoom = max font_zoom pitch.zoom in
  Option.map (fun (panel : fit) ->
    let center_x, center_y = Art.project ~x:(Match.width /. 2.) ~y:(Match.height /. 2.) in
    let x = b.x + center_x * b.w / (pitch.texels_w * pitch.sample) - panel.box.w / 2 in
    let y = b.y + center_y * b.h / (pitch.texels_h * pitch.sample) - panel.box.h / 2 in
    let bounds = { panel.box with
      x = max area.x (min (area.x + area.w - panel.box.w) x);
      y = max area.y (min (area.y + area.h - panel.box.h) y) } in
    let action = if full_time.replay = Waiting then None else Some (bounds, Rematch) in
    (item { panel with box = bounds } image, action)
  ) (fit ~density ~max_zoom area ~w:image.w ~h:image.h)

let join_notice count =
  let label =
    if count = 1 then "Player joined" else string_of_int count ^ " players joined"
  in
  let w = 35 + Pixel_font.measure label and h = 23 in
  let image = blank ~w ~h in
  let rect x y w h = paint image ~x ~y ~w ~h in
  rect 2 3 (w - 2) (h - 3) 0x132624ffl;
  rect 1 0 (w - 4) (h - 2) 0xa98757ffl;
  rect 0 1 (w - 2) (h - 4) 0xa98757ffl;
  rect 1 1 (w - 4) (h - 4) 0x263b34ffl;
  rect 3 0 (w - 8) 1 0xe8cb8dffl;
  let shirt x y fabric shade trim =
    rect (x + 2) y 7 2 fabric;
    rect (x + 1) (y + 1) 9 3 fabric;
    rect x (y + 2) 11 3 fabric;
    rect x (y + 4) 2 1 trim;
    rect (x + 9) (y + 4) 2 1 trim;
    rect (x + 3) (y + 2) 5 8 fabric;
    rect (x + 7) (y + 5) 1 5 shade;
    rect (x + 3) (y + 10) 5 1 shade;
    rect (x + 4) y 3 1 shade;
    rect (x + 5) (y + 1) 1 1 trim;
    rect (x + 6) (y + 3) 1 1 trim
  in
  shirt 4 3 0x719bd8ffl 0x394f72ffl 0xc6e0edffl;
  shirt 11 7 0xeb7357ffl 0x994b3fffl 0xffdeaeffl;
  rect 25 4 1 (h - 10) 0x526453ffl;
  text image ~x:29 ~y:5 ~scale:1 0xfff2d4ffl label;
  image

let join_layer ~density ~font_zoom ~width ~margin ~stadium_top scorebug stadium count =
  let image = join_notice count in
  let place (area : box) ~align ~min_zoom =
    match fit ~density ~max_zoom:font_zoom area ~w:image.w ~h:image.h with
    | Some f when f.sample = 1 && f.zoom >= min_zoom ->
        let x = match align with `Left -> area.x | `Right -> area.x + area.w - f.box.w in
        Some (item { f with box = { f.box with x } } image)
    | _ -> None
  in
  let beside_board = Option.bind scorebug (fun score ->
    let b = score.box in
    let x = b.x + b.w + 6 in
    place { x; y = b.y; w = width - margin - x; h = b.h } ~align:`Left ~min_zoom:font_zoom)
  in
  match beside_board with
  | Some _ -> beside_board
  | None ->
      (* The gap above the stadium and the first strip of scenery are clear of play. *)
      Option.bind stadium (fun pitch ->
        let b = pitch.box in
        let y = stadium_top + 2 in
        place { x = b.x + 3; y; w = b.w - 6; h = b.y + b.h / 12 - y }
          ~align:`Right ~min_zoom:1)

let wrap text max_width =
  let add (lines, current) word =
    let joined = if current = "" then word else current ^ " " ^ word in
    if current = "" || Pixel_font.measure joined <= max_width then lines, joined
    else current :: lines, word
  in
  let lines, current = List.fold_left add ([], "") (String.split_on_char ' ' text) in
  List.rev (current :: lines)

let create ?full_time ?joined ~width ~height ~density footer =
  let width = max 1 (min Playbit.max_canvas_edge width)
  and height = max 1 (min Playbit.max_canvas_edge height) in
  let margin = min 8 (width / 32) in
  let usable = width - 2 * margin in
  let font_zoom = max 1 (int_of_float (Float.round density)) in
  let points px = int_of_float (ceil (float (px * font_zoom) /. density)) in
  let measure text = points (Pixel_font.measure text) in
  let line = points Pixel_font.height + 4 in
  let left = measure footer.club and center = measure footer.participants
  and right = measure footer.hint in
  let across = max left right + 8 <= (usable - center) / 2 in
  let pair = left + 8 + center <= usable in
  let rows =
    if across then [[`Left, footer.club; `Center, footer.participants; `Right, footer.hint]]
    else if pair then [[`Left, footer.club; `Right, footer.participants]; [`Center, footer.hint]]
    else [[`Center, footer.club]; [`Center, footer.participants]; [`Center, footer.hint]]
  in
  let rows = List.filter (List.exists (fun (_, text) -> text <> "")) rows in
  let footer_y = max 0 (height - List.length rows * line - min 4 (height / 100)) in
  let labels = ref [] and action = ref None in
  let label ~align ~y ~row_h ~interactive text =
    let image = lettering text in
    let area = { x = margin; y; w = usable; h = min row_h (height - y) } in
    match fit ~density ~max_zoom:font_zoom area ~w:image.w ~h:image.h with
    | None -> ()
    | Some f ->
        let x = match align with `Left -> margin | `Center -> f.box.x | `Right -> width - margin - f.box.w in
        let b = { f.box with x } in
        labels := item { f with box = b } image :: !labels;
        if interactive then
          action := Option.map (fun a ->
            ({ x = max 0 (x - 4); y; w = b.w + 8; h = row_h }, a)) footer.action
  in
  List.iteri (fun row labels ->
    List.iter (fun (align, text) ->
      label ~align ~y:(footer_y + row * line) ~row_h:line ~interactive:(text = footer.hint) text
    ) labels) rows;
  let top = min 8 (height / 45) in
  let messages = match footer.message with
    | None -> []
    | Some text -> wrap text (max 1 (int_of_float (float usable *. density /. float font_zoom)))
  in
  let board_area = { x = 0; y = top; w = width; h = max 0 ((footer_y - top) / 4) } in
  (* Leave room for a goal call so the pitch stays under the pointer when a team scores. *)
  let stadium_top score_h = top + score_h + max 1 (List.length messages) * line + min 4 (height / 40) in
  (* Size the ground and scoreboard together: on a big display both should grow. *)
  let pitch_room = { x = 0; y = 0; w = width; h = max 0 (footer_y - top) } in
  let max_zoom = max 1 (min (device density width / Art.width)
    (device density pitch_room.h / (Art.height + scorebug_height))) in
  let scorebug =
    Option.map (fun f -> { f with box = { f.box with y = top } })
      (fit ~density ~max_zoom board_area ~w:scorebug_width ~h:scorebug_height) in
  let score_h = Option.fold ~none:0 ~some:(fun f -> f.box.h) scorebug in
  List.iteri (fun i message ->
    label ~align:`Center ~y:(top + score_h + i * line) ~row_h:line ~interactive:false message
  ) messages;
  let y = stadium_top score_h in
  let area = { x = 0; y; w = width; h = max 0 (footer_y - y - min 3 (height / 50)) } in
  let stadium = fit ~density ~max_zoom:max_int area ~w:Art.width ~h:Art.height in
  let result, action = match full_time, stadium with
    | Some full_time, Some pitch ->
        (match result_layer ~density ~font_zoom pitch full_time with
         | Some (result, action) -> (Some result, action)
         | None -> (None, None))
    | _ -> (None, !action) in
  let labels = List.rev !labels in
  let labels = match joined with
    | Some count when count > 0 ->
        (match join_layer ~density ~font_zoom ~width ~margin ~stadium_top:y scorebug stadium count with
         | Some notice -> notice :: labels
         | None -> labels)
    | _ -> labels
  in
  { width; height; stadium; scorebug; labels; result; action }

let draw layout ~pitch ~board =
  let add_layer fit w h pixels layers =
    match fit with
    | None -> layers
    | Some fit -> item fit { w; h; rgba = pixels () } :: layers
  in
  let layers = match layout.result with
    | None -> layout.labels
    | Some panel -> panel :: layout.labels
  in
  let layers = add_layer layout.scorebug scorebug_width scorebug_height board layers in
  let layers = add_layer layout.stadium Art.width Art.height pitch layers in
  Playbit.View.Rect
    { x = 0; y = 0; width = layout.width; height = layout.height; color = 0x1c3330ffl }
  :: layers
