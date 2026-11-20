(* One action button serves both keyboard and touch. With the ball it waits
   for a tap or hold; off the ball it tackles immediately on press. *)
type button = Up | Holding of float | Handled

type t = {
  arrows : bool array;
  mutable destination : (float * float) option;
  mutable button : button;
  mutable pass : bool;
  mutable shoot : bool;
}

let create () =
  { arrows = Array.make 4 false; destination = None; button = Up;
    pass = false; shoot = false }

let clear t =
  Array.fill t.arrows 0 4 false;
  t.destination <- None;
  t.button <- Up;
  t.pass <- false;
  t.shoot <- false

let press t ~has_ball =
  match t.button with
  | Up when has_ball -> t.button <- Holding 0.
  | Up -> t.pass <- true; t.button <- Handled
  | Holding _ | Handled -> ()

let release t =
  (match t.button with Holding _ -> t.pass <- true | Up | Handled -> ());
  t.button <- Up

(* Playbit key codes 1–4 are left, right, up and down, respectively. *)
let arrow t code ~pressed =
  t.arrows.(code - 1) <- pressed;
  if pressed then t.destination <- None

let direction t (player : Match.player) =
  match t.destination with
  | Some (x, y) ->
      let dx = x -. player.x and dy = y -. player.y in
      if dx *. dx +. dy *. dy < 0.49 then (t.destination <- None; 0., 0.)
      else dx, dy
  | None ->
      let axis positive negative =
        float (Bool.to_int t.arrows.(positive) - Bool.to_int t.arrows.(negative))
      in
      axis 1 0, axis 3 2

let shoot_hold = 0.16

let frame t elapsed player =
  (match t.button with
   | Holding held when held +. elapsed >= shoot_hold ->
       t.shoot <- true;
       t.button <- Handled
   | Holding held -> t.button <- Holding (held +. elapsed)
   | Up | Handled -> ());
  let dx, dy = match player with None -> 0., 0. | Some p -> direction t p in
  let length = sqrt (dx *. dx +. dy *. dy) in
  let dx, dy = if length = 0. then 0., 0. else dx /. length, dy /. length in
  let input = Match.{ dx; dy; pass = t.pass; shoot = t.shoot } in
  t.pass <- false;
  t.shoot <- false;
  input
