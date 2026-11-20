let width = 512
let height = 384
let stride = width * 4
let length = stride * height

let nearest n = int_of_float (if n >= 0. then n +. 0.5 else n -. 0.5)
let origin_x = 74.
let origin_y = 82.
let scale = 3.6

let project ~x ~y = (nearest (origin_x +. scale *. x), nearest (origin_y +. scale *. y))
let unproject ~x ~y = ((x -. origin_x) /. scale, (y -. origin_y) /. scale)

let rgba_write buf i r g b a =
  Bytes.unsafe_set buf i r;
  Bytes.unsafe_set buf (i + 1) g;
  Bytes.unsafe_set buf (i + 2) b;
  Bytes.unsafe_set buf (i + 3) a

let pixel buf x y color =
  if x >= 0 && x < width && y >= 0 && y < height then
    rgba_write buf (y * stride + x * 4)
      (Char.chr (color lsr 16)) (Char.chr (color lsr 8 land 255))
      (Char.chr (color land 255)) '\255'

let span ?(alpha = '\255') buf y x0 x1 color =
  if y >= 0 && y < height then (
    let r = Char.chr (color lsr 16) in
    let g = Char.chr (color lsr 8 land 255) in
    let b = Char.chr (color land 255) in
    let start = y * stride in
    for x = max 0 x0 to min (width - 1) x1 do
      rgba_write buf (start + x * 4) r g b alpha
    done)

let rect buf x y w h color =
  for row = y to y + h - 1 do
    span buf row x (x + w - 1) color
  done

let line buf (x0, y0) (x1, y1) color =
  let dx = abs (x1 - x0) and dy = -(abs (y1 - y0)) in
  let sx = if x0 < x1 then 1 else -1 in
  let sy = if y0 < y1 then 1 else -1 in
  let rec visit x y error =
    pixel buf x y color;
    if x <> x1 || y <> y1 then (
      let twice = 2 * error in
      let next_x, error = if twice >= dy then (x + sx, error + dy) else (x, error) in
      let next_y, error = if twice <= dx then (y + sy, error + dx) else (y, error) in
      visit next_x next_y error)
  in
  visit x0 y0 (dx + dy)

let polygon ?(alpha = '\255') buf color points =
  let points = Array.of_list points in
  let count = Array.length points in
  let top = Array.fold_left (fun n (_, y) -> min n y) height points in
  let bottom = Array.fold_left (fun n (_, y) -> max n y) 0 points in
  for y = max top 0 to min bottom (height - 1) do
    let left = ref max_int and right = ref min_int in
    for i = 0 to count - 1 do
      let x0, y0 = points.(i) and x1, y1 = points.((i + 1) mod count) in
      if y >= min y0 y1 && y <= max y0 y1 then (
        if y0 = y1 then (
          left := min !left (min x0 x1);
          right := max !right (max x0 x1))
        else (
          let x = x0 + (y - y0) * (x1 - x0) / (y1 - y0) in
          left := min !left x;
          right := max !right x))
    done;
    span ~alpha buf y !left !right color
  done

let ellipse buf x y rx ry color =
  for row = -ry to ry do
    let ratio = float row /. float ry in
    let dx = Float.to_int (float rx *. sqrt (max 0. (1. -. ratio *. ratio))) in
    span buf (y + row) (x - dx) (x + dx) color
  done

let hash a b =
  let n = a * 196613 lxor (b * 131101) in
  let n = (n lxor (n lsr 11)) * 21929 in
  (n lxor (n lsr 13)) land 16383

let ink = 0x1f2c27
let coral = 0xeb7357
let lapis = 0x6789dc
let white = 0xfff7da

let tile_grass buf =
  rect buf 0 0 width height 0x568453;
  (* Irregular islands of longer grass gather against the paths and trees. *)
  for y = 0 to height / 19 do
    for x = 0 to width / 28 do
      let n = hash x y in
      let px = x * 28 + n mod 11 and py = y * 19 + (n / 13) mod 9 in
      for i = 0 to 6 + n mod 8 do
        let b = hash n i in
        let xx = px + b mod 22 and yy = py + (b / 23) mod 8 in
        span buf yy xx (xx + 1 + b mod 4) 0x60905a;
        if i mod 4 = 0 then (
          pixel buf xx (yy - 1) 0x779950;
          pixel buf (xx + 3) (yy - 1) 0x416f4b)
      done
    done
  done

let paving buf x y w h =
  rect buf (x + 2) (y + 3) w h 0x456f3e;
  rect buf x y w h 0xb19d77;
  let stones = [|0xd7bf92; 0xdbc399; 0xceb58b; 0xd4bb92; 0xcbb38b|] in
  for row = 0 to (h - 2) / 6 do
    let top = y + 1 + row * 6 in
    let bottom = min (top + 4) (y + h - 2) in
    let rec lay col left =
      if left < x + w - 1 then (
        let n = hash (x + col) (y + row) in
        let next = left + 7 + n mod 7 in
        let a = max (x + 1) left and b = min (x + w - 2) (next - 2) in
        if a <= b && top <= bottom then (
          rect buf a top (b - a + 1) (bottom - top + 1) stones.(n mod Array.length stones);
          span buf top (min (a + 1) b) b 0xe3cda1;
          if n mod 4 = 0 && b - a > 4 then (
            span buf (min (top + 2) bottom) (a + 1) (a + 2) 0xc1ac85;
            pixel buf b bottom 0xb8a47e));
        lay (col + 1) next)
    in
    lay 0 (x + 1 - (row mod 2) * 5)
  done;
  span buf y x (x + w - 1) 0xe0c799

let tide = [|-4; -3; -2; -1; 0; 1; 2; 4; 5; 5; 4; 3;
             3; 2; 2; 1; 1; 0; -1; -1; -2; -3; -4; -4|]
let atmosphere_frames = Array.length tide

let ocean buf phase =
  let headlands = [|30; 38; 34; 26; 32; 39; 31; 28; 37; 34; 26; 31; 38|] in
  let shelf_rocks = [6, 36, 5; 13, 57, 3; 5, 153, 3; 14, 157, 5;
                     4, 252, 4; 10, 332, 6; 17, 340, 3] in
  for y = 0 to height - 1 do
    let i = y / 32 and a = y mod 32 in
    let sand = headlands.(i) + (headlands.(i + 1) - headlands.(i)) * a / 32 in
    (* Interpolate both swells alongshore. The crest stays continuous between bays. *)
    let t = phase + y / 12 and frac = y mod 12 in
    let swell offset =
      let i = (t + offset) mod atmosphere_frames in
      (tide.(i) * (12 - frac) + tide.((i + 1) mod atmosphere_frames) * frac) / 12 in
    let edge = min 43 (sand + swell 0) in
    span buf y 0 45 0xe0c58c;
    span buf y 0 (min 44 (sand + 6)) 0xc1ad81;
    span buf y 0 edge 0x95c7ab;
    span buf y 0 (edge - 2) 0x6bb5a4;
    span buf y 0 (sand - 7 + swell 5) 0x469ea2;
    span buf y 0 (sand - 15 + swell 10) 0x348499;
    span buf y 0 (sand - 22 + swell 16) 0x316e91;
    let grain = hash (y / 2) 61 in
    if grain mod 7 < 2 then pixel buf (sand + 6 + grain mod 5) y 0xc7ab7c;
    let broken = (y + 3 * (y / 21)) mod 13 in
    if swell 0 >= 0 && broken < 11 then (
      span buf y (edge - 1) edge 0xc8ddbe;
      if swell 0 >= 3 && broken < 6 then pixel buf (edge + 1) y 0xf1ecd2)
    else if broken < 7 then pixel buf edge y 0xa6cbaf;
    if swell 9 >= 1 && (y + phase / 4) mod 16 < 11 then (
      let curl = sand - 10 + swell 7 + abs (7 - y mod 15) / 3 in
      pixel buf curl y 0x84c4b4;
      if swell 9 >= 3 && y mod 9 < 3 then pixel buf (curl - 1) y 0xa6d5be)
  done;
  List.iter (fun (x, y, r) ->
    ellipse buf (x + 1) (y + 2) (r + 2) 3 0x4b999a;
    polygon buf 0x326d79 [x-r, y; x-2, y-3; x+2, y-3; x+r, y; x+r-2, y+2; x-r+1, y+2];
    span buf (y - 2) (x - 2) (x + 1) 0x80a99c;
    span buf (y - 1) (x - r + 2) x 0x649795;
    span buf (y + 3) (x - 2) (x + 2) 0x85beb1
  ) shelf_rocks;
  for i = 0 to 83 do
    let n = hash i 617 in
    let wave = tide.((phase + i) mod atmosphere_frames) in
    let x = n mod 20 + wave / 2
    and y = (n / 37 * 17) mod height in
    let light = if wave >= 3 then 0x86c0af else 0x51989c in
    span buf y x (x + 2 + n mod 4) light;
    if n mod 3 = 0 then span buf (y - 1) (x + 4) (x + 6) light
  done

let harbor_light buf phase =
  let x = 20 and y = 108 in
  let shape color points =
    polygon buf color (List.map (fun (dx,dy) -> x + dx, y + dy) points) in
  let block dx dy w h = rect buf (x+dx) (y+dy) w h in
  let beam ax ay bx by = line buf (x+ax,y+ay) (x+bx,y+by) in
  (* The tide runs through the cracks in the basalt footing. *)
  shape 0x568c8d [-14,4; -9,-1; 6,-4; 16,0; 23,1; 25,6; 11,9; -5,11; -15,8];
  List.iter (fun (dx,dy,w,h) ->
    shape 0x4b6066 [dx-w,dy; dx-w+2,dy-h; dx+1,dy-h-2;
                     dx+w,dy-2; dx+w-1,dy+2; dx+1,dy+4; dx-w+1,dy+2];
    shape 0x88928a [dx-w+1,dy-1; dx-w+3,dy-h; dx+1,dy-h-1;
                     dx+w-2,dy-2; dx+1,dy; dx-w+3,dy+1];
    beam (dx-w+3) (dy-h+1) dx (dy-h-1) 0xbab69b;
    if (phase / 4 + dx + 32) mod 4 < 2 then
      beam (dx-w+2) (dy+3) (dx-1) (dy+4) 0xc2d5b9
  ) [-9,5,6,3; 13,4,6,3; 3,7,7,4; 20,3,4,2; -2,2,10,5];
  (* Three facets carry the sunlight from warm limewash into sea-blue shade. *)
  shape 0x4c5960 [-10,1; -7,-26; -6,-31; 6,-31; 8,-26; 11,0; 8,3; -6,4];
  shape 0xdaccab [-9,0; -6,-28; -3,-29; -3,1; -6,2];
  shape 0xb5b7a8 [-3,1; -3,-29; 3,-29; 5,1];
  shape 0x859c9a [5,1; 3,-29; 6,-28; 9,-1];
  shape 0xb76b59 [-7,-22; -3,-23; -3,-16; -8,-15];
  shape 0xa65d51 [-3,-23; 4,-23; 5,-16; -3,-16];
  shape 0x81565a [4,-23; 6,-22; 7,-16; 5,-16];
  block (-3) (-11) 5 10 0x817a6b;
  block (-2) (-10) 3 8 0x48595b;
  block (-1) (-25) 2 3 0x465b61;
  beam (-7) (-7) (-5) (-7) 0xa59d89;
  block (-9) (-32) 18 3 0x3d595e;
  block (-9) (-33) 18 1 0xc8bfa1;
  block (-6) (-42) 13 10 0x3f646a;
  block (-4) (-41) 3 7 0x93b9a9;
  block 0 (-41) 3 7 0x71a49f;
  block 4 (-41) 2 7 0x56858a;
  block (-2) (-39) 3 4 0xebcd86;
  block (-2) (-38) 2 2 0xffe5a5;
  beam (-8) (-36) 8 (-36) 0x5a6765;
  List.iter (fun dx -> block dx (-36) 1 4 0x566463) [-8; 0; 8];
  shape 0x344f58 [-9,-42; -8,-44; -4,-46; -2,-49; 2,-49; 4,-46; 7,-45; 9,-42];
  shape 0x5c8b83 [-7,-44; -3,-46; -1,-48; 1,-48; 1,-45; -2,-43];
  beam (-7) (-43) 7 (-43) 0x83a393;
  block 0 (-52) 1 4 0x705f4c;
  block (-1) (-52) 3 2 0xddbb76

let quay buf =
  rect buf 46 0 4 height 0x73664c;
  paving buf 48 0 17 height;
  for i = 0 to height / 10 do
    let y = i * 10 in
    rect buf 46 y 4 8 0xb4a477;
    span buf y 46 49 0xe0c792
  done

let pier buf =
  rect buf 13 181 45 32 0x315f60;
  rect buf 9 177 47 31 0x714e3a;
  for y = 178 to 205 do
    let c = match (y - 178) mod 5 with
      | 0 -> 0xe1ba7f | 4 -> 0x885f49 | _ -> 0xbc8d60 in
    span buf y 10 55 c
  done;
  for i = 0 to 15 do
    let n = hash i 193 in
    let x = 12 + n mod 34 and y = 180 + 5 * (i mod 5) in
    span buf y x (x + 2 + n mod 5) 0xac7a56
  done;
  List.iter (fun (x, y) ->
    rect buf x y 5 7 0x674c3c;
    rect buf x y 4 4 0xe3bd80;
    rect buf (x + 1) (y + 1) 2 2 0xa77a53
  ) [9, 174; 9, 203; 31, 174; 31, 203];
  line buf (11, 176) (19, 179) 0x785e48;
  line buf (19, 179) (33, 176) 0xc5ac76

let skiff buf phase =
  let x = 17 + tide.((phase + 5) mod atmosphere_frames) / 3
  and y = 227 + tide.(phase) / 4 in
  let hull color outline =
    polygon buf color (List.map (fun (dx, dy) -> x + dx, y + dy) outline) in
  line buf (12, 207) (x - 2, y - 13) 0x816b47;
  ellipse buf (x + 3) (y + 2) 6 14 0x316f78;
  hull 0x374f4e [0,-15; 4,-11; 6,-3; 6,7; 4,13; -3,13; -5,8; -5,-4; -3,-11];
  hull 0xd7b67f [0,-13; 3,-9; 4,-2; 4,7; 3,10; -3,10; -4,5; -4,-3; -2,-9];
  hull 0x796148 [0,-9; 2,-6; 3,0; 3,7; -2,7; -3,3; -2,-5];
  span buf (y - 3) (x - 3) (x + 3) 0xdec79a;
  span buf (y + 4) (x - 3) (x + 3) 0xcba975;
  line buf (x - 3, y + 6) (x + 3, y - 6) 0xdfc28a;
  line buf (x + 3, y - 6) (x + 4, y - 8) 0xefe1ad;
  span buf (y + 11) (x - 2) (x + 3) 0x548f88

(* The lobes and the gaps between leaf sprays are drawn at sprite resolution.
   Each bough has the same light direction, even as its silhouette bends. *)
let leaf_spray = [|
  "    23    3     ";
  "   3553 3433    ";
  "  34553344432   ";
  " 3554345544333  ";
  " 34443454332432 ";
  "344433443234432 ";
  "2344333323443322";
  " 233234433333232";
  "  3244333222322 ";
  "  233332234322  ";
  "   32222333321  ";
  "    21  23221   ";
  "          21    ";
|]

let branch_clusters =
  [|(-17, -25, 7, 7); (-16, -16, 7, 6); (14, -16, 8, 7);
    (16, -24, 8, 8); (17, -33, 5, 5); (8, -35, 8, 7);
    (-13, -34, 7, 6); (-5, -37, 7, 6); (2, -35, 7, 8);
    (6, -25, 9, 9); (-7, -26, 9, 8); (-2, -16, 8, 8);
    (8, -12, 6, 5); (-11, -11, 5, 5); (-4, -9, 4, 4)|]

let wind = [|0; 0; 1; 2; 3; 3; 2; 1; 0; -1; -2; -1|]
let wind_frames = Array.length wind

(* Flat-topped, salt-pruned pines. Each needle pad has a long lip of sun
   over a dark hollow. The trunk stays planted as the outer pads flex. *)
let pine_pad = [|
  "           44         ";
  "       44554443       ";
  "     454444344433     ";
  "   445443344343433    ";
  "   344344443333234 3  ";
  " 3443433343322333223  ";
  "333333323323333222322 ";
  " 22233223322322221222 ";
  "   22222122221122 21  ";
  "     21   21  12      ";
|]

let sea_pine buf gust x y seed =
  let palette = [|0x294e4d; 0x305b51; 0x3d7158;
                  0x598754; 0x7c9e57; 0xa9b862|] in
  let shape color points =
    polygon buf color (List.map (fun (dx,dy) -> x + dx, y + dy) points) in
  let beam ax ay bx by = line buf (x+ax,y+ay) (x+bx,y+by) in
  shape 0x4c5140 [-7,2; -3,-3; -2,-12; -6,-20; -5,-29; -2,-29;
                   -2,-22; 2,-15; 4,-8; 3,-2; 7,2; 3,3; 0,1];
  beam (-5) 0 0 (-6) 0xa88958;
  beam 0 (-6) 1 (-12) 0xb29462;
  beam 1 (-13) (-4) (-21) 0x9c8156;
  shape 0x55553e [0,-13; 7,-16; 13,-22; 16,-24; 14,-19; 8,-14; 2,-11];
  beam 3 (-14) 9 (-17) 0xa58b5b;
  shape 0x52533f [-2,-16; -10,-19; -17,-24; -18,-27; -12,-22; -4,-20];
  beam (-13) (-23) (-8) (-20) 0x9c8156;
  let pad (dx,dy,w,h,shade) =
    let bend = gust * (-dy) / 32 in
    let px = x + dx + bend + (seed mod 3 - 1) * (-dy / 16) in
    let py = y + dy in
    for row = 0 to h - 1 do
      let needles = pine_pad.(row * Array.length pine_pad / h) in
      for col = 0 to w - 1 do
        let v = needles.[col * String.length needles / w] in
        if v <> ' ' then
          pixel buf (px + col - w / 2) (py + row)
            palette.(max 0 (Char.code v - Char.code '0' - shade))
      done
    done in
  List.iter pad [12,-32,21,12,1; -14,-31,25,11,1; 12,-27,28,10,1;
                 2,-42,29,13,0; -12,-38,24,10,0; -3,-34,28,11,0;
                 -14,-27,24,9,0; 9,-23,24,8,1]

let tree buf phase x y =
  let seed = hash x y in
  let gust = wind.((phase + (width - x) / 38 + y / 95) mod wind_frames) in
  let palette = match seed mod 3 with
    | 0 -> [|0x30584d; 0x3b7054; 0x558651; 0x799c52; 0xa4b660; 0xcfcd76|]
    | 1 -> [|0x30584d; 0x3d6e57; 0x558964; 0x77a46b; 0x9abd78; 0xc8d186|]
    | _ -> [|0x385d48; 0x537449; 0x708c4d; 0x93a258; 0xb5b966; 0xd9cd7c|]
  in
  let sx = x + 5 + gust / 2 in
  ellipse buf (sx + 2) (y + 2) 17 6 0x4b7c49;
  ellipse buf (sx - 6) (y + 1) 12 5 0x4b7c49;
  ellipse buf (sx + 10) (y - 1) 10 5 0x4b7c49;
  ellipse buf sx (y - 1) 13 5 0x416e43;
  List.iter (fun (dx,dy) ->
    span buf (y + dy) (sx + dx) (sx + dx + 2) 0x628d4e
  ) [8,5; 12,3; 15,4; -9,4; -12,2];
  if List.mem (x, y) [409,56; 494,112; 111,379; 411,372; 472,377] then
    sea_pine buf gust x y seed
  else (
  polygon buf 0x644b39
    [x - 6, y + 3; x - 3, y - 2; x - 2, y - 19; x + gust, y - 27;
     x + 5 + gust, y - 26; x + 4, y - 2; x + 8, y + 2];
  line buf (x - 5, y + 2) (x - 1, y - 3) 0xa2774c;
  line buf (x - 1, y - 3) (x + 1, y - 18) 0xc18d55;
  line buf (x + 2, y - 9) (x + 3, y - 17) 0x866044;
  line buf (x + 3, y + 1) (x + 7, y + 2) 0x4d4833;
  let bend dy = gust * min 36 (-dy) / 25 in
  line buf (x, y - 11) (x - 14 + bend (-21), y - 21) 0x72563c;
  line buf (x + 1, y - 15) (x + 16 + bend (-26), y - 26) 0x624b37;
  let cluster i (dx, dy, rx, ry) =
    let n = hash seed i in
    let dx = dx + n mod 5 - 2 and dy = dy + (n / 5) mod 3 - 1 in
    let cy = y + dy - (if i mod 3 = 0 then gust / 2 else 0) in
    (dx, dy, x + dx + bend dy, cy, rx, ry) in
  (* A cool heartwood shadow connects the boughs. *)
  Array.iteri (fun i branch ->
    let _, _, cx, cy, rx, ry = cluster i branch in
    ellipse buf cx cy (rx - 1) (ry - 1) palette.(1)
  ) branch_clusters;
  Array.iteri (fun i branch ->
    let dx, dy, cx, cy, rx, ry = cluster i branch in
    let shade = (if dx + dy < -29 then 0 else 1) + (if dy > -17 && dx > -8 then 1 else 0) in
    let w = rx * 2 + 3 and h = ry * 2 + 2 in
    for row = 0 to h - 1 do
      let spray = leaf_spray.(row * Array.length leaf_spray / h) in
      for col = 0 to w - 1 do
        let leaf = spray.[col * String.length spray / w] in
        if leaf <> ' ' then
          pixel buf (cx - rx + col) (cy - ry + row)
            palette.(max 0 (Char.code leaf - Char.code '0' - shade))
      done
    done
  ) branch_clusters;
  let leaf_phase = (phase + seed) mod wind_frames in
  if seed mod 3 = 0 && leaf_phase >= 4 && leaf_phase <= 9 then (
    let lx = x + 16 + leaf_phase and ly = y - 26 + (leaf_phase - 4) / 2 in
    span buf ly lx (lx + 1) 0xc1d971;
    pixel buf (lx + 1) (ly + 1) 0x719f48))

let flowers buf x y =
  ellipse buf (x + 3) (y + 6) 6 2 0x527c47;
  List.iter (fun (dx, dy, petal) ->
    let px = x + dx and py = y + dy in
    line buf (px, py + 1) (px + 1, y + 7) 0x406e43;
    span buf (py + 4) (px - 2) px 0x85aa60;
    span buf py (px - 1) (px + 1) petal;
    pixel buf px (py - 1) petal;
    pixel buf px (py + 1) 0xd39166
  ) [0,4,0xf1d692; 4,1,0xe5ae95; 8,4,0xf4d9a5]

let planter buf x y =
  polygon buf 0x477447 [x,y+2; x+26,y+2; x+29,y+6; x+27,y+9; x+3,y+9];
  rect buf x y 26 8 0x867357;
  rect buf (x + 1) (y + 1) 24 4 0x705e48;
  span buf (y + 5) x (x + 25) 0xd6bb86;
  for i = 0 to 6 do
    let px = x + 2 + i * 3 and py = y - hash i x mod 4 in
    ellipse buf (px + 1) (py + 2) 3 2 0x497746;
    span buf (py + 1) px (px + 2) 0x80a75a;
    span buf py px (px + 2) (if i mod 3 = 1 then 0xefa982 else 0xd78974);
    pixel buf (px + 1) (py - 1) 0xf3c593
  done

let clubhouse buf x y =
  let block dx dy w h = rect buf (x + dx) (y + dy) w h in
  let beam ax ay bx by = line buf (x + ax, y + ay) (x + bx, y + by) in
  let shape color points =
    polygon buf color (List.map (fun (dx, dy) -> x + dx, y + dy) points) in
  shape 0x426f4c [4,21; 78,25; 78,32; 74,34; 74,55; 11,57; 6,52];
  block 5 24 66 30 0x574a40;
  block 7 25 62 25 0xd8b887;
  shape 0xbca58b [7,28; 68,28; 68,33; 44,33; 41,35; 26,32; 7,31];
  block 7 44 62 6 0xbf9f78;
  for i = 0 to 7 do
    let dx = 8 + 8 * i in
    block dx 46 7 2 (if i mod 3 = 0 then 0xa58c70 else 0xc7ab82);
    block (dx + 3) 49 4 1 0xe0c798
  done;
  List.iter (fun dx ->
    block dx 27 3 23 0x815c47;
    block dx 31 1 17 0xab7c54
  ) [7; 27; 47; 66];
  List.iter (fun dx ->
    block dx 33 12 10 0x71584b;
    block (dx + 1) 34 10 8 0x365958;
    beam (dx + 1) 38 (dx + 5) 34 0x6f9e95;
    beam (dx + 5) 40 (dx + 10) 35 0x55847f;
    block (dx + 5) 34 1 9 0xb89364;
    block dx 38 12 1 0xc7a875;
    block (dx - 2) 33 2 10 0x6f8775;
    block (dx + 12) 33 2 10 0x536e62;
    block (dx - 1) 43 14 2 0xefcf96
  ) [13; 50];
  block 32 32 13 20 0x624a3d;
  block 34 34 9 16 0x926642;
  block 35 35 6 5 0x527b72;
  block 36 41 1 8 0xac8055;
  block 41 43 1 2 0xefce81;
  block 29 51 18 2 0xdcc69f;
  block 27 53 22 2 0x9f927a;
  shape 0x674a48 [0,24; 11,0; 64,0; 76,22; 76,28; 72,30; 0,29];
  shape 0xaa5f50 [2,23; 12,2; 63,2; 73,23; 73,26; 2,26];
  let tile = [|0xbb7056; 0xc67d5d; 0xcf8764; 0xb46552; 0xcd8460|] in
  let cool_tile = [|0xa26758; 0xb0725e; 0xb87860; 0x985d54; 0xa86a58|] in
  for row = 0 to 5 do
    let top = 2 + 4 * row and inset = max 3 (12 - row * 2) in
    for col = 0 to 10 do
      let left = col * 7 + (row mod 2) * 3 - 3 in
      let a = max inset left and b = min (74 - inset) (left + 6) in
      if a <= b then (
        let n = hash col row in
        let sheltered = a >= 60 - row * 2 in
        let clay = if sheltered then cool_tile else tile in
        block a top (b - a + 1) 3 clay.(n mod Array.length tile);
        if b > a then (
          block (a + 1) (top + 3) (b - a) 1 0x975549;
          block a (top + 1) 1 2 (if sheltered then 0xc58568 else 0xe49a6b);
          block b (top + 2) 1 1 0xaa5b4c))
    done
  done;
  beam 2 23 12 1 0xecac79;
  beam 64 2 74 23 0x935248;
  block 12 0 52 2 0xe9ab77;
  block 2 26 72 2 0xd49667;
  for i = 0 to 11 do block (3 + i * 6) 28 2 2 0x795446 done;
  (* A timber gable sits over the entrance, with the club's compass in its loft. *)
  shape 0x674c43 [22,27; 38,10; 54,27; 51,30; 25,30];
  shape 0xe2c18a [26,27; 38,14; 50,27];
  beam 22 26 38 9 0x855248;
  beam 23 26 38 10 0xf1b87d;
  beam 39 11 53 26 0xb26b50;
  block 30 27 16 2 0x94724e;
  ellipse buf (x + 38) (y + 22) 4 4 0x987752;
  ellipse buf (x + 38) (y + 22) 3 3 0x446a68;
  beam 37 23 40 20 0xf1d18d;
  block 16 (-2) 6 9 0x8c715f;
  block 16 (-2) 4 8 0xc6a584;
  block 15 (-3) 8 3 0xb68a6c;
  block 15 (-3) 7 1 0xe3c098;
  (* Salt-hardy ivy follows the sheltered corner below the eaves. *)
  beam 70 50 66 31 0x5a694c;
  List.iter (fun (dx,dy,w) ->
    block dx dy w 2 0x587551;
    block (dx-1) (dy-1) (max 1 (w-1)) 2 0x7f945a;
    block dx (dy-1) 1 1 0xa8ad67
  ) [66,32,3; 64,34,3; 67,35,4; 67,38,3; 65,41,2;
     69,42,3; 68,45,4; 70,48,3; 67,49,4]

let stands buf =
  let x = 175 and y = 16 and w = 208 in
  rect buf (x + 3) (y + 4) w 48 0x466f48;
  rect buf x y w 48 0x675849;
  rect buf (x + 2) (y + 2) (w - 4) 43 0x8c7a62;
  for row = 0 to 2 do
    let py = y + 3 + row * 13 in
    rect buf (x + 4) py (w - 8) 10 0xb39c79;
    span buf (py + 3) (x + 4) (x + w - 5) 0xa18a69;
    span buf (py + 8) (x + 4) (x + w - 5) 0xd9ba87;
    span buf (py + 10) (x + 4) (x + w - 5) 0x776857;
    List.iter (fun dx ->
      rect buf (x + dx) py 6 11 0xa59c85;
      span buf (py + 3) (x + dx) (x + dx + 5) 0xd6c8a2;
      span buf (py + 8) (x + dx) (x + dx + 5) 0xd6c8a2
    ) [6; 71; 135; 198]
  done;
  rect buf x y w 3 0x786250;
  span buf y (x + 1) (x + w - 2) 0xe3c997;
  rect buf x (y + 41) w 8 0x7e6954;
  for i = 0 to 24 do
    rect buf (x + 4 + i * 8) (y + 43) 6 5 0x977e5e
  done;
  span buf (y + 41) x (x + w - 1) 0xe2c895;
  List.iter (fun dx ->
    rect buf (x + dx) (y + 38) 4 13 0x695b4a;
    rect buf (x + dx) (y + 38) 2 10 0xd8bd85;
    span buf (y + 37) (x + dx - 1) (x + dx + 3) 0xedd5a0
  ) [1; 68; 137; w - 5];
  List.iter (fun (dx, color) ->
    let bx = x + dx and by = y + 41 in
    polygon buf 0x64584b [bx,by; bx+16,by; bx+16,by+5; bx+8,by+9; bx,by+6];
    polygon buf color [bx,by; bx+15,by; bx+15,by+4; bx+8,by+7; bx,by+5];
    line buf (bx+4,by+2) (bx+8,by+5) 0xf0d6a0;
    line buf (bx+8,by+5) (bx+12,by+2) 0xf0d6a0
  ) [24,0xb15f50; 88,0xb15f50; 116,0x577c99; 169,0x577c99];
  (* Two lengths of sailcloth are stretched between the terrace masts. The
     warm canvas faces the harbor and the blue canvas faces the tree line. *)
  for bay = 0 to 1 do
    let left = x + 2 + 102 * bay and span_w = 101 in
    let body, sun, seam, shade = if bay = 0 then
      (0xc48a6c,0xe4b181,0xd9a578,0x9c7161)
      else (0x6a9190,0x9cb8a5,0x83a49a,0x526f75) in
    let sag dx = dx * (span_w - dx) / 460 in
    for dx = 1 to span_w - 1 do
      let bow = sag dx in
      let aft = 1 + bow / 2 and fore = 8 + bow in
      for row = aft to fore - 1 do
        let bias = (row - aft) * (dx - 50) / 38 in
        let fold = (dx + 25 + bias) mod 25 in
        let tone = if fold < 2 then seam else if fold >= 24 then shade
                   else if row < aft + 2 && fold < 15 then seam else body in
        pixel buf (left+dx) row tone
      done;
      pixel buf (left+dx) aft sun;
      pixel buf (left+dx) (fore-1) seam;
      rect buf (left+dx) fore 1 (if dx mod 25 < 5 then 2 else 3) shade
    done;
    let pennants = [|0xd5a16f; 0xe2c999; 0x8fa9a2; 0xa98670|] in
    for i = 0 to 3 do
      let dx = i * 25 + 13 and by = 11 + sag (i * 25 + 13) in
      polygon buf pennants.((i + bay) mod 4)
        [left+dx,by; left+dx+5,by; left+dx+2,by+4]
    done
  done;
  List.iter (fun dx ->
    rect buf (x + dx) 2 2 14 0x846b51;
    span buf 2 (x + dx - 1) (x + dx + 2) 0xe8ce93
  ) [2; 104; 206]

let fan_colors = [|0x699275; 0xcf944f; 0x9c6172; 0xe8d3a0|]
let fan_skin = [|0xe6b07e; 0xce966a; 0xa7704f; 0xf4cc97|]
let fan_hair = [|0x58443c; 0x805744; 0x494642; 0xb99060; 0x9c9a84|]
let jump = [|0; 2; 4; 3; 1; 0|]

let crowd buf phase celebration =
  for row = 0 to 2 do
    let y = 19 + row * 13 in
    for col = 0 to 24 do
      let n = hash col row in
      if col mod 8 <> 0 && n mod 6 <> 0 then (
        let x = 182 + col * 8 + row mod 2 in
        let home = col <= 12 in
        let skin = fan_skin.((n lsr 5) mod Array.length fan_skin) in
        let shirt = if n mod 4 < 2 then (if home then 0xc45847 else 0x4a76bb)
                    else fan_colors.((n / 6) mod Array.length fan_colors) in
        let cheer = match celebration with
          | Some (team, age) ->
              let own_team = (team = Match.Home) = home in
              let origin = if team = Match.Home then 6 else 19 in
              let beat = age - abs (col - origin) / 4 - row mod 2 in
              if beat >= 0 && (own_team || n mod 5 = 0) then Some beat else None
          | None -> None in
        let idle = (phase + n / 11) mod wind_frames in
        let lift = match cheer with
          | Some beat -> jump.((beat + n mod 3) mod Array.length jump)
          | None -> if idle < 2 then 1 else 0 in
        let top = y - lift in
        let lean = if cheer = None && idle >= 7 && idle <= 9 then 1 else 0 in
        let hair = fan_hair.((n / 13) mod Array.length fan_hair) in
        let head = x + lean in
        span buf (y + 10) (x - 1) (x + 5) 0x84735a;
        rect buf (x - 1) (top + 6) 6 4 0x565649;
        rect buf (x - 1) (top + 5) 6 3 shirt;
        rect buf x (top + 7) 4 2 shirt;
        pixel buf x (top + 9) 0x424b46;
        pixel buf (x + 3) (top + 9) 0x424b46;
        rect buf head (top + 1) 4 4 hair;
        rect buf (head + 1) (top + 2) 3 3 skin;
        pixel buf (head + 3) (top + 3) 0x99684f;
        (match n mod 5 with
         | 0 -> span buf (top + 1) (head - 1) (head + 4) 0xc9ab79;
                span buf top head (head + 2) 0xefd19b
         | 1 -> span buf top (head + 1) (head + 3) hair;
                pixel buf (head + 4) (top + 4) hair
         | 2 -> span buf (top + 1) head (head + 4) shirt
         | _ -> pixel buf (head + 1) top hair);
        if n mod 4 = 0 then (
          span buf (top + 5) x (x + 3) 0xe6cca0;
          pixel buf (x + 2) (top + 6) 0xe6cca0);
        if cheer <> None then (
          line buf (x, top + 6) (x - 2, top + 2) shirt;
          line buf (x + 3, top + 6) (x + 5, top + 2) shirt;
          pixel buf (x - 2) (top + 1) skin;
          pixel buf (x + 5) (top + 1) skin)
        else if idle = 3 && n mod 5 = 0 then (
          pixel buf (x - 1) (top + 4) shirt;
          pixel buf (x - 2) (top + 3) skin))
    done
  done;
  Option.iter (fun (team, age) ->
    let color, left = match team with Match.Home -> (coral, 181) | Match.Away -> (lapis, 285) in
    let banner offset delay =
      if age >= delay then (
        let x = left + offset in
        let swing = wind.(((age - delay) * 2) mod wind_frames) in
        let tip_x = x + swing and top = 8 - swing / 2 in
        line buf (x, 25) (tip_x, top) 0x594631;
        line buf (x + 1, 25) (tip_x + 1, top) 0xf1db9a;
        polygon buf ink
          [tip_x + 1, top; tip_x + 17, top + 2; tip_x + 13, top + 7; tip_x + 2, top + 8];
        polygon buf color
          [tip_x + 2, top + 1; tip_x + 15, top + 3; tip_x + 12, top + 6; tip_x + 2, top + 7];
        span buf (top + 4) (tip_x + 4) (tip_x + 7) white)
    in
    banner 28 1;
    banner 72 3;
    for i = 0 to 13 do
      let n = hash i 802 in
      let time = age - n mod 6 in
      if time >= 0 then (
        let x = left + 9 + n mod 73 + time in
        let y = 5 + (n / 79) mod 23 + time * time / 8 in
        let tint = if i mod 3 = 0 then white else color in
        pixel buf x y tint;
        if (time + i) mod 3 = 0 then pixel buf (x + 1) y tint)
    done
  ) celebration

let wall buf x y w =
  rect buf (x + 2) (y + 3) w 8 0x457542;
  rect buf x y w 8 0x837154;
  rect buf x y w 5 0xc6b28a;
  span buf y x (x + w - 1) 0xe3cb9a;
  for col = 1 to (w - 1) / 12 do
    let xx = x + 12 * col in
    rect buf xx (y + 1) 1 4 0xa89370;
    rect buf (xx - 6) (y + 6) 1 2 0xa29271;
    let n = hash col x in
    if n mod 3 <> 0 then span buf (y + 2) (xx - 8) (xx - 6 + n mod 3) 0xd4be93;
    if n mod 4 = 0 then span buf (y + 5) (xx - 4) (xx - 1) 0xa69975
  done;
  List.iter (fun xx ->
    rect buf xx (y - 1) 5 10 0x968369;
    rect buf xx (y - 1) 4 7 0xcebb92;
    span buf (y - 1) xx (xx + 3) 0xebd6a8
  ) [x; x + w - 5]

let sign buf =
  let label = "NORTH QUAY" in
  let w = Pixel_font.measure label + 16 in
  let x = (width - w) / 2 and y = 340 in
  let a = x - 5 and b = x + w + 4 in
  let arch dx = 334 + (dx - width/2) * (dx - width/2) / 290 in
  for dx = a to b do
    let top = arch dx in
    rect buf dx top 1 3 0x405b55;
    pixel buf dx top 0xccb483;
    if (dx-a) mod 7 = 2 then rect buf dx (top+2) 1 (y-top) 0x4a6159
  done;
  List.iter (fun px ->
    polygon buf 0x68815b [px-3,357; px+4,355; px+14,361; px+13,365; px+2,364];
    rect buf (px-4) 338 9 23 0x8e8773;
    rect buf (px-3) 338 5 22 0xc7b794;
    for row = 0 to 3 do
      let py = 342 + row * 5 in
      span buf py (px-3) (px+3) 0xa79c84;
      pixel buf (px - 2 + (row mod 2) * 3) (py-1) 0xb1a68b
    done;
    rect buf (px-5) 358 11 4 0xa69c83;
    span buf 358 (px-5) (px+5) 0xddcba4;
    rect buf (px-6) 337 13 3 0x7c7c6c;
    span buf 337 (px-6) (px+6) 0xe1cea3;
    polygon buf 0xaaa68a [px-5,336; px-3,333; px+2,332; px+5,336];
    line buf (px-5,336) (px-2,333) 0xe7d5ae
  ) [a;b];
  polygon buf 0x665c4a [x-1,y+1; x+2,y-1; x+w-3,y-1; x+w,y+1;
                         x+w,y+12; x+w-3,y+15; x+2,y+15; x-1,y+12];
  rect buf x y w 13 0xcbb185;
  rect buf (x+1) (y+1) (w-2) 11 0x365f59;
  span buf (y+1) (x+3) (x+w-4) 0x578076;
  List.iter (fun dx -> pixel buf (x+dx) (y+6) 0xdfc797) [2;w-3];
  Pixel_font.draw (fun col row -> pixel buf col row 0xf5dc9f)
    ~x:(x + 8) ~y:(y + 1) label

let pitch buf =
  let left, top = project ~x:0. ~y:0. in
  let right, bottom = project ~x:Match.width ~y:Match.height in
  let w = right - left and h = bottom - top in
  rect buf (left - 3) (top - 3) (w + 7) (h + 8) 0x46733f;
  rect buf (left - 2) (top - 2) (w + 5) (h + 5) 0x62934c;
  (* A bank of sunlight falls across the western half. Both directions of cut
     use the same ramp, so the temperature of the light crosses every stripe. *)
  let turf = [|0x4d8e65; 0x529465; 0x589966; 0x5f9d66; 0x67a366;
               0x6ea767; 0x77ab67; 0x7eaf68; 0x86b469; 0x8db96b|] in
  let field_light x y =
    let cloud = (x - 124) * (x - 124) / 130 + (y - 67) * (y - 67) / 170 in
    let west = 53 - (x + y / 2) / 12 in
    max 14 (min 83 (west + max 0 (34 - cloud / 4))) in
  for py = 0 to h - 1 do
    for px = 0 to w - 1 do
      let n = hash (px / 3) (py / 2) in
      let cut = 1 - (px * 8 / w) mod 2 in
      let light = field_light px py + cut * 10 + n mod 5 - 2 in
      pixel buf (left + px) (top + py) turf.(min 9 (light / 10))
    done
  done;
  for row = 0 to h / 7 do
    for col = 0 to w / 7 do
      let n = hash col row in
      let dx = col * 7 + n mod 6 and dy = row * 7 + (n / 7) mod 6 in
      if dx + 2 < w && dy < h && n mod 4 = 0 then (
        let cut = 1 - (dx * 8 / w) mod 2 in
        let tone = min 9 (field_light dx dy / 10 + cut + 1) in
        span buf (top + dy) (left + dx) (left + dx + 1) turf.(tone))
    done
  done;
  (* Wear follows the keeper's short runs across the face of each goal. *)
  List.iter (fun (goal_x, inward) ->
    let middle = (top + bottom) / 2 in
    for i = 0 to 165 do
      let n = hash i (833 + goal_x) in
      let dx = 2 + n mod 22 and dy = (n / 23) mod 55 - 27 in
      if dx * dx * 2 + dy * dy < 780 then (
        let px = goal_x + inward * dx and py = middle + dy in
        let worn, moss, blade = if inward > 0 then (0xb0b16d,0x9daf69,0x91ad68)
                                else (0x8f9e69,0x799d66,0x659964) in
        let tone = if dx * dx * 3 + dy * dy < 240 && n mod 3 <> 0 then worn
                   else if n mod 2 = 0 then moss else blade in
        span buf py px (px + n mod 3) tone)
    done
  ) [left,1; right,-1];
  let chalk = 0xf1e6b9 in
  let mark x0 y0 x1 y1 =
    let ax, ay = project ~x:x0 ~y:y0 and bx, by = project ~x:x1 ~y:y1 in
    line buf (ax, ay) (bx, by) chalk;
    if ax = bx then line buf (ax + 1, ay) (bx + 1, by) 0xbaca8e
    else if ay = by then line buf (ax, ay + 1) (bx, by + 1) 0xbaca8e
  in
  let box x0 y0 x1 y1 =
    mark x0 y0 x1 y0; mark x1 y0 x1 y1;
    mark x1 y1 x0 y1; mark x0 y1 x0 y0 in
  box 0. 0. Match.width Match.height;
  mark 50. 0. 50. Match.height;
  box 0. 14. 15. 50.;
  box 85. 14. Match.width 50.;
  box 0. 23. 5. 41.;
  box 95. 23. Match.width 41.;
  let arc x y radius steps (cos_step, sin_step) (cos_start, sin_start) =
    let point cos_a sin_a =
      project ~x:(x +. radius *. cos_a) ~y:(y +. radius *. sin_a) in
    let rec draw n cos_a sin_a =
      if n > 0 then (
        let next_cos = cos_a *. cos_step -. sin_a *. sin_step in
        let next_sin = sin_a *. cos_step +. cos_a *. sin_step in
        line buf (point cos_a sin_a) (point next_cos next_sin) chalk;
        draw (n - 1) next_cos next_sin)
    in
    draw steps cos_start sin_start
  in
  let penalty_step = (0.992808636, 0.119712207) in
  let corner_step = (0.965925826, 0.258819045) in
  arc Match.center_x Match.center_y Match.center_circle 64 (0.995184727, 0.098017140) (1., 0.);
  arc 10.5 32. 8. 16 penalty_step (0.573519986, -0.819191568);
  arc 89.5 32. 8. 16 penalty_step (-0.573519986, 0.819191568);
  arc 0. 0. 2. 6 corner_step (1., 0.);
  arc Match.width 0. 2. 6 corner_step (0., 1.);
  arc Match.width Match.height 2. 6 corner_step (-1., 0.);
  arc 0. Match.height 2. 6 corner_step (0., -1.);
  List.iter (fun x ->
    let px, py = project ~x ~y:32. in
    rect buf (px - 1) (py - 1) 3 3 chalk
  ) [10.5; 50.; 89.5]

let goal buf x side =
  let front, y0 = project ~x ~y:Match.goal_top in
  let _, y1 = project ~x ~y:Match.goal_bottom in
  let back = front + side * 15 in
  polygon buf 0x4c7550 [front,y0+2; back+2,y0+6; back+2,y1+1; front,y1+3];
  polygon buf 0x618364 [front,y0; back,y0+4; back,y1-3; front,y1];
  (* The back stanchions pull the mesh into a shallow trapezoid. *)
  for depth = 1 to 4 do
    let dx = depth * 15 / 4 in
    let px = front + side * dx in
    line buf (px, y0 + depth) (px, y1 - depth + 1) 0x98ab8c
  done;
  let rows = max 1 ((y1 - y0) / 5) in
  for i = 0 to rows do
    let a = y0 + i * (y1 - y0) / rows in
    let b = y0 + 4 + i * (y1 - y0 - 7) / rows in
    line buf (front, a) (back, b) 0xc1c9aa
  done;
  line buf (back, y0 + 4) (back, y1 - 3) 0xd2d4b3;
  line buf (front, y0) (front, y1) 0xf1eed0;
  List.iter (fun (y, rear) ->
    line buf (front, y + 1) (back, rear + 1) 0x778976;
    line buf (front, y) (back, rear) white;
    rect buf (front - 1) (y - 1) 3 3 0xd3d4ad;
    pixel buf front y white
  ) [y0,y0+4; y1,y1-3]

let refreshment_cart buf x y =
  let block dx dy w h = rect buf (x + dx) (y + dy) w h in
  ellipse buf (x + 15) (y + 26) 14 3 0x4c794a;
  List.iter (fun dx ->
    ellipse buf (x + dx) (y + 24) 3 4 0x505346;
    ellipse buf (x + dx) (y + 24) 1 2 0xb09b73;
    block dx 5 2 17 0x846948
  ) [4; 20];
  block 2 16 23 7 0xa67d56;
  block 3 18 21 4 0xc8a473;
  block 1 16 25 2 0xe1bd85;
  List.iter (fun dx -> block dx 13 2 3 0xd39763; block dx 12 2 1 0xf4d599) [5; 8; 11];
  block 16 13 5 3 0x628c86;
  block 16 12 5 1 0xc7cab0;
  for row = 0 to 8 do
    let inset = (8 - row) / 3 in
    for col = inset to 26 - inset do
      let band = (col + inset) / 5 mod 2 in
      let color = match band, row >= 6 with
        | 0, false -> 0xd7c799 | 0, true -> 0xb3a37f
        | _, false -> 0x719c92 | _, true -> 0x547e76 in
      pixel buf (x + col) (y + row) color
    done
  done;
  for i = 0 to 4 do
    block (i * 5 + 1) 9 4 2 (if i mod 2 = 0 then 0xc9b98e else 0x628d81)
  done

let flag buf x y side =
  let x, y = project ~x ~y in
  span buf (y + 1) (x - 1) (x + 3) 0x507543;
  line buf (x, y) (x, y - 10) 0xf2d6a0;
  polygon buf coral [(x + side, y - 10); (x + side * 8, y - 8); (x + side, y - 5)];
  pixel buf (x + side) (y - 10) 0xfaa376

let stadium = lazy (
  let buf = Bytes.make length '\000' in
  tile_grass buf;
  quay buf;
  pier buf;
  paving buf 64 63 419 14;
  paving buf 449 77 19 243;
  paving buf 63 321 420 14;
  paving buf 241 335 30 49;
  clubhouse buf 81 8;
  stands buf;
  pitch buf;
  wall buf 65 71 146;
  wall buf 301 71 153;
  wall buf 66 319 163;
  wall buf 283 319 171;
  List.iter (fun (x, y) -> flowers buf x y)
    [391, 57; 465, 54; 482, 142; 477, 237; 66, 343;
     179, 345; 198, 366; 301, 362; 324, 347; 441, 345];
  planter buf 202 349;
  planter buf 283 349;
  refreshment_cart buf 478 181;
  goal buf 0. (-1);
  goal buf Match.width 1;
  List.iter (fun (x, y, side) -> flag buf x y side)
    [0., 0., 1; Match.width, 0., -1; 0., Match.height, 1; Match.width, Match.height, -1];
  sign buf;
  span buf 0 0 (width - 1) 0x608469;
  span buf (height - 1) 0 (width - 1) 0x1e3933;
  line buf (0, 0) (0, height - 1) 0x254945;
  line buf (width - 1, 0) (width - 1, height - 1) 0x254945;
  buf)

let tree_sites =
  [442, 48; 409, 56; 478, 64; 494, 112; 494, 298;
   150, 369; 411, 372; 361, 377; 472, 377; 111, 379]

(* Keep each animation restricted to the parts of the map it can affect. *)
let scenery_frames count regions draw =
  Array.init count (fun phase -> lazy (
    let buf = Bytes.copy (Lazy.force stadium) in
    draw buf phase;
    List.map (fun (x, y, w, h) ->
      let row_bytes = w * 4 in
      let pixels = Bytes.create (h * row_bytes) in
      for row = 0 to h - 1 do
        Bytes.blit buf ((y + row) * stride + x * 4) pixels (row * row_bytes) row_bytes
      done;
      (x, y, row_bytes, h, pixels)
    ) regions))

let tree_patches =
  scenery_frames wind_frames
    [371, 1, width - 372, 124; 459, 247, width - 460, 64; 76, 319, width - 77, height - 320]
    (fun buf phase -> List.iter (fun (x, y) -> tree buf phase x y) tree_sites)

let ocean_patches =
  scenery_frames atmosphere_frames [1, 1, 45, height - 2]
    (fun buf phase -> ocean buf phase; harbor_light buf phase; skiff buf phase; pier buf)

let paint_patches buf patches =
  List.iter (fun (x, y, row_bytes, h, pixels) ->
    for row = 0 to h - 1 do
      Bytes.blit pixels (row * row_bytes) buf ((y + row) * stride + x * 4) row_bytes
    done
  ) (Lazy.force patches)

let frame = lazy (Bytes.create length)

type restart_pose = Throw_windup of int | Throw_release of int | Kick_windup of int

type player = {
  x : float;
  y : float;
  home : bool;
  keeper : bool;
  selected : bool;
  moving : bool;
  facing_x : float;
  facing_y : float;
  number : int;
  restart_pose : restart_pose option;
}

let throw_heights = [|6; 9; 13; 17; 21; 24; 27; 30; 32; 33; 33; 31; 29; 27|]
let throw_height remaining = throw_heights.(Match.throw_windup - remaining)

let throw_reach remaining =
  if remaining >= 11 then 0.
  else if remaining >= 4 then -0.7
  else -. 0.18 *. float (remaining - 1)

let skin_tones = [|
  (0xefd2a0, 0xbe8a5e, 0x41352f);
  (0xb98259, 0x7d5b43, 0x2e2b27);
  (0xd8a575, 0x99654b, 0x594334);
  (0xf0bc8a, 0xb6815f, 0x3b312e);
  (0xa57250, 0x694b3e, 0x302c29);
  (0xebbc8c, 0xaf7558, 0x74513c);
  (0xd69a69, 0xa16e4f, 0x362d29);
|]

let shadow buf x y p =
  ellipse buf (x + 3) (y + 1) 7 3 0x548658;
  ellipse buf (x + 1) (y + 1) 4 2 0x447252;
  if p.selected then (
    ellipse buf x (y + 1) 8 4 (if p.home then 0xffe590 else 0xbcecf5);
    ellipse buf x (y + 1) 6 2 0x578d4c)

(* Heads are authored at playing size. The nose, ear and far cheek each have
   their own silhouette; opposing profiles use the same animation anchors. *)
let profile_head = [|
  "   hhhh  ";
  "  htthhh ";
  " hhthhhh ";
  " hhhdsss ";
  " hhsesps ";
  " hhsdssef";
  "  hdsssd ";
  "   fdssf ";
  "    fd   ";
|]
let front_head = [|
  "   hhh   ";
  "  htthhh ";
  " hhthhhhd";
  " hhdssssd";
  " dhsssess";
  " shpsspsd";
  "  dsssfs ";
  "   dssd  ";
  "    dd   ";
|]
let back_head = [|
  "   hhhh  ";
  "  htthhh ";
  " hhththhh";
  " hhhhhhhd";
  " hhhhhhhh";
  " shhhhdhs";
  "  dhhhhd ";
  "   dhhd  ";
  "    dd   ";
|]

let face_lights = [|
  0x78553d,0xf4dcb1; 0x55483e,0xd5a477; 0x946845,0xe1b78c;
  0x6b4937,0xf8cca1; 0x504638,0xbb8e67; 0xa38252,0xf3cd9b;
  0x69503d,0xe0af81
|]

let sprite buf x y p ticks =
  let skin, shade, hair = skin_tones.(p.number mod Array.length skin_tones) in
  let light, jersey, dark, shorts =
    if p.keeper && p.home then (0xffe690, 0xdca84c, 0x8b633c, 0x444a40)
    else if p.keeper then (0xa6f0d5, 0x5fb7a2, 0x36776b, 0x3a4d4d)
    else if p.home then (0xffa17d, 0xe96953, 0x9b433f, 0xf5e5c0)
    else (0xb2ccff, 0x638fda, 0x405e9b, 0x344f77)
  in
  let profile = abs_float p.facing_x > abs_float p.facing_y in
  let side = if p.facing_x < 0. then -1 else 1 in
  let back = not profile && p.facing_y < 0. in
  let shifting = p.moving && (ticks / 3 + p.number) mod 2 = 0 in
  let body = y - (if shifting then 1 else 0) in
  let foot hip offset py =
    let px = x + offset in
    line buf (x + hip, body - 6) (px + 1, py - 4) shade;
    line buf (x + hip - 1, body - 6) (px, py - 4) skin;
    rect buf px (py - 5) 2 2 shade;
    pixel buf px (py - 5) skin;
    rect buf px (py - 3) 2 2 light;
    pixel buf (px + 1) (py - 2) jersey;
    span buf (py - 1) px (px + 2) 0x423e35;
    span buf py (px - (if profile && side < 0 then 1 else 0)) (px + 2) ink;
    pixel buf (px + (if side < 0 then 0 else 2)) (py - 1) 0x908674
  in
  let advance = if profile && p.moving then side * 2 else 0 in
  (match p.restart_pose with
   | Some (Kick_windup remaining) ->
       let swing = if remaining > 4 then -4 else 7 - remaining in
       let dx = nearest (float swing *. p.facing_x) and dy = nearest (float swing *. p.facing_y) in
       foot (-2) (-4) y;
       line buf (x + 2, body - 6) (x + dx + 2, y + dy - 1) skin;
       foot 2 (dx + 1) (y + dy)
   | _ ->
       let step lifted = y - (if p.moving && lifted then 2 else 0) in
       foot (-2) (-4 + (if shifting then advance else -advance)) (step shifting);
       foot 2 (1 + (if shifting then -advance else advance)) (step (not shifting)));
  span buf (body - 8) (x - 3) (x + 3) ink;
  rect buf (x - 4) (body - 7) 8 2 shorts;
  span buf (body - 5) (x - 4) (x - 2) (if p.home && not p.keeper then 0xc7b99a else dark);
  span buf (body - 5) (x + 1) (x + 3) (if p.home && not p.keeper then 0xc7b99a else dark);
  pixel buf (x + 4) (body - 6) dark;
  pixel buf x (body - 6) dark;
  let arm offset lift =
    let py = body - 13 - lift and px = x + offset in
    rect buf px py 3 2 dark;
    span buf py px (px + 1) light;
    rect buf px (py + 2) 2 3 shade;
    pixel buf px (py + 2) skin;
    span buf (py + 4) px (px + 1) (if p.keeper then white else skin)
  in
  let arms_free = match p.restart_pose with
    | Some (Throw_windup _ | Throw_release _) -> false | _ -> true in
  if arms_free then (
    if profile then arm (if side < 0 then -5 else 3) (if shifting then 1 else 0)
    else (
      arm (-6) (if shifting then 1 else 0);
      arm 4 (if p.moving && not shifting then 1 else 0)));
  span buf (body - 14) (x - 2) (x + 2) jersey;
  rect buf (x - 4) (body - 13) 9 4 jersey;
  rect buf (x - 3) (body - 10) 7 2 jersey;
  span buf (body - 13) (x - 4) (x - 1) light;
  pixel buf (x - 3) (body - 12) light;
  rect buf (x + 4) (body - 12) 1 3 dark;
  pixel buf (x + 3) (body - 9) ink;
  span buf (body - 9) (x - 3) (x + 3) dark;
  if back then (
    span buf (body - 12) (x - 1) x light;
    pixel buf x (body - 11) white)
  else (
    pixel buf (if profile then x + 2 * side else x) (body - 13) white;
    pixel buf (if profile then x + side else x - 2) (body - 11) light);
  if profile && arms_free then (
    let shoulder = x - 2 * side in
    let elbow = shoulder - side * (if shifting then 3 else 1) in
    let hand = elbow + side * (if shifting then 1 else 3) in
    line buf (shoulder,body-12) (elbow,body-9) dark;
    line buf (shoulder-side,body-12) (elbow-side,body-9) jersey;
    line buf (elbow,body-8) (hand,body-9) shade;
    line buf (elbow,body-9) (hand,body-10) (if p.keeper then white else skin));
  let hx = if profile then x + side else x in
  let drawing = if profile then profile_head else if back then back_head else front_head in
  let hair_light, skin_light = face_lights.(p.number mod Array.length face_lights) in
  Array.iteri (fun row cells ->
    String.iteri (fun col detail ->
      if detail <> ' ' then (
        let color = match detail with
          | 'h' -> hair | 't' -> hair_light | 's' -> skin
          | 'd' -> shade | 'e' -> skin_light | 'f' -> hair_light | _ -> ink in
        let dx = (col - 4) * (if profile then side else 1) in
        pixel buf (hx + dx) (body - 22 + row) color)
    ) cells
  ) drawing;
  let draw_throw_arms hand_y =
    List.iter (fun side ->
      let shoulder_x = x + side * 4 and shoulder_y = body - 12 in
      let elbow_x = x + side * 7 and elbow_y = (body - 12 + hand_y) / 2 in
      let hand_x = x + side * 5 in
      line buf (shoulder_x + side, shoulder_y) (elbow_x + side, elbow_y) shade;
      line buf (shoulder_x, shoulder_y - 1) (elbow_x, elbow_y) skin;
      line buf (elbow_x + side, elbow_y) (hand_x + side, hand_y) shade;
      line buf (elbow_x, elbow_y) (hand_x, hand_y) skin;
      line buf (shoulder_x, shoulder_y) (shoulder_x + side, shoulder_y - 3) jersey;
      pixel buf (shoulder_x + side) (shoulder_y - 3) light;
      rect buf (hand_x - 1) (hand_y - 1) 3 3 shade;
      rect buf (hand_x - 1) (hand_y - 1) 2 2 skin
    ) [-1; 1] in
  (match p.restart_pose with
   | Some (Throw_windup remaining) ->
       let reach = nearest (scale *. throw_reach remaining *. p.facing_y) in
       draw_throw_arms (y + reach - throw_height remaining - 2)
   | Some (Throw_release elapsed) -> draw_throw_arms (body - 24 + elapsed * 2)
   | _ -> ());
  if p.selected then (
    let color = if p.home then 0xffe590 else 0xc9f3ff in
    let offset = match p.restart_pose with
      | Some (Throw_windup remaining) -> max 30 (throw_height remaining + 14)
      | _ -> 30 in
    let top = y - offset in
    polygon buf ink [(x - 5, top); (x + 5, top); (x, top + 6)];
    polygon buf color [(x - 3, top + 1); (x + 3, top + 1); (x, top + 4)])

let ball_panels = [|
  "   111   "; "  24432  "; " 2444442 "; "134411431"; "144411431";
  "133443321"; " 2333212 "; "  21122  "; "   111   "
|]
let ball_inks = [|0x253c3e; 0x879587; 0xd4d7b8; 0xfff8dd|]

let ball_sprite buf x y =
  Array.iteri (fun row panels ->
    String.iteri (fun col value ->
      if value <> ' ' then
        pixel buf (x+col-4) (y+row-4) ball_inks.(Char.code value - Char.code '1')
    ) panels
  ) ball_panels

let draw ~players ~ball_x ~ball_y ~ball_dx ~ball_dy ~ball_lift ~ball_held ~ticks ~atmosphere ~celebration =
  let buf = Lazy.force frame in
  Bytes.blit (Lazy.force stadium) 0 buf 0 length;
  paint_patches buf ocean_patches.(atmosphere);
  paint_patches buf tree_patches.(atmosphere mod wind_frames);
  crowd buf atmosphere celebration;
  let bx, by = project ~x:ball_x ~y:ball_y in
  let objects = Array.map (fun p ->
    let x, y = project ~x:p.x ~y:p.y in
    shadow buf x y p;
    (y, x, p)) players in
  if not ball_held then ellipse buf bx (by + 1) 5 2 0x365a40;
  Array.sort (fun (y, _, _) (y', _, _) -> Int.compare y y') objects;
  Array.iter (fun (y, x, p) -> sprite buf x y p ticks) objects;
  if ball_dx *. ball_dx +. ball_dy *. ball_dy > 2. then (
    let tx, ty = project ~x:(ball_x -. ball_dx) ~y:(ball_y -. ball_dy) in
    line buf (tx, ty - 2 - ball_lift) (bx, by - 2 - ball_lift) 0xbecc90);
  (* Keep the ball readable in a crowded goalmouth. *)
  ball_sprite buf bx (by - 3 - ball_lift);
  buf
