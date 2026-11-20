open Playbit

let sample_rate = 16000

let cheer_pcm () =
  let duration_ms = 1500 in
  let count = sample_rate * duration_ms / 1000 in
  let pcm = Bytes.create (2 * count) in
  let seed = ref 7301 and low = ref 0. and mid = ref 0. in
  let voice phase =
    1. -. 4. *. abs_float (float (phase mod sample_rate) /. float sample_rate -. 0.5) in
  let clap offset width =
    let rise = min 1. (float offset /. 12.) in
    let fall = max 0. (1. -. float offset /. float width) in
    rise *. fall *. fall in
  for i = 0 to count - 1 do
    seed := (!seed * 25173 + 13849) land 65535;
    let noise = float !seed /. 32768. -. 1. in
    low := !low +. (noise -. !low) *. 0.045;
    mid := !mid +. (noise -. !mid) *. 0.28;
    let roar = 0.72 *. !low +. 0.42 *. (!mid -. !low) +. 0.035 *. noise in
    let pitch_bend = i * i / 3400 in
    let voices =
      0.038 *. voice (i * 139 + pitch_bend)
      +. 0.028 *. voice (i * 167 + pitch_bend / 2)
      +. 0.017 *. voice (i * 229 - pitch_bend / 2) in
    let claps =
      noise *. (0.14 *. clap ((i + 24) mod 1673) 245
                +. 0.12 *. clap ((i + 382) mod 1151) 200) in
    let attack = min 1. (float i /. 620.) in
    let tail = min 1. (float (count - 1 - i) /. 11800.) in
    let sample = (roar +. voices +. claps) *. attack *. tail *. tail in
    Bytes.set_int16_le pcm (2 * i) (int_of_float (32767. *. sample))
  done;
  pcm

let sample duration_ms synth =
  let count = sample_rate * duration_ms / 1000 in
  let pcm = Bytes.create (2 * count) in
  for i = 0 to count - 1 do
    let value = max (-1.) (min 1. (synth i count)) in
    Bytes.set_int16_le pcm (2 * i) (int_of_float (32767. *. value))
  done;
  pcm

let noise seed =
  let state = ref seed in
  fun () ->
    state := (!state * 25173 + 13849) land 65535;
    float !state /. 32768. -. 1.

let triangle i hz =
  1. -. 4. *. abs_float (float (i * hz mod sample_rate) /. float sample_rate -. 0.5)

let ambient_pcm () =
  let white = noise 14123 and distant = noise 32081 in
  let surf_low = ref 0. and surf_mid = ref 0. and murmur = ref 0. in
  let pcm = sample 9000 (fun i _ ->
    let surf = white () and voices = distant () in
    surf_low := !surf_low +. (surf -. !surf_low) *. 0.015;
    surf_mid := !surf_mid +. (surf -. !surf_mid) *. 0.36;
    murmur := !murmur +. (voices -. !murmur) *. 0.085;
    let period = 3 * sample_rate in
    let wave = float ((i + sample_rate / 3) mod period) /. float period in
    let wash = max 0. (min (wave /. 0.4) ((1. -. wave) /. 0.6)) in
    let water = (!surf_low *. 0.19 +. (!surf_mid -. !surf_low) *. 0.14 +. surf *. 0.03)
                *. (0.28 +. 0.66 *. wash *. wash) in
    let swell = 0.7 +. 0.2 *. triangle i 1 in
    let stands = 0.18 *. !murmur *. swell +. 0.014 *. triangle (i + i / 23) 119 *. !murmur in
    water +. stands) in
  (* Blend the last quarter-second into the start so the loop has no click. *)
  let fade = sample_rate / 4 in
  let frames = Bytes.length pcm / 2 in
  let loop = Bytes.sub pcm (2 * fade) (2 * (frames - fade)) in
  for i = 0 to fade - 1 do
    let at = 2 * (frames - 2 * fade + i) in
    let before = Bytes.get_int16_le loop at and after = Bytes.get_int16_le pcm (2 * i) in
    Bytes.set_int16_le loop at ((before * (fade - i) + after * i) / fade)
  done;
  loop

let effect_pcm cue =
  let white = noise (8309 + Match.cue_index cue * 117) in
  let low = ref 0. in
  let duration_ms = match cue with
    | Match.Kick -> 105 | Shot -> 150 | Tackle -> 180 | Save -> 145
    | Out -> 165 | Whistle -> 360 | Cheer -> 1500 | Throw -> 190 in
  match cue with
  | Match.Cheer -> cheer_pcm ()
  | _ ->
      sample duration_ms (fun i count ->
        let hiss = white () in
        low := !low +. (hiss -. !low) *. 0.15;
        let phase = float i /. float count in
        let onset = min 1. (float i /. 28.) in
        let tail = 1. -. phase in
        let sound = match cue with
          | Match.Kick -> 0.26 *. triangle (i - i * i / 7300) 128 +. 0.19 *. !low
          | Shot -> 0.29 *. triangle (i - i * i / 8500) 91 +. 0.24 *. hiss
          | Tackle -> 0.30 *. !low +. 0.19 *. hiss *. (0.6 +. 0.4 *. triangle i 29)
          | Save -> 0.19 *. triangle (i - i * i / 7200) 104 +. 0.32 *. !low +. 0.08 *. hiss
          | Out -> 0.12 *. triangle i 420 +. 0.13 *. triangle i 620 +. 0.1 *. !low
          | Throw -> (0.28 *. (hiss -. !low) +. 0.13 *. !low) *. min 1. (phase *. 6.)
          | Whistle ->
              let breath = if phase < 0.38 || phase > 0.51 then 1. else 0. in
              let pitch = if phase < 0.5 then 1321 else 1571 in
              let time = if phase < 0.5 then i else i - count / 2 in
              let envelope = min 1. (float time /. 150.) in
              breath *. envelope *. (0.16 *. triangle (i + i / 92) pitch +. 0.048 *. hiss)
          | Cheer -> 0. in
        onset *. tail *. tail *. sound)

type track = { instance : Sound.instance; length : int64; gain : float }
type tracks = { bed : track; effects : track array; mutable looping : bool }
type output = Unopened | Opening of Audio.operation | Ready of tracks | Silent

let output = ref Unopened
let playing = ref false
let retry = ref 0.

let prepare audio ~gain pcm =
  let sound = Sound.open_pcm audio ~channel_count:1 ~sample_rate pcm in
  let nanos = Int64.div (Int64.mul (Int64.of_int (Bytes.length pcm / 2)) 1_000_000_000L)
    (Int64.of_int sample_rate) in
  { instance = Sound.instance sound 1l; length = nanos; gain }

let report error =
  log ("Match audio setup failed (" ^ string_of_int (Int32.to_int error) ^ ")")

let start track flags =
  try
    Sound.play track.instance ~gain:track.gain ~start:0L ~length:track.length ~flags;
    true
  with Host_error _ -> false

let ambience tracks =
  if !playing && not tracks.looping && !retry <= 0. then (
    tracks.looping <- start tracks.bed Sound.loop;
    retry := 0.5
  ) else if not !playing && tracks.looping then (
    (try Sound.stop tracks.bed.instance with Host_error _ -> ());
    tracks.looping <- false;
    retry := 0.
  )

let update ~elapsed ~active =
  playing := active;
  retry := max 0. (!retry -. elapsed);
  match !output with
  | Unopened when active ->
      (try output := Opening (Audio.open_begin ~preferred_sample_rate:0)
       with Host_error error -> output := Silent; report error)
  | Ready tracks -> ambience tracks
  | _ -> ()

let complete () =
  match !output with
  | Opening op ->
      (try match Audio.open_take op with
       | Audio.Pending -> ()
       | Audio.Ready audio ->
           (try
              let bed = prepare audio ~gain:0.52 (ambient_pcm ()) in
              let effects = Array.map (fun cue ->
                let gain = match cue with Match.Cheer -> 0.8 | Whistle -> 0.62 | _ -> 0.76 in
                prepare audio ~gain (effect_pcm cue)
              ) Match.cues in
              let tracks = { bed; effects; looping = false } in
              output := Ready tracks;
              ambience tracks
            with Host_error error ->
              (try Audio.close audio with Host_error _ -> ());
              output := Silent;
              report error)
       with Host_error error ->
         output := Silent;
         report error)
  | _ -> ()

let play cue =
  match !output with
  | Ready tracks -> ignore (start tracks.effects.(Match.cue_index cue) 0l)
  | _ -> ()
