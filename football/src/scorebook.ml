open Playbit

(* Resolving the scorebook opens the session volume used by the live channel.
   A reader can join the match; only a writer needs to keep the entry handle. *)
type state =
  | Closed
  | Opening of Data.operation * bool
  | Read_only
  | Writable of Data.entry
  | Saving of Data.entry * Data.operation

type t = { mutable state : state; mutable notice : string option }

let create () = { state = Closed; notice = None }

let unavailable book ready =
  book.state <- Closed;
  book.notice <- Some "Scorebook unavailable";
  ready ()

let open_session book ready =
  match book.state with
  | Opening _ -> ()
  | Read_only | Writable _ | Saving _ -> ready ()
  | Closed ->
      try
        let shared = Data.volume_info Data.Session in
        let op = Data.resolve shared.volume "scorebook" in
        book.state <- Opening (op, shared.can_write)
      with Host_error _ -> unavailable book ready

let save_error = Some "Couldn’t save the final score"

let complete book ready =
  match book.state with
  | Opening (op, writable) ->
      (match Data.take_entry op with
       | Data.Pending -> ()
       | Data.Ready entry ->
           book.state <- (if writable then Writable entry else Read_only);
           book.notice <- None;
           ready ()
       | exception Host_error _ -> unavailable book ready)
  | Saving (entry, op) ->
      (match Data.take_put op with
       | Data.Pending -> ()
       | Data.Ready () ->
           book.state <- Writable entry;
           book.notice <- None
       | exception Host_error _ ->
           book.state <- Writable entry;
           book.notice <- save_error)
  | Closed | Read_only | Writable _ -> ()

let full_time book (game : Match.t) =
  match book.state with
  | Writable entry ->
      let score =
        "Portside FC " ^ string_of_int game.home_score ^ " – " ^ string_of_int game.away_score ^ " Rovers" in
      (try
         let op = Data.put entry "last-result" (Bytes.of_string score) in
         book.state <- Saving (entry, op)
       with Host_error _ -> book.notice <- save_error)
  | Closed | Opening _ | Read_only | Saving _ -> ()
