module Gnat_scheduler = struct

    (* the private list of functions to call on idle, sorted higher
       priority first. *)
    let idle_handler : (int * (unit -> bool)) list ref = ref []

    (* [insert_idle_handler p f] inserts [f] as a new function to call
       on idle, with priority [p] *)
    let insert_idle_handler p f =
      let rec aux l =
        match l with
          | [] -> [p,f]
          | (p1,_) as hd :: rem ->
             if p > p1 then (p,f) :: l else hd :: aux rem
      in
      idle_handler := aux !idle_handler

    (* the private list of functions to call on timeout, sorted on
       earliest trigger time first. *)
    let timeout_handler : (float * float * (unit -> bool)) list ref = ref []

    (* [insert_timeout_handler ms t f] inserts [f] as a new function to call
       on timeout, with time step of [ms] and first call time as [t] *)
    let insert_timeout_handler ms t f =
      let rec aux l =
        match l with
          | [] -> [ms,t,f]
          | (_,t1,_) as hd :: rem ->
             if t < t1 then (ms,t,f) :: l else hd :: aux rem
      in
      timeout_handler := aux !timeout_handler

    (* public function to register a task to run on idle *)
    let idle ~(prio:int) f = insert_idle_handler prio f

    (* public function to register a task to run on timeout *)
    let timeout ~ms f =
      assert (ms > 0);
      let ms = float ms /. 1000.0 in
      let time = Unix.gettimeofday () in
      insert_timeout_handler ms (time +. ms) f

    let wait_for_idle () =
      try
        while true do
          match !idle_handler with
          | (p,f) :: rem ->
              idle_handler := rem;
              let b = f () in
              if b then insert_idle_handler p f
          | [] -> raise Exit
        done
      with Exit -> ()


     (* [main_loop interp] starts the scheduler. On idle, standard input is
        read.  When a complete line is read from stdin, it is passed
        as a string to the function [interp] *)
     let main_loop ending =
       try
         while true do
           (* attempt to run the first timeout handler *)
           (let time = Unix.gettimeofday () in
           match !timeout_handler with
           | (ms,t,f) :: rem when t <= time ->
              timeout_handler := rem;
              let b = f () in
              let time = Unix.gettimeofday () in
              if b then insert_timeout_handler ms (ms +. time) f
           | _ ->
              (* time is not yet passed *)
              (* attempt to run the first idle handler *)
               match !idle_handler with
              | (p,f) :: rem ->
                 idle_handler := rem;
                 let b = f () in
                 if b then insert_idle_handler p f
              | [] -> ())
         done
       with Exit -> ending ()

end