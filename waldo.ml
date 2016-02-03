 let rec wbw (alos: string list) : (string list) option = 
   match alos with 
   | [] -> None
   | "waldo"::[] -> Some []
   | "waldo"::follow::tl -> (match (wbw (follow::tl)) with  (* SUBTLE *)
      | None -> Some [follow]
      | Some res_list -> Some (follow::res_list))
   | _::tl -> wbw tl;;