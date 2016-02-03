#use "/course/cs017/src/ocaml/CS17setup.ml" ;;

(*#use "/Users/jun/Desktop/CS017setup.ml" ;;*)

type regexp_element = Lit of string | One | Any;;
type response_element = Text of string | Place of int;;
type phrase = string list;;
type pattern = regexp_element list;;
type response_template = response_element list;;
type rule = Rule of pattern * response_template;;

(* extract : phrase -> pattern -> ((phrase list) option)
Input: phrase of type phrase; pattern of type pattern.
Output: the portions of the input that match each wild card. 
		None if the pattern does not match the phrase.*)

let rec extract (phrase: phrase)(pattern: pattern) : ((phrase list) option) = 
  match phrase,pattern with
  | [],[] -> Some [] (*checked*)
  | [], hd1::follow::tl1 ->
    (match hd1 with
	|Lit hd1 -> None
	|One -> None
	|Any -> (match extract [] (List.tl pattern) with
   	  |None -> None
   	  |Some res-> Some([]::res)))
	| [], hd1::[] -> 
	  (match hd1 with
      |Lit hd1 -> None
	  |One -> None
	  |Any -> Some [[]])
	| hd::tl,[] -> None
	| hd :: tl, hd1::tl1 -> 
	  (match hd1 with
	  |Lit hd1 -> if hd==hd1 then extract tl tl1 else None
	  |One -> (match extract tl tl1 with
	    |None -> None
		|Some res_list -> Some ((hd::[])::res_list))
		|Any -> (match extract phrase tl1 with
		  |None -> (match extract tl pattern with
		    |None -> None
		    |Some res_list1 -> 
			  Some((hd::List.hd res_list1)::List.tl res_list1))
		  |Some res_list2-> Some ([] :: res_list2)));;
(*test*)
extract ["my mother sucks"][Any; Lit("mother"); Any];;

check_expect(extract ["my";"mother";"sucks"][Any;Lit("mother");Any])
  (Some[["my"];["sucks"]]);;
check_expect(extract ["I";"love";"college"][Any; Lit("college");Any])
  (Some[["I"; "love"];[]]);;
check_expect(extract [] []) (Some []);;
check_expect(extract [][Any;Any;Any]) (Some [[]; []; []]);;
check_expect(extract [][Any;Any]) (Some [[]; []]);;
check_expect(extract [][Any]) (Some [[]]);;
check_expect(extract ["cs";"17";"class";"is";"really"][Lit("cs");Any]) 
  (Some [["17"; "class"; "is"; "really"]]);;
check_expect(extract ["cs";"17";"class";"is";"really"][Lit("cs");Any;One]) 
  (Some [["17"; "class"; "is"]; ["really"]]);;
check_expect(extract ["cs";"17";"class";"is";"really"][Lit("cs");One;Any;One])
  (Some [["17"]; ["class"; "is"]; ["really"]]);;
check_expect(extract ["cs";"17";"class";"is";"really"]
					 [Lit("cs");Any;Lit("is");One])
  (Some [["17"; "class"]; ["really"]]);;
check_expect(extract ["cs";"17";"class";"is";"really"]
					 [Lit("cs");One;Any;One;Lit("really")]) 
  (Some[["17"]; ["class"]; ["is"]]);;
check_expect(extract [][Lit"cs"])None;;
check_expect(extract ["cs"][Lit"cs"]) (Some []);;
check_expect(extract ["cs"][Any]) (Some [["cs"]]);;
check_expect(extract ["cs"][One]) (Some [["cs"]]);;
check_expect(extract ["cs";"17"][Any]) (Some[["cs";"17"]]);;
check_expect(extract ["cs";"17"][One]) None;;
check_expect(extract ["cs";"17"][Lit "cs"]) None;;
check_expect(extract ["cs";"17"][]) None;;
check_expect(extract ["cs";"17"][Lit"cs";One]) (Some [["17"]]);;
check_expect(extract ["cs";"17";"is";"really"][Lit"cs";One;Lit"is";One])
  (Some [["17"];["really"]]);;
check_expect (extract ["cs";"17";"is";"really";"fun"][Lit"cs";One;Any])
  (Some [["17"];["is";"really";"fun"]]);;


(* make_response: (phrase list) -> response_template -> phrase 
Input: phraseList, list of phrase; type response_template;
Output: phrase representing a response according to the response template 
		and the list of phrases given.*)
let rec make_response (phraseList: phrase list): response_template -> phrase = function
  | [] -> []
  | Text hd :: tl -> hd::make_response phraseList tl
  | Place hd :: tl -> List.flatten(List.nth phraseList (hd-1) :: 
    make_response phraseList tl::[]);;
(*test*)
check_expect(make_response [["Eric"];["Alex"]] 
			[Text("Why was");Place(2);Text("hit by");Place(1);Text("?")])
  ["Why was"; "Alex"; "hit by"; "Eric"; "?"];;
check_expect(make_response [["cookies" ; "and" ; "cakes"]] 
			[Text("What do you like about") ; Place(1) ; Text("?")]) 
  ["What do you like about" ; "cookies" ; "and" ; "cakes" ; "?"];;
check_expect(make_response [["Eric"]][Place(1)]) ["Eric"];;
check_expect(make_response [["Eric"]][Text("Hey")]) ["Hey"];;
check_expect(make_response [["Eric"]][])[];;


(* eliza_respond -> (string list) -> (rule list) -> (string list) 
Input: phrase of type phrase; eliza_rules, list of rules. 
Output: list of strings representing eliza's response to the input phrase. *)
let rec eliza_respond (phrase: phrase): rule list -> string list =function 
  | [] -> failwith "rules cannot be empty"
  | Rule (a,b) :: tl -> (match (extract phrase a) with
    |None -> eliza_respond phrase tl
	|Some res_list -> make_response res_list b);;


let eliza_rules = 
	[Rule([Lit("I"); Any; Lit("my"); Any],
	      [Text("Why do you"); Place(1); Text("your"); Place(2); Text("?")]);
		  
	 Rule([One; Lit("hit"); Lit("me")],
	      [Text("Why were you hit by"); Place(1); Text("?")]);
		  
 	 Rule([Any; Lit("hit"); Any],
 	      [Text("Why was"); Place(2); Text("hit by"); Place(1); Text("?")]);
		  
 	 Rule([Any; Lit("mother"); Any],
 	      [Text("Tell me more about your mother.")]);
		  
 	 Rule([One; Lit("hates"); Lit("me")],
 	      [Text("Do you think"); Place(1); 
 	      	Text("hates you because of something that you did?")]);
		  
 	 Rule([Lit("I"); Any; Lit("hate"); Lit("you")],
 	      [Text("Why do you"); Place(1); Text("hate me?")]);

 	 Rule([Any; Lit("college");Any],
 		  [Text("Tell me more about your college experience.")]);

 	 Rule([Lit("I");Any;Lit("want");Any],
 		  [Text("Why do you");Place(1);Text("want");Place(2)]);

 	 Rule([Lit("I");Lit("am");Any],
 		  [Text("Don't be");Place(1)]);

 	 Rule([Any;Lit("Can");Lit("you");Any],
 		  [Text("No, I can't");Place(2);Text(", I'm not in the mood.")]);
	
	 Rule([Any;Lit("can");Lit("you");Any],
 		  [Text("No, I can't");Place(2);Text(", I'm not in the mood.")]);

	 Rule([One],
	 	  [Place(1);Text("?")]);

	 Rule([Any;Lit("I");Lit("got");Any;Lit("for");Lit("my");One;Any],
	 	  [Text("Wow, you got");Place(2);Text("for your ");Place(3);Text("!")]);

	 Rule([One;One;One;Any],
	 	  [Place(1);Place(3);Place(2);Place(4)]);

 	 Rule([Any],
 	      [Text("What have you been thinking about recently?")])];;
(*test*)
check_error (fun () -> (eliza_respond ["mother"] [])) "rules cannot be empty";;
check_expect(eliza_respond ["mother"] eliza_rules) 
  ["Tell me more about your mother."];;
check_expect(eliza_respond ["I";"like";"my";"cat"] eliza_rules)
  ["Why do you"; "like"; "your"; "cat"; "?"];;
check_expect(eliza_respond ["Mike";"hit";"me"] eliza_rules)
  ["Why were you hit by"; "Mike"; "?"];;
check_expect(eliza_respond ["Mike";"really";"hit";"the";"nail"] eliza_rules)
  ["Why was"; "the"; "nail"; "hit by"; "Mike"; "really"; "?"];;
check_expect(eliza_respond ["Clara";"hates";"me"]eliza_rules)
  ["Do you think"; "Clara"; "hates you because of something that you did?"];;
check_expect(eliza_respond ["I";"really";"freakin";"hate";"you"] eliza_rules)
  ["Why do you"; "really"; "freakin"; "hate me?"];;
check_expect(eliza_respond ["my";"mother";"sucks"]eliza_rules)
  ["Tell me more about your mother."];;
check_expect(eliza_respond["I";"love";"college"]eliza_rules)
  ["Tell me more about your college experience."];;
check_expect(eliza_respond["I";"really";"want";"the";"new";"car"]eliza_rules)
  ["Why do you";"really";"want";"the";"new";"car"];;
check_expect(eliza_respond["I";"am";"a";"party";"pooper"]eliza_rules)
  ["Don't be";"a";"party";"pooper"];;
check_expect(eliza_respond["can";"you";"look";"up"]eliza_rules)
  ["No, I can't";"look";"up";", I'm not in the mood."];;
check_expect(eliza_respond["Can";"you";"look";"up"]eliza_rules)
  ["No, I can't";"look";"up";", I'm not in the mood."];;
check_expect(eliza_respond["Luke";"I";"am";"your";"father";"?"]eliza_rules)
  ["Luke" ;"am" ;"I" ;"your" ;"father" ;"?"];;
check_expect(eliza_respond["I";"am";"so";"bored"]eliza_rules)
  ["Don't be"; "so" ;"bored"];;
check_expect(eliza_respond["shoot"]eliza_rules)
  ["shoot";"?"];;
check_expect(eliza_respond["Eliza!";"I";"got";"cake";"for";"my";"birthday";"."]eliza_rules)
  ["Wow, you got";"cake";"for your ";"birthday";"!"];;


(*#use "/Users/jun/Desktop/eliza_interactions.ml";;*)
#use"/course/cs017/src/eliza/eliza_interactions.ml";;