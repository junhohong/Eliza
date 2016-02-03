(* type/data definitions
type signatures
I/O contracts
templates
test cases *)
#use "/Users/jun/Desktop/CS017setup.ml" ;;


type regexp_element = Lit of string | One | Any;;
type response_element = Text of string | Place of int;;
type phrase = string list;;
type pattern = regexp_element list;;
type response_template = response_element list;;
type rule = Rule of pattern * response_template;;



(* extract : phrase -> pattern -> ((phrase list) option)
Input: string list, phrase; regexp_element list, pattern;
Output: the portions of the input that match each wild card. *)
(*hd::tl hd1::tl1(regexp_match phrase tl1 || regexp_match tl pattern)*)

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
	(*| hd::[], hd1::[] -> 
		(match hd1 with
		|Lit hd1-> if hd==hd1 then Some[] else None
		|One -> Some [hd::[]]
		|Any -> Some [hd::[]])*)
	| hd::tl,[] -> None
	(*| hd::tl, hd1::[] -> 
		(match hd1 with
		|Lit hd1 -> None
		|One -> None
		|Any -> Some [hd::tl])*)
	| hd :: tl, hd1::tl1 -> 
		(match hd1 with
		|Lit hd1 -> if hd==hd1 then extract tl tl1 else None
		|One -> (match extract tl tl1 with
			|None -> None
			|Some res_list -> Some ((hd::[])::res_list))
		|Any -> (match extract phrase tl1 with
			|None -> (match extract tl pattern with
				|None -> None
				|Some a -> Some((hd::List.hd a)::List.tl a))
			|Some res_list2-> Some ([] :: res_list2)));;
check_expect (extract [] []) (Some []);;
check_expect (extract [][Any;Any;Any]) (Some [[]; []; []]);;
check_expect (extract [][Any;Any]) (Some [[]; []]);;
check_expect (extract [][Any]) (Some [[]]);;
check_expect (extract ["cs";"17";"class";"is";"really"][Lit("cs");Any]) (Some [["17"; "class"; "is"; "really"]]);;
check_expect (extract ["cs";"17";"class";"is";"really"][Lit("cs");Any;One]) (Some [["17"; "class"; "is"]; ["really"]]);;
check_expect (extract ["cs";"17";"class";"is";"really"][Lit("cs");One;Any;One]) (Some [["17"]; ["class"; "is"]; ["really"]]);;
check_expect (extract ["cs";"17";"class";"is";"really"][Lit("cs");Any;Lit("is");One])(Some [["17"; "class"]; ["really"]]);;
check_expect (extract ["cs";"17";"class";"is";"really"][Lit("cs");One;Any;One;Lit("really")]) (Some[["17"]; ["class"]; ["is"]]);;
(* check case for [], _::_::_*)
check_expect (extract [][Lit"cs"])None;;
check_expect (extract ["cs"][Lit"cs"]) (Some []);;
check_expect (extract ["cs"][Any]) (Some [["cs"]]);;
check_expect (extract ["cs"][One]) (Some [["cs"]]);;
check_expect (extract ["cs";"17"][Any]) (Some[["cs";"17"]]);;
check_expect (extract ["cs";"17"][One]) None;;
check_expect (extract ["cs";"17"][Lit "cs"]) None;;
check_expect (extract ["cs";"17"][]) None;;
check_expect (extract ["cs";"17"][Lit"cs";One]) (Some [["17"]]);;
check_expect (extract ["cs";"17";"is";"really"][Lit"cs";One;Lit"is";One])
	(Some [["17"];["really"]]);;
check_expect (extract ["cs";"17";"is";"really";"fun"][Lit"cs";One;Any])
	(Some [["17"];["is";"really";"fun"]]);;


(*extract ["cs";"17";"is";"really"][Lit("cs");One;Lit("is");One];;*)

(* check_expect (extract ["cs";"17";"is";"really";"fun"] [Lit("cs");One;Lit("is");One;Lit("fun")]) Some[["17"];["really"]];;
check_expect (extract [] [Lit("cs");One;Lit("is");One;Lit("fun")]) None;;
check_expect (extract ["I";"am";"not";"me";"today"] [Lit ("I");Any;Lit("not");One]) None;;
check_expect (extract ["The";"right";"answer";"is";"right"] [Any;Lit("right");Any]) Some [["The"];["answer";"is";"right"]];;*)
(*hd::tl hd1::tl1(regexp_match phrase tl1 || regexp_match tl pattern)*)


(* make_response: (phrase list) -> response_template -> phrase 
Input: list of phrase, alop; list of response_element, response_template;
Output: List of string, phrase. *)
let rec make_response (phraseList: phrase list): response_template -> phrase = function
	| [] -> []
	| Text hd :: tl -> hd::make_response phraseList tl
	| Place hd :: tl -> List.flatten(List.nth phraseList (hd-1) :: make_response phraseList tl::[]);;


check_expect (make_response [["Eric"];["Alex"]] [Text("Why was");Place(2);Text("hit by");Place(1);Text("?")])
			["Why was"; "Alex"; "hit by"; "Eric"; "?"];;

check_expect (make_response [["cookies" ; "and" ; "cakes"]] [Text("What do you like about") ; Place(1) ; Text("?")]) 
			["What do you like about" ; "cookies" ; "and" ; "cakes" ; "?"];;
check_expect (make_response [["Eric"]][Place(1)]) ["Eric"];;
check_expect (make_response [["Eric"]][Text("Hey")]) ["Hey"];;


(* eliza_respond -> (string list) -> (rule list) -> (string list) 
Input: string list, alon; rule list, alor
Output: list of strings representing eliza's response to the input string list. *)
let rec eliza_respond (phrase: phrase) (eliza_rules: rule list) : string list = 
	match eliza_rules with
	| [] -> ["error"]
	| Rule (a,b) :: tl -> (match (extract phrase a) with
		|None -> eliza_respond phrase tl
		|Some res_list -> make_response res_list b);;

(*let rules= [ Rule([Lit("I") ; Any; Lit("want"); Any],
	[Text("Why do you"); Place(1); Text("want"); Place(2); Text("?")]);

Rule([Any; Lit("college") ; Any],
	[Text("Tell me about your college experience.")]);

Rule([Lit("I") ; Lit("am"); Any]
	[Text("Don’t be"); Place(1)]);

Rule([Any;Lit("can");Lit("I");Any]
	[Text("You can’t"); Place(2)]);

Rule([Any;Lit("can");Lit("you"):Any]
	[Text("No, I can’t");Place(2);Text("I’m not in the mood.")]);

Rule([Lit("The");One;Lit("test");Any])];;*)
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
 	      [Text("Do you think"); Place(1); Text("hates you because of something that you did?")]);
		  
 	 Rule([Lit("I"); Any; Lit("hate"); Lit("you")],
 	      [Text("Why do you"); Place(1); Text("hate me?")]);
		  
 	 Rule([Any],
 	      [Text("What have you been thinking about recently?")])];;

 eliza_respond ["mother"] eliza_rules;;



