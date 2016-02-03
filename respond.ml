
#use "/Users/jun/Desktop/CS017setup.ml" ;;


type regexp_element = Lit of string | One | Any
type response_element = Text of string | Place of int
type phrase = string list
type pattern = regexp_element list
type response_template = response_element list
type rule = Rule of pattern * response_template


(* eliza_respond -> (string list) -> (rule list) -> (string list) 
Input: string list, alon; rule list, alor
Output: list of strings representing eliza's response to the input string list. *)
let eliza_respond (phrase: phrase) (eliza_rules: rule list) : string list = 
	match eliza_rules with
	| [] -> ["error"]
	| (a,b) :: tl -> (match (extract phrase a) with
		|None -> eliza_respond phrase tl
		|Some res_list -> make_response res_list b);;
