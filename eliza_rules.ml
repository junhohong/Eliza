uses the regexp Any as a catch-all. If no other regexp matches the user's input, it will match Any. 

type regexp_element = Lit of string | One | Any
type response_element = Text of string | Place of int
type phrase = string list
type pattern = regexp_element list
type response_template = response_element list
type rule = Rule of pattern * response_template

Rule([Lit(“I”) ; Any; Lit(“want”); Any],
	[Text(“Why do you”); Place(1); Text(“want”); Place(2); Text(“?”)])
Rule([Any; Lit(“college”) ; Any],
	[Test(“Tell me about your college experience.”)])
Rule([Lit(“I”) ; Lit(“am”); Any]
	[Text(“Don’t be”); Place(1)])
Rule([Any;Lit(“can”);Lit(“I”);Any]
	[Text(“You can’t); Place(2)])
Rule([Any;Lit(“can”);Lit(“you”):Any]
	[Text(“No, I can’t);Place(2);Text(“I’m not in the mood.”)])
Rule([Lit(“The”);One;Lit(“test”);Any]
	[Text(“Ah,”);Place(1);Text(“I can help you with that!”)])
Rule (
