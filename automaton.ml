module Set = 
(struct

	type 't set = 't list
	
	type 't t = 't set

	let (add: 't -> 't set -> 't set) = 
		fun e s -> if List.mem e s then s else e::s 

	let (union: 't set -> 't set -> 't set) = 
		fun s1 s2 -> List.sort Pervasives.compare (List.fold_right add s1 (List.fold_right add s2 []))

	let (subseteq: 't set -> 't set -> bool) =
		fun s1 s2 -> List.for_all (fun e1 -> List.mem e1 s2) s1

end)


(* AUTOMATON *)

type 'elt set = 'elt Set.t
(* symbol *)
type symbol = string
let epsilon = ""
type alphabet = symbol set

type type_inp = int
type type_out = int
type beg = int
type fin = int
type time_interval = beg * fin
type timed_design = {
	precondition : string;
	time_constraint : time_interval; 
} 


type input = { 
	sym: symbol; 
	inp: type_inp ;
	execution_time : int;
}		

type output = {
	ins : input;
	out : type_out;
}

(* transition *)
type 'state transition = 'state * symbol * 'state



type 'state lt = {
	trans : 'state transition;
	guard_formular : string;
}

type 'state ls = {
	state : 'state; 
	timeDesign : timed_design;
}
 
(* automaton *)

type 'state automaton = 
{ 	
	location: 'state set; 
	inputs : input list;
	outputs :  output list;
	initial: 'state set;  
	transitions: ('state transition) list;
	lt_list: ('state lt) list;
	ls_list: ('state ls) list;
}

type 'state selection = 
	| From of 'state list
	| To   of 'state list
	| On   of symbol list
	| On_all_but of symbol list


let (get_transitions: 'state selection -> ('state transition) list -> ('state transition) list) = 
	fun selection transitions -> match selection with
		| From states -> List.filter (fun (q,_,_) -> List.mem q states) transitions 
		| To states   -> List.filter (fun (_,_,q) -> List.mem q states) transitions 
		| On symbols         -> List.filter (fun (_,s,_) -> List.mem s symbols) transitions 
		| On_all_but symbols -> List.filter (fun (_,s,_) -> not (List.mem s symbols)) transitions 

let (targets_of: ('state transition) list -> 'state set) =
fun transitions -> Set.union [] (List.map (fun (_,_,q) -> q) transitions)

let (sources_of: ('state transition) list -> 'state set) =
fun transitions -> Set.union [] (List.map (fun (q,_,_) -> q) transitions)

let (states_of: ('state transition) list -> 'state set) = 
fun transitions -> Set.union (sources_of transitions) (targets_of transitions) 

let (targets: 'state automaton -> 'state set -> symbol -> 'state set) = 
fun automaton states symbol -> targets_of (get_transitions (On [symbol]) (get_transitions (From states) automaton.transitions))  

(*let (get_func: 'state automaton -> ) =*)
(* Checking Lt condition *)	
(*let rec checking  automaton inputs current_state = match inputs with
	| 	head::[] -> if (guardFunc e1.inp highest lowest) then begin 
														if (isEqualOutput ((sum + e1.inp)/count) e1.out) then 
															if Set.subseteq (automaton.accepting) (targets automaton current_state e1.sym ) then  
																print_string "Recognize!"
															else print_string "Not Recognized!"
														else  print_string ("Not satisfied Lt at input [" ^e1.sym^ ","^ string_of_int(e1.inp) ^"," ^ string_of_int(e1.out)^"]")
													end
	| 	head::tail -> let tran = get_transitions (On head.sym) (get_transitions (From current_state) automaton.transitions)
						match tran with 
						| patt -> expr
						| _ -> expr2*)







												
												