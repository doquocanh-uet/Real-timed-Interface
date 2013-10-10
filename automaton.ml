	(* Set *)
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


	(* automatonOMATON *)


	type 'elt set = 'elt Set.t


	(* symbol *)
	type symbol = string
	let epsilon = ""
	type alphabet = symbol set


	(* transition *)


	type 'state transition = 'state * symbol * 'state
	
	type 'state transitions = ('state transition) set
	
	(*	type input = symbol*int *)
	
	 
	(* automatonomaton *)

	type 'state automatonomaton = 
	{ location: 'state set
	; alphabet: symbol set
	; initial: 'state set
	; accepting: 'state set
	; transitions: ('state transition) list
(*	; Input : input list *)

	}
	

	type 'state selection = 
	| From of 'state list
	| To   of 'state list
	| On   of symbol list
	| On_all_but of symbol list


	let (get_transitions: 'state selection -> ('state transition) list -> ('state transition) list) = 
	fun selection transitions -> 
	match selection with
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

	let (number_of_states: 'state automatonomaton -> int) = 
	fun automaton -> List.length (states_of automaton.transitions)

	let (targets: 'state automatonomaton -> 'state set -> symbol -> 'state set) = 
	fun automaton states symbol -> targets_of (get_transitions (On [symbol]) (get_transitions (From states) automaton.transitions))  

	let rec is_accepted automaton word current_state = match word with
	|head::tail::[] -> 	if Set.subseteq (automaton.accepting) (targets automaton current_state head) then 
							if (int_of_string(tail) < 5) 
								then print_string "Recognized"
							else 
								print_string "Not";
	|head::tail -> 	match tail with head1::tail1 -> if (int_of_string(head1) < 5) then is_accepted automaton tail1 (targets automaton current_state head)
													else print_string "Not accept"
													

	
	

