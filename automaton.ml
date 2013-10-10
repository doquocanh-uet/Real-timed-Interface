	(* List *)


	let forall_in = fun l f -> List.for_all f l 
	let exists_in = fun l f -> List.exists f l 


	let (foreach_in: 'x list -> ('x -> 'y list) -> 'y list) = 
	fun xs f -> List.fold_right (fun x ys -> (f x) @ ys) xs []


	let (do_foreach_in: 't list -> ('t -> unit) -> unit) = 
	fun l instruction -> List.iter instruction l


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


	(* AUTOMATON *)


	type 'elt set = 'elt Set.t


	(* symbol *)
	type symbol = string
	let epsilon = ""
	type alphabet = symbol set


	(* transition *)


	type 'state transition = 'state * symbol * 'state
	
	type 'state transitions = ('state transition) set
	
(*	type input = symbol*int *)
	
	 
	(* automaton *)

	type 'state automaton = 
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




	let (number_of_states: 'state automaton -> int) = 
	fun aut -> List.length (states_of aut.transitions)
	
	

	let (targets: 'state automaton -> 'state set -> symbol -> 'state set) = 
	fun aut states symbol -> targets_of (get_transitions (On [symbol]) (get_transitions (From states) aut.transitions))  

	let rec is_accepted aut word current_state = match word with
	| head::tail::[] -> if Set.subseteq (aut.accepting) (targets aut current_state head) then print_string "Recognized"
				else print_string "Not"
	|head::tail -> 	match tail with head1::tail1 -> if (int_of_string(head1) < 5) then is_accepted aut tail1 (targets aut current_state head1)
													else print_string "Not accept"
													

	
	

