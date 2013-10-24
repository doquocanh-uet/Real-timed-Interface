	open Xml
	
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
	
	type type_inp = int
	type type_out = int
	type ls = {
		start : int;
		endl : int;
	}
	
	type input = { 
		sym: symbol; 
		inp: type_inp ;
		out: type_out;
		start : int;
		endl : int;
	}		
	
	let high = 10
	let low = 3
	
	let checkLt x high low = x < high && x > low
	
	(* transition *)
	type 'state transition = 'state * symbol * 'state
	 
	(* automaton *)

	type 'state automaton = 
	{ 	location: 'state set; 
		alphabet: symbol set; 
		initial: 'state set; 
		accepting: 'state set; 
		transitions: ('state transition) list;
		inputs : input list;
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

	let highest = 10
	let lowest = 3
	let guardFunc x high low = (x < high && x > low)
	let isEqualOutput inp out = (inp = out)
	let clock_start = 0
	let clock_end = 0
	(* Checking Lt condition *)	
	let rec check_lt automaton inputs current_state sum count = match inputs with
		| 	e1::[] -> if (guardFunc e1.inp highest lowest) then begin 
															if (isEqualOutput ((sum + e1.inp)/count) e1.out) then 
																if Set.subseteq (automaton.accepting) (targets automaton current_state e1.sym ) then  
																	print_string "Recognize!"
																else print_string "Not Recognized!"
															else  print_string ("Not satisfied Lt at input [" ^e1.sym^ ","^ string_of_int(e1.inp) ^"," ^ string_of_int(e1.out)^"]")
														end
		| 	e1::tail -> if (guardFunc e1.inp highest lowest)then begin 
																	(*Random.self_init ();*)
	(*																clocks_start = clock + Random.int ( e1.endl - e1.start + 1 ) + e1.start; *)
																	clock_start = clock_start + Random.int 2;
																	clock_end = clock_start + Random.int 2;
																	if (clock_start >= e1.start && clock_end <= e1.endl) then
																		begin
																			if (isEqualOutput ((sum + e1.inp)/count) e1.out) then
																				begin 
																					check_lt automaton tail (targets automaton current_state e1.sym) (sum+e1.inp) (count+1)														
																				end
																			else  print_string ("Not satisfied Lt at input [" ^e1.sym^ ","^
																					string_of_int(e1.inp) ^"," ^ string_of_int(e1.out)^"]")
																		end
																	else print_string ("Not satisfied Ls : clock start = " ^ string_of_int(clock_start) ^ " clock_end = "^ 
																		string_of_int(clock_end)^ " and " ^ string_of_int(e1.start) ^" "^ string_of_int(e1.endl))
																end
						else  print_string ("Not satisfied Lt at input [" ^e1.sym^ ","^ string_of_int(e1.inp) ^"," ^ string_of_int(e1.out)^"]")
													