(* Correct input satisfy Lt condition and input fit output *)
let average1 = {
	location = [0;1];
	initial = [0];
	transitions = [(0,"a",0);(0,"b",1);(1,"a",1);(1,"b",0)];
	inputs = [{sym = "a"; inp = 4; execution_time = 2}; {sym = "a"; inp = 6; execution_time = 4};{sym = "a"; inp = 8 ; execution_time = 6}];
	outputs = [{ins = {sym = "a"; inp = 4; execution_time = 2}; out  = 4}];
	lt_list = [	{trans = (0,"a",0); guard_formula = "Func1"}; 
				{trans = (0,"b",1); guard_formula = "Func2"}; 
				{trans = (1,"a",1); guard_formula = "Func3"}; 
				{trans = (1,"b",0); guard_formula = "Func4"}];
	ls_list = [	{state = 0; timeDesign = {precondition = "condition1" ; time_constraint = (0,3)}};
				{state = 1; timeDesign = {precondition = "condition2" ; time_constraint = (3,6)}}];
};;

(* Incorrect input satisfy Lt condition but input and output don't fit 
let average2 = {
	location = [0];
	alphabet = ["a"];
	initial = [0];
	accepting = [0];
	transitions = [(0,"a",0)];
	inputs = [{sym = "a"; inp = 4; out = 4}; {sym = "a"; inp = 6; out = 5};{sym = "a"; inp = 8 ; out = 7}];
};;

check_lt average2 average2.inputs average2.initial 0 1;;

(* Incorrect input not satisfy Lt condition *)

let average3 = {
	location = [0];
	alphabet = ["a"];
	initial = [0];
	accepting = [0];
	transitions = [(0,"a",0)];
	inputs = [{sym = "a"; inp = 4; out = 4}; {sym = "a"; inp = 2; out = 5};{sym = "a"; inp = 8 ; out = 7}];
};;

check_lt average3 average3.inputs average3.initial 0 1;;
*)

