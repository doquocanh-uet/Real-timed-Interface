(* Correct input satisfy Lt condition and input fit output *)
let average1 = {
	location = [0];
	alphabet = ["a"];
	initial = [0];
	accepting = [0];
	transitions = [(0,"a",0)];
	inputs = [{sym = "a"; inp = 4; out = 4 ; start = 0 ; endl = 2}; {sym = "a"; inp = 6; out = 5 ; start = 2 ; endl = 4};{sym = "a"; inp = 8 ; out = 6; start = 4 ; endl = 6}];
};;
check_lt average1 average1.inputs average1.initial 0 1;;

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

