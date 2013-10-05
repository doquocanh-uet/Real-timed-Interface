let odd = {
	name = "odd";
	alphabet = ["a"];
	initial = [0];
	accepting = [1];
	transitions = [(0,"a",1);(1,"a",0)]
};;


is_accepted odd ["a";"a";"a";"a";"a";"a"] odd.initial;;