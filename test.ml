let odd = {
	location = [0;1];
	alphabet = ["a"];
	initial = [0];
	accepting = [1];
	transitions = [(0,"a",1);(1,"a",0)]
};;


is_accepted odd ["a";"2";"a";"4";"a";"3";"a";"1"] odd.initial;;



