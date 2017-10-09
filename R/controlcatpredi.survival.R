controlcatpredi.survival <-
function(
	min.p.cat = 5,
	addfor.g = 100,
	B=50,
  b.method = c("ncoutcome","coutcome"),
  print.gen = 0)	
	list(min.p.cat = min.p.cat, addfor.g = addfor.g, B = B , b.method = match.arg(b.method), print.gen = print.gen )
