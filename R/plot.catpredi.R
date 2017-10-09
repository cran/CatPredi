plot.catpredi <-
function(x, ...){
	# Fit the model
	formula <- update(x$formula, as.formula(paste("~ . + s(", x$cat.var, ")", sep = "")))
	fit.gam <- gam(formula, family = binomial, data = x$data)
	pos <- length(attr(terms.formula(formula, specials = c("s")),"specials")$s)
	plot(fit.gam, select = pos, shade = TRUE, ylab = paste("f(",x$cat.var,")"), all.terms = TRUE)
	for(i in x$results$cutpoints) {
		abline(v = i, lty = 2)
	}
	# Plot the OR - PENDIENTE
}
