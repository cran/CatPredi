plot.catpredi.survival <-
function(x, ...){
	# Fit the model      
	formula <- update(x$formula, as.formula(paste("~ . + pspline(", x$cat.var, ")", sep = "")))
	formula <- as.formula(Reduce(paste, deparse(formula)))
	fit.survival <- coxph(formula, data = x$data)
	pos <- attr(terms.formula(formula, specials = c("pspline")),"specials")$pspline - 1
	termplot(fit.survival, terms = pos, se = TRUE, ylabs = paste("f(",x$cat.var,")"))
	for(i in x$results$cutpoints) {
		abline(v = i, lty = 2)
	}
}
