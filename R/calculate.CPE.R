calculate.CPE <-
function(point, formula, cat.var, data.f, range, min.p.cat ) {
	var.names <- c(all.vars(formula), cat.var)
	X <- data.f[,cat.var]
	if (all(sapply(point, function(X, range){ res <- if(X> range[1] & X<range[2]){TRUE} else {FALSE}}, range = range))) {
		cutoffs <- sort(unique(c(max(X), min(X), point)))
		x.cut <- cut(X, cutoffs, include.lowest=TRUE,right=TRUE)
		if(length(levels(x.cut)) > 1 & all(table(x.cut)>min.p.cat)) {
			data.f[,"x.cut_"] <- x.cut
			formula.n <- stats::update(formula, stats::as.formula("~ . + x.cut_"))
			fit <- try(rms::cph(formula.n, data = data.f))
			#if(class(fit) == "try-error"){
			if("try-error" %in% class(fit)) {
				cpe <- NA
			} else {
			  # cpe <- coxcpe(fit, data.f)
				cpe <- CPE::phcpe2(coef = fit$coefficients, coef.var = fit$var, design = stats::model.matrix(fit, data = data.f))$CPE
			}
		} else {
			cpe <- NA
		}
	} else {
		cpe <- NA
	}
	cpe
}
