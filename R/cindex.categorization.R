cindex.categorization <-
function (x, y) {
	if (!is.Surv(y))
		y <- Surv(y)
	
	i <- is.na(x) | is.na(y)
	if (any(i)) {
		x <- x[!i]
		y <- y[!i, ]
	}
	k <- survConcordance.fit(y, x)
	cindex <- (k[1] + k[3]/2)/sum(k[1:3])
	cindex
}
