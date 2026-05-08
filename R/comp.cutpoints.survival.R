#' Selection of optimal number of cut points
#'
#' @param obj1 An object inheriting from class \code{"catpredi.survival"} for k number of cut points
#' @param obj2 An object inheriting from class \code{"catpredi.survival"} for k+1 number of cut points
#' @param V Number of bootstrap resamples. By default V=100
#'
#' @returns
#' This function returns an object of class "comp.cutpoints.survival" with the following components:
#' \describe{
#'   \item{CI.cor.diff}{the difference of the bias corrected concordance probability for the two categorical variables.}
#'   \item{icb.CI.diff}{bootstrap based confidence interval for the bias corrected concordance probability difference.}
#' }
#'
#' @description Compares two objects of class \code{"catpredi.survival"}
#'
#' @references I Barrio,  M.X Rodriguez-Alvarez, L Meira-Machado, C Esteban  and  I Arostegui (2017).
#'    Comparison of two discrimination indexes in the categorisation of continuous predictors in time-to-event studies.
#'    \emph{SORT}, 41:73-92
#'
#' @author Irantzu Barrio and Maria Xose Rodriguez-Alvarez.
#'
#' @seealso
#' \code{\link{catpredi.survival}}
#'
#' @examples
#' library(CatPredi)
#' library(survival)
#' set.seed(123)
#'
#' #Simulate data
#' n = 300
#' tauc = 1
#' X <- rnorm(n=n, mean=0, sd=2)
#' SurvT <- exp(2*X + rweibull(n = n, shape=1, scale = 1))   + rnorm(n, mean=0, sd=0.25)
#' # Censoring time
#' CensTime <- runif(n=n, min=0, max=tauc)
#' # Status
#' SurvS <- as.numeric(SurvT <= CensTime)
#' # Data frame
#' dat <- data.frame(X = X, SurvT = pmin(SurvT, CensTime), SurvS = SurvS)
#' \donttest{
#'   # Select 2 optimal cut points using the AddFor algorithm. Correct the c-index
#'     res.k2 <- catpredi.survival (formula= Surv(SurvT,SurvS)~1, cat.var="X", cat.points = 2,
#'                                  data = dat, method = "addfor", conc.index = "cindex",
#'                                  range = NULL, correct.index = TRUE)
#'   # Select 3 optimal cut points using the AddFor algorithm. Correct the c-index
#'     res.k3 <- catpredi.survival (formula= Surv(SurvT,SurvS)~1, cat.var="X", cat.points = 3,
#'                                  data = dat, method = "addfor", conc.index = "cindex",
#'                                  range = NULL, correct.index = TRUE)
#'   # Select optimal number of cut points
#'     comp <-  comp.cutpoints.survival(res.k2, res.k3, V = 100)
#' }
#' \dontshow{
#'   # Select 2 optimal cut points using the AddFor algorithm. Correct the c-index
#'     res.k2 <- catpredi.survival (formula= Surv(SurvT,SurvS)~1, cat.var="X", cat.points = 1,
#'                                  data = dat, method = "addfor", conc.index = "cindex",
#'                                  range = NULL, correct.index = TRUE,
#'                                  control=controlcatpredi.survival(grid=20))
#'   # Select 3 optimal cut points using the AddFor algorithm. Correct the c-index
#'     res.k3 <- catpredi.survival (formula= Surv(SurvT,SurvS)~1, cat.var="X", cat.points = 2,
#'                                  data = dat, method = "addfor", conc.index = "cindex",
#'                                  range = NULL, correct.index = TRUE,
#'                                  control=controlcatpredi.survival(grid=20))
#'   # Select optimal number of cut points
#'     comp <-  comp.cutpoints.survival(res.k2, res.k3, V = 2)
#' }
#'
#' @export
comp.cutpoints.survival <-
function(obj1,obj2, V=100) {
	if(obj1$correct.index==FALSE || obj2$correct.index==FALSE ) {
			stop("argument correct.index=TRUE is needed in catpredi.survival")
	}
	if(obj1$formula !=obj2$formula) {
		stop("The categorized variables are not comparable")
	}
	if(obj1$conc.index != obj2$conc.index) {
		stop("The concordance index selected in both objects must be the same")
	}
	if(obj1$control$B != obj2$control$B) {
		warning("The bootstrap resamples used for the optimism correction is different in both objects")
	}
	formula <- obj1$formula
	point1 <- obj1$results$cutpoints
	point2 <- obj2$results$cutpoints
	B1 <-   obj1$control$B
  B2 <-   obj2$control$B
  b.method <- obj1$control$b.method
	data <-   obj1$data
	cat.var <- obj1$cat.var
	var.names <- c(all.vars(formula), cat.var)
	X <- data[,cat.var]
	ci.b.1 <- ci.b.2 <- ci.b.diff <- vector(length = V)

	if(obj1$conc.index=="cindex") {
		ci.cor.diff <- obj2$results$Cindex.cor - obj1$results$Cindex.cor
		for (i in 1:V) {
			data.b <- bootstrap.sample(data, group = var.names[2], method = b.method)
			X.b <- data.b[,cat.var]
			# k
			sel.point = sort(unique(c(min(X,X.b), max(X,X.b), point1)))
			x.cut = cut(X.b, sel.point, include.lowest = TRUE, right = TRUE)
			data.b[,"x.cut1"] <- x.cut
			formula.n <- stats::update(formula, stats::as.formula("~ . + x.cut1"))

			fit.1 <- rms::cph(formula.n, data=data.b)
			cind.fit1 <- cindex.categorization(fit.1$linear.predictors, survival::Surv(data.b[,var.names[1]],data.b[,var.names[2]]))
			ci.b.1[i] <- cindex.opt.corrected(formula = formula, cat.var =  cat.var , data = data.b , c.points = point1 , cindex = cind.fit1 , B=B1 , b.method = b.method)

			# k = k+1
			sel.point = sort(unique(c(min(X,X.b), max(X,X.b), point2)))
			x.cut = cut(X.b, sel.point, include.lowest = TRUE, right = TRUE)
			data.b[,"x.cut2"] <- x.cut
			formula.n <- stats::update(formula, stats::as.formula("~ . + x.cut2"))

			fit.2 <- rms::cph(formula.n, data=data.b)
			cind.fit2 <- cindex.categorization(fit.2$linear.predictors, survival::Surv(data.b[,var.names[1]],data.b[,var.names[2]]))
			ci.b.2[i] <- cindex.opt.corrected(formula = formula, cat.var =  cat.var , data = data.b , c.points = point2 , cindex = cind.fit2 , B=B2 , b.method = b.method)

			ci.b.diff[i] <- ci.b.2[i] - ci.b.1[i]
		}
	} else {
		ci.cor.diff <- obj2$results$CPE.cor - obj1$results$CPE.cor
		for (i in 1:V) {
			data.b <- bootstrap.sample(data,var.names[2], b.method)
			X.b <- data.b[,cat.var]
			# k
			sel.point = sort(unique(c(min(X,X.b , na.rm=TRUE), max(X,X.b , na.rm=TRUE), point1)))
			x.cut = cut(X.b, sel.point, include.lowest = TRUE, right = TRUE)
			data.b[,"x.cut1"] <- x.cut
			formula.n <- stats::update(formula, stats::as.formula("~ . + x.cut1"))
			fit.1 <- rms::cph(formula.n, data=data.b)
			cpe.fit1 <- CPE::phcpe2(coef = fit.1$coefficients, coef.var = fit.1$var, design = stats::model.matrix(fit.1, data = data.b))$CPE #phcpe(fit.1,CPE.SE=FALSE, out.ties=FALSE)$CPE
			ci.b.1[i] <- cpe.opt.corrected(formula = formula, cat.var =  cat.var , data = data.b , c.points = point1 , cpe = cpe.fit1 , B = B1 , b.method = b.method)

			# k = k+1
			sel.point = sort(unique(c(min(X,X.b , na.rm=TRUE), max(X,X.b , na.rm=TRUE), point2)))
			x.cut = cut(X.b, sel.point, include.lowest = TRUE, right = TRUE)
			data.b[,"x.cut2"] <- x.cut
			formula.n <- stats::update(formula, stats::as.formula("~ . + x.cut2"))

			fit.2 <- rms::cph(formula.n, data=data.b)
			# cpe.fit2 <- coxcpe(fit.2, data.b)
			cpe.fit2 <- CPE::phcpe2(coef = fit.2$coefficients, coef.var = fit.2$var, design = stats::model.matrix(fit.2, data = data.b))$CPE #phcpe(fit.2,CPE.SE=FALSE, out.ties=FALSE)$CPE
			ci.b.2[i] <- cpe.opt.corrected(formula = formula, cat.var =  cat.var , data = data.b , c.points = point2 , cpe = cpe.fit2 , B = B2, b.method = b.method)

			ci.b.diff[i] <- ci.b.2[i] - ci.b.1[i]
		}
	}
	ci.diff <- stats::quantile(ci.b.diff, p = c(0.025, 0.975), na.rm=TRUE)
	res <- list( call = match.call() , CI.cor.diff = ci.cor.diff , icb.CI.diff = ci.diff)
	class(res) <- "comp.cutpoints.survival"
	res
}
