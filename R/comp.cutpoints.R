#' Selection of optimal number of cut points
#'
#' @param obj1 An object inheriting from class \code{"catpredi"} for k number of cut points
#' @param obj2 An object inheriting from class \code{"catpredi"} for k+1 number of cut points
#' @param V Number of bootstrap resamples. By default V=100
#'
#' @returns
#' This function returns an object of class \code{"comp.cutpoints"} with the following components:
#' \describe{
#'   \item{AUC.cor.diff}{the difference of the bias corrected AUCs for the two categorical variables.}
#'   \item{icb.auc.diff}{bootstrap based confidence interval for the bias corrected AUC difference.}
#' }
#'
#' @description Compares two objects of class \code{"catpredi"}.
#'
#' @references I Barrio, I Arostegui, M.X Rodriguez-Alvarez and  J.M Quintana (2017).
#'    A new approach to categorising continuous variables in prediction models:
#'    proposal and validation. \emph{Statistical Methods in Medical Research},
#'    26(6), 2586-2602.
#'
#' @author Irantzu Barrio, Maria Xose Rodriguez-Alvarez and Inmaculada Arostegui.
#'
#' @seealso
#' \code{\link{catpredi}}
#'
#' @examples
#' library(CatPredi)
#' set.seed(127)
#' #Simulate data
#' n = 100
#' #Predictor variable
#' xh <- rnorm(n, mean = 0, sd = 1)
#' xd <- rnorm(n, mean = 1.5, sd = 1)
#' x <- c(xh, xd)
#' #Response
#' y <- c(rep(0,n), rep(1,n))
#' # Data frame
#' df <- data.frame(y = y, x = x)
#' \dontshow{
#'   # Select 2 optimal cut points using the AddFor algorithm. Correct the AUC
#'     res.backaddfor.k2 <- catpredi(formula = y ~ 1, cat.var = "x", cat.points = 2,
#'                                   data = df, method = "backaddfor", range=NULL, correct.AUC=TRUE,
#'                                   control=controlcatpredi(grid=20))
#'   # Select 3 optimal cut points using the AddFor algorithm. Correct the AUC
#'     res.backaddfor.k3 <- catpredi(formula = y ~ 1, cat.var = "x", cat.points = 3,
#'                                   data = df, method = "backaddfor", range=NULL, correct.AUC=TRUE,
#'                                   control=controlcatpredi(grid=20))
#'     comp <-  comp.cutpoints(res.backaddfor.k2, res.backaddfor.k3, V = 10)
#'  }
#' \donttest{
#'   # Select 2 optimal cut points using the AddFor algorithm. Correct the AUC
#'     res.backaddfor.k2 <- catpredi(formula = y ~ 1, cat.var = "x", cat.points = 2,
#'                                   data = df, method = "backaddfor", range=NULL, correct.AUC=TRUE,
#'                                   control=controlcatpredi(grid=100))
#'   # Select 3 optimal cut points using the AddFor algorithm. Correct the AUC
#'     res.backaddfor.k3 <- catpredi(formula = y ~ 1, cat.var = "x", cat.points = 3,
#'                                   data = df, method = "backaddfor", range=NULL, correct.AUC=TRUE,
#'                                   control=controlcatpredi(grid=100))
#'
#'   # Select optimal number of cut points
#'     comp <-  comp.cutpoints(res.backaddfor.k2, res.backaddfor.k3, V = 100)
#'  }
#'
#' @export
comp.cutpoints <-
function(obj1, obj2, V = 100) {
	if(is.null(obj2$results$AUC.cor)==TRUE || is.null(obj1$results$AUC.cor)==TRUE) {
		stop("argument correct.AUC=TRUE is needed in catpredi")
	}
	if(obj1$formula != obj2$formula) {
		stop("The categorized variables are not comparable")
	}
	if(obj1$control$B != obj2$control$B) {
		warning("The bootstrap resamples used for the optimism correction of the AUC is different in both objects")
	}
	AUC.cor.diff <- obj2$results$AUC.cor - obj1$results$AUC.cor
	formula <- obj1$formula

	point1 <- obj1$results$cutpoints
	point2 <- obj2$results$cutpoints
	B1 <-   obj1$control$B
  B2 <-   obj2$control$B
  b.method <- obj1$control$b.method
	data <-   obj1$data
	cat.var <- obj1$cat.var
	var.names <- all.vars(formula)
	X <- data[,cat.var]
	Y <- data[,var.names[1]]

	auc.b.1 <- auc.b.2 <- auc.b.diff <- vector(length = V)

	for (i in 1:V) {
		data.b <- bootstrap.sample(data, group = var.names[1], method = b.method)
		Y.b <- data.b[,var.names[1]]
		X.b <- data.b[,cat.var]
		# k
		sel.point = sort(unique(c(min(X,X.b , na.rm=TRUE), max(X,X.b , na.rm=TRUE), point1)))
		x.cut = cut(X.b, sel.point, include.lowest = TRUE, right = TRUE)
		data.b[,"x.cut1"] <- x.cut
		formula.n <- stats::update(formula, stats::as.formula("~ . + x.cut1"))
			fit.1 <- mgcv::gam(formula.n, family = stats::binomial, data = data.b)
		auc.fit1 <- compute.empirical.AUC(fit.1$fitted[Y.b==1], fit.1$fitted[Y.b==0])
		auc.b.1[i] <- auc.opt.corrected(formula = formula, cat.var = cat.var, data = data.b, c.points = point1, AUC =auc.fit1 , B=B1, b.method = b.method)

		# k = k+1
		sel.point = sort(unique(c(min(X,X.b , na.rm=TRUE), max(X,X.b , na.rm=TRUE), point2)))
		x.cut = cut(X.b, sel.point, include.lowest = TRUE, right = TRUE)
		data.b[,"x.cut2"] <- x.cut
		formula.n <- stats::update(formula, stats::as.formula("~ . + x.cut2"))
		fit.2 <- mgcv::gam(formula.n, family = stats::binomial, data = data.b)
		auc.fit2 <- compute.empirical.AUC(fit.2$fitted[Y.b==1], fit.2$fitted[Y.b==0])
		auc.b.2[i] <- auc.opt.corrected(formula = formula, cat.var = cat.var, data = data.b, c.points = point2, AUC =auc.fit2 , B=B2, b.method = b.method)
		auc.b.diff[i] <- auc.b.2[i] - auc.b.1[i]
	}
	auc.diff <- stats::quantile(auc.b.diff, p = c(0.025, 0.975), na.rm=TRUE)
	res <- list( AUC.cor.diff = AUC.cor.diff , icb.auc.diff = auc.diff)
	class(res) <- "comp.cutpoints"
  res
}
