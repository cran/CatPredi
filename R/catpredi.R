#' Function to obtain optimal cut points to categorise a continuous predictor
#'    variable in a logistic regression model
#'
#' @param formula An object of class \code{\link{formula}} giving the model to be
#'    fitted in addition to the continuous covariate is aimed to categorise.
#'    This argument allows the user to specify whether the continuous predictor
#'    should be categorised in a univariable context, or in presence of other
#'    covariates or cofounders, i.e in a multiple logistic regression model.
#'    For instance, \code{Y ~ 1} indicates that the categorisation should be done in a
#'    univariable setting, with Y being the response variable. If the predictor
#'    variable is aimed to be categorised in a multivariable setting, this argument
#'    allows to specify whether the covariates should be modelled using linear
#'    or non linear effects. In the latest, the effects are estimated using the
#'    \pkg{mgcv} package.
#' @param cat.var Name of the continuous variable to categorise.
#' @param cat.points Number of cut points to look for.
#' @param data Data frame containing all needed variables.
#' @param method The algorithm selected to search for the optimal cut points.
#'    \code{"addfor"} if the AddFor algorithm is choosen, \code{"backaddfor"} if
#'    the BackAddFor algorithm is selected and \code{"genetic"} otherwise.
#' @param range The range of the continuous variable in which to look for the cut
#'    points. By default \code{NULL}, i.e, all the range.
#' @param correct.AUC A logical value. If \code{TRUE} the bias corrected AUC is estimated.
#' @param control Output of the \code{\link{controlcatpredi}} function.
#' @param ... Further arguments for passing on to the function \code{\link[rgenoud]{genoud}}
#'    of the package \pkg{rgenoud}.
#'
#' @return
#' Returns an object of class \code{"catpredi"} with the following components:
#' \describe{
#'   \item{call}{The matched call.}
#'   \item{method}{The algorithm selected in the call.}
#'   \item{formula}{The model formula used in the call.}
#'   \item{cat.var}{Name of the continuous variable to categorise.}
#'   \item{data}{The data frame used in the call.}
#'   \item{correct.AUC}{Logical value indicating whether bias-corrected AUC was used.}
#'   \item{results}{A list containing estimated cut points, AUC and
#'     bias-corrected AUC for each method.}
#'   \item{control}{The control parameters used in the call.}
#' }
#'
#' @description Returns an object with the optimal cut points to categorise a
#'  continuous predictor variable in a logistic regression model
#'
#' @references I Barrio, J Roca-Pardinas and I Arostegui (2021). Selecting the number
#'    of categories of the lymph node ratio in cancer research: A bootstrap-based
#'    hypothesis test. \emph{Statistical Methods in Medical Research}, 30(3), 926-940.
#'
#'    I Barrio, I Arostegui, M.X Rodriguez-Alvarez and  J.M Quintana (2017).
#'    A new approach to categorising continuous variables in prediction models:
#'     proposal and validation. \emph{Statistical Methods in Medical Research}, 26(6), 2586-2602.
#'
#'    S.N Wood (2006). Generalized Additive Models: An Introduction with R. Chapman and Hall/CRC.
#'
#' @author Irantzu Barrio, Maria Xose Rodriguez-Alvarez, Inmaculada Arostegui, Javier Roca-Pardinas and Xabier Amutxastegi.
#'
#' @seealso
#' \code{\link{controlcatpredi}},
#' \code{\link{comp.cutpoints}},
#' \code{\link{plot.catpredi}},
#' \code{\link{summary.catpredi}}
#'
#' @examples
#' library(CatPredi)
#' \dontrun{
#' set.seed(127)
#' #Simulate data
#' n = 100
#' #Predictor variable
#' xh <- rnorm(n, mean = 0, sd = 1)
#' xd <- rnorm(n, mean = 1.5, sd = 1)
#' x <- c(xh, xd)
#' #Response
#' y <- c(rep(0,n), rep(1,n))
#' #Covariate
#' zh <- rnorm(n, mean=1.5, sd=1)
#' zd <- rnorm(n, mean=1, sd=1)
#' z <- c(zh, zd)
#' # Data frame
#' df <- data.frame(y = y, x = x, z = z)
#'
#' # Select optimal cut points using the AddFor algorithm
#' res.addfor <- catpredi(formula = y ~ z, cat.var = "x", cat.points = 2,
#'                        data = df, method = "addfor", range=NULL, correct.AUC=FALSE,
#'                        control=controlcatpredi(grid=20))
#'
#' # Select optimal cut points using the BackAddFor algorithm
#' res.backaddfor <- catpredi(formula = y ~ z, cat.var = "x", cat.points = 3,
#'                            data = df, method = "backaddfor", range=NULL, correct.AUC=FALSE)
#' }
#' \dontrun{
#'   set.seed(127)
#'   #Simulate data
#'   n = 200
#'   #Predictor variable
#'   xh <- rnorm(n, mean = 0, sd = 1)
#'   xd <- rnorm(n, mean = 1.5, sd = 1)
#'   x <- c(xh, xd)
#'   #Response
#'   y <- c(rep(0,n), rep(1,n))
#'   #Covariate
#'   zh <- rnorm(n, mean=1.5, sd=1)
#'   zd <- rnorm(n, mean=1, sd=1)
#'   z <- c(zh, zd)
#'   # Data frame
#'   df <- data.frame(y = y, x = x, z = z)
#'
#'   # Select optimal cut points using the AddFor algorithm
#'   res.addfor <- catpredi(formula = y ~ z, cat.var = "x", cat.points = 3,
#'                          data = df, method = "addfor", range=NULL, correct.AUC=FALSE)
#'
#'   # Select optimal cut points using the BackAddFor algorithm
#'   res.backaddfor <- catpredi(formula = y ~ z, cat.var = "x", cat.points = 3,
#'                              data = df, method = "backaddfor", range=NULL, correct.AUC=FALSE)
#' }
#'
#' @export
catpredi <-
function(formula, cat.var, cat.points = 1, data, method = c("addfor","genetic","backaddfor"), range = NULL, correct.AUC = FALSE, control = controlcatpredi(), ...) {
	control <- do.call("controlcatpredi", control)

	if(missing(formula)) {
		stop("Argument \"formula\" is missing, with no default")
	}
	if(missing(data)) {
		stop("Argument \"data\" is missing, with no default")
	}
	if(missing(cat.var)) {
		stop("Argument \"cat.var\" is missing, with no default")
	}
	var.names <- c(all.vars(formula), cat.var)
	if(!all(var.names %in% names(data))) {
		stop("Not all needed variables are supplied in \"data\"")
	}
	data.res <- stats::na.omit(data[,var.names])
	unique.resp <- unique(data.res[,var.names[1]])
	if(length(unique.resp) != 2 || !is.numeric(unique.resp) || !all(unique.resp %in% c(0,1))) {
		stop("The response variable should be numeric and codified as 0 (healthy) and 1 (diseased)")
	}
	method <- match.arg(method)
	if(is.null(range)) {
		range <- range(data.res[,cat.var])
	}
	# Call the methods
	if(method == "addfor") {
		res <- k.points.max.auc(formula = formula, cat.var = cat.var, data = data.res,
		                        range = range, k = cat.points, l.s.points = control$grid,
		                        min.p.cat = control$min.p.cat)
		cutpoints <- res[,1]
		AUC = res[,2]

		if(correct.AUC == TRUE) {
			AUC.cor <- auc.opt.corrected(formula = formula, cat.var = cat.var, data = data.res ,
			                             c.points = cutpoints, AUC = AUC[length(cutpoints)], B=control$B ,
			                             b.method = control$b.method)
	 	} else {
			AUC.cor <- NULL
	 	}
	} else if(method == "genetic"){
		Dim <- matrix(ncol = 2, nrow = cat.points)
		Dim[,1] = range[1]*1.0
		Dim[,2] = range[2]*1.0
		res <- rgenoud::genoud(calculate.AUC, cat.points, max = TRUE, formula = formula,
		                       cat.var = cat.var, data.f = data.res, range = range,
		                       min.p.cat = control$min.p.cat, Domains = Dim, print.level = control$print.gen, ...)
		cutpoints <- res$par
		AUC = res$value

		if(correct.AUC == TRUE) {
			AUC.cor <- auc.opt.corrected(formula = formula, cat.var = cat.var, data = data.res ,
			                             c.points = cutpoints, AUC = AUC, B=control$B , b.method = control$b.method)
		} else {
			AUC.cor <- NULL
		}
	} else { # method == "backaddfor"
	  res <- backaddfor(formula = formula, cat.var = cat.var, data = data.res, range = range,
	                    k = cat.points, l.s.points = control$grid, min.p.cat = control$min.p.cat,
	                    eps = control$eps, repmax = control$B, ...)
	  cutpoints <- res$cuts
	  AUC = res$auc

	  if(correct.AUC == TRUE) {
	    AUC.cor <- auc.opt.corrected(formula = formula, cat.var = cat.var, data = data.res ,
	                                 c.points = cutpoints, AUC = AUC, B=control$B , b.method = control$b.method)
	  } else {
	    AUC.cor <- NULL
	  }
	}
	# Create the categorical covariate
	data[,paste0(cat.var,"_CatPredi")] <- cut(data[,cat.var], sort(unique(c(max(data[,cat.var], na.rm=TRUE),
	                                                                        min(data[,cat.var], na.rm=TRUE),
	                                                                        cutpoints))), include.lowest = TRUE, right = TRUE)
	results <- if(method == "addfor" & correct.AUC == TRUE) {
				list(cutpoints = cutpoints, AUC = AUC, AUC.cor = AUC.cor,  grid = control$grid)
			} else if(method == "genetic" & correct.AUC == TRUE) {
				list(cutpoints = cutpoints, AUC = AUC, AUC.cor = AUC.cor)
			} else if(method == "backaddfor" & correct.AUC == TRUE){
			  list(cutpoints = cutpoints, AUC = AUC, AUC.cor = AUC.cor,  grid = control$grid )
			} else if(method == "addfor" & correct.AUC == FALSE) {
				list(cutpoints = cutpoints, AUC = AUC, grid = control$grid)
			} else if(method == "genetic" & correct.AUC == FALSE){
			  list(cutpoints = cutpoints, AUC = AUC)
			} else{ # method == "backaddfor" & correct.AUC == FALSE
			  list(cutpoints = cutpoints, AUC = AUC, grid = control$grid)
			}

	res <- list(call = match.call(), method = method, formula = formula, cat.var = cat.var, data = data,
	            correct.AUC = correct.AUC, results = results , control = control)
	class(res) <- "catpredi"
	res
}
