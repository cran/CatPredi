#' Function to obtain optimal cut points to categorise a continuous predictor variable
#'    in a Cox proportional hazards regression model
#'
#' @param formula An object of class \code{\link{formula}} giving the model to be
#'    fitted in addition to the continuous covariate is aimed to categorise.
#'    The response must be a survival object as returned by the \code{\link[survival]{Surv}}
#'    function. This argument allows the user to specify whether the continuous
#'    predictor should be categorised in a univariable context, or in presence of
#'    other covariates or cofounders, i.e in a multiple Cox proportional hazards
#'    regression model. For instance, Surv(SurvT,SurvS)~1 indicates that the
#'    categorisation should be done in a univariable setting.
#' @param cat.var Name of the continuous variable to categorise.
#' @param cat.points Number of cut points to look for.
#' @param data Data frame containing all needed variables.
#' @param method The algorithm selected to search for the optimal cut points.
#'    \code{"addfor"} if the AddFor algorithm is choosen, \code{"backaddfor"} if
#'    the BackAddFor algorithm is selected and \code{"genetic"} otherwise.
#' @param conc.index The concordance probability estimator selected for maximisation
#'    purposes. "cindex" if the c-index concordance probability is choosen and
#'    "cpe" otherwise. The c-index and CPE are estimated using the \pkg{rms} and
#'    \pkg{CPE} packages, respectively.
#' @param range The range of the continuous variable in which to look for the cut
#'    points. By default \code{NULL}, i.e, all the range.
#' @param correct.index A logical value. If TRUE the bias corrected concordance
#'    probability is estimated.
#' @param control Output of the \code{\link{controlcatpredi.survival}} function.
#' @param ... Further arguments for passing on to the function \code{\link[rgenoud]{genoud}}
#'    of the package \pkg{rgenoud}.
#'
#' @returns
#' Returns an object of class \code{"catpredi.survival"} with the following components:
#' \describe{
#'   \item{call}{The matched call.}
#'   \item{method}{The algorithm selected in the call.}
#'   \item{formula}{an object of class \code{\link{formula}} giving the model to
#'      be fitted in addition to the continuous covariate is aimed to categorise.}
#'   \item{cat.var}{name of the continuous variable to categorise.}
#'   \item{data}{the data frame with the variables used in the call.}
#'   \item{correct.index}{The logical value used in the call.}
#'   \item{results}{a list with the estimated cut points, concordance probability
#'       and bias corrected concordance probability.}
#'   \item{control}{the control parameters used in the call.}
#'   When the c-index concordance probability is choosen, a list with the following
#'   components is obtained for each of the methods used in the call:
#'   \item{"cutpoints"}{Estimated optimal cut points.}
#'   \item{"Cindex"}{Estimated c-index.}
#'   \item{"Cindex.cor"}{Estimated bias corrected c-index.}
#'   When the CPE concordance probability is choosen, a list with the following
#'   components is obtained for each of the methods used in the call:
#'   \item{"cutpoints"}{Estimated optimal cut points.}
#'   \item{"CPE"}{Estimated CPE.}
#'   \item{"CPE.cor"}{Estimated bias corrected CPE.}
#' }
#'
#' @description Returns an object with the optimal cut points to categorise a continuous
#'    predictor variable in a Cox proportional hazards regression model
#'
#' @references I Barrio,  M.X Rodriguez-Alvarez, L Meira-Machado, C Esteban  and
#'    I Arostegui (2017). Comparison of two discrimination indexes in the categorisation
#'    of continuous predictors in time-to-event studies. \emph{SORT}, 41:73-92
#'
#'    M Gonen and  G Heller (2005). Concordance probability and discriminatory power
#'    in  proportional hazards regression. \emph{Biometrika}, 92:965-970.
#'
#'    F Harrell (2001). Regression modeling strategies: with applications to linear models,
#'    logistic and ordinal regression, and survival analysis. Springer.
#'
#' @author Irantzu Barrio and Maria Xose Rodriguez-Alvarez
#'
#' @seealso
#' \code{\link{controlcatpredi.survival}},
#' \code{\link{comp.cutpoints.survival}},
#' \code{\link{plot.catpredi.survival}},
#' \code{\link{catpredi}}
#'
#' @examples
#' library(CatPredi)
#' library(survival)
#' set.seed(123)
#' #Simulate data
#' n = 500
#' tauc = 1
#' X <- rnorm(n=n, mean=0, sd=2)
#' SurvT <- exp(2*X + rweibull(n = n, shape=1, scale = 1))   + rnorm(n, mean=0, sd=0.25)
#' # Censoring time
#' CensTime <- runif(n=n, min=0, max=tauc)
#' # Status
#' SurvS <- as.numeric(SurvT <= CensTime)
#' # Data frame
#' dat <- data.frame(X = X, SurvT = pmin(SurvT, CensTime), SurvS = SurvS)
#'
#' # Select optimal cut points using the AddFor algorithm
#' res <- catpredi.survival (formula= Surv(SurvT,SurvS)~1, cat.var="X", cat.points = 2,
#'                           data = dat, method = "addfor", conc.index = "cindex", range = NULL,
#'                           correct.index = FALSE)
#'
#' @export
catpredi.survival <-
function(formula, cat.var, cat.points = 1, data, method = c("addfor","genetic","backaddfor"), conc.index = c("cindex","cpe"), range = NULL, correct.index = FALSE, control = controlcatpredi.survival(), ...) {
	control <- do.call("controlcatpredi.survival", control)

	if(missing(formula)) {
		stop("Argument \"formula\" is missing, with no default")
	}
	if(is.character(formula))
		formula = stats::as.formula(formula)
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
	unique.resp <- unique(data.res[,var.names[2]])
	if(length(unique.resp) != 2 || !is.numeric(unique.resp) || !all(unique.resp %in% c(0,1))) {
		stop("The event status indicator should be numeric and codified as 0 (censored) and 1 (event at time)")
	}
	## COMO PODEMOS ASEGURAR QUE SEA UN OBJETO DE TIPO SURV??
	method <- match.arg(method)
	conc.index <- match.arg(conc.index)

	if(is.null(range)) {
		range <- range(data.res[,cat.var])
	}
	# Call the methods
	if(method == "addfor" & conc.index == "cindex") {
		res <- k.points.max.cind(formula = formula, cat.var = cat.var, data = data.res, range = range, k = cat.points, l.s.points = control$grid, min.p.cat = control$min.p.cat)
		cutpoints <- res[,1]
		Cindex = res[,2]
		# Correct the C-index
		if(correct.index == TRUE) {
			Cindex.cor <- cindex.opt.corrected(formula = formula, cat.var =  cat.var , data = data.res , c.points = cutpoints , cindex = Cindex[length(cutpoints)] , B=control$B, b.method = control$b.method )
	 	} else {
			Cindex.cor <- NULL
		}
	} else if(method == "addfor" & conc.index == "cpe") {
		res <- k.points.max.cpe(formula = formula, cat.var = cat.var, data = data.res, range = range, k = cat.points, l.s.points = control$grid, min.p.cat = control$min.p.cat)
		cutpoints <- res[,1]
		CPE = res[,2]
		# Correct the CPE
		if(correct.index == TRUE) {
			CPE.cor <- cpe.opt.corrected(formula = formula, cat.var =  cat.var , data = data.res , c.points = cutpoints , cpe = CPE[length(cutpoints)] , B=control$B , b.method = control$b.method)
		} else {
			CPE.cor <- NULL
		}
	} else if(method == "genetic" & conc.index == "cindex") {
		Dim <- matrix(ncol = 2, nrow = cat.points)
		Dim[,1] = range[1]*1.0
		Dim[,2] = range[2]*1.0
		res <- rgenoud::genoud(calculate.cind, cat.points, max = TRUE, formula = formula, cat.var = cat.var, data.f = data.res, range = range, min.p.cat = control$min.p.cat, Domains = Dim, print.level = control$print.gen, ...)
		cutpoints <- res$par
		Cindex = res$value
		# Correct the C-index
		if(correct.index == TRUE) {
			Cindex.cor <- cindex.opt.corrected(formula = formula, cat.var =  cat.var , data = data.res , c.points = cutpoints , cindex = Cindex , B=control$B, b.method = control$b.method)
		} else {
			Cindex.cor <- NULL
		}
	} else if(method == "genetic" & conc.index == "cpe"){
		Dim <- matrix(ncol = 2, nrow = cat.points)
		Dim[,1] = range[1]*1.0
		Dim[,2] = range[2]*1.0
		res <- rgenoud::genoud(calculate.CPE, cat.points, max = TRUE, formula = formula, cat.var = cat.var, data.f = data.res, range = range, min.p.cat = control$min.p.cat, Domains = Dim, print.level = control$print.gen, ...)
		cutpoints <- res$par
		CPE = res$value
		# Correct the CPE
		if(correct.index == TRUE) {
			CPE.cor <- cpe.opt.corrected(formula = formula, cat.var =  cat.var , data = data.res , c.points = cutpoints , cpe = CPE , B=control$B , b.method = control$b.method)
		} else {
			CPE.cor <- NULL
		}
	}  else if(method == "backaddfor" & conc.index == "cindex") {
	  Dim <- matrix(ncol = 2, nrow = cat.points)
	  Dim[,1] = range[1]*1.0
	  Dim[,2] = range[2]*1.0
	  res <- backaddfor.cind(formula = formula, cat.var = cat.var, data = data.res, range = range, k = cat.points, l.s.points = control$grid, min.p.cat = control$min.p.cat, eps = control$eps, repmax = control$B)
	  # res <- rgenoud::genoud(calculate.cind, cat.points, max = TRUE, formula = formula, cat.var = cat.var, data.f = data.res, range = range, min.p.cat = control$min.p.cat, Domains = Dim, print.level = control$print.gen, ...)
	  cutpoints <- res$cuts
	  Cindex = res$cind
	  # Correct the C-index
	  if(correct.index == TRUE) {
	    Cindex.cor <- cindex.opt.corrected(formula = formula, cat.var =  cat.var , data = data.res , c.points = cutpoints , cindex = Cindex , B=control$B, b.method = control$b.method)
	  } else {
	    Cindex.cor <- NULL
	  }
	} else { #if(method == "backaddfor" & conc.index == "cpe"){
	  Dim <- matrix(ncol = 2, nrow = cat.points)
	  Dim[,1] = range[1]*1.0
	  Dim[,2] = range[2]*1.0
	  res <- backaddfor(formula = formula, cat.var = cat.var, data = data.res, range = range, k = cat.points, l.s.points = control$grid, min.p.cat = control$min.p.cat, eps = control$eps, repmax = control$B, ...)
	  # res <- rgenoud::genoud(calculate.CPE, cat.points, max = TRUE, formula = formula, cat.var = cat.var, data.f = data.res, range = range, min.p.cat = control$min.p.cat, Domains = Dim, print.level = control$print.gen, ...)
	  cutpoints <- res$cuts
	  CPE = res$auc
	  # Correct the CPE
	  if(correct.index == TRUE) {
	    CPE.cor <- cpe.opt.corrected(formula = formula, cat.var =  cat.var , data = data.res , c.points = cutpoints , cpe = CPE , B=control$B , b.method = control$b.method)
	  } else {
	    CPE.cor <- NULL
	  }
	}
	# Create the categorical covariate
	data[,paste0(cat.var,"_CatPredi")] <- cut(data[,cat.var], sort(unique(c(max(data[,cat.var]), min(data[,cat.var]), cutpoints))), include.lowest = TRUE, right = TRUE)
	results <- if(method == "addfor" & conc.index == "cindex" & correct.index == TRUE ) {
					list(cutpoints = cutpoints, Cindex = Cindex, Cindex.cor = Cindex.cor,  grid = control$grid)
				} else if(method == "addfor" & conc.index == "cpe" & correct.index == TRUE) {
					list(cutpoints = cutpoints, CPE = CPE, CPE.cor = CPE.cor,  grid = control$grid)
				} else if(method == "genetic" & conc.index == "cindex" & correct.index == TRUE) {
					list(cutpoints = cutpoints, Cindex = Cindex, Cindex.cor = Cindex.cor)
				} else if(method == "genetic" & conc.index == "cpe" & correct.index == TRUE) {
					list(cutpoints = cutpoints, CPE = CPE, CPE.cor = CPE.cor)
				} else if(method == "backaddfor" & conc.index == "cindex" & correct.index == TRUE) {
				  list(cutpoints = cutpoints, Cindex = Cindex, Cindex.cor = Cindex.cor)
				} else if(method == "backaddfor" & conc.index == "cpe" & correct.index == TRUE) {
				  list(cutpoints = cutpoints, CPE = CPE, CPE.cor = CPE.cor)
				}else if(method == "addfor" & conc.index == "cindex" & correct.index == FALSE ) {
					list(cutpoints = cutpoints, Cindex = Cindex, grid = control$grid)
				} else if(method == "addfor" & conc.index == "cpe" & correct.index == FALSE) {
					list(cutpoints = cutpoints, CPE = CPE, grid = control$grid)
				} else if(method == "genetic" & conc.index == "cindex" & correct.index == FALSE) {
					list(cutpoints = cutpoints, Cindex = Cindex)
				} else if (method == "genetic" & conc.index == "cpe" & correct.index == FALSE){
					list(cutpoints = cutpoints, CPE = CPE)
				} else if(method == "backaddfor" & conc.index == "cindex" & correct.index == FALSE) {
				  list(cutpoints = cutpoints, Cindex = Cindex)
				} else { #if (method == "backaddfor" & conc.index == "cpe" & correct.index == FALSE){
				  list(cutpoints = cutpoints, CPE = CPE)
				}
	res <- list(call = match.call(), method = method, conc.index = conc.index, formula = formula, cat.var = cat.var, data = data, correct.index = correct.index, results = results, control =  control)
	class(res) <- "catpredi.survival"
	res
}
