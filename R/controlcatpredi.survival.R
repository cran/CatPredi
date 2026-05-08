#' Control function
#'
#' @param min.p.cat Set the minimun number of individuals in each category.
#' @param grid Grid size for the AddFor algorithm.
#' @param B Number of bootstrap replicates for the AUC bias correction procedure.
#' @param b.method Allows to specify whether the bootstrap resampling should be
#'    done considering or not the outcome variable.
#'    The option "ncoutcome" indicates that the data is resampled without taking
#'    into account the response variable, while "coutcome" indicates that the data
#'    is resampled in regard to the response variable.
#' @param print.gen Corresponds to the argument print.level of the \code{\link[rgenoud]{genoud}}
#'    function of the package \pkg{rgenoud}.
#'
#' @returns
#' A list with components for each of the possible arguments.
#'
#' @description Function used to set several parameters to control the selection
#'    of the optimal cut points in a Cox proportional hazards regression model.
#'
#' @references Mebane Jr, W. R., & Sekhon, J. S. (2011). Genetic optimization using
#'    derivatives: the rgenoud package for R. \emph{Journal of Statistical Software}
#'    42\bold{11}, 1-26.
#'
#' @author Irantzu Barrio and Maria Xose Rodriguez-Alvarez.
#'
#' @seealso
#' \code{\link{controlcatpredi.survival}}
#'
#' @export
#'
controlcatpredi.survival <-
function(
	min.p.cat = 5,
	grid = 100,
	B = 50,
  b.method = c("ncoutcome","coutcome"),
  print.gen = 0)	{
	list(min.p.cat = min.p.cat, grid = grid, B = B , b.method = match.arg(b.method), print.gen = print.gen )}
