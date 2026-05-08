#' A bootstrap-based hypothesis test to select the best number of categories for a
#'    continuous predictor variable in a logistic regression model
#'
#' @param obj1 An object inheriting from class \code{"catpredi"} for k number of cut points.
#' @param obj2 An object inheriting from class \code{"catpredi"} for k+1 number of cut points.
#' @param level The confidence level required for the hypothesis test. By default level = 0.95.
#' @param nb Number of bootstrap resamples. By default nb = 100
#' @param parallel A logical value. if TRUE the bootstrap is processed in parallel.
#' @param plot A logical value. if TRUE the density plot for the bootstrap statistic is provided.
#'
#' @returns
#' This function returns an object of class \code{"compare.AUC.ht"} with the following components:
#' \describe{
#'    \item{t.null}{test statistic, with the difference of the AUCs for the two objects.}
#'    \item{t.boot}{a vector with the \code{nb} bootstrap statistics.}
#'    \item{t.null}{empirical \code{level}-percentile of the bootstrap statistics vector.}
#' }
#'
#' @description Compares two objects of class "catpredi" to evaluate the significance
#'   of the improvement in model performance (in terms of the AUC) by adding k+1 cut-off
#'   points to the predictor variable.
#'
#' @references I Barrio, J Roca-Pardinas and I Arostegui (2021). Selecting the number
#'   of categories of the lymph node ratio in cancer research: A bootstrap-based
#'   hypothesis test. \emph{Statistical Methods in Medical Research}, 30(3), 926-940.
#'
#' @author Irantzu Barrio, Inmaculada Arostegui, Javier Roca-Pardinas and Xabier Amutxastegi.
#'
#' @seealso
#'  \code{\link{catpredi}},
#'  \code{\link{comp.cutpoints}}
#'
#' @importFrom foreach %dopar%
#'
#' @examples
#' library(CatPredi)
#' \dontrun{
#'   set.seed(127)
#'   #Simulate data
#'   n = 100
#'   #Predictor variable
#'   xh <- rnorm(n, mean = 0, sd = 1)
#'   xd <- rnorm(n, mean = 1.5, sd = 1)
#'   x <- c(xh, xd)
#'   #Response
#'   y <- c(rep(0,n), rep(1,n))
#'   # Data frame
#'   df <- data.frame(y = y, x = x)
#'   # Select 2 optimal cut points using the AddFor algorithm. Correct the AUC
#'     res.addfor.k2 <- catpredi(formula = y ~ 1, cat.var = "x", cat.points = 2,
#'                               data = df, method = "addfor", range=NULL, correct.AUC=TRUE,
#'                               control=controlcatpredi(grid=20))
#'   # Select 3 optimal cut points using the AddFor algorithm. Correct the AUC
#'     res.addfor.k3 <- catpredi(formula = y ~ 1, cat.var = "x", cat.points = 3,
#'                               data = df, method = "addfor", range=NULL, correct.AUC=TRUE,
#'                               control=controlcatpredi(grid=20))
#'     comp <-  comp.cutpoints(res.addfor.k2, res.addfor.k3, V = 10)
#'
#'   # Select 1 optimal cut points using the BackAddFor algorithm.
#'     res.backaddfor.k1 <- catpredi(formula = y ~ 1, cat.var = "x", cat.points = 1,
#'                                   data = df, method = "backaddfor", range=NULL, correct.AUC=FALSE)
#'   # Select 2 optimal cut points using the BackAddFor algorithm.
#'     res.backaddfor.k2 <- catpredi(formula = y ~ 1, cat.var = "x", cat.points = 2,
#'                                   data = df, method = "backaddfor", range=NULL, correct.AUC=FALSE)
#'   # Test if k=1 cut-off points is enough to categorise x
#'     comp.k1.k2 <-  compare.AUC.ht(res.backaddfor.k1, res.backaddfor.k2)
#' }
#'
#' @export
compare.AUC.ht <-
  function(obj1, obj2, level = 0.95, nb = 100, parallel = TRUE, plot = TRUE){
    if ((length(obj2$results$cutpoints) - length(obj1$results$cutpoints)) != 1) {
      stop(paste("Models (model 1 and model 2) do not have respectively", length(obj1$results$cutpoints), "and", length(obj1$results$cutpoints) + 1 ,"cut-off points."))
    }
    if (identical(obj1$formula, obj2$formula)){
      formula.ht <- obj1$formula
    } else {
      stop("The formulas of the models are not identical. Please verify that both formulas are the same.")
    }
    if (identical(obj1$cat.var, obj2$cat.var)) {
      cat.var.ht <- obj1$cat.var
    } else {
      stop("The variable to categorise is not the same in both models. Please ensure you are using the same variables.")
    }
    if (identical(obj1$data[, -ncol(obj1$data)],obj2$data[, -ncol(obj2$data)])) {
      data.ht <- obj1$data
    } else {
      stop("The datasets are not the same in both models. Please make sure you are using the same data.")
    }
    if (identical(obj1$method, obj2$method)) {
      method.ht <- obj1$method
    } else {
      stop("The methods are not the same in both models. Please ensure you are using the same method.")
    }
    if (identical(obj1$range, obj2$range)) {
      range.ht <- obj1$range
    } else {
      stop("Range argument is not the same in both models. Please verify they are specified in the same way.")
    }
    if (identical(obj1$correct.AUC, obj2$correct.AUC)) {
      correct.AUC.ht <- obj1$correct.AUC
    } else {
      stop("The correct.AUC argument is not the same in both models. Please verify they are defined in the same way.")
    }

    Tstatistic <- obj2$results$AUC - obj1$results$AUC

    nc <- length(obj1$results$cutpoints)
    variables <- all.vars(formula.ht)
    # res <- catpredi(formula = formula, cat.var = cat.var, cat.points = nc, data, method = method, range = range, control = controlcatpredi())
    formula.ht.n <- stats::update(formula.ht, stats::as.formula(paste0("~ . +", cat.var.ht, "_CatPredi")))
    model <- mgcv::gam(formula.ht.n, data = data.ht, family = "binomial")
    p.boot <- stats::predict(model, data.ht, type = "response")

    if (parallel) {
      n.cores <- max(1, parallel::detectCores() - 1)

      tryCatch({
        doParallel::registerDoParallel(n.cores)
      }, error = function(e) {
        message("Parallel setup failed: ", conditionMessage(e))
        message("Setting number of cores to 2.")
        n.cores <- 2
        doParallel::registerDoParallel(n.cores)
      })

    } else {
      n.cores <- 1
      doParallel::registerDoParallel(n.cores)
    }

    # Bootstrap
    df1.all <- foreach::foreach(i = 1:nb, .verbose = FALSE, .errorhandling = "stop", .inorder=FALSE, .combine = c) %dopar% {
      y <- stats::rbinom(n = nrow(data.ht), size = 1, prob = p.boot)
      data.ht$y <- y
      formula.n <- stats::update(formula.ht, stats::as.formula("y ~ ."))
      df1 <- catpredi(formula = formula.n, cat.var=cat.var.ht, cat.points = nc+1, data=data.ht, method = method.ht, range = range.ht, control = controlcatpredi())$results$AUC[length(catpredi(formula=formula.n, cat.var=cat.var.ht, cat.points = nc+1, data=data.ht, method = method.ht, range = range.ht, control = controlcatpredi())$results$AUC)]-
        catpredi(formula = formula.n, cat.var=cat.var.ht, cat.points = nc, data=data.ht, method = method.ht, range = range.ht, control = controlcatpredi())$results$AUC[length(catpredi(formula=formula.n, cat.var=cat.var.ht, cat.points = nc, data=data.ht, method = method.ht, range = range.ht, control = controlcatpredi())$results$AUC)]
      return(df1)
      gc()
    }
    # Stop the cluster
    doParallel::stopImplicitCluster()

    if(plot){
      plot(stats::density(df1.all), xlab = "t.boot", ylab = "", xlim = range(Tstatistic, stats::density(df1.all)$x),
           main = paste0("Formula = ", as.expression(formula.ht.n), "\n", nc, " vs ", nc+1, " cut-off points"))
      t <- stats::quantile(df1.all, level)
      graphics::abline(v = t, lty = 1)
      graphics::abline(v = Tstatistic, lty = 2)
      graphics::legend("topleft", legend = c(expression(t[level]),expression(t[null])), lty = 1:2, bty = "n", cex=1.5)
    }

    cat("\n\n*************************************************\n")
    cat(paste0("Bootstrap-based hypothesis test for ", nc, " vs ", nc+1, " cut points"))
    cat("\n*************************************************\n\n")

    if (Tstatistic <= t) {
      cat(paste("Don't reject the null hypothesis (H0). Thus, it is enough to take k =", nc, sep = " "))
    } else {
      cat(paste("Reject the null hypothesis (H0). Thus, it is advisable to take k =", nc + 1, sep = " "))
    }

    res<- list(t.null = Tstatistic, t.boost = df1.all, t.null = Tstatistic, t.alpha = t)
    class(res) <- "compare.AUC.ht"
    res
  }
