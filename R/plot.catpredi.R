#' Plot the optimal cut points.
#'
#' @param x An object of type catpredi.
#' @param ... Additional arguments to be passed on to other functions. Not yet implemented.
#'
#' @returns
#' This function returns the plot of the relationship between the predictor variable and the outcome.
#'
#' @description Plots the relationship between the predictor variable is aimed to
#'    categorise and the response variable based on a GAM model.
#'    Additionally, the optimal cut points obtained with the catpredi() function
#'    are drawn on the graph.
#'
#' @references  I Barrio, I Arostegui, M.X Rodriguez-Alvarez and  J.M Quintana (2017).
#'    A new approach to categorising continuous variables in prediction models:
#'    proposal and validation. \emph{Statistical Methods in Medical Research}, 26(6), 2586-2602.
#'
#'    I Barrio, J Roca-Pardinas and I Arostegui (2021). Selecting the number of
#'    categories of the lymph node ratio in cancer research: A bootstrap-based
#'    hypothesis test. \emph{Statistical Methods in Medical Research}, 30(3), 926-940.
#'
#' @author Irantzu Barrio, Maria Xose Rodriguez-Alvarez and Inmaculada Arostegui.
#'
#' @seealso
#' \code{\link{catpredi}}
#'
#' @examples
#'  library(CatPredi)
#'  \dontrun{
#'  set.seed(127)
#'  #Simulate data
#'  n = 100
#'  #Predictor variable
#'  xh <- rnorm(n, mean = 0, sd = 1)
#'  xd <- rnorm(n, mean = 1.5, sd = 1)
#'  x <- c(xh, xd)
#'  #Response
#'  y <- c(rep(0,n), rep(1,n))
#'  # Data frame
#'  df <- data.frame(y = y, x = x)
#'
#'  # Select optimal cut points using the AddFor algorithm
#'  res.backaddfor <- catpredi(formula = y ~ 1, cat.var = "x", cat.points = 3,
#'                             data = df, method = "backaddfor", range = NULL, correct.AUC = FALSE)
#'  # Plot
#'  plot(res.backaddfor)
#'  }
#' @export
plot.catpredi <-
function(x, ...){
  # Activate pause between plots
  old_ask <- graphics::par(ask = TRUE)

  # Ensure the setting is reset at the end
  on.exit(graphics::par(ask = old_ask))

  # Fit the model
  formula <- stats::update(x$formula, stats::as.formula(paste("~ . + s(", x$cat.var, ")", sep = "")))
  fit.gam <- mgcv::gam(formula, family = stats::binomial, data = x$data)
  pos <- length(attr(stats::terms.formula(formula, specials = c("s")),"specials")$s)
  plot(fit.gam, select = pos, shade = TRUE, ylab = paste0("f(",x$cat.var,")"), all.terms = TRUE)
  graphics::abline(v = x$results$cutpoints, lty = 2)

  # Plot the OR
  formula <- stats::update(x$formula, stats::as.formula(paste0("~ . +", x$cat.var,"_CatPredi")))
  fit.gam.cut <- mgcv::gam(formula, family = stats::binomial, data = x$data)
  x_CatPredi = unique(x$data[,paste0(x$cat.var,"_CatPredi")])
  sel.cuts <- paste0(x$cat.var,"_CatPredi",x_CatPredi[-1])
  ORs.xcut <- data.frame(x_CatPredi = x_CatPredi,
                         OR = exp(fit.gam.cut$coef)[c("(Intercept)",sel.cuts)],
                         CI_Lower = exp(stats::confint.default(fit.gam.cut))[c("(Intercept)",sel.cuts),1],
                         CI_Upper = exp(stats::confint.default(fit.gam.cut))[c("(Intercept)",sel.cuts),2])

  p <- ggplot2::ggplot(ORs.xcut, ggplot2::aes(x = x_CatPredi, y = OR)) +
    ggplot2::geom_point(size = 3, color = "blue") + # Plot the ORs
    ggplot2::geom_errorbar(ggplot2::aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "blue") + # Add error bars
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "red") + # Reference line at OR = 1
    ggplot2::coord_flip() + # Flip coordinates for better readability
    ggplot2::labs(
      title = "Odds Ratios with 95% Confidence Intervals",
      x = paste0(x$cat.var,"_CatPredi"),
      y = "Odds Ratio (OR)"
    ) +
    ggplot2::theme_minimal()
  print(p)
}
