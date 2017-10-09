\name{plot.catpredi}
\alias{plot.catpredi}
\title{
Plot the optimal cut points.
}
\description{
Plots the relationship between the predictor variable is aimed to categorise and the response variable based on a GAM model. 
Additionally, the optimal cut points obtained with the catpredi() function are drawn on the graph. 
}
\usage{
\method{plot}{catpredi}(x, ...)
}

\arguments{
  \item{x}{An object of type catpredi.}
  \item{\dots}{Additional arguments to be passed on to other functions. Not yet implemented.}
}
%\details{}
\value{
This function returns the plot of the relationship between the predictor variable and the outcome.
}
\references{
I Barrio, I Arostegui, M.X Rodriguez-Alvarez and  J.M Quintana (2015). A new approach to categorising continuous variables in prediction models: proposal and validation. \emph{Statistical Methods in Medical Research} (in press).
}
\author{
Irantzu Barrio, Maria Xose Rodriguez-Alvarez and Inmaculada Arostegui
}
%\note{}


\seealso{
See Also as \code{\link{catpredi}}.
}
\examples{
  library(CatPredi)
  set.seed(127)
  #Simulate data
  n = 100
  #Predictor variable
  xh <- rnorm(n, mean = 0, sd = 1)
  xd <- rnorm(n, mean = 1.5, sd = 1)
  x <- c(xh, xd)
  #Response
  y <- c(rep(0,n), rep(1,n))
  # Data frame
  df <- data.frame(y = y, x = x)
  
  # Select optimal cut points using the AddFor algorithm
  res.addfor <- catpredi(formula = y ~ 1, cat.var = "x", cat.points = 3, 
  				data = df, method = "addfor", range = NULL, correct.AUC = FALSE)
  # Plot
  plot(res.addfor)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{}
%\keyword{}% __ONLY ONE__ keyword per line
