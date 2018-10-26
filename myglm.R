
# Select Build, Build and reload to build and lode into the R-session.

myglm <- function(formula, data = list(), contrasts = NULL, family = "poisson",
                  response = exp,...){
  # Extract model matrix & responses
  mf <- model.frame(formula = formula, data = data)
  x  <- model.matrix(attr(mf, "terms"), data = mf, contrasts.arg = contrasts)
  y  <- model.response(mf)
  terms <- attr(mf, "terms")
  
  p <- ncol(x)
  n <- nrow(x)
  # Add code here to calculate coefficients, residuals, fitted values, etc...
  # and store the results in the list est
  est <- list(terms = terms, model = mf)
  logLikelihood <- NULL
  scoreFunction <- NULL
  
  if (family == "poisson") {
    response <- exp
    # the Poisson log-likelihood
    logLikelihood <- function(beta) {
      sum0 <- 0
      for (i in range(1, n)) {
        eta_i <- t(x[i,]) %*% beta
        lambda_i <- response(eta_i)
        sum0 <- sum0 + y[i]*log(lambda_i) - log(factorial(y[i])) - lambda_i
      }
      return(-sum0)
    }
    responsePrime <- exp
    
    scoreFunction <- function(beta) {
      sum0 <- rep(0, p)
      for (i in range(1, n)) {
        eta_i <- t(x[i,]) %*% beta
        sum0 <- sum0 + (y[i] - responsePrime(eta_i)) * x[i,]
      }
      return(-sum0)
    }
  } else {
    stop(sprintf("Unknown family %s", family))
  }
  scoreSquaredSum <- function (beta) {sum(scoreFunction(beta)^2)}
  logl = function(betas){
    l = -(y%*%X%*%betas-sum(exp(X%*%betas)))
    return(l)
  }
  
  beta_init <- matrix(0, nrow = p, ncol = 1)
  est$result <- optim(beta_init, fn = logl, method = 'BFGS')
  est$betaHat <- betaHat <- est$result$par
  rownames(betaHat) <- colnames(X)
  est$covar <- covar <- matrix(0, ncol = p, nrow = p)
  
  est$yhat <- yhat <- x %*% betaHat
  est$residuals <- residuals <- y - yhat
  est$dof <- dof <- n-p
  est$ssr <- ssr <- sum(residuals^2) # Residual sum of squares
  est$rse <- rse <- sqrt(ssr/dof) # Residual standard error
  est$sst <- sst <- sum((y-mean(y))^2)  # Total sum of squares
  est$sse <- sse <- sst-ssr
  est$r2 <- r2 <- 1-ssr/sst # R^2
  est$r2adj <- r2adj <- 1-(1-r2)*(n-1)/(n-p)
  est$Fstat <- Fstat <- (sse)/(p - 1) * (n-p)/ssr # F-statistic
  est$Fpval <- Fpval <- 1-pchisq(Fstat*(p-1), df = p-1)
  
  # z-test
  statistics <- rep(0, p)
  pvalues <- rep(0, p)
  for (j in 1:p) {
    statistics[j] <- betaHat[j] / sqrt(covar[j, j])
    pvalues[j] <- 2*(1-pnorm(abs(statistics[j])))
  }
  # Store call and formula used
  est$statistics <- statistics
  est$pvalues <- pvalues
  
  # Store call and formula used
  est$call <- match.call()
  est$formula <- formula
  
  # Set class name. This is very important!
  class(est) <- 'myglm'
  
  # Return the object with all results
  return(est)
}

print.myglm <- function(est, ...){
  # Code here is used when print(object) is used on objects of class "myglm"
  # Useful functions include cat, print.default and format
  cat("Call:\nmylm : formula = ")
  print(est$formula)
  cat('\nCoefficients:\n')
  v = as.vector(as.numeric(format(est$betaHat, digits = 4, nsmall = 4, trim = T))) # formatting s.t. there are only five decimals
  names(v) = rownames(est$betaHat)
  v
}

summary.myglm <- function(est, ...){
  # Code here is used when summary(object) is used on objects of class "myglm"
  # Useful functions include cat, print.default and format
  cat('Summary of myglm\n\n')
  
  cat("Residuals: \n")
  v = quantile(est$residuals, names = T)
  names(v) = c("Min", "1Q", "Median", "3Q", "Max")
  print(v, digits = 3)
  
  cat("\nCoefficients:\n")
  
  mat = as.matrix(cbind(est$betaHat, sqrt(diag(est$covar)), est$statistics, est$pvalues))
  colnames(mat) = c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
  print(mat, digits = 4)    # how many digits?
  cat("---\n")
  cat("Signif. codes:\t0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1")
  cat("\n\nResidual standard error:", est$rse, "on", est$dof, "degrees of freedom\n")
  cat("Multiple R-squared:", est$r2, "\tAdjusted R-squared:", est$r2adj, "\n")
  cat("F-statistic:", est$Fstat, "on", length(est$beta)-1, "and", est$dof, "DF, p-value: <", est$Fpval, "\n")
}

plot.myglm <- function(est, ...){
  # Code here is used when plot(object) is used on objects of class "myglm"
  
  library(ggplot2)
  # ggplot requires that the data is in a data.frame, this must be done here
  ggplot() + geom_point()
  
  # if you want the plot to look nice, you can e.g. use "labs" to add labels, and add colors in the geom_point-function
  
}

anova.myglm <- function(object, ...){
  # Code here is used when anova(object) is used on objects of class "myglm"
  
  # Components to test
  comp <- attr(object$terms, "term.labels")
  
  # Name of response
  response <- deparse(object$terms[[2]])
  
  # Fit the sequence of models
  txtFormula <- paste(response, "~", sep = "")
  model <- list()
  for(numComp in 1:length(comp)){
    if(numComp == 1){
      txtFormula <- paste(txtFormula, comp[numComp])
    }
    else{
      txtFormula <- paste(txtFormula, comp[numComp], sep = "+")
    }
    formula <- formula(txtFormula)
    model[[numComp]] <- myglm(formula = formula, data = object$model)
  }
  
  # Print Analysis of Variance Table
  cat('Analysis of Variance Table\n')
  cat(c('Response: ', response, '\n'), sep = '')
  cat('          Df  Sum sq X2 value Pr(>X2)\n')
  for(numComp in 1:length(comp)){
    # Add code to print the line for each model tested
  }
  
  invisible(model)
  
}


# You can put these functions somewhere else, but it is practical to keep them with the other functions you create
# optim requires thtat the first argument of the function optim shall optimize is the parameters over which minimization is to take place (par)
# it is common to include all other necessary arguments in a list called "args", but this can be done other ways as well
loglik_poi <- function(par, args = list(n = )){
  
  
}

