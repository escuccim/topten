#' Building a model using the top ten features
#'
#' This function analyses a data set, and chooses the top ten features that are the best predictions
#' of y.
#'
#' @param x a matrix of predictors, with at least ten columns
#' @param y a vector of the same length as x containing the outcomes
#' @return a vector of coefficients from the final fitted model
#' @author Eric Scuccimarra
#' @details
#' This function runs a univariate regression for each column in x against y, then chooses the top ten predictors
#' by p value. These predictors are then used to fit a final model against y, and returns the coefficients.
#' @seealso \code{lm}
#' @export
#' @importFrom stats lm
topten <- function(x, y) {
    m <- ncol(x)
    if(m < 10){
        stop("There are less than ten columns in the data!")
    }
    pvalues <- numeric(m)
    for(i in seq_len(m)) {
        fit <- lm(y ~ x[,i])
        summary <- summary(fit)
        pvalues[i] <- summary$coefficients[2,4]
    }
    ordered <- order(pvalues)[1:10]
    x10 <- x[, ordered]
    fit <- lm(y ~ x10)
    coef(fit)
}

#' Predicts outcomes using top ten features
#'
#' This function takes in a vector of coefficients produced by \code{topten} function and applies the
#' coefficients to a data set to return a vector of predictions.
#'
#' @param X a matrix of data with ten features
#' @param b a vector of coefficients for the model obtained from \code{topten}
#' @return a numeric vector of predicted values
#' @author Eric Scuccimarra
#' @details
#' This function multiplies a matrix of data by a vector of coefficients from a linear model to return a vector of predictions.
#'
#' @export
#'
predict10 <- function(X, b){
    X <- cbind(1, X)
    drop(X %*% b)
}
