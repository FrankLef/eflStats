#' Create a correlation matrix, given a vector of correlations
#'
#' Create a correlation matrix, given a vector of correlations. If no
#' vector of correlations, a random correlation matrix 2 x 2 will be created.
#'
#' Cretae a positive-definite matrix which represents the correlations given by
#' the user or, when \code{cors} is a count, a random correlation matrix of
#' size \code{cors} x \code{cors}.
#'
#' @param cors A vector of correlations of a the nb of variables.
#' @param tol Tolerance for the output matrix. Change only if you have a good
#' reason.
#'
#' @return Correlation matrix with the corelations \code{cors}, or, if
#' \code{cors} is a count, a random correlation matrix of \code{cors} variables.
#' @export
#'
#' @examples
#' Rho <- create_cor(c(0.25, 0.50, 0.75))
#' stopifnot(identical(dim(Rho), c(3L, 3L)))
#' Rho <- create_cor(3)
#' stopifnot(identical(dim(Rho), c(3L, 3L)))
create_cor <- function(cors = 2L, tol = 10e-6) {
  checkmate::assert_number(tol, lower = 10e-8, upper = 10e-1)

  # must get the nb of variables
  if(length(cors) == 1) {
    checkmate::assert_count(cors - 1, positive = TRUE)
    nvars <- cors
  } else {
    checkmate::assert_numeric(cors, lower = -1 + tol, upper = 1 - tol,
                              null.ok = TRUE)
    # for the given nb of correlations, i.e. nb of combinations of variables
    # get the nb of variables that must correspond to it.
    nvars <- calc_nvars(length(cors))
  }

  # a count was given, therefore we need to create a random correlation matrix
  if(length(cors) == 1) {
    # This was copied from simstudy::genCorMat(), a great package
    ev <- stats::runif(nvars, 0, 10)
    Z <- matrix(data = stats::rnorm(nvars^2), nrow = nvars)
    decomp <- qr(Z)
    Q <- qr.Q(decomp)
    R <- qr.R(decomp)
    d <- diag(R)
    ph <- d/abs(d)
    O <- Q %*% diag(ph)
    Z <- t(O) %*% diag(ev) %*% O
    Rho <- stats::cov2cor(Z)
  } else {
    Rho <- diag(x = 1, nrow = nvars)
    Rho[lower.tri(Rho)] <- cors
    Rho <- t(Rho)
    Rho[lower.tri(Rho)] <- cors
  }

  # Rho must be rouded, otherwise matrixcalc::is.positive.definite
  # is very sensitive to very small variation and claim the matrix
  # is not symmetric.
  Rho <- round(Rho, -log10(tol))

  # matrix must be positive definite
  if (!matrixcalc::is.positive.definite(Rho)) {
    msg <- "The correlarion matrix is not positive definite."
    msg_head <- cli::col_yellow(msg)
    msg_body <- c("i" = "You might want to use Matrix::nearPD().")
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "create_cor_error1")
  }

  Rho
}


#' Calculate the number of variables implied by a number of combinations
#'
#' Calculate the number of variables implied by a number of combinations..
#'
#' When creating combinations of variables, e.g. for a correlation/covariance
#' matrix, the nb of variables \code{nvars} implied a given number of
#' combinations \code{ncomb} can be deducted since \code{ncomb = binomial{nvars}{2}}
#' which gives \code{2 x nvars = ncombs^2 - ncomb} which is a quadratic equation
#' resolved with \code{ncomb = (sqrt(1 - 4 x (-2 x nvars) + 1) / 2}.
#'
#' @param ncomb Count, the number of combinations.
#'
#' @return Count of the number of variables.
#' @export
#'
#' @examples
#' nvars <- calc_nvars(6)
#' stopifnot(nvars == 4)
calc_nvars <- function(ncomb) {
  checkmate::assert_count(ncomb, positive = TRUE)

  ncomb <- -2 * ncomb
  assertthat::is.count(-ncomb)
  nvars <- (sqrt(1 - 4 * ncomb) + 1) / 2

  if(!assertthat::is.count(nvars)) {
    msg <- "Invalid nb of combinations, `ncomb` is invalid."
    msg_head <- cli::col_yellow(msg)
    msg_body <- c("i" = "Use choose(n, k) to find your nb of combinations.",
                  "i" = "e.g. 6 combinations required for 4 variables.")
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "calc_nvars_error1")
  }

  as.integer(nvars)
}
