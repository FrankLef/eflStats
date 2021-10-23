#' Create a correlation matrix, given a vector of correlations
#'
#' Create a correlation matrix, given a vector of correlations. If no
#' vector of correlations, a random correlation matrix 2 x 2 will be created.
#'
#' Create a positive-definite matrix which represents the correlations given by
#' the user or, when \code{cors} is a count, a random correlation matrix of
#' size \code{cors} x \code{cors}.
#'
#' @param cors A vector of correlations of a the nb of variables.
#' @param tol Tolerance for the output matrix. Change only if you have a good
#' reason.
#'
#' @seealso calc_nvars sim_cor_mat
#'
#' @return Correlation matrix with the correlations \code{cors}, or, if
#' \code{cors} is a count, a random correlation matrix of \code{cors} variables.
#' @export
#'
#' @examples
#' cors <- c(-0.025667622, -0.009168867, 0.065022678,
#'  0.189735095, -0.011068128, 0.075086348)
#' Rho <- create_cor_mat(cors)
#' stopifnot(identical(dim(Rho), c(4L, 4L)))
#' Rho <- create_cor_mat(4)
#' stopifnot(identical(dim(Rho), c(4L, 4L)))
create_cor_mat <- function(cors = 2L, tol = 1e-6) {
  checkmate::assert_numeric(cors)
  # matrixcalc::is.positive.definite uses tolearnce of 1e-8
  checkmate::assert_number(tol, lower = 1e-6, upper = 1e-4)

  if(length(cors) == 1) {
    checkmate::assert_count(cors - 1, positive = TRUE)
    nvars <- cors
    Rho <- sim_cor_mat(nvars)
  } else {
    checkmate::assert_numeric(cors, lower = -1 + tol, upper = 1 - tol)
    # for the given nb of correlations, i.e. nb of pairs of variables,
    # get the nb of variables that must correspond to it.
    nvars <- calc_nvars(length(cors))
    Rho <- diag(nrow = nvars)
    Rho[lower.tri(Rho)] <- cors
    Rho <- t(Rho)
    Rho[lower.tri(Rho)] <- cors
  }


  # Rho must be rounded, otherwise matrixcalc::is.positive.definite
  # is very sensitive to very small variation and claim the matrix
  # is not symmetric.
  Rho <- round(Rho, -log10(tol))

  # cat("\n", "inside: after rounding", "\n")
  # print(Rho)
  # cat("\n")
  # cat("\n", "inside: eigen after rounding", "\n")
  # print(eigen(Rho)$values)
  # cat("\n")

  # matrix must be positive definite
  ev <- eigen(Rho, only.values = TRUE)
  check <- ev$values < tol
  if (any(check)) {
    msg <- "The correlation matrix is not positive definite."
    msg_head <- cli::col_yellow(msg)
    msg_body <- c("x" = "Correlations have eigen values below the tolerance.",
                  "i" = sprintf("There are %d eigen values below tolerance = %f.",
                                sum(check), tol))
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "create_cor_error1")
  }

  Rho
}


#' Calculate the number of variables implied by a number of pairs
#'
#' Calculate the number of variables implied by a number of pairs.
#'
#' When creating pairs of variables, e.g. for a correlation/covariance
#' matrix, the nb of variables \code{nvars} implies a given number of
#' pairs \code{npairs}. \code{npairs} can be determined since
#' \code{npairs = binomial{nvars}{2}} which gives
#' \code{2 x nvars = npairs^2 - npairs} which is a quadratic equation that can
#' be resolved with \code{npairs = (sqrt(1 - 4 x (-2 x nvars) + 1) / 2}.
#'
#' @param npairs Count, the number of \strong{pairs}.
#'
#' @return Count, number of \strong{variables}.
#' @export
#'
#' @examples
#' nvars <- calc_nvars(6)
#' stopifnot(nvars == 4)
calc_nvars <- function(npairs) {
  checkmate::assert_count(npairs, positive = TRUE)

  npairs <- -2 * npairs
  assertthat::is.count(-npairs)
  nvars <- (sqrt(1 - 4 * npairs) + 1) / 2

  if(!assertthat::is.count(nvars)) {
    msg <- "Invalid nb of combinations, `npairs` is invalid."
    msg_head <- cli::col_yellow(msg)
    msg_body <- c("i" = "Use choose(n, 2) to find your nb of combinations.",
                  "i" = "e.g. 6 pairs required for 4 variables.")
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "calc_nvars_error1")
  }

  as.integer(nvars)
}



#' Simulate a correlation matrix from a given nb of variables
#'
#' Simulate a correlation matrix from a given nb of variables.
#'
#' Simulate a correlation matrix from a given nb of variables by creating
#' a covariance matrix and transforming it to a correlation matrix with
#' \code{stats::cov2cor}. The method was copied from \code{simstudy::genCorMat()}
#' to whom I express my gratitude.
#'
#'
#' @param nvars Positive number of variables, must be >= 2.
#' @param is_cor TRUE (default): Return the correlation matrix.
#' FALSE: return the covariance matrix.
#'
#' @return Correlation matrix \code{nvars x nvars}
#' @export
#'
#' @examples
#' mat <- sim_cor_mat(4)
#' stopifnot(identical(dim(mat), c(4L, 4L)))
sim_cor_mat <- function(nvars = 2, is_cor = TRUE) {
  checkmate::assert_count(nvars - 1, positive = TRUE)

  # This was copied from simstudy::genCorMat(), a great package
  ev <- stats::runif(nvars, 0, 10)  # the eigenvalues
  Z <- matrix(data = stats::rnorm(nvars^2), nrow = nvars)
  decomp <- qr(Z)
  Q <- qr.Q(decomp)
  R <- qr.R(decomp)
  d <- diag(R)
  ph <- d/abs(d)
  O <- Q %*% diag(ph)  # the orthogonal marix
  Z <- t(O) %*% diag(ev) %*% O

  if (is_cor) Z <- stats::cov2cor(Z)
}
