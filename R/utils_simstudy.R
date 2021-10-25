#' Inject pre-existing data into a \code{defData} from \code{simstudy}
#'
#' Inject pre-existing data into a \code{defData} from \code{simstudy}.
#'
#' Sometimes we need to inject data into a \code{defData} from \code{simstudy}.
#' This is particularly true when using correlated data which cannot be
#' manipulated with \code{defData} unless the double notation \code{..} in
#' \code{formula}.  This function is particularly useful when using correlated
#' data and avoid the tricks required when working with such data.
#'
#' @param dtDefs \code{defData} table  from \code{simstudy}.
#' @param dtData data.frame or data.table to inject.
#'
#' @return data with a \code{defData} together.
#' @export
#'
#' @examples
#' # create data definition with ..x notation
#' defs <- simstudy::defData(varname = "x0", dist = "normal",
#'   formula = 0, variance = 1)
#' defs <- simstudy::defData(defs, varname = "x1", dist = "nonrandom",
#'   formula = "..x1")
#' defs <- simstudy::defData(defs, varname = "x2", dist = "nonrandom",
#'   formula = "..x2")
#' defs <- simstudy::defData(defs, varname = "x3", dist = "nonrandom",
#'   formula = "..x3")
#' defs <- simstudy::defData(defs, varname = "mu", dist = "nonrandom",
#'   formula = "x0 + x1 + x2 + x3")
#' defs <- simstudy::defData(defs, varname = "sigma", dist = "exponential",
#'   formula = 1)
#' defs <- simstudy::defData(defs, varname = "y", dist = "normal",
#'   formula = "mu", variance = "sigma^2")
#' # correlation matrix used to generate correlated data fro x1, x2 and x3
#' Rho <- simstudy::genCorMat(nvars = 3, cors = 1:3 * 0.25)
#' # generate correlated data for x1, x2 and x3
#' dtCor <- simstudy::genCorData(n = 100L, mu = 0:2, sigma = 1:3 * 0.5,
#'   corMatrix = Rho, cnames = c("x1", "x2", "x3"))
#' # inject data into the definitions
#' out <- injectData(dtDefs = defs, dtData = dtCor)
#' stopifnot(identical(names(out), c("id", defs$varname)))
injectData <- function(dtDefs, dtData) {
  checkmate::assert_data_frame(dtDefs, min.rows = 1, min.cols = 1)
  checkmate::assert_data_frame(dtData, min.rows = 1, min.cols = 1)

  for (nm in names(dtData)) {
    # cat("\n")
    # print(nm)
    # cat("\n")
    # NOTE: .. notation in bracket is peculiar to datatables!
    #       unlist() because must have atomic vector
    assign(x = eval(nm), value = unlist(dtData[, nm], use.names = FALSE))
  }
  simstudy::genData(nrow(dtData), dtDefs)
}
