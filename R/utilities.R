
#' Gather \code{ggdist::point_interval} summaries (i.e. long format)
#'
#' Gather \code{ggdist::point_interval} summaries (i.e. long format).
#'
#' Take a summary from \code{ggdist::point_interval} and put it in long format
#' with columns \code{.variable} and .code{.value}.  Grouped variables will
#' be kept just as the \code{tidybayes::gather_variables()} does it.
#'
#' This function mimics the behavior of \code{tidybayes::gather_variables()}.
#' The code is actually pretty much the same.  See @R-tidybayes.
#'☻
#' @param data \code{ggdist::point_interval} summaries.
#' @param incl Summary variables to include in output.
#' @param fun \code{ggdist::point_interval} function such as \code{mean_qi}.
#' @param ... Extra arguments used by \code{fun}
#'
#' @importFrom dplyr group_vars %>%
#'
#' @return Point and interval summary in long format.
#' @export
#'
#' @examples
#' df <- rbind(
#'  data.frame(model = "alpha", a = runif(5), b = rnorm(5), c = rexp(5)),
#'  data.frame(model = "beta", a = runif(5), b = rnorm(5), c = rexp(5))
#'  )
gather_intervals <- function(data, incl = c(".width", ".point", ".interval"),
                             fun = ggdist::mode_qi, ...) {
  # This function is largely inspired by tidybayes::gather_variables().
  # NOTE 1: tidybayes::`gather_variables uses `group_by_at()` which is superseded
  #       by group(by(across(...))).
  # NOTE 2: tidybayes::`gather_variables used `gather` with list of varibales
  #         transformed with quo().  Gather is deprecated.  We use the
  #         recommended solution with `pivot-longer`.

  out <- tryCatch({
    fun(data, ...)
  },
  error = function(err) {
    msg <- sprintf("Error with %s. Did you give a character variable as input?",
                   deparse1(substitute(fun)))
    msg <- paste(errorCondition(err), msg)
    stop(msg)
  })

  # the grouping variables see code of tidybayes::gather_variables().
  # include variables listed in `incl`
  groups_ = dplyr::group_vars(data)
  all_groups <- union(groups_, incl)


  out %>%
    tidyr::pivot_longer(cols = -all_groups,
                        names_to = ".variable", values_to = ".value") %>%
    dplyr::group_by(dplyr::across(all_groups))
}
