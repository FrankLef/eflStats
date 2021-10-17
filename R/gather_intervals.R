
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
#' @param excl Regex pattern to exclude variables from output.  Usually to
#' exclude "*__" variables such as "lp__".
#' @param fun \code{ggdist::point_interval} function such as \code{mean_qi}.
#' @param ... Extra arguments used by \code{fun}
#'
#' @importFrom dplyr group_vars filter %>%
#'
#' @return Point and interval summary in long format.
#' @export
#'
#' @examples
#' df <- rbind(
#'  data.frame(model = "alpha", a = runif(5), b = rnorm(5), c = rexp(5)),
#'  data.frame(model = "beta", a = runif(5), b = rnorm(5), c = rexp(5))
#'  )
gather_intervals <- function(data,
                             incl = c(".width", ".point", ".interval"),
                             excl = "^.+__",
                             fun = ggdist::mode_qi, ...) {
  checkmate::assert_data_frame(data)
  checkmate::assert_character(incl)
  checkmate::assert_string(excl)
  checkmate::assert_function(fun)


  # get the group before using fun as fun will give ungrouped df
  groups_ <- dplyr::group_vars(data)


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
    msg <- sprintf("Error with %s.", deparse1(substitute(fun)))
    msg <- paste(errorCondition(err), msg)
    msg_head <- cli::col_yellow(msg)
    msg_body <- c("x" = "Did you give a character variable as input?")
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "gather_intervals_error1")
  })

  # the grouping variables see code of tidybayes::gather_variables().
  # include variables listed in `incl`
  all_groups <- union(groups_, incl)


  out <- out %>%
    tidyr::pivot_longer(cols = -all_groups,
                        names_to = ".variable", values_to = ".value") %>%
    dplyr::group_by(dplyr::across(all_groups))

  # remove excluded variables
  if(length(excl)){
    out <- out %>% dplyr::filter(!grepl(pattern = excl, x = .variable))
  }

  out
}


#' Gather \code{ggdist::point_interval} with .point, .lower and .upper columns
#'
#' Gather \code{ggdist::point_interval} with .point, .lower and .upper columns.
#'
#' Take a summary from \code{ggdist::point_interval} and put it in long format
#' with columns \code{.variable} and .code{.value}.  Grouped variables will
#' be kept just as the \code{tidybayes::gather_variables()} does it. Also
#' put the value in 3 columns: .point, .lower and .upper.
#'
#' This function mimics the behavior of \code{tidybayes::gather_variables()}.
#' The code is actually pretty much the same.  See @R-tidybayes.
#'☻
#' @param data \code{ggdist::point_interval} summaries.
#' @param incl Summary variables to include in output.
#' @param excl Regex pattern to exclude variables from output.  Usually to
#' exclude "*__" variables such as "lp__".
#' @param fun \code{ggdist::point_interval} function such as \code{mean_qi}.
#' @param ... Extra arguments used by \code{fun}
#'
#' @importFrom dplyr select group_vars rename_with filter %>%
#'
#' @return Point and interval summary in long format with 3 columns.
#' @export
#'
#' @examples
#' df <- rbind(
#'  data.frame(model = "alpha", a = runif(5), b = rnorm(5), c = rexp(5)),
#'  data.frame(model = "beta", a = runif(5), b = rnorm(5), c = rexp(5))
#'  )
gather_intervals_rng <- function(data,
                                 incl = c(".width", ".point", ".interval"),
                                 excl = "^.+__",
                                 fun = ggdist::mode_qi, ...) {
  checkmate::assert_data_frame(data)
  checkmate::assert_character(incl)
  checkmate::assert_string(excl)
  checkmate::assert_function(fun)

  # get the group before using fun as fun will give ungrouped df
  groups_ <- dplyr::group_vars(data)


  out <- tryCatch({
    fun(data, ...)
  },
  error = function(err) {
    msg <- sprintf("Error with %s.", deparse1(substitute(fun)))
    msg <- paste(errorCondition(err), msg)
    msg_head <- cli::col_yellow(msg)
    msg_body <- c("x" = "Did you give a character variable as input?")
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "gather_intervals_rng_error1")
  })


  # add names from ggdist to the groups
  all_groups <- union(groups_, incl)

  # when there is only one variable, .lower and .upper have no
  # prefix and we must prepend their variable name
  the_bounds <- c(".lower", ".upper")
  the_vars <- setdiff(names(out), c(all_groups, the_bounds))
  if(length(the_vars) == 1) {
    out <- out %>%
      dplyr::rename_with(.cols = the_bounds,
                         .fn = function(x) paste0(the_vars, x))
  }


  # The columns to pivot are the ones NOT in `all_groups` and the ones
  # with suffix .lower and .upper.
  # We add the suffix .value to those pivot columns.
  cols <- c("[.]lower$", "[.]upper$", all_groups)
  rgx <- paste(cols, collapse = "|")
  out <- out %>%
      dplyr::rename_with(.cols = !dplyr::matches(rgx),
                         .fn = function(x) paste(x, "value", sep = "."))


  # convert to long format using the name pattern
  # the pattern means that
  # (.*): the first part must have zero or more characters
  # ([.]value$|[.]lower$|[.]upper$): the second part must finish
  #                                  with .point, lower or .upper
  rgx_cols <- "[.]value$|[.]lower$|[.]upper$"
  rgx_names <- "(.*)([.]value$|[.]lower$|[.]upper$)"
  out <- out %>% tidyr::pivot_longer(
    cols = dplyr::matches(rgx_cols),
    names_to = c(".variable", "name"),
    names_pattern = rgx_names)

  # The "name" column contains 3 names: ".point", ".lower" and ".upper"
  # we use that column to pivot_wider using that column
  cols <- c(all_groups, ".variable")
  out <- out %>%
    tidyr::pivot_wider(id_cols = cols, names_from = "name")

  # remove excluded variables
  if(length(excl)){
    out <- out %>% dplyr::filter(!grepl(pattern = excl, x = .variable))
  }

  out
}
