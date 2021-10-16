
test_that("gather_intervals: Without grouping variables.", {
  the_width <- c(0.5, 0.75, 0.95)
  df <- data.frame(a = runif(10), b = rnorm(10), c = rexp(10))

  summ <- df %>%
    gather_intervals(fun = ggdist::mode_qi, .width = the_width)

  nm <- c(".width", ".point", ".interval", ".variable", ".value")
  expect_identical(names(summ), nm)
  nrows <- (length(df)) * length(c("", "lower", "upper")) * length(the_width)
  expect_equal(nrow(summ), nrows)
})


test_that("gather_intervals: With grouping variables.", {
  the_width <- c(0.5, 0.75, 0.95)
  df <- rbind(
    data.frame(model = "alpha", a = runif(5), b = rnorm(5), c = rexp(5)),
    data.frame(model = "beta", a = runif(5), b = rnorm(5), c = rexp(5))
  )

  summ <- df %>%
    dplyr::group_by(model) %>%
    gather_intervals(fun = ggdist::mode_qi, .width = the_width)
  # cat("\n")
  # print(summ)
  # cat("\n")

  nm <- c("model", ".width", ".point", ".interval", ".variable", ".value")
  expect_identical(names(summ), nm)

  nrows <- length(unique(df$model)) * (length(df) - 1) *
    length(c("", "lower", "upper")) * length(the_width)
  expect_equal(nrow(summ), nrows)
})



test_that("gather_intervals_rng: Without grouping variables.", {
  the_width <- c(0.50, 0.75, 0.95)
  df <- data.frame(a = runif(10), b = rnorm(10), c = rexp(10))
  summ <- df %>%
    gather_intervals_rng(fun = ggdist::mode_qi, .width = the_width)

  # cat("\n")
  # print(summ)
  # cat("\n")


  nm <- c(".width", ".point", ".interval", ".variable",
          ".estimate", ".lower", ".upper")
  expect_identical(names(summ), nm)
  nrows <- length(df) * length(the_width)
  expect_equal(nrow(summ), nrows)
})


test_that("gather_intervals_rng: With grouping variables.", {
  the_width <- c(0.5, 0.75, 0.95)
  df <- rbind(
    data.frame(model = "alpha", a = runif(5), b = rnorm(5), c = rexp(5)),
    data.frame(model = "beta", a = runif(5), b = rnorm(5), c = rexp(5))
  )

  summ <- df %>%
    dplyr::group_by(model) %>%
    gather_intervals_rng(fun = ggdist::mode_qi, .width = the_width)
  # cat("\n")
  # print(summ)
  # cat("\n")

  nm <- c("model", ".width", ".point", ".interval", ".variable",
          ".estimate", ".lower", ".upper")
  expect_identical(names(summ), nm)

  nrows <-  length(unique(df$model)) * (length(df) - 1) * length(the_width)
  expect_equal(nrow(summ), nrows)
})
