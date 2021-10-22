test_that("calc_nvars", {
  # ncomb must be a count
  rgx <- "Assertion on 'ncomb'"
  expect_error(calc_nvars(ncomb = 0), regexp = rgx)

  # ncomb is invalid
  expect_error(calc_nvars(ncomb = 4), class = "calc_nvars_error1")

  n <- 2L
  nvars <- calc_nvars(ncomb = choose(n, 2))
  expect_identical(nvars, n)

  n <- 4L
  nvars <- calc_nvars(ncomb = choose(n, 2))
  expect_identical(nvars, n)
})


test_that("create_cor: Input error", {

  # tolerance must be between 10e-8 and 10e-1
  rgx <- "Assertion on 'tol'"
  # skip("manual")
  expect_error(create_cor(tol = 0), regexp = rgx)

  # length(cors) != choose(nvars, 2)
  # skip("manual")
  rgx <- "Assertion on 'cors - 1'"
  expect_error(create_cor(cors = 1), regexp = rgx)

  # length(cors) != choose(nvars, 2)
  # skip("manual")
  expect_error(create_cor(cors = runif(4)), class = "calc_nvars_error1")

  # abs(cors) must be >= lower tolerance
  rgx <- "Assertion on 'cors'"
  # skip("manual")
  expect_error(create_cor(cors = c(-1, 0, 0.1)), regexp = rgx)

  # abs(cors) must be <= upper tolerance
  rgx <- "Assertion on 'cors'"
  # skip("manual")
  expect_error(create_cor(cors = c(-0.1, 0, 1)), regexp = rgx)
})


test_that("create_cor: With nb of vars for random matrix.", {

  Rho <- create_cor()
  # skip("manual")
  expect_type(Rho, "double")
  # skip("manual")
  expect_identical(dim(Rho), c(2L, 2L))

  Rho <- create_cor(4)
  # skip("manual")
  expect_type(Rho, "double")
  # skip("manual")
  expect_identical(dim(Rho), c(4L, 4L))

})

test_that("create_cor: With vector of correlations.", {

  Rho <- create_cor(cors = c(-0.75, -0.25, -0.1, 0.1, 0.25, 0.75))

  # cat("\n")
  # str(Rho)
  # cat("\n")

  # skip("manual")
  expect_type(Rho, "double")
  # skip("manual")
  expect_identical(dim(Rho), c(4L, 4L))
})
