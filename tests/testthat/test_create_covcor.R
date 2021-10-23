test_that("calc_nvars", {
  # npairs must be a count
  rgx <- "Assertion on 'npairs'"
  expect_error(calc_nvars(0), regexp = rgx)

  # npairs is invalid
  expect_error(calc_nvars(4), class = "calc_nvars_error1")

  n <- 4L
  nvars <- calc_nvars(choose(n, 2))
  expect_identical(nvars, n)
})

test_that("sim_cor_mat", {

  # nvars must be a count >= 2
  rgx <- "Assertion on 'nvars - 1'"
  expect_error(sim_cor_mat(1), regexp = rgx)

  mat <- sim_cor_mat(4)
  ev <- eigen(mat, only.values = TRUE)
  expect_true(all(ev$values > 0))
})

test_that("create_cor_mat: Input error", {

  # tolerance must be between 10e-8 and 10e-1
  rgx <- "Assertion on 'tol'"
  # skip("manual")
  expect_error(create_cor_mat(tol = 0), regexp = rgx)

  # length(cors) != choose(nvars, 2)
  # skip("manual")
  rgx <- "Assertion on 'cors - 1'"
  expect_error(create_cor_mat(cors = 1), regexp = rgx)

  # length(cors) != choose(nvars, 2)
  # skip("manual")
  expect_error(create_cor_mat(cors = runif(4)), class = "calc_nvars_error1")

  # abs(cors) must be >= lower tolerance
  rgx <- "Assertion on 'cors'"
  # skip("manual")
  expect_error(create_cor_mat(cors = c(-1, 0, 0.1)), regexp = rgx)

  # abs(cors) must be <= upper tolerance
  rgx <- "Assertion on 'cors'"
  # skip("manual")
  expect_error(create_cor_mat(cors = c(-0.1, 0, 1)), regexp = rgx)

  # invalid correlations which gives a non positive.definite matrix
  cors <- c(-0.9, -0.6, -0.3, 0.3, 0.6, 0.9)
  expect_error(create_cor_mat(cors), class = "create_cor_error1")
})


test_that("create_cor_mat: With nb of vars for random matrix.", {

  Rho <- create_cor_mat()
  # skip("manual")
  expect_type(Rho, "double")
  # skip("manual")
  expect_identical(dim(Rho), c(2L, 2L))

  Rho <- create_cor_mat(4)
  # skip("manual")
  expect_type(Rho, "double")
  # skip("manual")
  expect_identical(dim(Rho), c(4L, 4L))

})

test_that("create_cor_mat: With vector of correlations.", {

  # this sequence gives a positivedefinite matrix
  cors <- seq(from = 0.1, to = 0.9, length.out = 6)
  Rho <- create_cor_mat(cors)

  # cat("\n")
  # str(Rho)
  # cat("\n")

  # skip("manual")
  expect_type(Rho, "double")
  # skip("manual")
  expect_identical(dim(Rho), c(4L, 4L))
})
