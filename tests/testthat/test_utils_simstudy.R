# declarations ------------------------------------------------------------

library(simstudy)
defs <- defData(varname = "x0", dist = "normal", formula = 0, variance = 1)
defs <- defData(defs, varname = "x1", dist = "nonrandom",
                    formula = "..x1")
defs <- defData(defs, varname = "x2", dist = "nonrandom",
                    formula = "..x2")
defs <- defData(defs, varname = "x3", dist = "nonrandom",
                    formula = "..x3")
defs <- defData(defs, varname = "mu", dist = "nonrandom",
                    formula = "x0 + x1 + x2 + exp(x3)")
defs <- defData(defs, varname = "sigma", dist = "exponential",
                    formula = 1)
defs <- defData(defs, varname = "y", dist = "normal",
                    formula = "mu", variance = "sigma^2")
Rho <- genCorMat(nvars = 3, cors = 1:3 * 0.25)
dtCor <- genCorData(n = 100L, mu = 0:2, sigma = 1:3 * 0.5, corMatrix = Rho,
                    cnames = c("x1", "x2", "x3"))
# str(dtCor)
# print(defs)

# tests -------------------------------------------------------------------

test_that("injectData: input error", {
  rgx <- "Assertion on 'dtDefs'"
  expect_error(injectData(dtDefs = data.frame(), dtData = dtCor), regexp = rgx)
  rgx <- "Assertion on 'dtData'"
  expect_error(injectData(dtDefs = defs, dtData = data.frame()), regexp = rgx)
})


test_that("injectData: data.table", {
  out <-  injectData(dtDefs = defs, dtData = dtCor)
  # str(out)

  expect_s3_class(out, "data.table")
  expect_identical(names(out), c("id", defs$varname))
  out_means <- sapply(out[, c("x1", "x2", "x3")], FUN = mean)
  data_means <- sapply(dtCor[, c("x1", "x2", "x3")], FUN = mean)
  # expect_equal(dim(out), c(nrow(dtCor), nrow(defs) + 1))

  expect_equal(out_means, data_means)
})

test_that("injectData: data.frame", {
  out <-  as.data.frame(injectData(dtDefs = defs, dtData = dtCor))
  # str(out)

  expect_s3_class(out, "data.frame")
  expect_identical(names(out), c("id", defs$varname))
  out_means <- sapply(out[, c("x1", "x2", "x3")], FUN = mean)
  data_means <- sapply(dtCor[, c("x1", "x2", "x3")], FUN = mean)
  expect_equal(out_means, data_means)
})
