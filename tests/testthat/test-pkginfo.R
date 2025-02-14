base_path <- "c:/packages/pkgdiff/tests/testthat"
base_path <- "."

dev <- FALSE


test_that("info1: pinfo() basic functionality.", {

  flst <- list()

  flst[["myfunc1"]] <- c("param1", "param2")


  flst[["myfunc2"]] <- c("param1", "param2")


  res <- pinfo("logr", "1.3.7", release = as.Date("2022-01-01"),
                  title = "My title", description = "My description",
                  maintainer = "David Bosak",
                  depends = "R (>= 4.0)", linkingto = "something",
                  suggests = "common", enhances = NULL, license = "CC0",
                  repository = "https://cran.com",
                  functions = flst)

  res

  expect_equal("pinfo" %in% class(res), TRUE)
  expect_equal(res$PackageName, "logr")
  expect_equal(res$Version, "1.3.7")
  expect_equal(res$Title, "My title")
  expect_equal(res$Maintainer, "David Bosak")
  expect_equal(res$Suggests, "common")
  expect_equal(res$License, "CC0")
  expect_equal(res$Release, as.Date("2022-01-01"))
  expect_equal(length(res$Functions), 2)

})



test_that("info2: pkg_info() default params github.", {

  res <- pkg_info("logr")

  expect_equal("pinfo" %in% class(res), TRUE)
  expect_equal(res$Package, "logr")
  expect_equal(is.null(res$Version), FALSE)


})

test_that("info3: pkg_info() version params github.", {

  res <- pkg_info("logr", "1.3.5")

  expect_equal("pinfo" %in% class(res), TRUE)
  expect_equal(res$Package, "logr")
  expect_equal(res$Version, "1.3.5")


})


test_that("info4: get_functions() works as expected.", {

  fs <- c("descriptions", "descriptions<-")

  pth <- file.path(base_path, "data/descriptions.R")

  res <- get_functions(pth, fs, NULL)

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 2)
  expect_equal(is.null(res[["descriptions"]]), FALSE)
  expect_equal(res[["descriptions"]], "x")

})


test_that("info5: pkg_info() default params cran.", {

  res <- pkg_info("ards")

  res

  expect_equal("pinfo" %in% class(res), TRUE)
  expect_equal(res$Package, "ards")
  expect_equal(is.null(res$Version), FALSE)
  expect_equal(length(res$Functions) > 0, TRUE)

  res <- pkg_info("Matrix")

  res

  expect_equal("pinfo" %in% class(res), TRUE)
  expect_equal(res$Package, "Matrix")
  expect_equal(is.null(res$Version), FALSE)
  expect_equal(length(res$Functions) > 0, TRUE)

})

test_that("info6: pkg_info() cache parameter, exportPatterns, and special cases.", {

  # Ignore cache and take directly from CRAN
  # Old versions had no functions.  Was using exportPattern ^[^\\.]
  res <- pkg_info("ggplot2", "0.8.9", cache = FALSE)

  res

  expect_equal("pinfo" %in% class(res), TRUE)
  expect_equal(res$Package, "ggplot2")
  expect_equal(is.null(res$Version), FALSE)
  expect_equal(length(res$Functions) > 0, TRUE)

  # Had no functions.  Was using export pattern ^[[:alpha:]]+
  res <- pkg_info("ccRemover", "latest", cache = FALSE)

  res

  expect_equal("pinfo" %in% class(res), TRUE)
  expect_equal(res$Package, "ccRemover")
  expect_equal(is.null(res$Version), FALSE)
  expect_equal(length(res$Functions) > 0, TRUE)

  # Had no functions.  Not sure what problem was.  Is working now.
  res <- pkg_info("digest", "latest", cache = FALSE)

  res

  expect_equal("pinfo" %in% class(res), TRUE)
  expect_equal(res$Package, "digest")
  expect_equal(is.null(res$Version), FALSE)
  expect_equal(length(res$Functions) > 0, TRUE)

  # R6 package.  Appears to be working good.
  # All exported functions captured.
  res <- pkg_info("tinkr", "latest", cache = FALSE)

  res

  expect_equal("pinfo" %in% class(res), TRUE)
  expect_equal(res$Package, "tinkr")
  expect_equal(is.null(res$Version), FALSE)
  expect_equal(length(res$Functions) > 0, TRUE)
  expect_equal(is.null(res$Functions[["yarn"]]), FALSE)

  # S4 package.
  res <- pkg_info("Matrix", "latest", cache = FALSE)

  res

  expect_equal("pinfo" %in% class(res), TRUE)
  expect_equal(res$Package, "Matrix")
  expect_equal(is.null(res$Version), FALSE)
  expect_equal(length(res$Functions) > 0, TRUE)
  expect_equal(is.null(res$Functions[["Schur"]]), FALSE)

})




# Not doing good with S4 classes
# pth <- "https://cran.r-project.org/src/contrib/Matrix_1.7-2.tar.gz"
#
# library(packageDiff)
# res2 <- packageDiff::pkgInfo(pth)
#
# length(res$Functions)
# length(res2$ExportedFunctions)
#
# length(res2$FormalArgs)

