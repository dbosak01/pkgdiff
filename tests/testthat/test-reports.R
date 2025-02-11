dev <- FALSE

test_that("reports1: report_breakages() works as expected.", {

  pkgs <- c("common", "knitr", "rvest")
  vrs <- c("1.1.3", "1.39", "0.3.6")

  if (dev) {

    pkgs <- c("admiral", "common", "rvest")

    pth <- "C:\\Users\\dbosa\\AppData\\Local\\R\\win-library\\4.2"
    lst1 <- installed_packages(pkgs,
                                 repos = pth)

  } else {

    lst1 <- data.frame(Package = pkgs, Version = vrs)
  }

  lst2 <- get_latest_data(pkgs)[ , c("Package", "Version")]

  res <- repo_breakages(lst1, lst2)

  expect_equal(is.data.frame(res$summary), TRUE)

  expect_equal(nrow(res$summary) == 3, TRUE)

  expect_equal(ncol(res$summary) == 4, TRUE)


})

test_that("reports2: report_stability() works as expected.", {

  pkgs <- c("common", "fmtr", "libr", "defineR")

  if (dev) {

    pkgs <- c("admiral", "common", "rvest")

  }

  res <- repo_stability(pkgs)

  expect_equal(is.data.frame(res), TRUE)

  expect_equal(nrow(res) == 4, TRUE)

  expect_equal(ncol(res) == 9, TRUE)


})

