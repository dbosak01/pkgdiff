dev <- FALSE

test_that("reports1: report_breakages() works as expected.", {

  pkgs <- c("crayon", "common", "rvest")

  if (dev) {

    pkgs <- c("admiral", "common", "rvest")

    pth <- "C:\\Users\\dbosa\\AppData\\Local\\R\\win-library\\4.2"
    lst1 <- get_installed_packages(pkgs,
                                 repos = pth)

  } else {

    lst1 <- get_installed_packages(pkgs)
  }

  lst2 <- get_latest_data(pkgs)[ , c("Package", "Version")]

  res <- report_breakages(lst1, lst2)

  expect_equal(is.data.frame(res$summary), TRUE)

  expect_equal(nrow(res$summary) == 3, TRUE)

  expect_equal(ncol(res$summary) == 4, TRUE)


})

test_that("reports2: report_stability() works as expected.", {

  pkgs <- c("crayon", "common", "rvest")

  if (dev) {

    pkgs <- c("admiral", "common", "rvest")

  }

  res <- report_stability(pkgs, releases = 5)

  expect_equal(is.data.frame(res), TRUE)

  expect_equal(nrow(res) == 3, TRUE)

  expect_equal(ncol(res) == 4, TRUE)


})

