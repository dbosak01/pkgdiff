dev <- FALSE

test_that("reports1: repo_breakages() works as expected.", {

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

test_that("reports2: repo_stability() works as expected.", {

  pkgs <- c("common", "fmtr", "libr", "defineR")

  if (dev) {

    pkgs <- c("admiral", "common", "rvest")

  }

  res <- repo_stability(pkgs)

  expect_equal(is.data.frame(res), TRUE)

  expect_equal(nrow(res) == 4, TRUE)

  expect_equal(ncol(res) == 9, TRUE)


})

test_that("reports3: repo_breakages() handles edge cases.", {


  if (dev) {

    r1 <- pkg_repo(ver = "4.2.1")
    r2 <- pkg_repo(ver = "latest")

    # What happens when you upgrade
    # the whole repository
    # Lot of situations to handle
    res <- repo_breakages(r1, r2)

    res$summary
    res$details$rmarkdown
    res$details$admiral$RemovedFunctions

    expect_equal(is.data.frame(res$summary), TRUE)
    expect_equal(nrow(res$summary) > 0, TRUE)
    expect_equal(ncol(res$summary) == 5, TRUE)
    expect_equal(any(res$summary$Breakages == TRUE), TRUE)

  } else {

    expect_equal(TRUE, TRUE)
  }

})


test_that("reports4: repo_stability() handles edge cases.", {


  if (dev) {

    r1 <- pkg_repo(ver = "4.2.1")


    # What happens when you get stability scores
    # for a whole repository

    # Failed on appgen (not a real package)
    # Failed on KMsurv (no releases within 10 years)
    # concatenate package getting no score
    # All above fixed now.
    res <- repo_stability(r1$Package)

    expect_equal(is.null(res), FALSE)
    expect_equal(nrow(res) > 0, TRUE)
    expect_equal(ncol(res), 9)


  } else {

    expect_equal(TRUE, TRUE)
  }

})


