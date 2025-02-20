dev <- FALSE


test_that("repo1: get_base_paths() works as expected.", {

  if (dev) {

    res <- get_base_paths(.libPaths())

    res

    expect_equal(is.null(res), FALSE)
    expect_equal(length(res), 2)


  } else {
    expect_equal(TRUE, TRUE)
  }

})


test_that("repo2: get_rversion() works as expected.", {


  res <- get_rversion()

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(nchar(res) > 0, TRUE)


})


test_that("repo3: pkg_repo() basic functionality no params.", {

  res <- pkg_repo()

  expect_equal("data.frame" %in% class(res), TRUE)
  expect_equal("prepo" %in% class(res), TRUE)
  expect_equal(is.null(attr(res, "Version")), FALSE)
  expect_equal(is.null(attr(res, "LibPaths")), TRUE)
})


test_that("repo4: pkg_repo() basic functionality no params.", {

  res <- pkg_repo(c("common", "rvest", "crayon"))

  res

  expect_equal("data.frame" %in% class(res), TRUE)
  expect_equal("prepo" %in% class(res), TRUE)
  expect_equal(is.null(attr(res, "Version")), FALSE)
  expect_equal(is.null(attr(res, "LibPaths")), TRUE)
  expect_equal(nrow(res) == 3, TRUE)

  res <- pkg_repo(c("common", "rvest", "crayon"), ver = "latest")

  res

  expect_equal("data.frame" %in% class(res), TRUE)
  expect_equal("prepo" %in% class(res), TRUE)
  expect_equal(is.null(attr(res, "Version")), TRUE)
  expect_equal(is.null(attr(res, "LibPaths")), TRUE)
  expect_equal(nrow(res) == 3, TRUE)

})



test_that("repo5: split_version() works as expected.", {

   res <- split_version("4.1")

   expect_equal(res$lver, "R-4.1.*")
   expect_equal(res$ver, "4.1")

   res <- split_version("4.1.2")
   expect_equal(res$lver, "R-4.1.2")
   expect_equal(res$ver, "4.1")

   res <- split_version("4")
   expect_equal(res$lver, "R-4.*")
   expect_equal(res$ver, "4")
})

test_that("repo6: pkg_repo() rversion parameter.", {

  if (dev) {

    expect_error(pkg_repo(c("common", "rvest", "crayon"), "4.2"))

    res <- pkg_repo(c("common", "rvest", "crayon"), "4.2.1")

    res

    expect_equal("data.frame" %in% class(res), TRUE)
    expect_equal("prepo" %in% class(res), TRUE)
    expect_equal(attr(res, "Version"), "4.2.1")
    expect_equal(length(attr(res, "LibPaths")), 2)
    expect_equal(nrow(res) == 3, TRUE)

  } else {

    expect_equal(TRUE, TRUE)
  }

})


test_that("repo7: pkg_repo() libpaths parameter.", {

  if (dev) {

    pths <- c("C:/Users/dbosa/AppData/Local/R/win-library/4.2",
             "C:/Program Files/R/R-4.2.1/library")

    res <- pkg_repo(c("common", "rvest", "crayon"), libpaths = pths)

    res

    expect_equal("data.frame" %in% class(res), TRUE)
    expect_equal("prepo" %in% class(res), TRUE)
    expect_equal(attr(res, "Version"), NA)
    expect_equal(length(attr(res, "LibPaths")), 2)
    expect_equal(nrow(res) == 3, TRUE)

  } else {

    expect_equal(TRUE, TRUE)
  }

})


test_that("repo8: pkg_repo() version and libpaths parameter.", {

  if (dev) {

    pths <- c("C:/Users/dbosa/AppData/Local/R/win-library/",
              "C:/Program Files/R/")

    res <- pkg_repo(c("common", "rvest", "crayon"), rversion = "4.2.1",
                    libpaths = pths)

    expect_equal("data.frame" %in% class(res), TRUE)
    expect_equal("prepo" %in% class(res), TRUE)
    expect_equal(attr(res, "Version"), "4.2.1")
    expect_equal(length(attr(res, "LibPaths")), 2)
    expect_equal(nrow(res) == 3, TRUE)

  } else {

    expect_equal(TRUE, TRUE)
  }

})


test_that("repo9: pkg_repo() non-CRAN package.", {


  p <- c("common", "rvest", "forker")

  res <-  pkg_repo(p)

  expect_equal(nrow(res) == 3, TRUE)
  expect_equal("forker" %in% res$Package, TRUE)

  p <- c("forker")

  res <-  pkg_repo(p)

  expect_equal(nrow(res) == 1, TRUE)
  expect_equal("forker" %in% res$Package, TRUE)


})






