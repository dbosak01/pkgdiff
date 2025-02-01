
dev <- FALSE

test_that("utilities1: get_archive_versions() works as expected.", {

  res <- get_archive_versions("fmtr")

  expect_equal(is.data.frame(res), TRUE)

  expect_equal(nrow(res) > 0, TRUE)

  expect_equal(ncol(res) == 5, TRUE)




  if (dev) {

    res <- get_archive_versions(c("fmtr", "common", "rvest"))

    expect_equal(is.data.frame(res), TRUE)

    expect_equal(nrow(res) > 0, TRUE)

    expect_equal(ncol(res) == 5, TRUE)

  }
})


test_that("utilities2: get_latest_version() works as expected.", {

  res <- get_latest_version("logr")

  expect_equal(is.data.frame(res), FALSE)

  expect_equal(length(res) == 1, TRUE)

  res <- get_latest_version(c("logr", "fmtr", "common"))

  expect_equal(is.data.frame(res), FALSE)

  expect_equal(is.list(res), FALSE)

  expect_equal(length(res) == 3, TRUE)


})

test_that("utilities3: get_version() works as expected.", {


  vers <- c("logr_1.2.3.tar.gz", "logr_1.2.4.tar.gz", "logr_1.3.0.tar.gz")


  res <- get_version(vers)

  expect_equal(res[1], "1.2.3")
  expect_equal(res[2], "1.2.4")
  expect_equal(res[3], "1.3.0")

})

test_that("utilities4: get_installed_packages() works as expected.", {


  res <- get_installed_packages()

  expect_equal(is.data.frame(res), TRUE)

  expect_equal(nrow(res) > 1, TRUE)

  expect_equal(ncol(res) == 2, TRUE)

  if (dev) {
    res <- get_installed_packages(c("logr", "fmtr", "common", "reporter"))

    expect_equal(is.data.frame(res), TRUE)

    expect_equal(nrow(res) > 1, TRUE)

    expect_equal(ncol(res) == 2, TRUE)

    pth <- "C:\\Users\\dbosa\\AppData\\Local\\R\\win-library\\4.2"

    res <- get_installed_packages(c("logr", "fmtr", "common", "reporter"),
                                  repos = pth)

    expect_equal(is.data.frame(res), TRUE)

    expect_equal(nrow(res) > 1, TRUE)

    expect_equal(ncol(res) == 2, TRUE)
  }

})


test_that("utilities5: get_current_version() works as expected.", {


  res <- get_current_version("common")

  expect_equal(is.data.frame(res), FALSE)

  expect_equal(length(res), 1)

  expect_equal(nchar(res) > 0, TRUE)

})


test_that("utilities6: get_file_name() works as expected.", {

  res <- get_file_name("common", "1.2.3")

  expect_equal(res, "common_1.2.3.tar.gz")
})


test_that("utilities7: get_latest_data() works as expected.", {

  res <- get_latest_data("fmtr")

  expect_equal(is.data.frame(res), TRUE)

  expect_equal(nrow(res) == 1, TRUE)

  expect_equal(ncol(res) == 5, TRUE)

  if (dev) {

     res <- get_latest_data(c("common", "purr", "fmtr", "rvest"), skip_size = TRUE)

     expect_equal(is.data.frame(res), TRUE)

     expect_equal(nrow(res) == 3, TRUE)

     expect_equal(ncol(res) == 5, TRUE)

  }

  res <- get_latest_data("purr")

  expect_equal(is.null(res), TRUE)

})


test_that("utilities8: get_removed_functions() works as expected.", {

  if (dev) {

    a1 <- "https://cran.r-project.org/src/contrib/Archive/admiral/admiral_0.12.3.tar.gz"
    a2 <- "https://cran.r-project.org/src/contrib/Archive/admiral/admiral_1.0.0.tar.gz"

    # Lot of changes
    info1 <- suppressWarnings(packageDiff::pkgInfo(a1))
    info2 <- suppressWarnings(packageDiff::pkgInfo(a2))

    res <- get_removed_functions(info1, info2)

    expect_equal(length(res) > 0, TRUE)


  } else {

    expect_equal(TRUE, TRUE)
  }

})



test_that("utilities9: get_removed_parameters() works as expected.", {

  if (dev) {

    a1 <- "https://cran.r-project.org/src/contrib/Archive/admiral/admiral_0.12.3.tar.gz"
    a2 <- "https://cran.r-project.org/src/contrib/Archive/admiral/admiral_1.0.0.tar.gz"

    # Lot of changes
    info1 <- suppressWarnings(packageDiff::pkgInfo(a1))
    info2 <- suppressWarnings(packageDiff::pkgInfo(a2))

    res <- get_removed_parameters(info1, info2)

    expect_equal(length(res) > 0, TRUE)


  } else {

    expect_equal(TRUE, TRUE)
  }

})

test_that("utilities10: get_added_functions() works as expected.", {

  if (dev) {

    a1 <- "https://cran.r-project.org/src/contrib/Archive/admiral/admiral_0.12.3.tar.gz"
    a2 <- "https://cran.r-project.org/src/contrib/Archive/admiral/admiral_1.0.0.tar.gz"

    # Lot of changes
    info1 <- suppressWarnings(packageDiff::pkgInfo(a1))
    info2 <- suppressWarnings(packageDiff::pkgInfo(a2))

    res <- get_added_functions(info1, info2)

    expect_equal(length(res) > 0, TRUE)


  } else {

    expect_equal(TRUE, TRUE)
  }

})

test_that("utilities11: get_added_parameters() works as expected.", {

  if (dev) {

    a1 <- "https://cran.r-project.org/src/contrib/Archive/admiral/admiral_0.12.3.tar.gz"
    a2 <- "https://cran.r-project.org/src/contrib/Archive/admiral/admiral_1.0.0.tar.gz"

    # Lot of changes
    info1 <- suppressWarnings(packageDiff::pkgInfo(a1))
    info2 <- suppressWarnings(packageDiff::pkgInfo(a2))

    res <- get_added_parameters(info1, info2)

    expect_equal(length(res) > 0, TRUE)


  } else {

    expect_equal(TRUE, TRUE)
  }

})

test_that("utilities12: get_all_functions() works as expected.", {

  if (dev) {

    a1 <- "https://cran.r-project.org/src/contrib/Archive/admiral/admiral_0.12.3.tar.gz"
    a2 <- "https://cran.r-project.org/src/contrib/Archive/admiral/admiral_1.0.0.tar.gz"

    # Lot of changes
    info1 <- suppressWarnings(packageDiff::pkgInfo(a1))
    info2 <- suppressWarnings(packageDiff::pkgInfo(a2))

    res <- get_all_functions(info1, info2)

    expect_equal(length(res) > 0, TRUE)


  } else {

    expect_equal(TRUE, TRUE)
  }

})

test_that("utilities13: get_latest_info() works as expected.", {


  res <- get_latest_info("tibble")

  expect_equal(is.null(res), FALSE)

  expect_equal("pkgInfo" %in% class(res), TRUE)

})


test_that("utilities14: get_archive_info() works as expected.", {


  res <- get_archive_info("fmtr", "1.0.1")

  expect_equal(is.null(res), FALSE)

  expect_equal("pkgInfo" %in% class(res), TRUE)

})

test_that("utilities15: get_all_parameters() works as expected.", {


  inf <- get_archive_info("fmtr", "1.0.1")

  res <- get_all_parameters(inf)

  expect_equal(is.null(res), FALSE)

  expect_equal("pkgInfo" %in% class(inf), TRUE)

  expect_equal(length(res) > 0, TRUE)

})

test_that("utilities16: get_parameter_count() works as expected.", {


  inf <- get_archive_info("fmtr", "1.0.1")

  res <- get_parameter_count(inf)

  expect_equal(is.null(res), FALSE)

  expect_equal(res, 37)

})

test_that("utilities17: get_archive_versions() for negative tests.", {


  # Non-exisitant package name
  res <- get_archive_versions("definrr")

  # No error
  expect_equal(is.null(res), TRUE)

  res <- get_archive_versions("defineR")

  expect_equal(is.null(res), TRUE)

})


test_that("utilities18: get_all_infos() works as expected.", {

  res <- get_all_infos("procs")

  res

  expect_equal(is.list(res), TRUE)
  expect_equal(length(res) > 0, TRUE)
  expect_equal("pkgInfo" %in% class(res[[1]]), TRUE)

})
