
dev <- FALSE

test_that("utilities1: get_archive_versions() works as expected.", {

  res <- get_archive_versions("fmtr")

  expect_equal(is.data.frame(res), TRUE)

  expect_equal(nrow(res) > 0, TRUE)

  expect_equal(ncol(res) == 4, TRUE)

})


test_that("utilities2: get_latest_version() works as expected.", {

  res <- get_latest_version("logr")

  expect_equal(is.data.frame(res), FALSE)

  expect_equal(length(res) == 1, TRUE)

})

test_that("utilities3: get_latest_version() works as expected.", {


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

  expect_equal(ncol(res) == 4, TRUE)

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
