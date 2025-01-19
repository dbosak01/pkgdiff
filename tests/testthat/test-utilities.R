

test_that("utilities1: get_archive_versions() works as expected.", {

  res <- get_archive_versions("logr")

  expect_equal(is.data.frame(res), TRUE)

  expect_equal(nrow(res) > 0, TRUE)

  expect_equal(ncol(res) == 4, TRUE)

})


test_that("utilities2: get_latest_version() works as expected.", {

  res <- get_latest_version("logr")

  expect_equal(is.data.frame(res), TRUE)

  expect_equal(nrow(res) == 1, TRUE)

  expect_equal(ncol(res) == 4, TRUE)

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

