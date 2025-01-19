

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
