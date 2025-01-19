

test_that("breakage1: get_breaking_changes() works as expected.", {

  res <- get_breaking_changes("logr", "1.3.7", "1.3.8")

  expect_equal(is.data.frame(res), TRUE)

  expect_equal(nrow(res) > 0, TRUE)

  expect_equal(ncol(res) == 4, TRUE)

})
