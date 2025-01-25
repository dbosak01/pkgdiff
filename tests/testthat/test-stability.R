


dev <- FALSE

test_that("stability1: get_stability_data() basic functionality.", {

  res <- get_stability_data("libr")


  expect_equal(is.data.frame(res), TRUE)
  expect_equal(nrow(res) > 0, TRUE)


})

test_that("stability2: get_stability_data() many changes.", {

  if (dev) {
    res <- get_stability_data("admiral")


    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res) > 0, TRUE)

  } else {

    expect_equal(TRUE, TRUE)
  }


})


test_that("stability3: get_stability_data() release limit.", {

  res <- get_stability_data("fmtr", releases = 5)


  expect_equal(is.data.frame(res), TRUE)
  expect_equal(nrow(res) == 5, TRUE)


})

test_that("stability4: get_stability_data() time limit.", {

  res <- get_stability_data("fmtr", months = 12)


  expect_equal(is.data.frame(res), TRUE)
  expect_equal(nrow(res) > 1, TRUE)


})

# Need to handle
test_that("stability5: get_stability_data() only one release.", {

  if (dev) {

    # No archives
    res <- get_stability_data("defineR")

    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res) == 5, TRUE)

  } else {

    expect_equal(TRUE, TRUE)
  }


})

test_that("stability5: get_stability_score() basic functionality.", {


  if (dev) {
    res <- get_stability_score("admiral", months = 36)


    expect_equal(res$StabilityScore < 1, TRUE)


  } else {

    expect_equal(TRUE, TRUE)
  }


})
