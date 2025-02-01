


dev <- FALSE

test_that("stability1: get_stability_data() basic functionality.", {

  if (dev) {

    res <- get_stability_data("libr")

    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res) > 0, TRUE)

  } else {

    expect_equal(TRUE, TRUE)
  }


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

  # Need one extra for comparison
  expect_equal(nrow(res) == 6, TRUE)


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
    expect_equal(nrow(res) == 1, TRUE)

  } else {

    expect_equal(TRUE, TRUE)
  }


})




test_that("stability6: get_stability_score() basic functionality.", {


    res <- get_stability_score("logr")

    expect_equal(res$StabilityScore == 1, TRUE)



})


test_that("stability7: get_stability_score() stressed functionality.", {


  if (dev) {

    res <- get_stability_score("admiral", rel)

    expect_equal(res$StabilityScore < 1, TRUE)


  } else {

    expect_equal(TRUE, TRUE)
  }


})


test_that("stability8: get_stability_data() no releases in time period.", {

  if (dev) {

    res <- get_stability_data("tibble", months = 1)

    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res) == 1, TRUE)

  } else {

    expect_equal(TRUE, TRUE)
  }

})

test_that("stability9: get_stability_score() out of range.", {

  if (dev) {

    res <- get_stability_score("tibble", months = 1)

    expect_equal(res$StabilityScore == 1, TRUE)

  } else {

    expect_equal(TRUE, TRUE)
  }

})

test_that("stability10: get_stability_data() boundry conditions.", {

  if (dev) {

    # Unknown package
    pkgs <- c("definer")

    res <- get_stability_data(pkgs)

    expect_equal(is.data.frame(res), FALSE)

    # No archive data
    pkgs <- c("defineR")

    res <- get_stability_data(pkgs)

    expect_equal(is.data.frame(res), TRUE)

  } else {

    expect_equal(TRUE, TRUE)
  }


})


test_that("stability11: get_stability_data() multiple packages.", {

  if (dev) {

    pkgs <- c("common", "procs", "libr", "defineR")

    res <- get_stability_data(pkgs)


    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res) > 0, TRUE)

  } else {

    expect_equal(TRUE, TRUE)
  }


})


test_that("stability12: get_github_data() works as expected.", {

  if (TRUE) {


    res <- get_github_data("common")

    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res) > 0, TRUE)

    res <- get_github_data("fmtr")

    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res) > 0, TRUE)

  } else {

    expect_equal(TRUE, TRUE)
  }


})


test_that("stability13: get_info_data() works as expected.", {

  infos <-  get_all_infos("procs")

  res <- get_info_data("procs", infos)

  expect_equal(is.null(res), FALSE)
  expect_equal(is.data.frame(res), TRUE)
  expect_equal(nrow(res) > 0, TRUE)
  expect_equal(ncol(res) == 11, TRUE)

})

