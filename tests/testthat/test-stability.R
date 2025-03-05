


dev <- FALSE

test_that("stability1: get_stability_data() basic functionality.", {


  res <- get_stability_data("procs")

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

  # Need one extra for comparison
  expect_equal(nrow(res) == 5, TRUE)


})

test_that("stability4: get_stability_data() time limit.", {

  res <- get_stability_data("fmtr", months = 12)


  expect_equal(is.data.frame(res), TRUE)
  expect_equal(nrow(res) > 1, TRUE)


})

# Need to handle
test_that("stability5: get_stability_data() only one release.", {


  # No archives
  res <- get_stability_data("defineR")

  expect_equal(is.data.frame(res), TRUE)
  expect_equal(nrow(res) == 1, TRUE)

})




test_that("stability6: pkg_stability() basic functionality.", {


  res <- pkg_stability("procs")

  res

  expect_equal(res$StabilityScore == 1, TRUE)



})


test_that("stability7: pkg_stability() stressed functionality.", {



  res <- pkg_stability("admiral", releases = 5)

  res

  expect_equal(res$StabilityScore < 1, TRUE)


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

test_that("stability9: pkg_stability() out of range.", {


  res <- pkg_stability("tibble", months = 1)

  expect_equal(res$StabilityScore == 1, TRUE)



})

test_that("stability10: get_stability_data() boundry conditions.", {


  # Unknown package
  pkgs <- c("definer")


  res <- get_stability_data(pkgs)
  expect_equal(is.null(res), TRUE)


  # No archive data
  pkgs <- c("defineR")

  res <- get_stability_data(pkgs)

  expect_equal(is.data.frame(res), TRUE)


})


test_that("stability11: get_stability_data() multiple packages.", {


    pkgs <- c("common", "procs", "libr", "defineR")

    res <- get_stability_data(pkgs)


    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res) > 0, TRUE)



})


test_that("stability12: get_github_data() works as expected.", {



  res <- get_github_data("procs")

  expect_equal(is.data.frame(res), TRUE)
  expect_equal(nrow(res) > 0, TRUE)

  res <- get_github_data("fmtr")

  expect_equal(is.data.frame(res), TRUE)
  expect_equal(nrow(res) > 0, TRUE)




})


test_that("stability13: get_info_data() works as expected.", {

  infos <- get_all_infos("procs")

  res <- get_info_data("procs", infos)

  expect_equal(is.null(res), FALSE)
  expect_equal(is.data.frame(res), TRUE)
  expect_equal(nrow(res) > 0, TRUE)
  expect_equal(ncol(res) == 11, TRUE)

})



test_that("stability14: pkg_stability() one release.", {

  if (dev) {

    res <- pkg_stability("defineR")

    res

    expect_equal(res$StabilityScore == 1, TRUE)

  } else {

    expect_equal(TRUE, TRUE)
  }


})


test_that("stability15: pkg_stability() package edge cases.", {

  # Unknown package
  # Should not give an error
  res <- pkg_stability("forker")

  res
  expect_equal(is.null(res), FALSE)


  # Score coming out NaN
  res <- pkg_stability("KMsurv")

  res
  expect_equal(res$StabilityScore == 1, TRUE)


  # Only one release
  res <- pkg_stability("ards")

  expect_equal(res$StabilityScore == 1, TRUE)

  # No score because no archive versions.
  # Should still be stability 1.
  # Not sure why it isn't coming out.
  res <- pkg_stability("concatenate")

  expect_equal(res$StabilityScore == 1, TRUE)

})



