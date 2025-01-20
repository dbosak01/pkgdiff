
dev <- FALSE

test_that("diff1: get_diff() basic functionality.", {

  res <- get_diff("logr", "1.3.7", "1.3.8")


  expect_equal("pdiff" %in% class(res), TRUE)
  expect_equal(res$PackageName, "logr")
  expect_equal(res$Version1, "1.3.7")
  expect_equal(res$Version2, "1.3.8")
  expect_equal(nchar(res$Version1Path) > 0, TRUE)
  expect_equal(nchar(res$Version2Path) > 0, TRUE)


})


test_that("diff2: get_diff() many changes.", {

  res <- get_diff("logr", "1.3.4", "1.3.7")


  expect_equal("pdiff" %in% class(res), TRUE)
  expect_equal(res$PackageName, "logr")
  expect_equal(res$Version1, "1.3.4")
  expect_equal(res$Version2, "1.3.7")
  expect_equal(nchar(res$Version1Path) > 0, TRUE)
  expect_equal(nchar(res$Version2Path) > 0, TRUE)


})


test_that("diff3: get_diff() breaking changes.", {

  if (dev) {
    # Lot of changes
    res <- get_diff("admiral", "1.1.0", "1.2.0")

    expect_equal(res$PackageAge > 0, FALSE)
    expect_equal(is.null(res$FirstRelease), FALSE)
    expect_equal(is.null(res$LastRelease), FALSE)

    expect_equal(res$NumReleases > 0, TRUE)

    expect_equal(res$BreakingChanges, TRUE)
    expect_equal(res$NumBreakingChanges > 0, TRUE)
    expect_equal(length(res$DeprecatedFunctions) > 0, TRUE)
    expect_equal(length(res$DeprecatedParameters) > 0, TRUE)

  } else {

    expect_equal(TRUE, TRUE)
  }

})



test_that("diff4: view_details() works.", {

  if (dev) {

    # Not many changes
    res <- get_diff("logr", "1.3.7", "1.3.8")


    view_details(res)

    # Lot of changes
    res <- get_diff("admiral", "1.1.0", "1.2.0")


    view_details(res)


  } else {

    expect_equal(TRUE, TRUE)
  }
})



test_that("diff1: get_diff() basic functionality.", {

  if (dev) {

    res <- get_diff("logr", "1.3.6", "1.3.7")

    print(res)

    res <- get_diff("admiral", "1.1.0", "1.2.0")

    print(res)

    # If no errors, it is good
    expect_equal(TRUE, TRUE)

  } else {
    expect_equal(TRUE, TRUE)
  }

})




