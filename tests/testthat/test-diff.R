
dev <- FALSE

test_that("diff1: pkg_diff() basic functionality.", {

  res <- pkg_diff("logr", "1.3.7", "1.3.8")


  expect_equal("pdiff" %in% class(res), TRUE)
  expect_equal(res$PackageName, "logr")
  expect_equal(res$Version1, "1.3.7")
  expect_equal(res$Version2, "1.3.8")

})


test_that("diff2: pkg_diff() many changes.", {

  res <- pkg_diff("logr", "1.3.4", "1.3.7")


  expect_equal("pdiff" %in% class(res), TRUE)
  expect_equal(res$PackageName, "logr")
  expect_equal(res$Version1, "1.3.4")
  expect_equal(res$Version2, "1.3.7")
  # expect_equal(nchar(res$Version1Path) > 0, TRUE)
  # expect_equal(nchar(res$Version2Path) > 0, TRUE)


})


test_that("diff3: pkg_diff() breaking changes.", {


  # Lot of changes
  res <- pkg_diff("admiral", "1.1.0", "1.2.0")

  expect_equal(res$BreakingChanges, TRUE)
  expect_equal(length(res$RemovedFunctions) > 0, TRUE)
  expect_equal(length(res$RemovedParameters) > 0, TRUE)

})



# test_that("diff4: view_details() works.", {
#
#   if (dev) {
#
#     # Not many changes
#     res <- pkg_diff("logr", "1.3.7", "1.3.8")
#
#
#     view_details(res)
#
#     # Lot of changes
#     res <- pkg_diff("admiral", "1.1.0", "1.2.0")
#
#
#     view_details(res)
#
#     # No error is good
#     expect_equal(TRUE, TRUE)
#
#   } else {
#
#     expect_equal(TRUE, TRUE)
#   }
# })



test_that("diff5: pkg_diff() check printing.", {


  res <- pkg_diff("logr", "1.3.6", "1.3.7")

  print(res)

  res <- pkg_diff("admiral", "0.12.3", "1.0.0")

  print(res)

  # If no errors, it is good
  expect_equal(TRUE, TRUE)


})

test_that("diff7: pkg_diff_by_version() old version.", {

  res <- pkg_diff("logr", "1.2.9", "1.3.0")

  expect_equal("pdiff" %in% class(res), TRUE)
  expect_equal(res$PackageName, "logr")
  expect_equal(res$Version1, "1.2.9")
  expect_equal(res$Version2, "1.3.0")


})



test_that("diff8: pkg_diff() parameter checks.", {

  expect_error(pkg_diff("logxx", "1.3.7", "1.3.8"))

  expect_error(pkg_diff("logr", "0.0.0", "1.3.8"))

  expect_error(pkg_diff("logr", "1.3.7", "0.0.0"))

  # No error OK
  # expect_error(pkg_diff("logr", "1.3.7", "1.3.6"))
  #
  # pkg_diff("logr", "1.3.6", "1.3.7")

})


test_that("diff9: pkg_diff() with pkgInfo works.", {

 inf <- pkg_info("logr", "1.0.4")

 df <- pkg_diff("logr", "1.0.3", inf)

 expect_equal(is.null(df), FALSE)
 expect_equal("pdiff" %in% class(df), TRUE)
 expect_equal(df$Version1, "1.0.3")
 expect_equal(df$Version2, "1.0.4")



 inf <- pkg_info("logr", "1.0.3")

 df <- pkg_diff("logr", inf, "1.0.4")

 df
 expect_equal(is.null(df), FALSE)
 expect_equal("pdiff" %in% class(df), TRUE)
 expect_equal(df$Version1, "1.0.3")
 expect_equal(df$Version2, "1.0.4")


})


test_that("diff10: print.pdiff() works as expected.", {

  res <- pkg_diff("fmtr", "1.5.7", "1.5.8")

  print(res)

  print(res, verbose = FALSE)

  expect_equal(TRUE, TRUE)

  res <- pkg_diff("fmtr", "1.5.5", "1.5.7")

  print(res)

  print(res, verbose = FALSE)

  expect_equal(TRUE, TRUE)
})






