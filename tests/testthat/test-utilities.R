
dev <- FALSE

test_that("utilities1: get_archive_versions() works as expected.", {

  res <- get_archive_versions("fmtr")

  res

  expect_equal(is.data.frame(res), TRUE)

  expect_equal(nrow(res) > 0, TRUE)

  expect_equal(ncol(res) == 5, TRUE)




  if (dev) {

    res <- get_archive_versions(c("fmtr", "common", "rvest"))

    res

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

  res <- get_latest_version("Matrix")

  expect_equal(is.data.frame(res), FALSE)

  expect_equal(is.list(res), FALSE)

  expect_equal(length(res) == 1, TRUE)

  res <- get_latest_version("grid")

  expect_equal(is.data.frame(res), FALSE)

  expect_equal(is.list(res), FALSE)

  expect_equal(length(res) == 1, TRUE)

  expect_equal(res[[1]] == "archived", TRUE)

})

test_that("utilities3: get_version() works as expected.", {


  vers <- c("logr_1.2.3.tar.gz", "logr_1.2.4.tar.gz", "logr_1.3.0.tar.gz")


  res <- get_version(vers)

  expect_equal(res[1], "1.2.3")
  expect_equal(res[2], "1.2.4")
  expect_equal(res[3], "1.3.0")

})

test_that("utilities4: installed_packages() works as expected.", {


  res <- installed_packages()

  expect_equal(is.data.frame(res), TRUE)

  expect_equal(nrow(res) > 1, TRUE)

  expect_equal(ncol(res) == 2, TRUE)

  if (dev) {
    res <- installed_packages(c("logr", "fmtr", "common", "reporter"))

    expect_equal(is.data.frame(res), TRUE)

    expect_equal(nrow(res) > 1, TRUE)

    expect_equal(ncol(res) == 2, TRUE)

    pth <- "C:\\Users\\dbosa\\AppData\\Local\\R\\win-library\\4.2"

    res <- installed_packages(c("logr", "fmtr", "common", "reporter"),
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

    # Lot of changes
    info1 <- pkg_info("admiral", "0.12.3")
    info2 <- pkg_info("admiral", "1.0.0")

    res <- get_removed_functions(info1, info2)

    expect_equal(length(res) > 0, TRUE)


  } else {

    expect_equal(TRUE, TRUE)
  }

})



test_that("utilities9: get_removed_parameters() works as expected.", {

  if (dev) {

    # Lot of changes
    info1 <- pkg_info("admiral", "0.12.3")
    info2 <- pkg_info("admiral", "1.0.0")

    res <- get_removed_parameters(info1, info2)

    expect_equal(length(res) > 0, TRUE)


  } else {

    expect_equal(TRUE, TRUE)
  }

})

test_that("utilities10: get_added_functions() works as expected.", {

  if (dev) {

    # Lot of changes
    info1 <- pkg_info("admiral", "0.12.3")
    info2 <- pkg_info("admiral", "1.0.0")

    res <- get_added_functions(info1, info2)

    expect_equal(length(res) > 0, TRUE)


  } else {

    expect_equal(TRUE, TRUE)
  }

})

test_that("utilities11: get_added_parameters() works as expected.", {

  if (dev) {

    # Lot of changes
    info1 <- pkg_info("admiral", "0.12.3")
    info2 <- pkg_info("admiral", "1.0.0")

    res <- get_added_parameters(info1, info2)

    expect_equal(length(res) > 0, TRUE)


  } else {

    expect_equal(TRUE, TRUE)
  }

})

test_that("utilities12: get_all_functions() works as expected.", {

  if (dev) {


    # Lot of changes
    info1 <- pkg_info("admiral", "0.12.3")
    info2 <- pkg_info("admiral", "1.0.0")

    res <- get_all_functions(info1, info2)

    expect_equal(length(res) > 0, TRUE)


  } else {

    expect_equal(TRUE, TRUE)
  }

})

test_that("utilities13: pkg_info() works as expected.", {


  res <- pkg_info("tibble", "latest")

  expect_equal(is.null(res), FALSE)

  expect_equal("pinfo" %in% class(res), TRUE)

})


test_that("utilities14: pkg_info() works as expected.", {


  res <- pkg_info("fmtr", "1.0.1")

  expect_equal(is.null(res), FALSE)

  expect_equal("pinfo" %in% class(res), TRUE)

})

test_that("utilities15: get_all_parameters() works as expected.", {


  inf <- pkg_info("fmtr", "1.0.1")

  res <- get_all_parameters(inf)

  res

  expect_equal(is.null(res), FALSE)

  expect_equal("pinfo" %in% class(inf), TRUE)

  expect_equal(length(res) > 0, TRUE)

})

test_that("utilities16: get_parameter_count() works as expected.", {


  inf <- pkg_info("fmtr", "1.0.1")

  res <- get_parameter_count(inf)

  res

  expect_equal(is.null(res), FALSE)

  expect_equal(res, 36)

})

test_that("utilities17: get_archive_versions() for negative tests.", {


  # Non-exisitant package name
  res <- get_archive_versions("definrr")

  # No error
  expect_equal(is.null(res), TRUE)

  res <- get_archive_versions("defineR")

  res

  expect_equal(is.null(res), TRUE)

})


test_that("utilities18: get_all_infos() works as expected.", {

  res <- get_all_infos("procs")

  res

  expect_equal(is.list(res), TRUE)
  expect_equal(length(res) > 0, TRUE)
  expect_equal("pinfo" %in% class(res[[1]]), TRUE)

  res <- get_all_infos("procs", c("1.0.3", "1.0.4"))

  res

  expect_equal(is.list(res), TRUE)
  expect_equal(length(res) == 2, TRUE)
  expect_equal("pinfo" %in% class(res[[1]]), TRUE)

  res <- get_all_infos("forker")

  res

  expect_equal(is.null(res), TRUE)

})

test_that("utilities19: github_packages() works as expected.", {

  res <- github_packages()

  expect_equal(length(res) > 0, TRUE)
  expect_equal("logr" %in% names(res), TRUE)

  res <- github_packages("logr")

  expect_equal(length(res) == 1, TRUE)
  expect_equal("logr" %in% names(res), TRUE)


  res <- github_packages("loxx")
  expect_equal(is.na(res[[1]]) == TRUE, TRUE)

})

test_that("utilities20: get_all_versions() works as expected.", {

  res <- get_all_versions("procs")

  res

  expect_equal(is.data.frame(res), TRUE)
  expect_equal(nrow(res) > 0, TRUE)
  expect_equal(ncol(res), 5)

})


test_that("utilities21: github_package() works as expected.", {

  res <- github_package("tibble")

  expect_equal(is.data.frame(res$stability), TRUE)
  expect_equal(nrow(res$stability) > 0, TRUE)
  expect_equal("pinfo" %in% class(res$infos[[1]]), TRUE)
  expect_equal(length(res$infos) > 2, TRUE)

})


# test_that("utilities22: get_fastest_info() works as expected.", {
#
#   if (dev) {
#     res <- get_fastest_info("procs", "1.0.6")
#
#     expect_equal("pinfo" %in% class(res), TRUE)
#
#     res <- get_fastest_info("procs", "1.0.5")
#
#     expect_equal("pinfo" %in% class(res), TRUE)
#
#     res <- get_fastest_info("ccRemover", "1.0.4")
#
#     expect_equal("pinfo" %in% class(res), TRUE)
#
#   } else {
#
#     expect_equal(TRUE, TRUE)
#   }
#
# })


test_that("utilities23: get_fastest_infos() works as expected.", {

  if (dev) {

    res <- get_fastest_infos("procs", "1.0.5", "1.0.6")

    expect_equal(length(res), 2)
    expect_equal("pinfo" %in% class(res[[1]]), TRUE)

    # Why no functions?
    res <- get_fastest_infos("ccRemover", "1.0.3", "1.0.4")

    expect_equal(length(res), 2)
    expect_equal("pinfo" %in% class(res[[1]]), TRUE)

  } else {

    expect_equal(TRUE, TRUE)
  }

})


test_that("utilities24: available_packages() works as expected.", {

  res <- available_packages()

  expect_equal(is.null(res), FALSE)
  expect_equal(is.data.frame(res), TRUE)
  expect_equal(nrow(res) > 0, TRUE)

})

test_that("utilities25: refresh_package_lists() works as expected.", {

  if (dev) {

    res <- refresh_package_lists()

    # expect_equal(res, TRUE)
    expect_equal(is.null(e$AvailablePackages), FALSE)
    expect_equal(is.null(e$SavedPackages), FALSE)
    expect_equal(nrow(e$AvailablePackages) > 0, TRUE)
    expect_equal(length(e$SavedPackages) > 0, TRUE)

  } else {

    expect_equal(TRUE, TRUE)
  }

})

test_that("utilities26: is_popular() works as expected.", {

  if (dev) {

    res <- is_popular("logr")

    expect_equal(res, TRUE)

    res <- is_popular("procs")

    expect_equal(res, TRUE)

    res <- is_popular("ards")

    expect_equal(res, FALSE)

    res <- is_popular("ards", 20)

    expect_equal(res, TRUE)


  } else {

    res <- is_popular("dplyr")

    expect_equal(res, TRUE)
  }

})


test_that("utilities27: github_update() works as expected.", {

  res <- github_update()

  expect_equal("POSIXct" %in% class(res), TRUE)

})
