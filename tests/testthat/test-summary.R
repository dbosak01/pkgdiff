base_path <- "c:/packages/pkgdiff/tests/testthat"
base_path <- "."

dev <- FALSE


test_that("summary1: gen_chart() basic functionality.", {

  if (dev) {

    ipth <- file.path(tempdir(), "stringr_stability.svg")

    sb <- pkg_stability("stringr")

    res <- gen_chart("stringr", sb$StabilityData, ipth, "")


    expect_equal(file.exists(res), TRUE)
  } else {

    expect_equal(TRUE, TRUE)
  }


})

test_that("summary2: gen_html() basic functionality.", {

  idir <- file.path(tempdir(), "images")
  ipth <- file.path(tempdir(), "images/stringr_stability.svg")
  fpth <- file.path(tempdir(), "stringr_stability.html")

  if (!dir.exists(idir)) {
    dir.create(idir)
  }


  # pkg, fpth, ipth
  res <- gen_html("stringr", fpth, ipth, NULL, NULL)


  expect_equal(file.exists(res), TRUE)


  # file.show(res)


})




test_that("summary3: pkg_summary() basic functionality.", {



  res <- pkg_summary("stringr")


  expect_equal(file.exists(res), TRUE)


  res <- pkg_summary("stringr", view = FALSE)


  expect_equal(file.exists(res), TRUE)




  # file.show(res)


})




test_that("summary4: pkg_summary() more breaking changes.", {



  res <- pkg_summary("admiral")


  expect_equal(file.exists(res), TRUE)

  res <- pkg_summary("gtsummary")


  expect_equal(file.exists(res), TRUE)


  # file.show(res)


})


test_that("summary5: pkg_summary() less breaking changes.", {



  res <- pkg_summary("fmtr")


  expect_equal(file.exists(res), TRUE)

  res <- pkg_summary("logr")


  expect_equal(file.exists(res), TRUE)


  # file.show(res)


})


test_that("summary6: pkg_summary() few releases.", {



  res <- pkg_summary("ards")



  expect_equal(file.exists(res), TRUE)


  # file.show(res)


})

test_that("summary7: pkg_summary() no releases.", {



  res <- pkg_summary("bork")



  expect_equal(file.exists(res), FALSE)


  # file.show(res)


})

test_that("summary8: pkg_summary() base package.", {



  res <- pkg_summary("tools")



  expect_equal(file.exists(res), TRUE)


  # file.show(res)


})


test_that("summary9: pkg_summary() path argument.", {


  pth <- file.path(base_path, "output/str01.html")

  res <- pkg_summary("stringr", path = pth)



  expect_equal(file.exists(res), TRUE)


  # file.show(res)


})


test_that("summary10: pkg_summary() releases and months parameters.", {



  res <- pkg_summary("admiral", releases = 10)


  expect_equal(file.exists(res), TRUE)


  res <- pkg_summary("admiral", months = 18)


  expect_equal(file.exists(res), TRUE)


  res <- pkg_summary("admiral", months = 18, releases = 3)


  expect_equal(file.exists(res), TRUE)


  # file.show(res)


})




