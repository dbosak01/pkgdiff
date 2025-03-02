


#' @title Collect Stability Data for a Package
#' @param pkgs A character vector of one or more package names.
#' @param releases An integer indicating the number of releases to collect
#' stability data for. For example, \code{releases = 10} will return stability
#' data for the last 10 releases of the package. Default is NULL, which means
#' the function will return data for all releases.
#' @param months An integer indicating the number of months back to collect
#' stability data for.  For example, \code{months = 24} will collect
#' stability data for the previous 2 years.  Default is NULL, meaning there is
#' no limitation on the number of release months, and the function will collect
#' data from all releases.
#' @family stability
#' @import common
#' @noRd
get_stability_data <- function(pkgs, releases = NULL, months = NULL) {


  ret <- NULL
  for (pkg in pkgs) {

    obj <- pkg_stability(pkg, releases = releases, months = months)

    dat <- obj$StabilityData

    if (!is.null(dat)) {
      if (is.null(ret))
        ret <- dat
      else
        ret <- rbind(ret, dat)
    }

  }


  return(ret)

}


#' @title Calculate Stability Score for a Package
#' @description
#' The \code{pkg_stablity} function calculates a stability score for
#' a specified package.  The score is calculated as the percentage of
#' releases with no breaking changes.  A breaking change is defined as
#' either a removed function or removed function parameter. The object
#' also returns the raw data from which the stability score is calculated.
#' This data can be useful for review or even custom analytics.
#' @details
#' The \code{pkg_stability} function is used to get an overall feel for the
#' stability of a package.  The stability score and other information returned
#' by the function are based on data retrieved from CRAN.  The function
#' compares each version of a package and determines if any functions or
#' function parameters have been removed from the previous release.
#' If a release removes functions or parameters contained in the previous release,
#' it is flagged as a "breaking release" or "breaking change".
#'
#' The stability score is calculated as the percentage of non-breaking releases.
#' For example, if a package has 10 releases, and one breaking release,
#' the stability score will be .9, or 90%.
#'
#' The stability assessment is a categorization of the stability score. The
#' aim of the assessment is to provide a general evaluation of the package,
#' whether it is stable or unstable.  The
#' assessment has 5 categories:
#' \itemize{
#'   \item {\strong{Perfect}: Stability score of 100.  Package has never
#'   had a breaking release in its entire history.
#'   }
#'   \item {\strong{Very Stable}: Stability score between 95 and 100. Package
#'   has had a breaking release, but very rarely: less than 1 in 20 releases.
#'   }
#'   \item {\strong{Stable}: Stability score between 90 and 95.  The package
#'   has had some breaking releases, but it is still rather rare: less than
#'   1 in 10 releases.
#'   }
#'   \item {\strong{Somewhat unstable}: Stability score between 80 and 90. The
#'   package sometimes has a breaking release, at a rate of about 1 in 5.
#'   }
#'   \item {\strong{Unstable}: Stability score below 80.  The package breaks
#'   more frequently than every 5th release.  This frequency of breaking releases
#'   is considered unstable.
#'   }
#' }
#' As can be seen above, the stability assessment is weighted heavily toward
#' the high end.  This categorization is intentionally designed to encourage
#' package stability, and discourage the number of breaking changes.
#'
#' @section Stability Data: Stability calculations are based on a
#' data frame of stability information gathered from each package release.
#' The data frame has one row per release.  Each row
#' contains comparison information against the prior release.  The
#' columns are organized as follows:
#' \itemize{
#'   \item {\strong{Package}: The package name.
#'   }
#'   \item {\strong{Version}: The version number of the release.
#'   }
#'   \item {\strong{FileName}: The file name of the package.
#'   }
#'   \item {\strong{Release}: The release date of the package.
#'   }
#'   \item {\strong{Size}: The size of the package file on disk.
#'   }
#'   \item {\strong{AF}: The number of functions added from the previous release.
#'   }
#'   \item {\strong{AP}: The number of functions that had parameters added from the
#'   previous release.
#'   }
#'   \item {\strong{RF}: The number of functions removed from the previous release.
#'   }
#'   \item {\strong{RP}: The number of functions that had parameters removed from
#'   the previous release.
#'   }
#'   \item {\strong{BC}: Whether the release had any breaking changes.
#'   Breaking changes are removed functions or removed parameters.  Values
#'   are zero or one.  The value one (1) means the release had breaking changes.
#'   The value zero (0) means the release had no breaking changes.
#'   }
#'   \item {\strong{TF}: The total number of functions in the package.
#'   }
#' }
#' The above data will be included in the function print out.  It can also be
#' accessed on the returned object from the "data" list item, i.e.
#' \code{obj$data}.
#' @section Disclaimers: Note that a "breaking release" does not currently factor
#' in the number of
#' functions deprecated. One deprecated function counts the same as 10 deprecated
#' functions within a release.  Current methodology also does not differentiate
#' between removed
#' functions and removed parameters. They are both considered "breaking", and
#' counted equally.
#'
#' The methodology also does not factor in
#' changed functions.  The reason is simply that it is much more difficult to
#' identify whether or not a changed function is breaking.  That is to say,
#' you may still have breaking changes that are not identified by the
#' \strong{pkgdiff} package.
#' @section Performance:  Performance of the \code{pkg_stability} function
#' can vary greatly depending on the package selected.  The information
#' for some packages has been cached on Github, and can be retrieved very
#' quickly.  For packages that have not been cached, the function must
#' download each version of the package from the CRAN archive, and compare
#' consecutive versions.  This process can take considerable time. The
#' most popular packages on CRAN have been cached.  If there is a package
#' you query frequently that has not been cached, please submit
#' an issue to the \strong{pkgdiff} issue log on Github. In the issue description
#' simply request that the package be added to the cache.
#'
#' For more information on the package cache, see \code{vignette("pkgdiff-cache")}.
#'
#' For more information about how the stability score and assessment are
#' calculated, see \code{vignette("pkgdiff-stability")}.
#' @param pkg The name of the package.
#' @param releases An integer indicating the number of releases to collect
#' stability data for. For example, \code{releases = 10} will return stability
#' data for the last 10 releases of the package. Default is NULL, which means
#' the function will return data for all releases.
#' @param months An integer indicating the number of months back to collect
#' stability data for.  For example, \code{months = 24} will collect
#' stability data for the previous 2 years.  Default is NULL, meaning there is
#' no limitation on the number of release months, and the function will collect
#' data from all releases.
#' @returns An stability score object of class "pdiff_score".  The object
#' contains the stability score, plus other useful information such as
#' the release and version ranges, the number of releases, and number of
#' breaking releases. All of these items can be accessed using dollar sign ($)
#' syntax.
#' @examples
#' # View package stability
#' pkg_stability("curl")
#' # # A stability score: curl package
#' # - Age: 10.33 years
#' # - Score: 98.0
#' # - Assessment: Very Stable
#' # - Version Range: 0.2/6.2.1
#' # - Release Range: 2014-11-20/2025-02-19
#' # - Release Count: 51
#' # - Breaking Releases: 1
#' # - Data:
#' #   Package Version          FileName    Release   Size AF AP RF RP BC TF
#' # 1     curl   6.2.1 curl_6.2.1.tar.gz 2025-02-19 911.7K  0  0  0  0  0 46
#' # 2     curl   6.2.0 curl_6.2.0.tar.gz 2025-01-23 911.3K  0  1  0  0  0 46
#' # 3     curl   6.1.0 curl_6.1.0.tar.gz 2025-01-06   911K  0  1  0  0  0 46
#' # 4     curl   6.0.1 curl_6.0.1.tar.gz 2024-11-14   911K  0  0  0  0  0 46
#' # 5     curl   6.0.0 curl_6.0.0.tar.gz 2024-11-05   911K  1  1  0  1  1 46
#' # 6     curl   5.2.3 curl_5.2.3.tar.gz 2024-09-20   700K  0  0  0  0  0 45
#' # 7     curl   5.2.2 curl_5.2.2.tar.gz 2024-08-26   701K  0  0  0  0  0 45
#' # 8     curl   5.2.1 curl_5.2.1.tar.gz 2024-03-02   700K  0  0  0  0  0 45
#' # 9     curl   5.2.0 curl_5.2.0.tar.gz 2023-12-08   699K  1  0  0  0  0 45
#' # 10    curl   5.1.0 curl_5.1.0.tar.gz 2023-10-02   697K  0  1  0  0  0 44
#' # 11    curl   5.0.2 curl_5.0.2.tar.gz 2023-08-14   697K  0  0  0  0  0 44
#' # 12    curl   5.0.1 curl_5.0.1.tar.gz 2023-06-08   697K  0  1  0  0  0 44
#' # 13    curl   5.0.0 curl_5.0.0.tar.gz 2023-01-12   666K  1  1  0  0  0 44
#' # 14    curl   4.3.3 curl_4.3.3.tar.gz 2022-10-06   655K  0  0  0  0  0 43
#' # 15    curl   4.3.2 curl_4.3.2.tar.gz 2021-06-23   775K  0  0  0  0  0 43
#' # 16    curl   4.3.1 curl_4.3.1.tar.gz 2021-04-30   775K  0  0  0  0  0 43
#' # 17    curl     4.3   curl_4.3.tar.gz 2019-12-02   658K  0  0  0  0  0 43
#' # 18    curl     4.2   curl_4.2.tar.gz 2019-09-24   652K  0  0  0  0  0 43
#' # 19    curl     4.1   curl_4.1.tar.gz 2019-09-16   652K  0  1  0  0  0 43
#' # 20    curl     4.0   curl_4.0.tar.gz 2019-07-22   370K  3  0  0  0  0 43
#' # 21    curl     3.3   curl_3.3.tar.gz 2019-01-10   363K  2  0  0  0  0 40
#' # 22    curl     3.2   curl_3.2.tar.gz 2018-03-28   358K  0  0  0  0  0 38
#' # 23    curl     3.1   curl_3.1.tar.gz 2017-12-12   358K  0  0  0  0  0 38
#' # 24    curl     3.0   curl_3.0.tar.gz 2017-10-06   403K  1  2  0  0  0 38
#' # 25    curl   2.8.1 curl_2.8.1.tar.gz 2017-07-22   400K  0  1  0  0  0 37
#' # 26    curl     2.7   curl_2.7.tar.gz 2017-06-26   397K  1  1  0  0  0 37
#' # 27    curl     2.6   curl_2.6.tar.gz 2017-04-27   396K  0  1  0  0  0 36
#' # 28    curl     2.5   curl_2.5.tar.gz 2017-04-14   396K  1  0  0  0  0 36
#' # 29    curl     2.4   curl_2.4.tar.gz 2017-03-24   394K  2  0  0  0  0 35
#' # 30    curl     2.3   curl_2.3.tar.gz 2016-11-24   391K  0  0  0  0  0 33
#' # 31    curl     2.2   curl_2.2.tar.gz 2016-10-21   389K  0  0  0  0  0 33
#' # 32    curl     2.1   curl_2.1.tar.gz 2016-09-22   388K  0  0  0  0  0 33
#' # 33    curl     2.0   curl_2.0.tar.gz 2016-09-17   389K 10  0  0  0  0 33
#' # 34    curl     1.2   curl_1.2.tar.gz 2016-08-13   287K  0  0  0  0  0 23
#' # 35    curl     1.1   curl_1.1.tar.gz 2016-07-26   277K  0  0  0  0  0 23
#' # 36    curl     1.0   curl_1.0.tar.gz 2016-07-24   277K  0  0  0  0  0 23
#' # 37    curl   0.9.7 curl_0.9.7.tar.gz 2016-04-10   258K  0  0  0  0  0 23
#' # 38    curl   0.9.6 curl_0.9.6.tar.gz 2016-02-17   256K  1  0  0  0  0 23
#' # 39    curl   0.9.5 curl_0.9.5.tar.gz 2016-01-23   255K  1  0  0  0  0 22
#' # 40    curl   0.9.4 curl_0.9.4.tar.gz 2015-11-20   240K  0  1  0  0  0 21
#' # 41    curl   0.9.3 curl_0.9.3.tar.gz 2015-08-25   240K  0  0  0  0  0 21
#' # 42    curl   0.9.2 curl_0.9.2.tar.gz 2015-08-08   239K  1  0  0  0  0 21
#' # 43    curl   0.9.1 curl_0.9.1.tar.gz 2015-07-04   237K  0  0  0  0  0 20
#' # 44    curl     0.9   curl_0.9.tar.gz 2015-06-19   236K  0  0  0  0  0 20
#' # 45    curl     0.8   curl_0.8.tar.gz 2015-06-06   235K  0  0  0  0  0 20
#' # 46    curl     0.7   curl_0.7.tar.gz 2015-05-22   236K  0  0  0  0  0 20
#' # 47    curl     0.6   curl_0.6.tar.gz 2015-05-19   235K 14  2  0  0  0 20
#' # 48    curl     0.5   curl_0.5.tar.gz 2015-02-01    16K  0  0  0  0  0  6
#' # 49    curl     0.4   curl_0.4.tar.gz 2015-01-08    16K  4  0  0  0  0  6
#' # 50    curl     0.3   curl_0.3.tar.gz 2014-12-01   7.5K  1  0  0  0  0  2
#' # 51    curl     0.2   curl_0.2.tar.gz 2014-11-20   5.5K  1  1  0  0  0  1
#'
#' # View package stability
#' pkg_stability("ggplot2")
#' # # A stability score: ggplot2 package
#' # - Age: 17.75 years
#' # - Score: 82.5
#' # - Assessment: Somewhat Unstable
#' # - Version Range: 0.5/3.5.1
#' # - Release Range: 2007-06-01/2024-04-23
#' # - Release Count: 50
#' # - Breaking Releases: 29
#' # - Data:
#' #   Package Version               FileName    Release Size  AF  AP  RF RP BC  TF
#' # 1  ggplot2   3.5.1   ggplot2_3.5.1.tar.gz 2024-04-23 3.4M   0   4   0  0  0 711
#' # 2  ggplot2   3.5.0   ggplot2_3.5.0.tar.gz 2024-02-23 3.4M  37  92  26  6  1 711
#' # 3  ggplot2   3.4.4   ggplot2_3.4.4.tar.gz 2023-10-12 3.0M   0   0   0  0  0 700
#' # 4  ggplot2   3.4.3   ggplot2_3.4.3.tar.gz 2023-08-14 2.1M   0   0   0  0  0 700
#' # 5  ggplot2   3.4.2   ggplot2_3.4.2.tar.gz 2023-04-03 3.0M   3   0   0  1  1 700
#' # 6  ggplot2   3.4.1   ggplot2_3.4.1.tar.gz 2023-02-10 3.0M   2   1   0  0  0 697
#' # 7  ggplot2   3.4.0   ggplot2_3.4.0.tar.gz 2022-11-04 3.0M  28  15   0  0  0 695
#' # 8  ggplot2   3.3.6   ggplot2_3.3.6.tar.gz 2022-05-03 2.9M   0   0   0  1  1 667
#' # 9  ggplot2   3.3.5   ggplot2_3.3.5.tar.gz 2021-06-25 2.9M   0   0   0  0  0 667
#' # 10 ggplot2   3.3.4   ggplot2_3.3.4.tar.gz 2021-06-16 2.9M   8  16   0  1  1 667
#' # 11 ggplot2   3.3.3   ggplot2_3.3.3.tar.gz 2020-12-30 2.9M   0   0   0  0  0 659
#' # 12 ggplot2   3.3.2   ggplot2_3.3.2.tar.gz 2020-06-19 2.9M  17   5   0  1  1 659
#' # 13 ggplot2   3.3.1   ggplot2_3.3.1.tar.gz 2020-05-29 2.9M   0   0   0  0  0 642
#' # 14 ggplot2   3.3.0   ggplot2_3.3.0.tar.gz 2020-03-05 2.9M  68  47   0  1  1 642
#' # 15 ggplot2   3.2.1   ggplot2_3.2.1.tar.gz 2019-08-11 3.1M   0   0   0  0  0 574
#' # 16 ggplot2   3.2.0   ggplot2_3.2.0.tar.gz 2019-06-16 3.0M   3   7   3  0  1 574
#' # 17 ggplot2   3.1.1   ggplot2_3.1.1.tar.gz 2019-04-07 2.7M   0   2   0  0  0 574
#' # 18 ggplot2   3.1.0   ggplot2_3.1.0.tar.gz 2018-10-25 2.7M   5   8   0  0  0 574
#' # 19 ggplot2   3.0.0   ggplot2_3.0.0.tar.gz 2018-07-03 2.7M  89  53   6  4  1 569
#' # 20 ggplot2   2.2.1   ggplot2_2.2.1.tar.gz 2016-12-30 2.1M   0   0   0  0  0 486
#' # 21 ggplot2   2.2.0   ggplot2_2.2.0.tar.gz 2016-11-11 2.1M  22  27  21  2  1 486
#' # 22 ggplot2   2.1.0   ggplot2_2.1.0.tar.gz 2016-03-01 1.5M  11   6   0  2  1 485
#' # 23 ggplot2   2.0.0   ggplot2_2.0.0.tar.gz 2015-12-18 1.5M 159  94 103 33  1 474
#' # 24 ggplot2   1.0.1   ggplot2_1.0.1.tar.gz 2015-03-17 2.2M   0   0   0  0  0 418
#' # 25 ggplot2   1.0.0   ggplot2_1.0.0.tar.gz 2014-05-21 2.2M  12   5   0  0  0 418
#' # 26 ggplot2 0.9.3.1 ggplot2_0.9.3.1.tar.gz 2013-03-02 2.2M   0   1   0  0  0 406
#' # 27 ggplot2   0.9.3   ggplot2_0.9.3.tar.gz 2012-12-05 2.2M   4   4   0  0  0 406
#' # 28 ggplot2 0.9.2.1 ggplot2_0.9.2.1.tar.gz 2012-09-11 2.2M   0   0   0  0  0 402
#' # 29 ggplot2   0.9.2   ggplot2_0.9.2.tar.gz 2012-09-04 2.2M  26  16   8  6  1 402
#' # 30 ggplot2   0.9.1   ggplot2_0.9.1.tar.gz 2012-05-08 9.1M  13  11   0  3  1 384
#' # 31 ggplot2   0.9.0   ggplot2_0.9.0.tar.gz 2012-03-01 1.6M 282   4 149  5  1 371
#' # 32 ggplot2   0.8.9   ggplot2_0.8.9.tar.gz 2010-12-23 2.0M   4   1   2  0  1 238
#' # 33 ggplot2   0.8.8   ggplot2_0.8.8.tar.gz 2010-07-05 2.0M   6   0   2  0  1 236
#' # 34 ggplot2   0.8.7   ggplot2_0.8.7.tar.gz 2010-03-02 2.0M   2   0   0  0  0 232
#' # 35 ggplot2   0.8.6   ggplot2_0.8.6.tar.gz 2010-02-18 2.0M   0   0   0  0  0 230
#' # 36 ggplot2   0.8.5   ggplot2_0.8.5.tar.gz 2009-12-16 2.0M   9   2   4  0  1 230
#' # 37 ggplot2   0.8.4   ggplot2_0.8.4.tar.gz 2009-12-09 2.0M   3   0   2  0  1 225
#' # 38 ggplot2   0.8.3   ggplot2_0.8.3.tar.gz 2009-04-20 2.0M  51   2   6  2  1 224
#' # 39 ggplot2   0.8.2   ggplot2_0.8.2.tar.gz 2009-02-25 2.0M  11   1  10  0  1 179
#' # 40 ggplot2   0.8.1   ggplot2_0.8.1.tar.gz 2008-12-14 1.9M  19   3   3  3  1 178
#' # 41 ggplot2     0.8     ggplot2_0.8.tar.gz 2008-11-21 1.9M  25   6   2  2  1 162
#' # 42 ggplot2     0.7     ggplot2_0.7.tar.gz 2008-10-05 1.9M  45   7  26  3  1 139
#' # 43 ggplot2     0.6     ggplot2_0.6.tar.gz 2008-04-03 1.9M  30   5 125  2  1 120
#' # 44 ggplot2   0.5.7   ggplot2_0.5.7.tar.gz 2008-01-11 1.8M  16   3   3  0  1 215
#' # 45 ggplot2   0.5.6   ggplot2_0.5.6.tar.gz 2007-10-20 1.8M  36   0   5  0  1 202
#' # 46 ggplot2   0.5.5   ggplot2_0.5.5.tar.gz 2007-09-01 1.8M   1   0   0  2  1 171
#' # 47 ggplot2   0.5.4   ggplot2_0.5.4.tar.gz 2007-07-08 1.8M   3   0   1  0  1 170
#' # 48 ggplot2   0.5.2   ggplot2_0.5.2.tar.gz 2007-06-18 1.8M   2   0   0  0  0 168
#' # 49 ggplot2   0.5.1   ggplot2_0.5.1.tar.gz 2007-06-10 1.8M  11   1   5  0  1 166
#' # 50 ggplot2     0.5     ggplot2_0.5.tar.gz 2007-06-01 7.1M 160 160   0  0  0 160
#' @family pdiff
#' @export
pkg_stability <- function(pkg, releases = NULL, months = NULL) {


  d <- structure(list(), class = c("pdiff_score", "list"))

  pk <- github_packages(pkg)
  if (is.na(pk))
    dat <- get_cran_data(pkg, releases, months)
  else
    dat <- get_github_data(pkg, releases, months)
  if (!is.null(dat)) {
    if (nrow(dat) == 1) {

      spn <- elapsed_months( dat$Release[[nrow(dat)]])

      # Populate pdiff_score object
      d$PackageName <- pkg
      d$PackageAge <- sprintf("%0.2f years", spn / 12)
      d$FirstRelease <- dat$Release[[1]]
      d$LastRelease <- dat$Release[[1]]
      d$StabilityScore <- 1
      d$Assessment <- get_stability_assessment(1)
      d$NumReleases <- 1
      d$BreakingReleases <- 0
      d$FirstVersion <- dat$Version[[1]]
      d$LastVersion <- dat$Version[[1]]
      d$AddedFunctions <- 0
      d$AddedParameters <- 0
      d$RemovedFunctions <- 0
      d$RemovedParameters <- 0
      d$StabilityData <- dat

    } else {

      # Calculate stability score
      scr <- stability_score(dat)

      spn <- elapsed_months( dat$Release[[nrow(dat)]])

      # Populate pdiff_score object
      d$PackageName <- pkg
      d$PackageAge <- sprintf("%0.2f years", spn / 12)
      d$FirstRelease <- dat$Release[[nrow(dat)]]
      d$LastRelease <- dat$Release[[1]]
      d$StabilityScore <- scr
      d$Assessment <- get_stability_assessment(scr)
      d$NumReleases <- nrow(dat)
      d$BreakingReleases <- sum(dat$BC, na.rm = TRUE)
      d$FirstVersion <- dat$Version[[nrow(dat)]]
      d$LastVersion <- dat$Version[[1]]
      d$AddedFunctions <- sum(dat$AF, na.rm = TRUE)
      d$AddedParameters <- sum(dat$AP, na.rm = TRUE)
      d$RemovedFunctions <- sum(dat$RF, na.rm = TRUE)
      d$RemovedParameters <- sum(dat$RP, na.rm = TRUE)
      d$StabilityData <- dat

    }
  } else {

    d$PackageName <- pkg
    d$Repository <- "?"
  }

  return(d)
}


#' @title Print a Package Difference Stability Score Object
#' @param x The package difference to print
#' @param ... Follow-on parameters to the print function
#' @param verbose Whether to print in summary or list-style.
#' @family pdiff
#' @import crayon
#' @export
print.pdiff_score <- function(x, ..., verbose = FALSE) {

  if (verbose == TRUE) {

    print(unclass(x))

  } else {

    grey60 <- crayon::make_style(grey60 = "#999999")
    cat(grey60("# A stability score: " %+%
                 as.character(x$PackageName) %+% " package\n"))

    if (!is.null(x$Repository)) {

      cat(paste0("- Repository: ", x$Repository, "\n"))

    }

    if (!is.null(x$PackageAge))
      cat(paste0("- Age: ", x$PackageAge, "\n"))

    if (!is.null(x$StabilityScore)) {

      scr <- sprintf("%.1f", x$StabilityScore * 100)
      cat(paste0("- Score: ", scr, "\n"))

    }

    if (!is.null(x$Assessment)) {

      cat(paste0("- Assessment: ", x$Assessment, "\n"))

    }

    if (!is.null(x$FirstRelease))
      cat(paste0("- Version Range: ", x$FirstVersion, "/", x$LastVersion, "\n"))

    if (!is.null(x$FirstRelease))
      cat(paste0("- Release Range: ", x$FirstRelease, "/", x$LastRelease, "\n"))

    if (!is.null(x$NumReleases))
      cat(paste0("- Release Count: ", as.character(x$NumReleases), "\n"))

    if (!is.null(x$BreakingReleases))
      cat(paste0("- Breaking Releases: ", as.character(x$BreakingReleases), "\n"))


    if (!is.null(x$StabilityData)) {
      cat(paste0("- Data: \n"))

      print(x$StabilityData)
    }



  }

  invisible(x)
}



# Get Data ----------------------------------------------------------------



#' @noRd
get_github_data <- function(pkgname, releases = NULL, months = NULL) {

  fl <- paste0(pkgname, ".RData")

  # https://github.com/dbosak01/pkgdiffdata/blob/main/data/common.Rdata
  # https://github.com/dbosak01/pkgdiffdata/raw/refs/heads/main/data/common.Rdata
  # https://github.com/dbosak01/pkgdiffdata/raw/refs/heads/main/data/common.Rdata
  # https://github.com/dbosak01/pkgdiffdata/raw/refs/heads/main/data/common.Rdata

  pth <- file.path("https://github.com/dbosak01/pkgdiffdata/raw/refs/heads/main/data",
                   fl)

  murl <- url(pth)
  info <- get(load(gzcon(murl)))
  close(murl)

  dat <- info$stability

  if (!is.null(releases)) {

    if (nrow(dat) > releases)
      dat <- dat[seq(1, releases + 1), ]
  }

  if (!is.null(months)) {
    dm <- Sys.Date() - ((months + 1) * 30)

    # Handle situation where last package release date
    # is before the requested release range.
    if (max(dat$Release) < dm) {
      dat <- dat[1, ]
    } else {
      dat <- subset(dat, dat$Release >= dm)
    }
  }

  rownames(dat) <- NULL

  return(dat)
}


#' @noRd
get_cran_data <- function(pkgname, releases = NULL, months = NULL) {

  #browser()

  # Get latest package data
  ldat <- get_latest_data(pkgname)

  # Get archive data
  adat <- get_archive_versions(pkgname)

  if (!is.null(ldat) || !is.null(adat)) {

    # Initialize data
    if (!is.null(adat) && !is.null(ldat))
      dat <- rbind(ldat, adat)
    else if (!is.null(ldat))
      dat <- ldat
    else if (!is.null(adat))
      dat <- adat

    # Initialize count columns
    dat$AF <- NA
    dat$AP <- NA
    dat$RF <- NA
    dat$RP <- NA
    dat$BC <- NA
    dat$TF <- NA

    # Clear out rownames
    rownames(dat) <- NULL

    # Deal with releases parameter
    if (!is.null(releases)) {

      if (nrow(dat) > releases)
        dat <- dat[seq(1, releases + 1), ]
    }

    if (!is.null(months)) {
      dm <- Sys.Date() - ((months + 1) * 30)

      # Handle situation where last package release date
      # is before the requested release range.
      if (max(dat$Release) < dm) {
        dat <- dat[1, ]
      } else {
        dat <- subset(dat, dat$Release >= dm)
      }
    }

    if (nrow(dat) <= 1) {

      dat <- ldat

      inf <- pkg_info(pkgname, "latest")

      dat$AF <- length(inf$Functions)
      dat$AP <- length(inf$Functions)
      dat$RF <- 0
      dat$RP <- 0
      dat$BC <- 0
      dat$TF <- length(inf$Functions)

    } else {

      # browser()

      # Set up loop counter
      idxs <- seq(1, nrow(dat) - 1)

      # Lag earlier info for performance
      laginfo <- NULL

      for (idx in idxs) {

        # Get version strings
        v2 <- dat[idx, "Version"]
        v1 <- dat[idx + 1, "Version"]

        cat(paste0("Comparing ", pkgname, " ", v1, "/", v2, "\n"))

        if (is.null(laginfo)) {
          diff <- tryCatch({
            pkg_diff(pkgname, v1, v2)
          }, error = function(e) {
            NULL
          })
        } else {
          diff <- tryCatch({
            pkg_diff(pkgname, v1, laginfo)
          }, error = function(e) {
            NULL
          })

        }

        if (!is.null(diff)) {

          if (is.null(diff$AddedFunctions))
            dat[idx, "AF"] <- 0
          else
            dat[idx, "AF"] <- length(diff$AddedFunctions)

          if (is.null(diff$AddedParameters))
            dat[idx, "AP"] <- 0
          else
            dat[idx, "AP"] <- length(diff$AddedParameters)

          if (is.null(diff$RemovedFunctions))
            dat[idx, "RF"] <- 0
          else
            dat[idx, "RF"] <- length(diff$RemovedFunctions)

          if (is.null(diff$RemovedParameters))
            dat[idx, "RP"] <- 0
          else
            dat[idx, "RP"] <- length(diff$RemovedParameters)

          if (dat[idx, "RF"] > 0 | dat[idx, "RP"] > 0)
            dat[idx, "BC"] <- 1
          else
            dat[idx, "BC"] <- 0

          if (is.null(diff$AllFunctions))
            dat[idx, "TF"] <- 0
          else
            dat[idx, "TF"] <- length(diff$AllFunctions)

          laginfo <- diff$Version1DiffInfo

        }


      }

    }


    # Take out first release, as it will not be compared.
    if (!is.null(releases)) {

      if (nrow(dat) > releases)
        dat <- dat[seq(1, releases), ]
    }

    # Special handling for first release
    if (nrow(dat) > 0) {

      idx <- nrow(dat)
      if (is.na(dat[[idx, "TF"]])) {
        if (!is.na(dat[[idx, "Version"]])) {

          v1 <- dat[[idx, "Version"]]

          inf <- get_info_cran(pkgname, v1)
          dat[[idx, "AF"]] <- length(inf$Functions)
          dat[[idx, "AP"]] <- length(get_all_parameters(inf))
          dat[[idx, "RF"]] <- 0
          dat[[idx, "RP"]] <- 0
          dat[[idx, "BC"]] <- 0
          dat[[idx, "TF"]] <- length(inf$Functions)

        }
      }
    }

    # Add labels
    common::labels(dat) <- list(AF = "Added Functions",
                                AP = "Added Parameters",
                                RF = "Removed Functions",
                                RP = "Removed Parameters",
                                BC = "Breaking Changes",
                                TC = "Total Functions")
  } else {

    # Package not found
    dat <- NULL
  }

  return(dat)

}



# Assume versions are in proper order.
#' @import common
#' @noRd
get_info_data <- function(pkgname, pkginfos, skip_first = FALSE) {

  # browser()

  # Get table data for all versions
  dat <- get_all_versions(pkgname)

  # Extract desired versions
  versions <- names(pkginfos)

  # Subset table for desired versions
  dat <- subset(dat, dat$Version %in% versions)

  if (length(versions) == 1) {

    inf <- pkginfos[[versions]]

    dat$AF <- length(inf$Functions)
    dat$AP <- length(inf$Functions)
    dat$RF <- 0
    dat$RP <- 0
    dat$BC <- 0
    dat$TF <- length(inf$Functions)

  } else {

    # browser()

    idxs <- seq(1, length(versions) - 1)


    for (idx in idxs) {

      v2info <- pkginfos[[idx]]
      v1info <- pkginfos[[idx + 1]]

      v2 <- v2info$Version
      v1 <- v1info$Version

      cat(paste0("Comparing ", pkgname, " ", v1, "/", v2, "\n"))

      diff <- tryCatch({
        pkg_diff(pkgname, v1info, v2info)
      }, error = function(e) {
        NULL
      })


      if (!is.null(diff)) {

        if (is.null(diff$AddedFunctions))
          dat[idx, "AF"] <- 0
        else
          dat[idx, "AF"] <- length(diff$AddedFunctions)

        if (is.null(diff$AddedParameters))
          dat[idx, "AP"] <- 0
        else
          dat[idx, "AP"] <- length(diff$AddedParameters)

        if (is.null(diff$RemovedFunctions))
          dat[idx, "RF"] <- 0
        else
          dat[idx, "RF"] <- length(diff$RemovedFunctions)

        if (is.null(diff$RemovedParameters))
          dat[idx, "RP"] <- 0
        else
          dat[idx, "RP"] <- length(diff$RemovedParameters)

        if (dat[idx, "RF"] > 0 | dat[idx, "RP"] > 0)
          dat[idx, "BC"] <- 1
        else
          dat[idx, "BC"] <- 0

        if (is.null(diff$AllFunctions))
          dat[idx, "TF"] <- 0
        else
          dat[idx, "TF"] <- length(diff$AllFunctions)


      }

    }

  }

  # Deal with first release
  if (nrow(dat) > 0 && !skip_first) {

    idx <- nrow(dat)
    if (is.na(dat[[idx, "TF"]])) {
      if (!is.na(dat[[idx, "Version"]])) {

        v1 <- dat[[idx, "Version"]]

        inf <- pkginfos[[v1]]
        dat[[idx, "AF"]] <- length(inf$Functions)
        dat[[idx, "AP"]] <- length(get_all_parameters(inf))
        dat[[idx, "RF"]] <- 0
        dat[[idx, "RP"]] <- 0
        dat[[idx, "BC"]] <- 0
        dat[[idx, "TF"]] <- length(inf$Functions)

      }
    }
  }

  # Add labels
  common::labels(dat) <- list(AF = "Added Functions",
                              AP = "Added Parameters",
                              RF = "Removed Functions",
                              RP = "Removed Parameters",
                              BC = "Breaking Changes",
                              TC = "Total Functions")

  rownames(dat) <- NULL

  return(dat)

}


# Utilities ---------------------------------------------------------------

#' @noRd
get_stability_assessment <- function(score) {

  ret <- "Unknown"
  if (!is.nan(score)) {
    if (score == 1) {
      ret <- "Perfect"
    } else if (score >= .95) {
      ret <- "Very Stable"
    } else if (score >= .9) {

      ret <- "Stable"
    } else if (score >= .8) {
      ret <- "Somewhat Unstable"
    } else {

      ret <- "Unstable"
    }
  }

  return(ret)
}

#' @noRd
elapsed_months <- Vectorize(function(start_date) {
  ed <- as.POSIXlt(Sys.Date())
  sd <- as.POSIXlt(start_date)
  ret <- 12 * (ed$year - sd$year) + (ed$mon - sd$mon)
  return(ret)
})

#' @noRd
stability_score <- function(dat) {

  # Calculate elapsed months for each release
  el <- elapsed_months(dat$Release)

  # After 10 years, don't count it
  tm <- ifelse(el > 120, 0, 1)

  # Get breaking releases
  br <- dat$BC

  # Weight each breaking release
  clc <- ifelse(br == 1, el * .01, 0)

  # Don't let weights go over 1
  mx <- ifelse(clc > 1, 1, clc)

  # Get numerator
  nd <- sum(br - mx, na.rm = TRUE)

  if (sum(tm) == 0)
    # Denominator is zero
    # Return 100%
    ret <- 1
  else {

    # Calculate percentage
    ret <- 1 - (nd / sum(tm))
  }

  return(ret)
}



