

#' @title Identify Breakages for a Repository
#' @description
#' The \code{repo_breakages} function generates a data frame
#' of breakage information for multiple packages.  This function can be used
#' to identify breakages for a small set of packages, or even an entire repository.
#' @details
#' The \code{repo_breakages} function aims to help with repository upgrades.
#' The purpose is to identify packages that will break if you upgrade
#' your repository.
#'
#' The function accepts two data frames as input.
#' Each data frame should have two columns: "Package" and "Version".
#' The "r1" data frame is for your current repo.  The "r2" data frame
#' is for the upgrade repo.
#'
#' When executed, \code{repo_breakages} will then match up the
#' source and target versions, run a difference operation for each package,
#' and look for breaking changes.
#'
#' The result will be a table of package names, the source and target version,
#' and whether or not there are any breakages. If there are
#' breakages, the difference object for that package will be
#' added to a "details" list.  This difference object can show you
#' exactly which functions or function parameters were removed.
#'
#' The \code{repo_breakages} function is one of primary motivations for
#' the \strong{pkgdiff} package.  This mass-comparison ability did not
#' exist in any other R package prior to \strong{pkgdiff}.
#' @param r1 A data frame that identifies the source repository packages
#' and versions. The default value is "current", which means
#' the function will use the current versions of all packages in
#' the current R repository.
#' @param r2 A data frame that identifies the target repository packages
#' and versions. The default value is "latest", which means
#' the function will use the latest versions of all packages on CRAN.
#' @returns A list containing a summary table and a list of difference
#' objects. The summary table will contain one row for each package. The
#' columns show the package name, source and target versions, and whether
#' there are any breaking changes between the specified versions.  If
#' breaking changes are found, the difference object for that package
#' will be included in the details list.
#' @seealso Use the \code{\link{pkg_repo}} function to help gather
#' version information for each package set.  Also see \code{\link{repo_stability}}
#' to collect stability information on multiple packages.
#' @family prepo
#' @examples
#' # Create package vector
#' pkgs <- c("curl", "dplyr", "crayon", "stringr")
#'
#' # Get backdated versions
#' r1 <- data.frame(Package = pkgs,
#'                  Version = c("5.2.1", "1.1.4", "1.5.2", "1.5.0"))
#'
#' # Get latest versions from CRAN
#' r2 <- pkg_repo(pkgs, "latest")
#'
#' # Find any breaking changes
#' res <- repo_breakages(r1, r2)
#' # Comparing curl v5.2.1/v6.2.1
#' # Comparing crayon v1.5.2/v1.5.3
#' # Comparing stringr v1.5.0/v1.5.1
#'
#' # View results
#' res
#' # # A repo breakages object
#' # - Run Datetime: 2025-03-01 17:47 UTC
#' # - Summary:
#' #   Package Version1 Version2 Breakages
#' # 1    curl    5.2.1    6.2.1      TRUE
#' # 2   dplyr    1.1.4    1.1.4     FALSE
#' # 3  crayon    1.5.2    1.5.3     FALSE
#' # 4 stringr    1.5.0    1.5.1     FALSE
#' # - Details:
#' #   # A difference object: curl package
#' #   - Comparing: v5.2.1/v6.2.1
#' # - Breaking Changes: TRUE
#' # - Added Functions: 1
#' # - Added Parameters: 3
#' # - Removed Parameters:
#' #   - multi_download(): timeout
#' @import common
#' @export
repo_breakages <- function(r1 = "current", r2 = "latest") {

  if (!is.data.frame(r1)) {
    if (r1 == "current") {
      r1 <- installed_packages()
    }
  }

  if (!is.data.frame(r1)) {
    stop("r1 package list must be a data frame.")
  }

  if (!"Package"  %in% names(r1) ||
      !"Version" %in% names(r1)) {
    stop("V1 package list must contain the columns 'Package' and 'Version'.")
  }

  if (!is.data.frame(r2)) {
    if (r2 == "latest") {

      lver <- get_latest_version(r1$Package)

      r2 <- data.frame(Package = r1$Package,
                            Version = lver)

    }
  }

  if (!"Package"  %in% names(r2) ||
      !"Version" %in% names(r2)) {
    stop("V2 package list must contain the columns 'Package' and 'Version'.")
  }

  # Necessary?
  # if (nrow(r1) != nrow(r2)) {
  #   stop("v1 and v2 package lists must be the same size.")
  # }

  dat <- data.frame("Package" = NA, Version1 = NA, Version2 = NA,
                    Breakages = FALSE)
  det <- list()

  # browser()

  for (idx in seq_len(nrow(r1))) {

    pkg <- r1$Package[[idx]]
    v1 <- r1$Version[[idx]]
    v2 <- r2[r2$Package == pkg, "Version"]

    bc <- FALSE
    if (is.na(v1)) {
      message(paste0("Source version for package '", pkg, "' is missing."))
      bc <- NA
    } else if (length(v2) == 0) {
      message(paste0("Target version for package '", pkg, "' is missing."))
      bc <- NA
    } else if (v1 != v2) {
      cat(paste0("Comparing ", pkg, " v", v1, "/v", v2, "\n"))
      diff <- tryCatch({suppressWarnings(pkg_diff(pkg, v1, v2))},
                       error = function(er){NULL})
      if (!is.null(diff))
        bc <- diff$BreakingChanges
      else
        bc <- NA
    }

    dat[[idx, "Package"]] <- pkg
    dat[[idx, "Version1"]] <- v1
    dat[[idx, "Version2"]] <- ifelse(length(v2) == 0, NA, v2)
    dat[[idx, "Breakages"]] <- bc

    if (!is.na(bc)) {
      if (bc) {
        det[[pkg]] <- diff
      }
    }
  }

  ret <- list(Datetime = Sys.time(), Summary = dat, Details = det)

  class(ret) <- c("rbreak", class(ret))

  return(ret)

}

#' @title Print a Repo Breakages Object
#' @description
#' Print routine for a repo breakages object of class "rbreak".
#' @param x The repo breakages object to print.
#' @param ... Follow-on parameters to the print function.
#' @param verbose If FALSE, prints only the difference removals.
#' If TRUE, prints both additions and removals. Default is FALSE.
#' @family prepo
#' @import crayon
#' @examples
#' # Create package vector
#' pkgs <- c("curl", "dplyr", "crayon", "stringr")
#'
#' # Get backdated versions
#' r1 <- data.frame(Package = pkgs,
#'                  Version = c("5.2.1", "1.1.4", "1.5.2", "1.5.0"))
#'
#' # Get latest versions from CRAN
#' r2 <- pkg_repo(pkgs, "latest")
#'
#' # Find any breaking changes
#' res <- repo_breakages(r1, r2)
#' # Comparing curl v5.2.1/v6.2.1
#' # Comparing crayon v1.5.2/v1.5.3
#' # Comparing stringr v1.5.0/v1.5.1
#'
#' # View results
#' print(res)
#' # # A repo breakages object
#' # - Run Datetime: 2025-03-01 19:22 UTC
#' # - Summary:
#' #   Package Version1 Version2 Breakages
#' # 1    curl    5.2.1    6.2.1      TRUE
#' # 2   dplyr    1.1.4    1.1.4     FALSE
#' # 3  crayon    1.5.2    1.5.3     FALSE
#' # 4 stringr    1.5.0    1.5.1     FALSE
#' # - Details:
#' #   # A difference object: curl package
#' #   - Comparing: v5.2.1/v6.2.1
#' # - Breaking Changes: TRUE
#' # - Added Functions: 1
#' # - Added Parameters: 3
#' # - Removed Parameters:
#' #   - multi_download(): timeout
#' @export
print.rbreak <- function(x, ..., verbose = FALSE) {

  grey60 <- crayon::make_style(grey60 = "#999999")
  cat(grey60(paste0("# A repo breakages object\n")))


  if (!is.null(x$Datetime)) {
    tmstmp <- format(x$Datetime, format = "%Y-%m-%d %H:%M UTC")
    cat(paste0("- Run Datetime: ", tmstmp , "\n"))

  }


  if (!is.null(x$Summary)) {
    cat(paste0("- Summary:\n"))
    print(as.data.frame(x$Summary))
  }


  if (!is.null(x$Details)) {
    cat(paste0("- Details:\n"))
    for (d in x$Details) {
      print(d, verbose = verbose)
    }
  }

  invisible(x)
}



#' @title Generate Stability Scores for a Repository
#' @description The function generates stability scores
#' for a vector of packages. If passing a large number of packages,
#' be prepared for the
#' function to run for a considerable amount of time.
#' @details
#' To assess stability for a package, \strong{pkgdiff} has to compare all
#' releases of a package, and identify breaking changes between each release.
#' This comparison takes time, especially for packages that have been
#' active for many years.
#'
#' Therefore, the first step of the function is to compare all releases
#' for each package in the input vector.  The function will send messages
#' to the console to let you know which package it is currently comparing.
#' These messages will let you know how far the function is through the processing.
#'
#' Once all the comparisons are completed, the function will return a data
#' frame summary of stability results.  The data frame will have one
#' row for each package in the input vector.  The data frame will also
#' show some information about the comparison, and give a stability assessment
#' for each package.  The data frame columns are as follows:
#' \itemize{
#'   \item {\strong{Package}: The package name.}
#'   \item {\strong{FV}: The first version of the package.}
#'   \item {\strong{LV}: The last version of the package.}
#'   \item {\strong{FR}: The first release date of the package.}
#'   \item {\strong{LR}: The last release date of the package.}
#'   \item {\strong{TR}: The total number of releases.}
#'   \item {\strong{BR}: The number of breaking releases.}
#'   \item {\strong{Score}: The stability score.}
#'   \item {\strong{Assessment}: The stability assessment.}
#' }
#' To learn how the package stability score and assessment are calculated,
#' see \code{vignette("pkgdiff-stability")}.  Additional information is
#' included in the documentation of the \code{\link{pkg_stability}}
#' function.
#'
#' If a package is not found in the package cache, the function
#' will download and compare each version of the package on CRAN.  This
#' process may increase the processing time.  See \code{vignette("pkg-cache")}
#' and \code{\link{pkg_cache}} for additional information.
#'
#' If a package does not exist on CRAN, a row for that package will still
#' be returned.  However, all data values will be NA.  This situation may
#' occur if a package exists on Github, but has not yet been submitted
#' to CRAN.  The \strong{pkgdiff} stability functions only work with packages
#' that have been published to CRAN.
#' @param pkgs A vector of package names.
#' @param releases An integer number of releases to assess.  The
#' function will then limit the scope of the assessment to the specified
#' number of releases.
#' @param months An integer number of months from the current date.  The
#' function will then limit the scope of the assessment to the specified
#' number of months.
#' @returns A data frame of information regarding the stability of
#' each package in the input vector.
#' @family prepo
#' @import common
#' @examples
#' # Create vector of packages
#' vct <- c("curl", "dplyr", "rvest", "tidymodels")
#'
#' # Get stablity scores
#' res <- repo_stability(vct)
#' # Getting stability score for package 'curl'...
#' # Getting stability score for package 'dplyr'...
#' # Getting stability score for package 'rvest'...
#' # Getting stability score for package 'tidymodels'...
#'
#' # View stability results
#' res
#' # A repo stability object
#' # - Run Datetime: 2025-03-01 17:53 UTC
#' # - Summary:
#' #   Package    FV    LV         FR         LR TR BR Score        Assessment
#' # 1       curl   0.2 6.2.1 2014-11-20 2025-02-19 51  1  98.0       Very Stable
#' # 2      dplyr   0.1 1.1.4 2014-01-16 2023-11-17 45 20  87.5 Somewhat Unstable
#' # 3      rvest 0.1.0 1.0.4 2014-11-22 2024-02-12 14  4  93.2            Stable
#' # 4 tidymodels 0.0.1 1.3.0 2018-07-27 2025-02-21 14  0 100.0           Perfect
#'
#' @export
repo_stability <- function(pkgs, releases = NULL, months = NULL) {

  # Create data frame structure
  dat <- data.frame("Package" = NA, FV = NA, LV = NA,
                    FR = NA, LR = NA,
                    TR = NA, BR = NA, Score = NA, Assessment = NA)

  idx <- 1

  for (pkg in pkgs) {

    cat(paste0("Getting stability score for package '", pkg, "'...\n"))

    # Get stability data for package
    rpt <- pkg_stability(pkg, releases = releases, months = months)

    # Put values into data frame
    dat[[idx, "Package"]] <- pkg
    if (!is.null(rpt$FirstVersion))
      dat[[idx, "FV"]] <- rpt$FirstVersion
    if (!is.null(rpt$LastVersion))
      dat[[idx, "LV"]] <- rpt$LastVersion
    if (!is.null(rpt$FirstRelease))
      dat[[idx, "FR"]] <- as.Date(rpt$FirstRelease, origin = '1970-01-01')
    if (!is.null(rpt$LastRelease))
      dat[[idx, "LR"]] <- as.Date(rpt$LastRelease, origin = '1970-01-01')
    if (!is.null(rpt$NumReleases))
      dat[[idx, "TR"]] <- rpt$NumReleases
    if (!is.null(rpt$BreakingReleases))
      dat[[idx, "BR"]] <- rpt$BreakingReleases
    if (!is.null(rpt$StabilityScore))
      dat[[idx, "Score"]] <- rpt$StabilityScore
    if (!is.null(rpt$StabilityScore))
      dat[[idx, "Assessment"]] <- get_stability_assessment(rpt$StabilityScore)

    idx <- idx + 1
  }

  if (!is.null(dat)) {

    # Assign lables
    common::labels(dat) <- list(FV = "First Version", LV = "Last Version",
                                FR = "First Release", LR = "Last Release",
                                TR = "Total Releases", BR = "Breaking Releases",
                                Score = "Stability Score",
                                Assessment = "Stability Assessment",
                                Package = "Package Name")

    # Ensure release dates come out as dates
    dat$FR <- as.Date(dat$FR, origin = '1970-01-01')
    dat$LR <- as.Date(dat$LR, origin = '1970-01-01')
  }

  ret <- dat

  attr(ret, "Datetime") <- Sys.time()

  class(ret) <- c("rstability", class(ret))

  return(ret)
}

#' @title Print a Repo Stability Object
#' @description
#' Print routine for a repot stability object of class "rstability".
#'
#' @param x The repo stability object to print.
#' @param ... Follow-on parameters to the print function.
#' @family prepo
#' @import crayon
#' @examples
#' # Create vector of packages
#' vct <- c("curl", "dplyr", "rvest", "tidymodels")
#'
#' # Get stablity scores
#' res <- repo_stability(vct)
#'
#' # View stability scores
#' print(res)
#' # # A repo stability object
#' # - Run Datetime: 2025-03-01 19:19 UTC
#' # - Summary:
#' #      Package    FV    LV         FR         LR TR BR Score        Assessment
#' # 1       curl   0.2 6.2.1 2014-11-20 2025-02-19 51  1  98.0       Very Stable
#' # 2      dplyr   0.1 1.1.4 2014-01-16 2023-11-17 45 20  87.5 Somewhat Unstable
#' # 3      rvest 0.1.0 1.0.4 2014-11-22 2024-02-12 14  4  93.2            Stable
#' # 4 tidymodels 0.0.1 1.3.0 2018-07-27 2025-02-21 14  0 100.0           Perfect
#' @export
print.rstability <- function(x, ...) {

  grey60 <- crayon::make_style(grey60 = "#999999")
  cat(grey60(paste0("# A repo stability object\n")))


  dt <- attr(x, "Datetime")

  if (!is.null(dt)) {
    tmstmp <- format(dt, format = "%Y-%m-%d %H:%M UTC")
    cat(paste0("- Run Datetime: ", tmstmp , "\n"))

  }


  if (!is.null(x)) {
    cat(paste0("- Summary:\n"))
    df <- as.data.frame(x)
    scr <- sprintf("%.1f", df$Score * 100)
    df$Score <- scr
    print(df)
  }


  invisible(x)
}


