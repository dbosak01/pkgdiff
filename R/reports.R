

#' @title Identify Breakages for a Repository
#' @description
#' The \code{repo_breakages} function generates a data frame
#' of breakage information of a list of packages or entire repository.
#' @param r1 A data frame that identifies the source repository packages
#' and versions. The default value is "current", which means
#' the function will use the current versions of all package in
#' the current R repository.
#' @param r2 A data frame that identifies the target repository packages
#' and versions. The default value is "latest", which means
#' the function will use the latest versions of all packages on CRAN.
#' @family reports
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

  ret <- list(summary = dat, details = det)

  return(ret)

}



#' @title Generate Stability Scores
#' @description The function generates stability scores
#' for a vector of packages. If passing a large number of packages,
#' be prepared for the
#' function to run for a considerable amount of time.
#' @param pkgs A data frame that identifies the source packages
#' and versions.
#' @param releases An integer number of releases to assess stability.
#' @param months An integer number of months from the current date
#' to assess stability.
#' @returns A data frame of information regarding the stability of the
#' packages. There will be one row per package.
#' @family reports
#' @import common
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

  return(ret)
}
