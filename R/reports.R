

#' @title Identify Breakages for a Repository
#' @description
#' The \code{repo_breakages} function generates a data frame
#' of breakage information of a list of packages or entire repository.
#' @param v1pkgs A data frame that identifies the source packages
#' and versions.
#' @param v2pkgs A data frame that identifies the target packages
#' and versions.  By default,
#' @family reports
#' @import common
#' @export
repo_breakages <- function(v1pkgs = "current", v2pkgs = "latest") {

  if (!is.data.frame(v1pkgs)) {
    if (v1pkgs == "current") {
      v1pkgs <- installed_packages()
    }
  }

  if (!is.data.frame(v1pkgs)) {
    stop("V1 package list must be a data frame.")
  }

  if (!"Package"  %in% names(v1pkgs) ||
      !"Version" %in% names(v1pkgs)) {
    stop("V1 package list must contain the columns 'Package' and 'Version'.")
  }

  if (!is.data.frame(v2pkgs)) {
    if (v2pkgs == "latest") {

      lver <- get_latest_version(v1pkgs$Package)

      v2pkgs <- data.frame(Package = v1pkgs$Package,
                            Version = lver)

    }
  }

  if (!"Package"  %in% names(v2pkgs) ||
      !"Version" %in% names(v2pkgs)) {
    stop("V2 package list must contain the columns 'Package' and 'Version'.")
  }

  if (nrow(v1pkgs) != nrow(v2pkgs)) {
    stop("v1 and v2 package lists must be the same size.")
  }

  dat <- data.frame("Package" = NA, Version1 = NA, Version2 = NA,
                    Breakages = FALSE)
  det <- list()

  # browser()

  for (idx in seq_len(nrow(v1pkgs))) {

    pkg <- v1pkgs$Package[[idx]]
    v1 <- v1pkgs$Version[[idx]]
    v2 <- v2pkgs[v2pkgs$Package == pkg, "Version"]

    bc <- FALSE
    if (v1 != v2) {
      cat(paste0("Comparing ", pkg, " v", v1, "/v", v2, "\n"))
      diff <- suppressWarnings(pkg_diff(pkg, v1, v2))
      bc <- diff$BreakingChanges
    }

    dat[[idx, "Package"]] <- pkg
    dat[[idx, "Version1"]] <- v1
    dat[[idx, "Version2"]] <- v2
    dat[[idx, "Breakages"]] <- bc

    if (bc) {

      det[[pkg]] <- diff
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

  ret <- NULL

  dat <- data.frame("Package" = NA, FV = NA, LV = NA,
                    FR = NA, LR = NA,
                    TR = NA, BR = NA, Score = NA, Assessment = NA)

  idx <- 1

  for (pkg in pkgs) {

    rpt <- pkg_stability(pkg, releases = releases, months = months)

    dat[[idx, "Package"]] <- pkg
    dat[[idx, "FV"]] <- rpt$FirstVersion
    dat[[idx, "LV"]] <- rpt$LastVersion
    dat[[idx, "FR"]] <- as.Date(rpt$FirstRelease, origin = '1970-01-01')
    dat[[idx, "LR"]] <- as.Date(rpt$LastRelease, origin = '1970-01-01')
    dat[[idx, "TR"]] <- rpt$NumReleases
    dat[[idx, "BR"]] <- rpt$BreakingReleases
    dat[[idx, "Score"]] <- rpt$StabilityScore
    dat[[idx, "Assessment"]] <- get_stability_assessment(rpt$StabilityScore)

    idx <- idx + 1
  }

  if (!is.null(dat)) {

    common::labels(dat) <- list(FV = "First Version", LV = "Last Version",
                                FR = "First Release", LR = "Last Release",
                                TR = "Total Releases", BR = "Breaking Releases",
                                Score = "Stability Score",
                                Assessment = "Stability Assessment",
                                Package = "Package Name")

    dat$FR <- as.Date(dat$FR, origin = '1970-01-01')
    dat$LR <- as.Date(dat$LR, origin = '1970-01-01')
  }



  ret <- dat

  return(ret)
}
