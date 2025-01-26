


#' @title Collect Stability Data for a Package
#' @param pkgname The name of the package.
#' @param releases An integer indicating the number of releases to collect
#' stability data for. For example, \code{releases = 10} will return stability
#' data for the last 10 releases of the package. Default is NULL, which means
#' the function will return data for all releases.
#' @param months An integer indicating the number of months back to collect
#' stability data for.  For example, \code{months = 24} will collect
#' stability data for the previous 2 years.  Default is NULL, meaning there is
#' no limitation on the number of release months, and the function will collect
#' data from all releases.
#' @return A data frame of stability information for the targeted package.
#' The data frame will contain one row for each release.  Each row will
#' contains comparison information against the prior release.  The
#' columns will be organized as follows:
#' \itemize{
#'   \item {\strong{Version}: The version number of the release.
#'   }
#'   \item {\strong{FileName}: The file name of the package.
#'   }
#'   \item {\strong{Release}: The release date of the package.
#'   }
#'   \item {\strong{Size}: The size of the package file on disk.
#'   }
#'   \item {\strong{AF}: The number of added functions.
#'   }
#'   \item {\strong{AP}: The number of added parameters.
#'   }
#'   \item {\strong{RF}: The number of removed functions.
#'   }
#'   \item {\strong{RP}: The number of removed parameters.
#'   }
#'   \item {\strong{BC}: Whether the release had any breaking changes.
#'   Breaking changes are removed functions or removed parameters.  Values
#'   are zero or one.  The value one means the release had breaking changes.
#'   }
#'   \item {\strong{TF}: The total number of functions in the package.
#'   }
#'
#'
#' }
#' @family stability
#' @import common
#' @export
get_stability_data <- function(pkgname, releases = NULL, months = NULL) {

  #browser()

  ldat <- get_latest_data(pkgname)
  adat <- get_archive_versions(pkgname)

  dat <- rbind(ldat, adat)
  dat$AF <- NA
  dat$AP <- NA
  dat$RF <- NA
  dat$RP <- NA
  dat$BC <- NA
  dat$TF <- NA

  rownames(dat) <- NULL

  if (!is.null(releases)) {

    if (nrow(dat) > releases)
      dat <- dat[seq(1, releases + 1), ]
  }

  if (!is.null(months)) {
    dm <- Sys.Date() - ((months + 1) * 30)
    dat <- subset(dat, dat$Release >= dm)
  }

  idxs <- seq(1, nrow(dat) - 1)

  for (idx in idxs) {

    v2 <- dat[idx, "Version"]
    v1 <- dat[idx + 1, "Version"]

    cat(paste0("Comparing ", pkgname, " ", v1, "/", v2, "\n"))

    diff <- tryCatch({
     get_diff(pkgname, v1, v2)
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

  if (!is.null(releases)) {

    if (nrow(dat) > releases)
      dat <- dat[seq(1, releases), ]
  }

  common::labels(dat) <- list(AF = "Added Functions",
                              AP = "Added Parameters",
                              RF = "Removed Functions",
                              RP = "Removed Parameters",
                              BC = "Breaking Changes",
                              TC = "Total Functions")

  return(dat)

}


#' @title Calculate Stability Score for a Package
#' @description
#' The \code{get_stablity_score} function calculates a stability score for
#' a specified package.  The score is calculated as the percentage of
#' releases with no breaking changes.  A breaking change is defined as
#' either a removed function or removed function parameter. The object
#' also returns the raw data from which the stability score is calculated.
#' This data can be useful for review or even custom analytics.  For a
#' description of the stability data frame, see \code{\link{get_stability_data}}.
#' @param pkgname The name of the package.
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
#' breaking releases.
#' @family stability
#' @export
get_stability_score <- function(pkgname, releases = NULL, months = NULL ) {


  d <- structure(list(), class = c("pdiff_score", "list"))

  dat <- get_stability_data(pkgname, releases, months)

  # Calculate stability score
  scr <- 1 - (sum(dat$BC, na.rm = TRUE) / (nrow(dat) - 1))

  # Populate pdiff_score object
  d$PackageName <- pkgname
  d$FirstRelease <- dat$Release[[nrow(dat)]]
  d$LastRelease <- dat$Release[[1]]
  d$StabilityScore <- scr
  d$NumReleases <- nrow(dat)
  d$BreakingReleases <- sum(dat$BC, na.rm = TRUE)
  d$FirstVersion <- dat$Version[[nrow(dat)]]
  d$LastVersion <- dat$Version[[1]]
  d$AddedFunctions <- sum(dat$AF, na.rm = TRUE)
  d$AddedParameters <- sum(dat$AP, na.rm = TRUE)
  d$RemovedFunctions <- sum(dat$RF, na.rm = TRUE)
  d$RemovedParameters <- sum(dat$RP, na.rm = TRUE)
  d$StabilityData <- dat

  return(d)
}


#' @title Print a Package Difference Stability Score Object
#' @param x The package difference to print
#' @param ... Follow-on parameters to the print function
#' @param verbose Whether to print in summary or list-style.
#' @family stability
#' @import crayon
#' @export
print.pdiff_score <- function(x, ..., verbose = FALSE) {

  if (verbose == TRUE) {

    print(unclass(x))

  } else {

    grey60 <- crayon::make_style(grey60 = "#999999")
    cat(grey60("# A stability score: " %+%
                 as.character(x$PackageName) %+% " package\n"))
    if (!is.null(x$StabilityScore)) {

      scr <- sprintf("%.1f", x$StabilityScore * 100)
      cat(paste0("- Score: ", scr, "\n"))

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


