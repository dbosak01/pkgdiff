


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



