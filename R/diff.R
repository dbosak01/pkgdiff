
#' @title Get a Package Difference Object
#' @param pkgname The package name.
#' @param v1 The earlier package version.
#' @param v2 The later package version.
#' @family pdiff
#' @import packageDiff
#' @export
get_diff <- function(pkgname, v1 = "current",
                              v2 = "latest") {

  #browser()

  d <- structure(list(), class = c("pdiff", "list"))

  if (v1 == "current") {
    v1 <- get_current_version(pkgname)
  }

  # Collect data
  v2data <- get_latest_data(pkgname)
  v2archive <- get_archive_versions(pkgname)
  vLatest <- v2data$Version[1]

  # Get first release date
  if (nrow(v2archive) > 0)
    vFirst <- v2archive[nrow(v2archive), "Date"]
  else
    vFirst <- vLatest

  if (v2 == "latest") {
    v2 <- vLatest
  }

  # https://cran.r-project.org/src/contrib/logr_1.3.8.tar.gz
  currentpath = "https://cran.r-project.org/src/contrib/"
  # https://cran.r-project.org/src/contrib/Archive/logr/logr_1.0.3.tar.gz
  archivepath = "https://cran.r-project.org/src/contrib/Archive"

  # Get path to v1 package
  v1_path <- file.path(archivepath, pkgname, get_file_name(pkgname, v1))

  # Get path to v2 package
  if (v2 == vLatest)
    v2_path <- file.path(currentpath, get_file_name(pkgname, v2))
  else
    v2_path <- file.path(archivepath, pkgname, get_file_name(pkgname, v2))

  # Get Diff Info Objects
  v1_diff_info <- packageDiff::pkgInfo(v1_path)
  v2_diff_info <- packageDiff::pkgInfo(v2_path)

  # Get time span
  spn <-  v2data$Date[1] - vFirst

  # Get deprecated functions
  depf <- get_deprecated_functions(v1_diff_info, v2_diff_info)

  # Get deprecated parameters
  depp <- get_deprecated_parameters(v1_diff_info, v2_diff_info)

  # Get breaking changes
  if (length(depf) > 0 || length(depp) > 0)
    bc <- TRUE
  else
    bc <- FALSE

  # Populate pdiff object
  d$PackageName <- pkgname
  d$PackageAge <- sprintf("%0.2f years", spn / 30 / 12)
  d$FirstRelease <- vFirst
  d$LastRelease <- v2data$Date[1]
  d$NumReleases <- nrow(v2archive) + 1
  d$Version1 <- v1
  d$Version2 <- v2
  d$Version1Path <- v1_path
  d$Version2Path <- v2_path
  d$Version1DiffInfo <- v1_diff_info
  d$Version2DiffInfo <- v2_diff_info
  d$DeprecatedFunctions <- depf
  d$DeprecatedParameters <- depp
  #d$ChangedFunctions <- cf
  d$BreakingChanges <- bc

  return(d)

}

#' @title Print a Package Difference Object
#' @param x The package difference to print
#' @param ... Follow-on parameters to the print function
#' @param verbose Whether to print in summary or list-style.
#' @family pdiff
#' @import crayon
#' @export
print.pdiff <- function(x, ..., verbose = FALSE) {





  if (verbose == TRUE) {

    print(unclass(x))

  } else {

    grey60 <- crayon::make_style(grey60 = "#999999")
    cat(grey60("# A difference object: " %+%
                 as.character(x$PackageName) %+% " package\n"))

    if (!is.null(x$PackageAge))
      cat(paste0("- Age: ", x$PackageAge, "\n"))

    if (!is.null(x$FirstRelease))
      cat(paste0("- First Release: ", x$FirstRelease, "\n"))

    if (!is.null(x$LastRelease))
      cat(paste0("- Last Release: ", x$LastRelease, "\n"))

    if (!is.null(x$NumReleases))
      cat(paste0("- Release Count: ", as.character(x$NumReleases), "\n"))

    if (!is.null(x$Version1))
      cat(paste0("- Comparing: ", "v", x$Version1, "/v", x$Version2, "\n"))

    if (!is.null(x$BreakingChanges))
      cat(paste0("- Breaking Changes: ", as.character(x$BreakingChanges), "\n"))

    if (!is.null(x$DeprecatedFunctions)) {
      cat(paste0("- Deprecated Functions: \n"))

      for (nm in x$DeprecatedFunctions) {
        cat(paste0("  - ",  nm, "()\n"))
      }
    }

    if (!is.null(x$DeprecatedParameters)) {
      if (length(x$DeprecatedParameters) > 0) {
        cat(paste0("- Deprecated Parameters: \n"))

        nms <- names(x$DeprecatedParameters)
        for (nm in nms) {
          parms <- paste(x$DeprecatedParameters[[nm]], collapse = ", ")
          cat(paste0("  - ",  nm, "(): ", parms, "\n"))
        }
      }
    }

  }

  invisible(x)
}

#' @title View Package Difference Details
#' @description The \strong{view_details} function displays package differences
#' in the RStudio viewer, so they can be examined in detail. Package information
#' is taken from CRAN.
#' @param diff A package difference object of class "pdiff".  This object is
#' returned from \code{\link{get_diff}}.
#' @return A package version comparison is displyed in the RStudio viewer.
#' @family pdiff
#' @import packageDiff
#' @export
view_details <- function(diff) {

  if (!"pdiff" %in% class(diff)) {

    stop("Parameter 'diff' must be an object of class 'pdiff'.")
  }

  d1 <- diff$Version1DiffInfo
  d2 <- diff$Version2DiffInfo


  pkgDiff(d1, d2)

}

