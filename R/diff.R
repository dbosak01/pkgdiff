

#' @title Get a Package Difference Object
#' @param pkg The package name.
#' @param v1 The earlier package version.  Default is "current", which
#' means the function will look up the currently installed version.
#' @param v2 The later package version.  The default is "latest", which is the
#' latest version of the package available on CRAN.
#' @family pdiff
#' @export
pkg_diff <- function(pkg, v1 = "current",
                     v2 = "latest") {

  #browser()

  d <- structure(list(), class = c("pdiff", "list"))

  v1_diff_info <- NULL
  v2_diff_info <- NULL

  # print("Debug 1")

  if ("pinfo" %in% class(v1)) {
    v1_diff_info <- v1
    v1 <- v1$Version
  } else if (v1 == "current") {
    v1 <- get_current_version(pkg)

    if (length(v1) == 0) {
      stop(paste0("There is no current version of the package '", pkg, "'."))
    }
  }

  # print("Debug 2")

  # Collect data
  if ("pinfo" %in% class(v2)) {
    v2_diff_info <- v2
    v2 <- v2$Version
  }

  # print("Debug 3")

  v2data <- get_latest_data(pkg)
  vLatest <- v2data$Version[[1]]

  if (v2 == "latest") {
    v2 <- vLatest
  }
  # print("Debug 4")

  # Check that versions exist
  avers <- get_all_versions(pkg)

  if (!v1 %in% avers$Version) {
    stop("Version '", v1, "' of package '", pkg, "' does not exist on CRAN.")
  }

  if (!v2 %in% avers$Version) {
    stop("Version '", v2, "' of package '", pkg, "' does not exist on CRAN.")
  }

  if (is.null(v1_diff_info) && is.null(v2_diff_info)) {
    infos <- get_fastest_infos(pkg, v1, v2)
    v1_diff_info <- infos[[v1]]
    v2_diff_info <- infos[[v2]]
  }
  if (is.null(v1_diff_info)) {
    v1_diff_info <- pkg_info(pkg, v1, cache = FALSE)
  }
  if (is.null(v2_diff_info)) {
    v2_diff_info <- pkg_info(pkg, v2, cache = FALSE)
  }

  # print("Debug 5")

  if (is.null(v1_diff_info)) {

    stop(paste0("Could not retrieve package ", pkg, " v",  v1))
  }

  if (is.null(v2_diff_info)) {

    stop(paste0("Could not retrieve package ", pkg, " v",  v2))
  }

  if (!is.null(v1_diff_info) && !is.null(v2_diff_info)) {


    # Get deprecated functions
    depf <- get_removed_functions(v1_diff_info, v2_diff_info)

    # print("Debug 6")

    # Get deprecated parameters
    depp <- get_removed_parameters(v1_diff_info, v2_diff_info)

    # print("Debug 7")

    # Get breaking changes
    if (length(depf) > 0 || length(depp) > 0)
      bc <- TRUE
    else
      bc <- FALSE

    addf <- get_added_functions(v1_diff_info, v2_diff_info)
    addp <- get_added_parameters(v1_diff_info, v2_diff_info)
    af <- get_all_functions(v1_diff_info, v2_diff_info)

    # print("Debug 8")

    # Populate pdiff object
    d$PackageName <- pkg
    d$Version1 <- v1
    d$Version2 <- v2
    d$Version1DiffInfo <- v1_diff_info
    d$Version2DiffInfo <- v2_diff_info
    d$AddedFunctions <- addf
    d$AddedParameters <- addp
    d$RemovedFunctions <- depf
    d$RemovedParameters <- depp
    d$BreakingChanges <- bc
    d$AllFunctions <- af

  }

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
    cat(grey60(paste0("# A difference object: ",
                 as.character(x$PackageName), " package\n")))

    # if (!is.null(x$PackageAge))
    #   cat(paste0("- Age: ", x$PackageAge, "\n"))
    #
    # if (!is.null(x$FirstRelease))
    #   cat(paste0("- First Release: ", x$FirstRelease, "\n"))
    #
    # if (!is.null(x$LastRelease))
    #   cat(paste0("- Last Release: ", x$LastRelease, "\n"))
    #
    # if (!is.null(x$NumReleases))
    #   cat(paste0("- Release Count: ", as.character(x$NumReleases), "\n"))

    if (!is.null(x$Version1))
      cat(paste0("- Comparing: ", "v", x$Version1, "/v", x$Version2, "\n"))

    if (!is.null(x$BreakingChanges))
      cat(paste0("- Breaking Changes: ", as.character(x$BreakingChanges), "\n"))

    if (!is.null(x$AddedFunctions)) {
      cat(paste0("- Added Functions: \n"))

      for (nm in x$AddedFunctions) {
        cat(paste0("  - ",  nm, "()\n"))
      }
    }

    if (!is.null(x$AddedParameters)) {
      if (length(x$AddedParameters) > 0) {
        cat(paste0("- Added Parameters: \n"))

        nms <- names(x$AddedParameters)
        for (nm in nms) {
          parms <- paste(x$AddedParameters[[nm]], collapse = ", ")
          cat(paste0("  - ",  nm, "(): ", parms, "\n"))
        }
      }
    }

    if (!is.null(x$RemovedFunctions)) {
      cat(paste0("- Removed Functions: \n"))

      for (nm in x$RemovedFunctions) {
        cat(paste0("  - ",  nm, "()\n"))
      }
    }

    if (!is.null(x$RemovedParameters)) {
      if (length(x$RemovedParameters) > 0) {
        cat(paste0("- Removed Parameters: \n"))

        nms <- names(x$RemovedParameters)
        for (nm in nms) {
          parms <- paste(x$RemovedParameters[[nm]], collapse = ", ")
          cat(paste0("  - ",  nm, "(): ", parms, "\n"))
        }
      }
    }



  }

  invisible(x)
}

# @title View Package Difference Details
# @description The \strong{view_details} function displays package differences
# in the RStudio viewer, so they can be examined in detail. Package information
# is taken from CRAN.
# @param diff A package difference object of class "pdiff".  This object is
# returned from \code{\link{pkg_diff}}.
# @param docs Whether to include the function documentation
# in the viewer comparison.  Default is TRUE.
# @return A package version comparison is displyed in the RStudio viewer.
# @family pdiff
# @export
# view_details <- function(diff, docs = TRUE) {
#
#   if (!"pdiff" %in% class(diff)) {
#
#     stop("Parameter 'diff' must be an object of class 'pdiff'.")
#   }
#
#   d1 <- diff$Version1DiffInfo
#   d2 <- diff$Version2DiffInfo
#
#
#   pkgDiff(d1, d2, doc = docs, )
#
# }

