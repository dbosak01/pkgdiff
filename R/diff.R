
# Rename to pkg_diff?


#' @title Get a Package Difference Object
#' @param pkgname The package name.
#' @param v1 The earlier package version.  Default is "current", which
#' means the function will look up the currently installed version.
#' @param v2 The later package version.  The default is "latest", which is the
#' latest version of the package available on CRAN.
#' @family pdiff
#' @export
pkg_diff <- function(pkgname, v1 = "current",
                     v2 = "latest") {

  #browser()

  d <- structure(list(), class = c("pdiff", "list"))

  v1_diff_info <- NULL
  v2_diff_info <- NULL

  print("Debug 1")

  if ("pkgInfo" %in% class(v1)) {
    v1_diff_info <- v1
    v1 <- v1$Version
  } else if (v1 == "current") {
    v1 <- get_current_version(pkgname)
  }

  print("Debug 2")

  # Collect data
  if ("pkgInfo" %in% class(v2)) {
    v2_diff_info <- v2
    v2 <- v2$Version
  }

  print("Debug 3")

  v2data <- get_latest_data(pkgname)
  vLatest <- v2data$Version[[1]]

  if (v2 == "latest") {
    v2 <- vLatest
  }
  print("Debug 4")

  infos <- get_fastest_infos(pkgname, v1, v2)
  v1_diff_info <- infos[[v1]]
  v2_diff_info <- infos[[v2]]

  print("Debug 5")

  if (is.null(v1_diff_info)) {

    stop(paste0("Could not retrieve package ", pkgname, " v",  v1))
  }

  if (is.null(v2_diff_info)) {

    stop(paste0("Could not retrieve package ", pkgname, " v",  v2))
  }

  if (!is.null(v1_diff_info) && !is.null(v2_diff_info)) {


    # Get deprecated functions
    depf <- get_removed_functions(v1_diff_info, v2_diff_info)

    print("Debug 6")

    # Get deprecated parameters
    depp <- get_removed_parameters(v1_diff_info, v2_diff_info)

    print("Debug 7")

    # Get breaking changes
    if (length(depf) > 0 || length(depp) > 0)
      bc <- TRUE
    else
      bc <- FALSE

    addf <- get_added_functions(v1_diff_info, v2_diff_info)
    addp <- get_added_parameters(v1_diff_info, v2_diff_info)
    af <- get_all_functions(v1_diff_info, v2_diff_info)

    print("Debug 8")

    # Populate pdiff object
    d$PackageName <- pkgname
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



# get_diff <- function(pkgname, v1 = "current",
#                               v2 = "latest") {
#
#   #browser()
#
#   d <- structure(list(), class = c("pdiff", "list"))
#
#   v1_diff_info <- NULL
#   v2_diff_info <- NULL
#
#   if ("pkgInfo" %in% class(v1)) {
#     v1_diff_info <- v1
#     v1 <- v1$Version
#   } else if (v1 == "current") {
#     v1 <- get_current_version(pkgname)
#   }
#
#   # Collect data
#   if ("pkgInfo" %in% class(v2)) {
#     v2_diff_info <- v2
#     v2 <- v2$Version
#   }
#
#   # Get latest version
#   v2data <- get_latest_data(pkgname)
#   v2archive <- get_archive_versions(pkgname)
#   vLatest <- v2data$Version[[1]]
#
#   # Get first release date
#   if (nrow(v2archive) > 0)
#     vFirst <- v2archive[nrow(v2archive), "Release"]
#   else
#     vFirst <- vLatest
#
#   if (v2 == "latest") {
#     v2 <- vLatest
#   }
#
#   currentpath = "https://cran.r-project.org/src/contrib/"
#   archivepath = "https://cran.r-project.org/src/contrib/Archive"
#
#   # Get path to v1 package
#   v1_path <- file.path(archivepath, pkgname, get_file_name(pkgname, v1))
#
#   # Get path to v2 package
#   if (v2 == vLatest)
#     v2_path <- file.path(currentpath, get_file_name(pkgname, v2))
#   else
#     v2_path <- file.path(archivepath, pkgname, get_file_name(pkgname, v2))
#
#   #browser()
#
#   # Get Diff Info Objects
#   if (is.null(v1_diff_info)) {
#     v1_diff_info <- tryCatch({suppressWarnings(packageDiff::pkgInfo(v1_path))},
#                            error = function(e){NULL})
#   }
#
#   if (is.null(v2_diff_info)) {
#     v2_diff_info <- tryCatch({suppressWarnings(packageDiff::pkgInfo(v2_path))},
#                            error = function(e){NULL})
#   }
#
#   if (is.null(v1_diff_info)) {
#
#     stop(paste0("Could not retrieve package ", pkgname, " v",  v1, " from ",
#                 v1_path))
#   }
#
#   if (is.null(v2_diff_info)) {
#
#     stop(paste0("Could not retrieve package ", pkgname, " v",  v2, " from ",
#                 v2_path))
#   }
#
#   if (!is.null(v1_diff_info) && !is.null(v2_diff_info)) {
#
#     # Get time span
#     spn <-  v2data$Release[1] - vFirst
#
#     # Get deprecated functions
#     depf <- get_removed_functions(v1_diff_info, v2_diff_info)
#
#     # Get deprecated parameters
#     depp <- get_removed_parameters(v1_diff_info, v2_diff_info)
#
#     # Get breaking changes
#     if (length(depf) > 0 || length(depp) > 0)
#       bc <- TRUE
#     else
#       bc <- FALSE
#
#     addf <- get_added_functions(v1_diff_info, v2_diff_info)
#     addp <- get_added_parameters(v1_diff_info, v2_diff_info)
#     af <- get_all_functions(v1_diff_info, v2_diff_info)
#
#     # Populate pdiff object
#     d$PackageName <- pkgname
#     d$Version1 <- v1
#     d$Version2 <- v2
#     d$Version1Path <- v1_path
#     d$Version2Path <- v2_path
#     d$Version1DiffInfo <- v1_diff_info
#     d$Version2DiffInfo <- v2_diff_info
#     d$AddedFunctions <- addf
#     d$AddedParameters <- addp
#     d$RemovedFunctions <- depf
#     d$RemovedParameters <- depp
#     d$BreakingChanges <- bc
#     d$AllFunctions <- af
#
#   }
#
#   return(d)
#
# }



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

