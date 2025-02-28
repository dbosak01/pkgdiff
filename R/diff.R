

#' @title Compare Package Versions
#' @description
#' The \code{pkg_diff} function is used to compare two different versions of
#' a package.  The function returns an object that contains the differences.
#' Differences include any functions added or removed. The object may
#' be printed directly, or stored in a variable and examined programmatically.
#' @details
#' The difference operation only compares functions that are exported
#' from the package. It does not compare internal functions or function documentation.
#' Exported functions are identified in the package namespace.
#'
#' The \code{pkg_diff} function can compare any two versions of a package.
#' They do not need to be consecutive. They must, however, exist in the CRAN
#' archive.
#'
#' If the package versions are included in the \strong{pkgdiff} cache, the
#' function will pull them from there.  Otherwise, they will be pulled from the
#' CRAN mirror.  The function does not access any packages on the local system.
#' @param pkg The package name.
#' @param v1 The earlier package version. Pass the version value as a quoted string.
#' Default is "current", which means the function will look up the currently
#' installed version.
#' @param v2 The later package version. Pass the version value as a quoted string.
#' The default is "latest", which is the
#' latest version of the package available on CRAN.
#' @returns An object of class "pdiff", which contains information regarding
#' the differences between two versions of a package.  The object includes
#' a list of functions that were added, a list of function parameters that were added,
#' a list of functions removed, and a list of function parameters that
#' were removed.  The object also contains some general information like
#' the package versions examined, and a TRUE or FALSE flag indicating whether
#' there were any breaking changes.
#' @examples
#' # View package stability
#' pkg_stability("rsample")
#' # # A stability score: rsample package
#' # - Age: 7.58 years
#' # - Score: 80.3
#' # - Assessment: Somewhat Unstable
#' # - Version Range: 0.0.1/1.2.1
#' # - Release Range: 2017-07-08/2024-03-25
#' # - Release Count: 16
#' # - Breaking Releases: 6
#' # - Data:
#' #   Package Version             FileName    Release   Size  AF AP RF RP BC  TF
#' # 1  rsample   1.2.1 rsample_1.2.1.tar.gz 2024-03-25 320.9K   1  0  0  0  0 401
#' # 2  rsample   1.2.0 rsample_1.2.0.tar.gz 2023-08-23   321K  43  6  1  3  1 400
#' # 3  rsample   1.1.1 rsample_1.1.1.tar.gz 2022-12-07   318K  37  5  0  0  0 358
#' # 4  rsample   1.1.0 rsample_1.1.0.tar.gz 2022-08-08   306K  72  1  0  0  0 321
#' # 5  rsample   1.0.0 rsample_1.0.0.tar.gz 2022-06-24   268K   1  0  1  0  1 249
#' # 6  rsample   0.1.1 rsample_0.1.1.tar.gz 2021-11-08   274K   4  1  0  1  1 249
#' # 7  rsample   0.1.0 rsample_0.1.0.tar.gz 2021-05-08   274K   0  5  0  0  0 245
#' # 8  rsample   0.0.9 rsample_0.0.9.tar.gz 2021-02-17   269K  19  0  6  0  1 245
#' # 9  rsample   0.0.8 rsample_0.0.8.tar.gz 2020-09-23   261K  59  0  0  0  0 232
#' # 10 rsample   0.0.7 rsample_0.0.7.tar.gz 2020-06-04   248K 101  1  0  0  0 173
#' # 11 rsample   0.0.6 rsample_0.0.6.tar.gz 2020-03-31   299K   7  2  0  0  0  72
#' # 12 rsample   0.0.5 rsample_0.0.5.tar.gz 2019-07-13   297K   4  5  0  0  0  65
#' # 13 rsample   0.0.4 rsample_0.0.4.tar.gz 2019-01-07   254K   0  0  4  0  1  61
#' # 14 rsample   0.0.3 rsample_0.0.3.tar.gz 2018-11-20   254K   7  0  1  0  1  65
#' # 15 rsample   0.0.2 rsample_0.0.2.tar.gz 2017-11-12   339K  24  0  0  0  0  59
#' # 16 rsample   0.0.1 rsample_0.0.1.tar.gz 2017-07-08   180K  35 35  0  0  0  35
#'
#' # Examine differences between versions
#' pkg_diff("rsample", "1.1.1", "1.2.0")
#' # # A difference object: rsample package
#' # - Comparing: v1.1.1/v1.2.0
#' # - Breaking Changes: TRUE
#' # - Added Functions:
#' #   - analysis.default()
#' #   - analysis.initial_validation_split()
#' #   - analysis.rsplit()
#' #   - analysis.val_split()
#' #   - assessment.default()
#' #   - assessment.initial_validation_split()
#' #   - assessment.rsplit()
#' #   - assessment.val_split()
#' #   - dim.initial_validation_split()
#' #   - group_initial_validation_split()
#' #   - initial_validation_split()
#' #   - initial_validation_time_split()
#' #   - int_bca.bootstraps()
#' #   - int_pctl.bootstraps()
#' #   - int_t.bootstraps()
#' #   - print.initial_validation_split()
#' #   - testing.default()
#' #   - testing.initial_validation_split()
#' #   - testing.rsplit()
#' #   - testing.val_split()
#' #   - training.default()
#' #   - training.initial_validation_split()
#' #   - training.rsplit()
#' #   - training.val_split()
#' #   - validation()
#' #   - validation.default()
#' #   - validation.initial_validation_split()
#' #   - validation.val_split()
#' #   - validation_set()
#' #   - vec_cast.data.frame.validation_set()
#' #   - vec_cast.tbl_df.validation_set()
#' #   - vec_cast.validation_set.data.frame()
#' #   - vec_cast.validation_set.tbl_df()
#' #   - vec_cast.validation_set.validation_set()
#' #   - vec_ptype_abbr.group_initial_validation_split()
#' #   - vec_ptype_abbr.initial_validation_split()
#' #   - vec_ptype_abbr.validation_set()
#' #   - vec_ptype2.data.frame.validation_set()
#' #   - vec_ptype2.tbl_df.validation_set()
#' #   - vec_ptype2.validation_set.data.frame()
#' #   - vec_ptype2.validation_set.tbl_df()
#' #   - vec_ptype2.validation_set.validation_set()
#' #   - vec_restore.validation_set()
#' # - Added Parameters:
#' #   - int_pctl(): ...
#' #   - int_t(): ...
#' #   - testing(): ...
#' #   - tidy.nested_cv(): unique_ind
#' #   - tidy.rset(): unique_ind
#' #   - training(): ...
#' # - Removed Functions:
#' #   - gather()
#' # - Removed Parameters:
#' #   - int_bca(): statistics, alpha, .fn
#' #   - int_pctl(): statistics, alpha
#' #   - int_t(): statistics, alpha
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
#' @param verbose Whether to print all added and removed functions
#' and function parameters.  Default is TRUE.  If FALSE, the function
#' will print removed functions and parameters, but only print
#' counts of added functions and parameters.
#' @family pdiff
#' @import crayon
#' @export
print.pdiff <- function(x, ..., verbose = TRUE) {


    grey60 <- crayon::make_style(grey60 = "#999999")
    cat(grey60(paste0("# A difference object: ",
                 as.character(x$PackageName), " package\n")))

    if (!is.null(x$Version1))
      cat(paste0("- Comparing: ", "v", x$Version1, "/v", x$Version2, "\n"))

    if (!is.null(x$BreakingChanges))
      cat(paste0("- Breaking Changes: ", as.character(x$BreakingChanges), "\n"))


    if (verbose == FALSE) {

      if (!is.null(x$AddedFunctions))
        cat(paste0("- Added Functions: ", length(x$AddedFunctions), "\n"))

      if (!is.null(x$AddedParameters))
        if (length(x$AddedParameters) > 0)
          cat(paste0("- Added Parameters: ", length(x$AddedParameters), "\n"))

    } else {


      if (!is.null(x$AddedFunctions)) {
        if (length(x$AddedFunctions > 0)) {
          cat(paste0("- Added Functions: \n"))

          for (nm in x$AddedFunctions) {
            cat(paste0("  - ",  nm, "()\n"))
          }
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

