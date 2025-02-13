#' @title Identify Differences Between Package Versions
#'
#' @description The \strong{pkgdiff} package contains functions to
#' identify differences between versions of R packages. The \strong{pkgdiff}
#' package was written specifically to detect breaking changes and
#' help with package upgrades.
#'
#' @section Functions:
#' The main functions included in the \strong{pkgdiff} package are
#' as follows:
#' \itemize{
#'   \item {\code{\link{pkg_diff}}: Compares two package versions, and
#'   returns an object that contains information about the differences.
#'   Information returned from this function is relatively high level.}
#'   \item {\code{\link{pkg_stability}}: Compiles information on package
#'   stability over time, and calculates a score based on this information.
#'   This function is useful when making package selection choices.
#' to a vector.}
#' }
#'
#' @keywords internal
#' @aliases pkgdiff-package
#' @name pkgdiff
"_PACKAGE"


# Globals -----------------------------------------------------------------


e <- new.env(parent = emptyenv())
e$LoadDate <- Sys.Date()
e$GithubPath <- "https://github.com/dbosak01/pkgdiffdata/raw/refs/heads/main/data"

e$RStudioPath <- "https://cran.rstudio.com/src/contrib"
e$Mirror <- "https://cran.rstudio.com/"
e$CranArchivePath <- "https://cran.r-project.org/src/contrib/Archive"
e$CranCurrentPath <- "https://cran.r-project.org/src/contrib/"
e$MirrorArchivePath <- "https://cran.rstudio.com/src/contrib/Archive"
e$MirrorCurrentPath <- "https://cran.rstudio.com/src/contrib/"
e$CranPackagePath <- "https://cran.r-project.org/web/packages"
e$AvailablePackages <- NULL
e$SavedPackages <- NULL


.onAttach <- function(...) {


  refresh_package_lists()

}


# Utility -----------------------------------------------------------------


#' @import utils
#' @noRd
refresh_package_lists <- function(force = FALSE) {

  mror <- e$Mirror

  options(repos = mror)

  ret <- FALSE

  ts <- Sys.Date() - e$LoadDate
  if (is.null(e$AvailablePackages) ||
      is.null(e$SavedPackages) || ts > 1 || force == TRUE) {

    e$AvailablePackages <- available_packages()

    e$SavedPackages <- github_packages()

    ret <- TRUE
  }

  return(ret)
}
