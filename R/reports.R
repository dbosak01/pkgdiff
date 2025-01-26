

#' @title Generate a Report of Upgrade Breakages
#' @param v1pkgs A data frame that identifies the source packages
#' and versions.
#' @param v2pkgs A data frame that identifies the target packages
#' and versions.  By default,
#' @family reports
#' @import common
#' @export
report_breakages <- function(v1pkgs = "current", v2pkgs = "latest") {

  if (!is.data.frame(v1pkgs)) {
    if (v1pkgs == "current") {
      v1pkgs <- get_installed_packages()
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

      v2_pkgs <- data.frame(Package = v1pkgs$Package,
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
    v2 <- subset(v2pkgs, v2pkgs$Package == pkg, "Version")

    bc <- FALSE
    if (v1 != v2) {
      cat(paste0("Comparing ", pkg, " v", v1, "/v", v2, "\n"))
      diff <- suppressWarnings(get_diff(pkg, v1, v2))
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



#' @title Generate a Report of Upgrade Breakages
#' @param pkgs A data frame that identifies the source packages
#' and versions.
#' @family reports
#' @export
report_stability <- function(pkgs) {



}
