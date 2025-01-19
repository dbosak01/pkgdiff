



library(packageDiff)

# Compare two versions - Not many differences
p1 <- pkgInfo("c:/packages/logr_1.3.7.tar.gz")
p2 <- pkgInfo("c:/packages/logr_1.3.8.tar.gz")

#' @import packageDiff
#' @export
get_diff <- function(pkgname, v1 = "current",
                              v2 = "latest") {
  if (v1 == "current") {

    cdat <- get_current_version(pkgname)

    v1 <- cdat$Version[1]
  }

  if (v2 == "latest") {

    ldat <- get_latest_version(pkgname)

    v2 <- ldat$Version[1]
  }

  d <- structure(list(...), class = c("pdiff", "list"))




  d$BreakingChanges <- FALSE
  d$DeprecatedFunctions <- NULL
  d$DeprecatedParameters <- NULL
  d$ChangedFunctions <- NULL

}



print.pdiff <- function() {



}

view_diff <- function(pkgname, v1 = "current",
                      v2 = "latest") {

  if (v1 == "current") {

    cdat <- get_current_version(pkgname)

    v1 <- cdat$Version[1]
  }

  if (v2 == "latest") {

    ldat <- get_latest_version(pkgname)

    v2 <- ldat$Version[1]
  }


}

