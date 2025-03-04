

# Package Repo ------------------------------------------------------------



#' @title Retrieve Package Versions from a Repository
#' @description
#' Combines information related to a package repository. The function
#' retrieves all the packages in a repository for a specified version
#' of R.  Results will list the package name and version.
#' @param pkgs A vector of package names used to subset the repository list.
#' Default is NULL, which means all packages in the repository will be returned.
#' @param ver The R version of the repository. Pass the R version
#' as a quoted string. Default is "current",
#' meaning the current version of the repository. The value "latest"
#' will return the latest versions on CRAN.
#' @param libpaths A vector of paths specifying the locations of the repositories
#' to query. Default
#' is NULL, meaning the function will use the default R locations on the machine.
#' These default paths are identified by the Base R function \code{.libPaths()}.
#' @returns An object of class "prepo".  The object will contain a data
#' table of R packages with corresponding version numbers.  This table
#' may then be passed to \code{\link{repo_breakages}} or
#' \code{\link{repo_stability}}.
#' @examples
#' # Create vector of packages
#' pkgs <- c("common", "dplyr", "rvest", "stringr")
#'
#' # Retrieve latest versions
#' pkg_repo(pkgs, ver = "latest")
#' # # A package repo object
#' # - Repo Version: latest
#' # - Packages:
#' #   Package Version
#' # 1  common   1.1.3
#' # 2   dplyr   1.1.4
#' # 3   rvest   1.0.4
#' # 4 stringr   1.5.1
#'
#' @family prepo
#' @import common
#' @export
pkg_repo <- function(pkgs = NULL, ver = "current", libpaths = NULL) {

  mver <- NULL

  refresh_package_lists()

  if (ver == "current") {
    lst <- installed_packages(pkgs, repos = libpaths)

    if (is.null(libpaths))
      mver <- get_rversion()
    else
      mver <- "current"

  } else if (ver == "latest") {

    if (is.null(pkgs))
      lst <- e$AvailablePackages[ , c("Package", "Version")]
    else
      lst <- e$AvailablePackages[e$AvailablePackages$Package %in% pkgs,
                                 c("Package", "Version")]

    mver <- ver

  } else {

    if (is.null(libpaths)) {

      libpaths <- .libPaths()
    }

    if (!is.null(ver)) {

      mver <- ver

      # Default repo
      bp <- get_base_paths(libpaths)

      vpths <- c()

      sver <- split_version(ver)

      idx <- 1
      for (pbp in bp) {
        tmp <- common::dir.find(pbp, sver$lver, up = 0, down = 2)
        if (is.null(tmp))
          tmp <- common::dir.find(pbp, sver$ver, up = 0, down = 2)
        else {

          if (length(tmp) > 1) {
            stop("Multiple releases found for R version ", ver, ".\n",
                 "Please use a three-level version specification:\n",
                 paste(tmp, collapse = "\n"))
          }

          if (basename(tmp) != "library")
            tmp <- file.path(tmp, "library")

        }

        if (!is.null(tmp)) {
          vpths[idx] <- tmp
          idx <- idx + 1
        }
      }

      libpaths <- vpths

      lst <- installed_packages(pkgs, repos = vpths)


    } else {

      lst <- installed_packages(pkgs, repos = libpaths)

    }

  }

  if (nrow(lst) < length(pkgs)) {

    mpkgs <- pkgs[!pkgs %in% lst$Package]
    mdf <- data.frame(Package = mpkgs, Version = NA)
    lst <- rbind(lst, mdf)
  }

  ret <- lst
  class(ret) <- c("prepo", class(ret))
  rownames(ret) <- NULL

  if (!is.null(mver))
    attr(ret, "Version") <- mver
  if (!is.null(libpaths))
    attr(ret, "LibPaths") <- libpaths

  return(ret)
}


#' @title Print a Package Repo Object
#' @description
#' Print routine for a package repo object of class "prepo".
#' @param x The package repo to print.
#' @param ... Follow-on parameters to the print function.
#' @family prepo
#' @import crayon
#' @examples
#' # Create vector of packages
#' pkgs <- c("tidymodels", "rsample", "parsnip", "recipes", "workflows")
#'
#' # Retrieve latest versions
#' res <- pkg_repo(pkgs, ver = "latest")
#'
#' # Print results
#' print(res)
#' # # A package repo object
#' # - Repo Version: latest
#' # - Packages:
#' #      Package Version
#' # 1    parsnip   1.3.0
#' # 2    recipes   1.1.1
#' # 3    rsample   1.2.1
#' # 4 tidymodels   1.3.0
#' # 5  workflows   1.2.0
#' @export
print.prepo <- function(x, ...) {

  grey60 <- crayon::make_style(grey60 = "#999999")
  cat(grey60(paste0("# A package repo object\n")))


  ver <- attr(x, "Version")

  if (!is.null(ver)) {
    cat(paste0("- Repo Version: ", ver , "\n"))
  }

  pths <- attr(x, "LibPaths")

  if (!is.null(pths)) {
    cat(paste0("- Lib Paths:\n"))
    cat(paste0("  ", pths, "\n"))
  }

  if (!is.null(x)) {
    cat(paste0("- Packages:\n"))
    print(as.data.frame(x))
  }

  invisible(x)
}


# Utilities ---------------------------------------------------------------



get_base_paths <- function(pths) {

  ver <- get_rversion()

  lst <- strsplit(pths, "/", fixed = TRUE)

  ret <- c()
  idx <- 1
  for (li in lst) {
    gpos <- grepl(ver, li, fixed = TRUE)
    tmp <- ""
    for (pt in seq_len(length(li))) {
      if (gpos[pt] == FALSE) {
        tmp <- paste0(tmp, li[pt], sep = "/")
      } else
        break
    }
    ret[length(ret) + 1] <- tmp
  }

  return(ret)
}

get_rversion <- function() {

  mj <- R.version$major
  pos <- as.numeric(regexpr(".", R.version$minor, fixed = TRUE))
  mn <- substring(R.version$minor, 1, pos - 1)
  ver <- paste0(mj, ".", mn)

  return(ver)
}

split_version <- function(rversion) {

  tpos <- gregexpr(".", rversion, fixed = TRUE)

  ret <- list()
  if (length(tpos[[1]]) == 1) {
    ret$lver <- paste0("R-", rversion, ".*")
    ret$ver <- rversion
  } else if (length(tpos[[1]]) > 1) {
    ret$lver <- paste0("R-", rversion)
    ret$ver <- substring(rversion, 1, tpos[[1]][2] - 1)
  } else if (length(tpos[[1]]) == 0) {
    ret$lver <- paste0("R-", rversion)
    ret$ver <- rversion
  }


  return(ret)
}
