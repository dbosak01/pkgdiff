


#' @title A Package Repository
#' @description
#' Combines information related to a package repository. The function
#' retrieves all the packages in a repository for a specified version
#' of R.  Results will list the package name and version.
#' @param pkgs A vector of package names used to subset the repository list.
#' Default is NULL, which means all packages in the repository will be returned.
#' @param ver The R version of the repository. Default is "current",
#' meaning the current version of the repository.  The value "latest"
#' will return the latest version of the CRAN repository.
#' If you have multiple repositories, you may also pass the R version number
#' of the desired repository.
#' @param libpaths A vector of paths specifying the locations of the repositories
#' to query. Default
#' is NULL, meaning the function will use the default R locations on the machine.
#' These default paths are identified by the Base R function \code{.libPaths()}.
#' @returns An object of class "pkgrepo".  The object will contain the
#' location of the repo, and a table of R packages with corresponding
#' version numbers.
#' @family pdiff
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
      mver <- NA

  } else if (ver == "latest") {

    if (is.null(pkgs))
      lst <- e$AvailablePackages[ , c("Package", "Version")]
    else
      lst <- e$AvailablePackages[e$AvailablePackages$Package %in% pkgs,
                                 c("Package", "Version")]

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
