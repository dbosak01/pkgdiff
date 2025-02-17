


# Screen Scraping ---------------------------------------------------------



#' @import rvest
#' @import utils
#' @noRd
get_latest_data <- function(pkgs,
                                 skip_size = FALSE) {



  if (is.null(pkgs)) {
    stop("pkgs parameter cannot be NULL.")
  }

  ret <- NULL

  lst <- e$AvailablePackages[e$AvailablePackages$Package %in% pkgs, ]

  for(pkg in pkgs) {

    lrw <- lst[lst$Package == pkg, ]


    if (nrow(lrw) == 0) {

      message(paste0("Package data for '", pkg, "' not found."))
    } else {

      # Get package version
      ver <- lrw$Version

      # Convert dates
      date <- lrw$Release

      # Get package source
      src <- get_file_name(pkg, ver)

      sz <- NA
      if (skip_size == FALSE) {

        # Get temp file name
        flnm <- tempfile()

        currentpath = e$CranCurrentPath

        # Get path to package
        url <- file.path(currentpath, src)

        # Download from CRAN
        rc <- tryCatch({utils::download.file(url, flnm, quiet = TRUE)},
                       error = function(e){1})

        # Get size

        if (rc == 0) {
          sz <- format(structure(file.size(flnm), class = "object_size"),
                       units = "auto")
          sz <- sub(" Kb", "K", sz, fixed = TRUE)
          sz <- sub(" Mb", "M", sz, fixed = TRUE)
          sz <- sub(" Gb", "G", sz, fixed = TRUE)
          unlink(flnm)
        }
      }

      # print(pkg)
      # print(ver)
      # print(src)
      # print(as.Date(date))
      # print(sz)
      # Create data from from captured info
      rw <- data.frame(Package = pkg, Version = ver, FileName = src,
                       "Release" = as.Date(date, origin = '1970-01-01'),
                       "Size" = sz)

      if (is.null(ret)) {
        ret <- rw
      } else {
        ret <- rbind(ret, rw)
      }

    }
  }

  return(ret)
}


#' @import rvest
#' @import utils
#' @noRd
get_latest_data_back <- function(pkgs,
                            skip_size = FALSE) {

  baseurl = e$CranPackagePath

  if (is.null(pkgs)) {
    stop("pkgs parameter cannot be NULL.")
  }

  ret <- NULL

  for(pkg in pkgs) {

    # Concat url
    url <- file.path(baseurl, pkg, "index.html")

    # Read and parse page into data frame
    page <- tryCatch({rvest::read_html(url)},
                     error = function(e){NULL})
    if (is.null(page)) {

      message(paste0("Package data for '", pkg, "' not found."))
    } else {

      tables <- html_elements(page, "table")
      table1 <- html_table(tables[1], fill = TRUE)[[1]]
      table3 <- html_table(tables[3], fill = TRUE)[[1]]

      # Replace non-breaking space
      table1$X1 <- gsub('\xc2\xa0+', " ", table1$X1)
      table3$X1 <- gsub('\xc2\xa0+', " ", table3$X1)

      # Get package version
      ver <- subset(table1, table1$X1 == "Version:")[[2]]

      # Convert dates
      date <- subset(table1, table1$X1 == "Published:")[[2]]

      # Get package source
      src <- subset(table3, grepl("Package\\s+source:", table3$X1))[[2]]

      if (length(src) == 0)
        src <- get_file_name(pkg, ver)

      sz <- NA
      if (skip_size == FALSE) {

        # Get temp file name
        flnm <- tempfile()

        currentpath = e$CranCurrentPath

        # Get path to package
        url <- file.path(currentpath, src)

        # Download from CRAN
        rc <- tryCatch({utils::download.file(url, flnm, quiet = TRUE)},
                       error = function(e){1})

        # Get size

        if (rc == 0) {
          sz <- format(structure(file.size(flnm), class = "object_size"),
                       units = "auto")
          sz <- sub(" Kb", "K", sz, fixed = TRUE)
          sz <- sub(" Mb", "M", sz, fixed = TRUE)
          sz <- sub(" Gb", "G", sz, fixed = TRUE)
          unlink(flnm)
        }
      }

      # print(pkg)
      # print(ver)
      # print(src)
      # print(as.Date(date))
      # print(sz)
      # Create data from from captured info
      rw <- data.frame(Package = pkg, Version = ver, FileName = src,
                       "Release" = as.Date(date, origin = '1970-01-01'),
                       "Size" = sz)

      if (is.null(ret)) {
        ret <- rw
      } else {
        ret <- rbind(ret, rw)
      }

    }
  }

  return(ret)
}

#' @noRd
get_latest_version <- function(pkgs) {

  #browser()
  ret <- c()

  lst <- e$AvailablePackages[e$AvailablePackages$Package %in% pkgs, ]

  pos <- 1
  nfpkgs <- c()
  for (pkg in pkgs) {

    if (nrow(lst) == 0) {
      dat <- get_latest_data_back(pkg, skip_size = TRUE)[1, "Version"]
    } else {

      dat <- lst[lst$Package == pkg, "Version"]
    }

    if (!is.null(dat)) {
      if (length(dat) > 0) {
        ret[pos] <- dat
      } else {
        nfpkgs[length(nfpkgs) + 1] <- pkg
      }

    } else {
      nfpkgs[length(nfpkgs) + 1] <- pkg
    }

    pos <- pos + 1
  }

  if (!is.null(ret))
    names(ret) <- pkgs[!pkgs %in% nfpkgs]

  return(ret)
}


#' @import rvest
#' @import common
#' @noRd
get_archive_versions <- function(pkgs) {


  # baseurl = "https://cran.r-project.org/src/contrib/Archive"
  baseurl <- e$CranArchivePath

  if (is.null(pkgs)) {
    stop("pkgs parameter cannot be NULL.")
  }

  ret <- NULL

  for (pkg in pkgs) {

    # Concat url
    url <- file.path(baseurl, pkg)

    # Read and parse page into data frame
    page <- tryCatch({rvest::read_html(url)},
                     error = function(e){NULL})

    if (is.null(page)) {
      message(paste0("Archive versions of '", pkg, "' not available."))
    } else {

      tables <- html_elements(page, "table")
      table1 <- html_table(tables[1], fill = TRUE)[[1]]

      # Get column names
      nms <- names(table1)

      # Clear out unnecessary columns
      if (nms[5] == "Description") {
        table1[[5]] <- NULL
      }
      if (nms[1] == "") {
        table1[[1]] <- NULL
      }


      # Clear out unnecessary rows
      tmp <- subset(table1, table1$Name != "Parent Directory" & table1$Name != "")

      # Convert dates
      tmp[["Last modified"]] <- as.Date(tmp[["Last modified"]],
                                        origin = '1970-01-01')

      # Extract package version
      vers <- get_version(tmp$Name)

      # Create final data frame
      tmp <- data.frame(Package = pkg, Version = vers, FileName = tmp$Name,
                        Release = tmp[["Last modified"]],
                        Size = tmp$Size)

      # Sort descending
      tmp <- sort(tmp, by = "Release", ascending = FALSE)

      if (is.null(ret)) {
        ret <- tmp
      } else {
        ret <- rbind(ret, tmp)
      }
    }
  }

  return(ret)
}

get_all_versions <- function(pkg) {

  ldat <- get_latest_data(pkg)
  adat <- get_archive_versions(pkg)


  ret <- ldat

  if (!is.null(adat)) {

    ret <- rbind(ret, adat)
  }

  return(ret)
}

# File Utilities ----------------------------------------------------------



#' @noRd
get_version <- function(pkgname) {

  # Get rid of file extensions
  step1 <- sub(".tar.gz", "", pkgname, fixed = TRUE)

  # Find first underscore
  pos <- regexpr("_", step1, fixed = TRUE)

  ret <- substring(step1, pos + 1)

  return(ret)

}


get_file_name <- function(pkgname, version) {

  ret <- paste0(pkgname, "_", version, ".tar.gz")

  return(ret)
}



# Local Repo Utilities -------------------------------------------------------



#' @noRd
get_current_version <- function(pkgname) {

  dat <- installed_packages()

  sdat <- subset(dat, dat$Package == pkgname, "Version")

  ret <- sdat[["Version"]]

  return(ret)
}

#' @title Get Installed Packages and Versions
#' @description A utility function to return the installed packages
#' in the current repository and their
#' versions numbers.
#' @param pkgs Optional vector of package names.  If supplied,
#' this vector will be used to filter the repository package list.
#' @param repos The path to the desired repository. This parameter
#' is optional.  If not supplied, the function will use the
#' repository associated with the currently running version of R.
#' @returns A data frame containing each installed
#' package and its version number.
#' @examples
#' # Get all packages in repo
#' res <- installed_packages()
#'
#' # View top 10 results
#' res[1:12, ]
#' #        Package Version
#' # 1        abind   1.4-8
#' # 2         ards   0.1.1
#' # 3      askpass   1.2.0
#' # 4   assertthat   0.2.1
#' # 5    backports   1.5.0
#' # 6    base64enc   0.1-3
#' # 7         bigD   0.3.0
#' # 8  BiocManager 1.30.23
#' # 9          bit   4.0.5
#' # 10       bit64   4.0.5
#' @import utils
#' @noRd
installed_packages <- function(pkgs = NULL, repos = NULL) {

  ip <- as.data.frame(utils::installed.packages(repos)[,c(1,3:4)])
  rownames(ip) <- NULL
  ret <- ip[is.na(ip$Priority),1:2,drop=FALSE]

  if (!is.null(pkgs)) {
     ret <- subset(ret, ret$Package %in% pkgs)
  }

  rownames(ret) <- NULL

  return(ret)
}



# Difference Utilities ----------------------------------------------------



#' @noRd
get_removed_functions <- function(v1info, v2info) {

  ret <- c()

  idx <- names(v1info$Functions) %in% names(v2info$Functions)

  if (any(idx == FALSE)) {
    ret <- names(v1info$Functions)[!idx]
  }

  return(ret)
}

#' @noRd
get_added_functions <- function(v1info, v2info) {

  ret <- c()

  idx <- !names(v2info$Functions) %in% names(v1info$Functions)

  if (any(idx == TRUE)) {
    ret <- names(v2info$Functions)[idx]
  }

  return(ret)
}

#' @noRd
get_removed_parameters <- function(v1info, v2info) {

  ret <- list()

  ret <- c()

  args1 <- v1info$Functions
  args2 <- v2info$Functions

  # Get function names
  nms <- names(args1)

  # Filter out deprecated functions
  nms <- nms[nms %in% names(args2)]

  ret <- list()

  for (nm in nms) {

    # Get args for both versions
    f1 <- args1[[nm]]
    f2 <- args2[[nm]]

    # Strip out empty string
    f1 <- f1[nchar(f1) > 0]
    f2 <- f2[nchar(f2) > 0]

    # Look for removed functions
    dp <- f1[!f1 %in% f2]

    # if there are any, add to list
    if (length(dp) > 0) {

      ret[[nm]] <- dp
    }
  }


  return(ret)
}




#' @noRd
get_added_parameters <- function(v1info, v2info) {

  ret <- list()

  ret <- c()

  args1 <- v1info$Functions
  args2 <- v2info$Functions

  # Get function names
  nms <- names(args1)

  # Filter out deprecated functions
  nms <- nms[nms %in% names(args2)]

  ret <- list()

  # Loop through functions
  for (nm in nms) {

    f1 <- args1[[nm]]
    f2 <- args2[[nm]]

    # Compare parameters
    dp <- f2[!f2 %in% f1]

    # Add to return value
    if (length(dp) > 0) {
      ret[[nm]] <- dp
    }
  }


  return(ret)
}


#' @noRd
get_all_functions <- function(v1info, v2info) {

  ret <- c()

  if (!is.null(v2info)) {
    ret <- names(v2info$Functions)
  }
  return(ret)
}

#' @noRd
get_all_parameters <- function(info) {

  ret <- list()

  ret <- c()

  args1 <- info$Functions

  # Get function names
  nms <- names(args1)

  ret <- list()

  for (nm in nms) {

    dp <- args1[[nm]]

    # Remove empty string
    # dp <- dp[nchar(dp) > 0]

    if (length(dp) > 0) {

      ret[[nm]] <- dp
    }
  }


  return(ret)
}


#' @noRd
get_all_parameters_back <- function(info) {

  ret <- list()

  ret <- c()

  args1 <- info$FormalArgs

  # Get function names
  nms <- names(args1)

  # Filter out non-exported names
  nms <- nms[nms %in% info$ExportedFunctions]


  ret <- list()

  for (nm in nms) {

    dp <- args1[[nm]]

    if (length(dp) > 0) {

      ret[[nm]] <- dp
    }
  }


  return(ret)
}


get_parameter_count <- function(info) {


  ap <- get_all_parameters(info)

  ret <- 0
  for (nm in names(ap)) {

    ret <- ret + length(ap[[nm]])

  }

  return(ret)
}




# Retrieving Infos --------------------------------------------------------


# Fix
#' @noRd
get_all_infos <- function(pkg, versions = NULL) {

  ret <- NULL

  # print("Debug A")
  lVersion <- get_latest_version(pkg)

  # print("Debug B")

  if (is.null(versions)) {

    adat <- get_archive_versions(pkg)
    versions <- c(lVersion, adat$Version)
  }

  # print("Debug C")

  for (ver in versions) {

      info <- get_info_cran(pkg, ver)

    if (!is.null(info)) {
      ret[[ver]] <- info
    }
  }

  # print("Debug D")

  return(ret)

}

# @noRd
# get_fastest_info <- function(pkgname, version) {
#
#   ret <- NULL
#
#   if (!is.na(github_packages(pkgname))) {
#
#     info <- github_package(pkgname)
#     ret <- info$infos[[version]]
#
#   }
#
#   if (is.null(ret)) {
#
#     ret <- get_all_infos(pkgname, version)[[1]]
#   }
#
#   return(ret)
# }

# Special case needed for two infos
#' @noRd
get_fastest_infos <- function(pkgname, v1, v2, cache = TRUE) {

  inf1 <- NULL
  inf2 <- NULL
  ret <- list()

  if (!is.na(github_packages(pkgname)) && cache == TRUE) {

    info <- github_package(pkgname)
    if ("infos" %in% names(info)) {
      inf1 <- info$infos[[v1]]
      inf2 <- info$infos[[v2]]
      # if (!is.null(inf1) && !is.null(inf2))
      #   print("Retrived from github")
    }
  }

  if (is.null(inf1)) {

    inf1 <- get_info_cran(pkgname, v1)
  }

  if (is.null(inf2)) {

    inf2 <- get_info_cran(pkgname, v2)
  }

  ret[[v1]] <- inf1
  ret[[v2]] <- inf2

  return(ret)
}




# Github Utilities --------------------------------------------------------

#' @noRd
github_packages <- function(pkg = NULL) {

  pth <- "https://github.com/dbosak01/pkgdiffdata/raw/refs/heads/main/packages.rds"

  murl <- url(pth)
  ret <- get(load(gzcon(murl)))
  close(murl)

  if (!is.null(pkg)) {
    ret <- ret[pkg]
  }

  return(ret)
}

#' @noRd
github_package <- function(pkg) {


  # https://github.com/dbosak01/pkgdiffdata/raw/refs/heads/main/data/procs.Rdata
  # https://github.com/dbosak01/pkgdiffdata/blob/main/data/procs.Rdata
  # pth <- "https://github.com/dbosak01/pkgdiffdata/blob/main/data"
  pth <- e$GithubPath


  fl <- file.path(pth, paste0(pkg, ".RData"))

  info <- tryCatch({
      murl <- url(fl)
      ret <- get(load(gzcon(murl)))
      close(murl)
      ret
    },
      error = function(e){NULL})

  return(info)

}


github_update <- function() {

  pth <- "https://github.com/dbosak01/pkgdiffdata/raw/refs/heads/main/LastUpdate.RData"

  murl <- url(pth)
  ret <- get(load(gzcon(murl)))
  close(murl)


  return(ret)
}


# Global Lists ------------------------------------------------------------

#' @import utils
#' @import tools
#' @noRd
available_packages <- function() {

  mror <- e$Mirror

  # Get available packages
  flds1 <- c("Package", "Repository")
  apkgs <- as.data.frame(available.packages(repos = mror))[ , flds1]

  flds2 <- c("Package", "Version", "Date/Publication", "Maintainer",
             "Description", "Title", "Depends", "LinkingTo", "Suggests",
             "Enhances", "License")

  # Get CRAN packages
  dpkgs <- tools::CRAN_package_db()[ , flds2]

  # Remove duplicates
  dpkgs <- dpkgs[!duplicated(dpkgs$Package, fromLast = TRUE), ]

  # Merge to add repository
  ret <- merge(dpkgs, apkgs, by.x = c("Package"),
               by.y = c("Package"))

  # Format date
  ret$ReleaseDate <- as.Date(ret$`Date/Publication`)

  return(ret)
}


#' @noRd
#' @import cranlogs
is_popular <- function(pkg, count = 350) {

  dld <- sum(cranlogs::cran_downloads(pkg,
                                      when = "last-month")[["count"]])

  if (dld > count)
    ret <- TRUE
  else
    ret <- FALSE

  return(ret)
}


# Other -------------------------------------------------------------------



#' Extract R Package
#' Untar an R package into a temp directory.
#' @param x The compressed (tar.gz) build file of an R package, either local or
#' URL.
#' @return List of files extracted.
#' @import utils
#' @noRd
unzip_package <- function(pth) {
  td <- tempdir()
  if(grepl('^https?:', pth)) {
    url <- pth
    pth <- file.path(td, basename(pth))
    utils::download.file(url, destfile = pth, quiet = TRUE)
  }
  nm <- sub('\\.tar\\.gz', '', basename(pth))
  dir <- file.path(td, nm)
  utils::untar(pth, exdir = dir)
  ret <-  list.files(dir, full.names = TRUE)

  return(ret)
}



