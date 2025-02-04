


# Screen Scraping ---------------------------------------------------------



#' @import rvest
#' @import utils
#' @noRd
get_latest_data <- function(pkgs,
                            skip_size = FALSE) {

  baseurl = "https://cran.r-project.org/web/packages"

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
        src <- ""

      sz <- NA
      if (skip_size == FALSE) {

        # Get temp file name
        flnm <- tempfile()

        currentpath = "https://cran.r-project.org/src/contrib/"

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
                       "Release" = as.Date(date), "Size" = sz)

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

  ret <- c()

  pos <- 1
  for (pkg in pkgs) {

    dat <- get_latest_data(pkg, skip_size = TRUE)

    ret[pos] <- dat$Version[1]

    pos <- pos + 1
  }

  names(ret) <- pkgs

  return(ret)
}


#' @import rvest
#' @import common
#' @noRd
get_archive_versions <- function(pkgs) {

  baseurl = "https://cran.r-project.org/src/contrib/Archive"

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
      tmp[["Last modified"]] <- as.Date(tmp[["Last modified"]])

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
#' @export
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

  idx <- v1info$ExportedFunctions %in% v2info$ExportedFunctions

  if (any(idx == FALSE)) {
    ret <- v1info$ExportedFunctions[!idx]
  }

  return(ret)
}

#' @noRd
get_added_functions <- function(v1info, v2info) {

  ret <- c()

  idx <- !v2info$ExportedFunctions %in% v1info$ExportedFunctions

  if (any(idx == TRUE)) {
    ret <- v2info$ExportedFunctions[idx]
  }

  return(ret)
}

#' @noRd
get_removed_parameters <- function(v1info, v2info) {

  ret <- list()

  ret <- c()

  args1 <- v1info$FormalArgs
  args2 <- v2info$FormalArgs

  # Get function names
  nms <- names(args1)

  # Filter out non-exported names
  nms <- nms[nms %in% v1info$ExportedFunctions]

  # Filter out deprecated functions
  nms <- nms[nms %in% v2info$ExportedFunctions]

  ret <- list()

  for (nm in nms) {

    f1 <- args1[[nm]]
    f2 <- args2[[nm]]

    dp <- f1[!f1 %in% f2]

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

  args1 <- v1info$FormalArgs
  args2 <- v2info$FormalArgs

  # Get function names
  nms <- names(args1)

  # Filter out non-exported names
  nms <- nms[nms %in% v1info$ExportedFunctions]

  # Filter out deprecated functions
  nms <- nms[nms %in% v2info$ExportedFunctions]

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
    ret <- v2info$ExportedFunctions
  }
  return(ret)
}



#' @noRd
get_all_parameters <- function(info) {

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

#' @noRd
get_latest_info <- function(pkgname) {


  currentpath = "https://cran.r-project.org/src/contrib/"

  v2data <- get_latest_data(pkgname)
  vLatest <- v2data$Version[[1]]


  # Get path to v2 package
  pth <- file.path(currentpath, get_file_name(pkgname, vLatest))



  ret <- tryCatch({suppressWarnings(pkgInfo(pth))},
                  error = function(e){NULL})



  return(ret)
}


get_archive_info <- function(pkgname, version) {

  archivepath = "https://cran.r-project.org/src/contrib/Archive"

  pth <- file.path(archivepath, pkgname, get_file_name(pkgname, version))


  ret <- tryCatch({suppressWarnings(pkgInfo(pth))},
                  error = function(e){NULL})



  return(ret)

}


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

    if (ver == lVersion)
      info <- get_latest_info(pkg)
    else
      info <- get_archive_info(pkg, ver)

    if (!is.null(info)) {
      ret[[info$Version]] <- info
    }
  }

  # print("Debug D")

  return(ret)

}

#' @noRd
get_fastest_info <- function(pkgname, version) {

  ret <- NULL

  if (!is.na(github_packages(pkgname))) {

    info <- github_package(pkgname)
    ret <- info$infos[[version]]

  }

  if (is.null(ret)) {

    ret <- get_all_infos(pkgname, version)[[1]]
  }

  return(ret)
}

#' @noRd
get_fastest_infos <- function(pkgname, v1, v2) {

  inf1 <- NULL
  inf2 <- NULL
  ret <- list()

  if (!is.na(github_packages(pkgname))) {

    info <- github_package(pkgname)
    if ("infos" %in% names(info)) {
      inf1 <- info$infos[[v1]]
      inf2 <- info$infos[[v2]]
      # if (!is.null(inf1) && !is.null(inf2))
      #   print("Retrived from github")
    }
  }

  if (is.null(inf1)) {

    inf1 <- get_all_infos(pkgname, v1)[[1]]
  }

  if (is.null(inf2)) {

    inf2 <- get_all_infos(pkgname, v2)[[1]]
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
  pth <- "https://github.com/dbosak01/pkgdiffdata/raw/refs/heads/main/data"


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


github_versions <- function(pkg) {



}

