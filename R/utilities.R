
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

      # Get package name
      src <- subset(table3, table3$X1 == "Package source:")[[2]]

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

  for (pkg in pkgs) {

    dat <- get_latest_data(pkg, skip_size = TRUE)

    ret[[pkg]] <- dat$Version[1]

  }

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
      stop(paste0("Cannot open url: ", url))
    }

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

  return(ret)
}



#' @noRd
get_version <- function(pkgname) {

  # Get rid of file extensions
  step1 <- sub(".tar.gz", "", pkgname, fixed = TRUE)

  # Find first underscore
  pos <- regexpr("_", step1, fixed = TRUE)

  ret <- substring(step1, pos + 1)

  return(ret)

}


#' @noRd
get_current_version <- function(pkgname) {

  dat <- get_installed_packages()

  sdat <- subset(dat, dat$Package == pkgname, "Version")

  ret <- sdat[["Version"]]

  return(ret)
}

get_file_name <- function(pkgname, version) {

  ret <- paste0(pkgname, "_", version, ".tar.gz")

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
#' res <- get_installed_packages()
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
get_installed_packages <- function(pkgs = NULL, repos = NULL) {

  ip <- as.data.frame(utils::installed.packages(repos)[,c(1,3:4)])
  rownames(ip) <- NULL
  ret <- ip[is.na(ip$Priority),1:2,drop=FALSE]

  if (!is.null(pkgs)) {
     ret <- subset(ret, ret$Package %in% pkgs)
  }

  rownames(ret) <- NULL

  return(ret)
}

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

  for (nm in nms) {

    f1 <- args1[[nm]]
    f2 <- args2[[nm]]

    dp <- f2[!f2 %in% f1]

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



