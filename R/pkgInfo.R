

# Experiments -------------------------------------------------------------

# compareVersion("1.0", "1.0-1")

# getNamespaceExports("fmtr")
# pdb <- tools::CRAN_package_db()

# names(pdb)
#
# getOption("repos")
#
# pdb[1, ]
#
# utils::findCRANmirror("web")
#
# utils::available.packages()
#
# get("R_CRAN_WEB")
#
# packageVersion("fmtr")
# packageDate("fmtr")
# packageDescription("fmtr")
#
# contrib.url("fmtr")
# .packages(all.available = TRUE)
#
# installed.packages()
#
# cranlogs::cran_top_downloads(count = 100)
# cranlogs::cran_downloads()
# cranlogs::cran_downloads(c("accrual", "accessr", "fmtr", "procs"))

# res1 <- as.data.frame(installed.packages())
#
# res1[res1$Package == "tools", ]
#
# res2 <- as.data.frame(available.packages())
#
# res2[res2$Package %in% c("tools", "utils"), ]


# Package Info ------------------------------------------------------------


#' @title A Package Information Class
#' @description
#' Combines information related to a package. Objects of this class are
#' used in several other functions in the \strong{pkgdiff} system.  This
#' class also makes a compact storage format for pre-processed package
#' information.
#' @param pkg The package name.
#' @param ver The package version.
#' @param release The package release date.
#' @param title The package title.
#' @param description The package description.
#' @param maintainer The package maintainer.
#' @param depends A set of package dependencies.
#' @param linkingto The packages this package links to.
#' @param suggests The packages this package suggests.
#' @param enhances The package this package enhances.
#' @param license The package license.
#' @param repository The package repository.
#' @param functions The functions associated with this package.
#' @returns An object of class "pinfo".
#' @family pdiff
#' @noRd
pinfo <- function(pkg, ver = "current", release = NULL, title = NULL, description = NULL,
                     maintainer = NULL,
                     depends = NULL, linkingto = NULL, imports = NULL,
                     suggests = NULL, enhances = NULL, license = NULL,
                     repository = NULL, functions = NULL) {

  inf <- structure(list(), class = c("pinfo", "list"))

  inf$PackageName <- pkg
  inf$Version <- ver
  inf$Title <- title
  inf$Description <- description
  inf$Maintainer <- maintainer
  inf$Depends <- depends
  inf$LinkingTo <- linkingto
  inf$Imports <- imports
  inf$Suggests <- suggests
  inf$Enhances <- enhances
  inf$License <- license
  inf$Repository <- repository
  inf$ReleaseDate <- release
  inf$Functions <- functions

  return(inf)
}



#' @title Print a Package Info Object
#' @description
#' Print routine for a package info object of class "pinfo".
#' @param x The package info to print.
#' @param ... Follow-on parameters to the print function.
#' @param verbose Whether to print all function names or not.
#' Default is FALSE.  When FALSE, only a count of the functions
#' will be printed.
#' @family pdiff
#' @import crayon
#' @returns The package info object, invisibly.
#' @examples
#' # Capture package info
#' res <- pkg_info("patchwork")
#'
#' # View package info
#' print(res)
#' # # A package info object: patchwork package
#' # - Version: v1.3.0
#' # - Release Date: 2024-09-16
#' # - Title: The Composer of Plots
#' # - Maintainer: Thomas Lin Pedersen <thomasp85@gmail.com>
#' #   - License: MIT + file LICENSE
#' # - Description: The 'ggplot2' package provides a strong API for sequentially
#' # building up a plot, but does not concern itself with composition of multiple
#' # plots. 'patchwork' is a package that expands the API to allow for
#' # arbitrarily complex composition of plots by, among others, providing
#' # mathematical operators for combining multiple plots. Other packages that try
#' # to address this need (but with a different approach) are 'gridExtra' and
#' # 'cowplot'.
#' # - Imports: ggplot2 (>= 3.0.0), gtable, grid, stats, grDevices, utils,
#' # graphics, rlang (>= 1.0.0), cli, farver
#' # - Suggests: knitr, rmarkdown, gridGraphics, gridExtra, ragg,
#' # testthat (>= 2.1.0), vdiffr, covr, png, gt (>= 0.11.0)
#' # - Downloads/Month: 239555
#' # - Repository: CRAN
#' # - Cached: TRUE
#' # - Functions: 90
#' @export
print.pinfo <- function(x, ..., verbose = FALSE) {




    # pkg, version, release, title = NULL, description = NULL,
    # maintainer = NULL,
    # depends = NULL, linkingto = NULL,
    # suggests = NULL, enhances = NULL, license = NULL,
    # repository = NULL, functions = NULL

    grey60 <- crayon::make_style(grey60 = "#999999")
    cat(grey60(paste0("# A package info object: ",
                      as.character(x$PackageName), " package\n")))

    if (!is.null(x$Version))
      cat(paste0("- Version: ", "v", x$Version, "\n"))

    if (!is.null(x$Release))
      cat(paste0("- Release Date: ", as.character(x$Release), "\n"))

    if (!is.null(x$Title))
      cat(paste0("- Title: ", as.character(x$Title), "\n"))

    if (!is.null(x$Maintainer))
      cat(paste0("- Maintainer: ", as.character(x$Maintainer), "\n"))

    if (!is.null(x$License))
      cat(paste0("- License: ", as.character(x$License), "\n"))

    if (!is.null(x$Description))
      cat(paste0("- Description: ", as.character(x$Description), "\n"))

    if (!is.null(x$Depends))
      cat(paste0("- Depends: ", as.character(x$Depends), "\n"))

    if (!is.null(x$LinkingTo))
      cat(paste0("- Linking To: ", as.character(x$LinkingTo), "\n"))

    if (!is.null(x$Suggests))
      cat(paste0("- Imports: ", as.character(x$Imports), "\n"))

    if (!is.null(x$Suggests))
      cat(paste0("- Suggests: ", as.character(x$Suggests), "\n"))

    if (!is.null(x$Enhances))
      cat(paste0("- Enhances: ", as.character(x$Enhances), "\n"))

    if (!is.null(x$LastMonthDownloads)) {

      if (x$LastMonthDownloads > 0)
        cat(paste0("- Downloads/Month: ", as.character(x$LastMonthDownloads), "\n"))

    }

    if (!is.null(x$Suggests))
      cat(paste0("- Repository: ", as.character(x$Repository), "\n"))

    if (!is.null(x$Cached))
        cat(paste0("- Cached: ", as.character(x$Cached), "\n"))

    if (!is.null(x$Archived))
      cat(paste0("- Archived: ", as.character(x$Archived), "\n"))

    if (!is.null(x$BasePackage)) {
      if (x$BasePackage == TRUE)
        cat(paste0("- Base Package: TRUE\n"))
    }

    if (verbose == FALSE) {

      if (!is.null(x$Functions))
        cat(paste0("- Functions: ", length(x$Functions), "\n"))

    } else {

      if (!is.null(x$Functions)) {
        cat(paste0("- Functions: \n"))

        for (nm in names(x$Functions)) {
          prms <- paste0(x$Functions[[nm]], collapse = ", ")
          if (nchar(prms) == 0)
            cat(paste0("  - ", nm, "()\n"))
          else
            cat(paste0("  - ", nm, "(): ", prms, "\n"))

        }
      }
    }


  invisible(x)
}


# Package Cache --------------------------------------------------------



#' @title Queries the Package Cache
#' @description
#' The \code{pkg_cache} function queries the package cache, and
#' returns information on which packages are included in the cache.
#' The function also returns the last version of the package cached.
#' @details
#' The \strong{pkgdiff} cache is used to speed up \strong{pkgdiff} functions.
#' Information about each package is pre-processed and stored in the cache
#' on Github.  The functions then retrieve this pre-processed information
#' instead of pulling packages down from CRAN.  To learn more about the cache,
#' refer to \code{vignette('pkgdiff-cache')}.
#' @param pkgs A vector of package names to retrieve cache information about.
#' Default is NULL, which means to return all packages in the cache.
#' @returns An data frame showing the package name and latest package
#' version stored in the cache.  If the package is not stored in the cache,
#' the package version will be NA.  The "Last Update" time stamp is the
#' last day and time the cache was updated.
#' @examples
#' # View single package
#' pkg_cache("dplyr")
#' # # A package cache object
#' # - Last Update: 2025-02-25 14:58 UTC
#' # - Packages:
#' #   Package Version
#' # 1   dplyr   1.1.4
#'
#' # View multiple packages
#' pkgs <- c("dplyr", "tidyr", "stringr")
#' pkg_cache(pkgs)
#' # # A package cache object
#' # - Last Update: 2025-02-25 14:58 UTC
#' # - Packages:
#' #   Package Version
#' # 1   dplyr   1.1.4
#' # 2   tidyr   1.3.1
#' # 3 stringr   1.5.1
#'
#' # Get all packages
#' res <- pkg_cache()
#'
#' # View first 10
#' res[1:10, ]
#' # A package cache object
#' # - Last Update: 2025-02-25 14:58 UTC
#' # - Packages:
#' #        Package Version
#' # 1           A3   1.0.0
#' # 2   abbreviate     0.1
#' # 3     abc.data     1.1
#' # 4          abc   2.2.2
#' # 5  ABCanalysis   1.2.1
#' # 6          abd   0.2-8
#' # 7        abind   1.4-8
#' # 8      acepack   1.6.1
#' # 9       actuar   3.3-5
#' # 10         ada   2.0-5
#' @family pdiff
#' @export
pkg_cache <- function(pkgs = NULL) {

  ov <- github_packages(pkgs)

  nms <- names(ov)
  names(ov) <- NULL

  if (is.null(pkgs)) {
    ret <- data.frame(Package = nms, Version = ov)
  } else {
    ret <- data.frame(Package = pkgs, Version = ov)
  }

  attr(ret, "LastUpdated") <- github_update()

  class(ret) <- c("pcache", "data.frame", "list")

  return(ret)
}



#' @title Print a Package Cache Object
#' @description
#' Print routine for a package cache object of class "pcache".
#' @param x The package cache to print.
#' @param ... Follow-on parameters to the print function.
#' @family pdiff
#' @import crayon
#' @returns The package cache object, invisibly.
#' @examples
#' # Create vector of packages
#' pkgs <- c("ggplot2", "patchwork", "gt")
#'
#' # Capture cache versions
#' res <- pkg_cache(pkgs)
#'
#' # Print cache versions
#' print(res)
#' # # A package cache object
#' # - Last Update: 2025-03-01 20:51 UTC
#' # - Packages:
#' #     Package Version
#' # 1   ggplot2   3.5.1
#' # 2 patchwork   1.3.0
#' # 3        gt  0.11.1
#' @export
print.pcache <- function(x, ...) {

  grey60 <- crayon::make_style(grey60 = "#999999")
  cat(grey60(paste0("# A package cache object\n")))


  lu <- attr(x, "LastUpdate")

  if (!is.null(lu)) {
    tmstmp <- format(lu, format = "%Y-%m-%d %H:%M UTC")
    cat(paste0("- Last Update: ", tmstmp , "\n"))

  }

  if (!is.null(x)) {
    cat(paste0("- Packages:\n"))
    print(as.data.frame(x))
  }

  invisible(x)
}


# Get Info ----------------------------------------------------------------



#' @title Get Information for a Package
#' @description
#' Combines information related to a package. Objects of this class are
#' used by several other functions in the \strong{pkgdiff} system.  This
#' class also makes a compact storage format for pre-processed package
#' information.
#' @details
#' Package information is unique for each version of a package.  Since
#' a package can have multiple versions, you must select which version
#' you wish to return information for.  By default, the version
#' returned is the current version of the package on the current machine.
#' You may also specify a previous version number from the CRAN archive.
#' Another option is to specify the latest version on CRAN.  See the
#' "ver" parameter for additional details on how to select these
#' different package versions.
#'
#' The package info object contains a list of functions and function
#' parameters associated with the specified version of the package.
#' You may access this list using the \code{$Functions} item name.
#'
#' Most other information contained in the info object is retrieved
#' from the package description file. One exception is the downloads
#' per month.  This information is retrieved from CRAN logs.
#'
#' In addition, the package cache status is appended to the info object.
#' The package cache status indicates whether the package info
#' has been stored in the \strong{pkgdiff} Github cache.  Packages that
#' have been stored in the cache enjoy faster results from
#' \strong{pkgdiff} functions.
#'
#' Note that \code{pkg_info} and other \strong{pkgdiff} functions
#' only work with contributed CRAN packages.  They do not
#' work with Base R packages.
#' @param pkg The package name as a quoted string. This parameter is
#' required.
#' @param ver The version of the package to retrieve information for.
#' Pass the version as a quoted string.
#' Special values are "current" and
#' "latest".  The value "current" is the current version of the package
#' running on the machine.  The value "latest" is the latest version
#' of the package from CRAN.
#' @param cache Whether to retrieve the info from the Github cache, or
#' from CRAN.  If TRUE, the function will first search the cache, and
#' return the info if available.  If the info is not available in the
#' Github cache, or the cache parameter is set to FALSE, the info will be
#' retrieved from CRAN.
#' @returns A package information object of class "pinfo".  This object contains
#' a set of general information about the package, such as the version,
#' release date, maintainer, title, etc.  Most of this information comes
#' from the package description file.  In addition, the info object also
#' contains a list of functions in the package and their parameters.
#'
#' If the package has been archived on CRAN, info will be returned,
#' but the "Archived" flag will be set to TRUE. If the
#' package is not found on CRAN, the function will emit a message and return
#' NULL.
#'
#' If the package is a Base R package, a reduced number of fields will be returned,
#' and the package will be marked with \code{Base Package: TRUE}.  Function
#' lists are not available for Base packages.
#' @examples
#' # View package info
#' pkg_info("glue")
#' # A package info object: glue package
#' # - Version: v1.7.0
#' # - Release Date: 2024-01-09
#' # - Title: Interpreted String Literals
#' # - Maintainer: Jennifer Bryan <jenny@posit.co>
#' # - License: MIT + file LICENSE
#' # - Description: An implementation of interpreted string literals, inspired by
#' # Python's Literal String Interpolation
#' # <https://www.python.org/dev/peps/pep-0498/> and Docstrings
#' # <https://www.python.org/dev/peps/pep-0257/> and Julia's Triple-Quoted
#' # String Literals
#' # <https://docs.julialang.org/en/v1.3/manual/strings/#Triple-Quoted-String-Literals-1>.
#' #   - Depends: R (>= 3.6)
#' # - Imports: methods
#' # - Suggests: crayon, DBI (>= 1.2.0), dplyr, knitr, magrittr, rlang,
#' # rmarkdown, RSQLite, testthat (>= 3.2.0), vctrs (>= 0.3.0),
#' # waldo (>= 0.3.0), withr
#' # - Downloads/Month: 1463244
#' # - Repository: CRAN
#' # - Cached: TRUE
#' # - Functions: 24
#'
#' # Get info object
#' res <- pkg_info("glue")
#'
#' # Extract package version
#' res$Version
#' # [1] "1.7.0"
#'
#' # Extract maintainer
#' res$Maintainer
#' # [1] "Jennifer Bryan <jenny@posit.co>"
#'
#' # Extract function list
#' res$Functions
#' # $`[.glue`
#' # [1] "x"   "i"   "..."
#' #
#' # $`[[.glue`
#' # [1] "x"   "i"   "..."
#' #
#' # $`+.glue`
#' # [1] "e1" "e2"
#' #
#' # $as.character.glue
#' # [1] "x"   "..."
#' #
#' # $as_glue
#' # [1] "x"   "..."
#' #
#' # $as_glue.character
#' # [1] "x"   "..."
#' #
#' # $as_glue.default
#' # [1] "x"   "..."
#' #
#' # $as_glue.glue
#' # [1] "x"   "..."
#' #
#' # $backtick
#' # [1] "x"
#' #
#' # $double_quote
#' # [1] "x"
#' #
#' # $glue
#' # [1] "..."          ".sep"         ".envir"
#' # [4] ".open"        ".close"       ".na"
#' # [7] ".null"        ".comment"     ".literal"
#' # [10] ".transformer" ".trim"
#' # ...
#' @family pdiff
#' @export
pkg_info <- function(pkg, ver = "current", cache = TRUE) {

  # browser()

  lv <- get_latest_version(pkg, msg = FALSE)
  archived <- FALSE
  if (!is.null(lv)) {
    if (lv == "archived")
      archived <- TRUE
  }

  based <- is_base(pkg)

  tst <- github_packages(pkg)

  if (is.null(ver)) {

    ver <- get_current_version(pkg)

  } else if (ver == "current") {

    ver <- get_current_version(pkg)

  } else if (ver == "latest") {

    ver <- get_latest_version(pkg)
  }

  if (length(ver) == 0) {
    ver <- lv
  } else if (is.na(ver)) {
    ver <- lv
  }

  if (!is.null(ver) && based == FALSE) {
    tmp <- get_archive_versions(pkg)
    if (archived) {

      if (nrow(tmp) > 0) {

        ver <- tmp[1, "Version"]
        archived <- TRUE

      }
    }

    # No archive versions
    if (is.null(tmp)) {
      if (ver != lv) {
        ver <- lv
      }
    } else if (!ver %in% tmp$Version) {  # Current version doesn't match anything

      ver <- lv
    }
  }

  if (is.null(ver)) {

    message(paste0("No version info available for '", pkg, "'."))
    ret <- NULL

  } else {

    if (!is.null(tst) && !is.na(tst) && cache == TRUE) {
      ret <- get_info_github(pkg, ver)

      if (is.null(ret)) {

        message(paste0("Version '", ver, "' of '", pkg, "' not found.\n",
                       "Getting latest version."))
        ver <- get_latest_version(pkg)

        ret <- get_info_cran(pkg, ver)
      }
    } else if (based == TRUE) {

      ret <- get_info_description(pkg)

      if (archived)
        ret$Archived <- TRUE

      ret$Cached <- FALSE

    } else {

        ret <- get_info_cran(pkg, ver)
    }

    if (archived == FALSE) {
      dm  <- cranlogs::cran_downloads(pkg, when = "last-month")
      ret$LastMonthDownloads <- sum(dm$count)

      cs <- github_packages(pkg)

      if (!is.na(cs))
        ret$Cached <- TRUE
      else
        ret$Cached <- FALSE

    } else {

      ret$Archived <- as.logical(archived)
    }

    if (based) {

      ret$BasePackage <- TRUE
    } else {
      ret$BasePackge <- FALSE
    }

  }

  return(ret)
}

#' @noRd
get_info_github <- function(pkg, ver) {

  gpkg <- github_package(pkg)

  ret <- NULL

  if (ver %in% names(gpkg$infos)) {
    ret <- gpkg$infos[[ver]]
  }

  return(ret)
}

#' @import utils
#' @import tools
#' @noRd
get_info_cran <- function(pkg, ver) {

  # Construct path
  if (get_latest_version(pkg) == ver ) {

    pth <- file.path(e$MirrorCurrentPath, get_file_name(pkg, ver))

  } else {

    pth <- file.path(e$MirrorArchivePath, pkg, get_file_name(pkg, ver))
  }

  # Unzip package
  package <- unzip_package(pth)

  # Deconstruct path
  bp <- basename(package)
  dn <- dirname(package)

  # Automatic clean up
  on.exit(unlink(dn, recursive = TRUE))

  # Get description
  pd <- utils::packageDescription(bp, dn)

  if (!is.null(pd$Depends))
    pd$Depends <- gsub('\n', ' ', pd$Depends)

  if (!is.null(pd$LinkingTo))
    pd$LinkingTo <- gsub('\n', ' ', pd$LinkingTo)

  if (!is.null(pd$Imports))
    pd$Imports <- gsub('\n', ' ', pd$Imports)

  if (!is.null(pd$Suggests))
    pd$Suggests <- gsub('\n', ' ', pd$Suggests)

  if (!is.null(pd$Enhances))
    pd$Enhances <- gsub('\n', ' ', pd$Enhances)

  # Create package info
  pi <- pinfo(pkg, ver,
              release = as.Date(pd$`Date/Publication`),
              title = pd$Title, description = pd$Description,
              maintainer = pd$Maintainer,
              depends = pd$Depends,
              linkingto = pd$LinkingTo,
              imports = pd$Imports,
              suggests = pd$Suggests,
              enhances = pd$Enhances,
              license = pd$License,
              repository = pd$Repository)

  ## All functions
  code_files <- common::file.find(file.path(package, 'R'), "*.R", up = 0,
                                  down = 2)
  # Collated functions
  coll <- pd$Collate
  if(!is.null(coll)) {
    coll_order <- strsplit(coll, "[[:space:]]")[[1]]
    coll_order <- file.path(package, 'R', gsub("'", '', coll_order))
    code_files <- c(coll_order, setdiff(code_files, coll_order))
  }

  # Read namespace file
  nsf <- tryCatch({parseNamespaceFile(bp, dn)},
                  error = function(er) {
                    message(paste0("Package ", pkg, " version ", ver,
                                   " has no namespace file."))
                   NULL
                  })

  if (is.null(nsf)) {
     nsf <- list()
     nsf$exportPatterns <- "."
  }

  exp <- nsf$exports

  ## Exports
  # exp_list <- nsf[grep('export', names(nsf))]
  # patterns <- grep('Patterns', names(exp_list))
  # pat_list <- unname(unlist(exp_list[patterns]))
  # exp <- unname(unlist(exp_list[-patterns]))
  if(!is.null(nsf$S3methods)) {
    if(nrow(nsf$S3methods)) {
      s3_fun <- paste(nsf$S3methods[,1], nsf$S3methods[,2], sep = '.')
      exp <- c(exp, s3_fun)
    }
  }
  # if(length(pat_list)) {
  #   pat_fun <- fun[unlist(lapply(pat_list, grep, fun))]
  #   exp <- c(exp, pat_fun)
  # }
  # exp <- sort(unique(exp))

  mthds <- NULL
  if (length(nsf$exportMethods) > 0) {
      exp <- c(exp, nsf$exportMethods)
      mthds <- nsf$exportMethods
  }

  exp <- sort(unique(exp))
  # browser()

  # Prepare function list
  funcs <- list()
  for (nm in exp) {
    funcs[[nm]] <- ""  # Just export everything
  }

  exppat <- NULL
  if (length(nsf$exportPatterns) > 0)
    exppat <- nsf$exportPatterns

  # Extract function parameters
  for (fl in code_files) {
    # print(fl)
    tmp <- get_functions(fl, exp, exppat, mthds)
    for (nm in names(tmp)) {
      funcs[[nm]] <- tmp[[nm]]
    }
  }


  pi$Functions <- funcs


  # Need to deal with S4 methods
  # Somehow get method parameters
  # nsf$exportClasses
  # nsf$exportMethods

  return(pi)
}


#' @noRd
get_info_description <- function(pkg) {

  ret <- NULL

  ds <- packageDescription(pkg)

  if (length(ds) > 1) {

    blt <- strsplit(ds$Built, ";", fixed = TRUE)[[1]][3]

    ret <- pinfo(pkg, ds$Version,
                release = as.Date(blt),
                title = ds$Title,
                description = ds$Description,
                maintainer = ds$Maintainer,
                imports = ds$Imports,
                license = ds$License)

    ret$BasePackage <- is_base(pkg)
  }

  return(ret)
}

# Export Patterns:
# ^[^\\.]  (starts with a dot)
# ^[[:alpha:]]+ (starts with an alphabetic character)
# . (everything except newline)

#' @noRd
get_functions <- function(filepath, funcs, exppat, mthds = NULL) {
  # print("Debug 1")
  code <- tryCatch({parse(filepath)},
                   error = function(er) {
                     NULL
                   })
  segs <- as.list(code)
  ret <- list()
  # print("Debug 2")
  for (ii in seq_along(segs)) {
    part <- segs[[ii]]
    if (!is.null(part)) {
      strf <- deparse1(part)
      # print("Debug 3")
      mflag <- FALSE
      if (grepl('setMethod', strf, fixed = TRUE)) {
        # Get method name
        myreg <- regexec('setMethod\\(\\s*"([^"]+)"', strf)

        rm <- regmatches(strf, myreg)

        if (length(rm[[1]]) > 1) {

          nm <- rm[[1]][2]
        } else {
          nm <- "-not found-"
        }
        nm <- sub("\"", "", nm)
        mflag <- TRUE
      } else {
        # Get function name
        tokens <- strsplit(strf, " ", fixed = TRUE)[[1]]
        nm <- gsub("`", "", tokens[[1]], fixed = TRUE)
      }
      # print("Debug 4")
      if (!is.null(exppat))
        pat <- any(suppressWarnings(grepl(exppat, nm)))
      else
        pat <- FALSE

      # print("Debug 5")
      if (nm %in% funcs || pat) {
        ne <- new.env()
        # Not working on methods
        # Functions are fine
        tryCatch({suppressWarnings(eval(part, ne))},
                 error = function(er){ne[[nm]] <- NULL})
        # print("Debug 6")
        if (is.function(ne[[nm]])) {
          prms <- tryCatch({names(formals(ne[[nm]]))},
                           error = function(er) {""})

          ret[[nm]] <- prms
          # print("Debug 7")
        }
      }
    }
  }
  return(ret)
}



# Package Version ---------------------------------------------------------

#' @title Retrieves All Versions of a Package
#' @description
#' The \code{pkg_versions} function queries the CRAN archive,
#' and returns a data frame of package version information for
#' all releases of the package. This function may be used to understand
#' what versions of a package are available. It is often used in
#' conjunction with \code{\link{pkg_info}} to get a general overview
#' of a package.
#' @param pkg A package name to return versions for.  Pass the package
#' name as a quoted string.  This parameter is required.
#' @returns A data frame with one row per package release, showing the
#' version and date of each release. If the package is not found on CRAN,
#' a message will be generated and the function will return NULL.
#' @examples
#' # View package versions
#' pkg_versions("rsample")
#' #    Package Version             FileName    Release   Size
#' # 1  rsample   1.2.1 rsample_1.2.1.tar.gz 2024-03-25 320.9K
#' # 2  rsample   1.2.0 rsample_1.2.0.tar.gz 2023-08-23   321K
#' # 3  rsample   1.1.1 rsample_1.1.1.tar.gz 2022-12-07   318K
#' # 4  rsample   1.1.0 rsample_1.1.0.tar.gz 2022-08-08   306K
#' # 5  rsample   1.0.0 rsample_1.0.0.tar.gz 2022-06-24   268K
#' # 6  rsample   0.1.1 rsample_0.1.1.tar.gz 2021-11-08   274K
#' # 7  rsample   0.1.0 rsample_0.1.0.tar.gz 2021-05-08   274K
#' # 8  rsample   0.0.9 rsample_0.0.9.tar.gz 2021-02-17   269K
#' # 9  rsample   0.0.8 rsample_0.0.8.tar.gz 2020-09-23   261K
#' # 10 rsample   0.0.7 rsample_0.0.7.tar.gz 2020-06-04   248K
#' # 11 rsample   0.0.6 rsample_0.0.6.tar.gz 2020-03-31   299K
#' # 12 rsample   0.0.5 rsample_0.0.5.tar.gz 2019-07-13   297K
#' # 13 rsample   0.0.4 rsample_0.0.4.tar.gz 2019-01-07   254K
#' # 14 rsample   0.0.3 rsample_0.0.3.tar.gz 2018-11-20   254K
#' # 15 rsample   0.0.2 rsample_0.0.2.tar.gz 2017-11-12   339K
#' # 16 rsample   0.0.1 rsample_0.0.1.tar.gz 2017-07-08   180K
#' @family pdiff
#' @export
pkg_versions <- function(pkg) {

  ret <- get_all_versions(pkg)

  return(ret)
}





