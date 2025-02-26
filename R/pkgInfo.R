

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
# myp <- parse("c:/packages/pkgdiff/R/utilities.R", keep.source = TRUE)
#
# methods::getFunction("github_package", where = myp)
#
# myl <- as.list(myp)
#
# myfunc <- myl[[22]]
#
# is.call(myfunc)
#
# myfunc[[3]]
#
# deparse(myfunc[[3]])
#
# cranlogs::cran_top_downloads(count = 100)
# cranlogs::cran_downloads()
# cranlogs::cran_downloads(c("accrual", "accessr", "fmtr", "procs"))


# "Package"    "Version"    "Depends"    "LinkingTo"
# "Suggests"   "Enhances"   "License"    "Repository"


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
#' @param x The package info to print.
#' @param ... Follow-on parameters to the print function.
#' @param verbose Whether to print all function names or not.
#' Default is FALSE.  When FALSE, only a count of the functions
#' will be printed.
#' @family pdiff
#' @import crayon
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

    if (!is.null(x$LastMonthDownloads))
      cat(paste0("- Downloads/Month: ", as.character(x$LastMonthDownloads), "\n"))

    if (!is.null(x$Suggests))
      cat(paste0("- Repository: ", as.character(x$Repository), "\n"))

    if (!is.null(x$Cached))
        cat(paste0("- Cached: ", as.character(x$Cached), "\n"))

    if (!is.null(x$Archived))
      cat(paste0("- Archived: ", as.character(x$Archived), "\n"))

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



#' @title Returns Information on the Package Cache
#' @description
#' The \code{pkg_cache} function queries the package cache, and
#' returns information on which packages are included in the cache.
#' The function also returns the last version of the package cached.
#' @details
#' The \strong{pkgdiff} cache is used to speed up \strong{pkgdiff} functions.
#' Information about each package is pre-processed and stored in the cache
#' on Github.  The functions then retrieve this pre-processed information
#' instead of pulling packages down from CRAN.  To learn more about the cache,
#' refer to \code{vignette('Package Cache')}.
#' @param pkgs A vector of package names to retrieve cache information about.
#' Default is NULL, which means to return all packages in the cache.
#' @returns An data frame showing the package name and latest package
#' version stored in the cache.  If the package is not stored in the cache,
#' the package version will be NA.
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
#' @param x The package cache to print.
#' @param ... Follow-on parameters to the print function.
#' @family pdiff
#' @import crayon
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
#' used in several other functions in the \strong{pkgdiff} system.  This
#' class also makes a compact storage format for pre-processed package
#' information.
#' @param pkg The package name.
#' @param ver The package version. Special values are "current" and
#' "latest".  The value "current" is the current version of the package
#' running on the machine.  The value "latest" is the latest version
#' of the package from CRAN.
#' @param cache Whether to retrieve the info from the github cache, or
#' from CRAN.  If TRUE, the function will first search the cache, and
#' return the info if available.  If the info is not available in the
#' github cache, or the cache parameter is set to FALSE, the info will be
#' retrieved from CRAN.
#' @family pdiff
#' @export
pkg_info <- function(pkg, ver = "current", cache = TRUE) {

  archived <- NULL
  tst <- github_packages(pkg)

  if (is.null(ver)) {
    ver <- get_current_version(pkg)
  } else if (ver == "current") {

    ver <- get_current_version(pkg)

  } else if (ver == "latest") {

    ver <- get_latest_version(pkg)
  }

  if (length(ver) == 0)
    ver <- get_latest_version(pkg)

  if (ver == "archived") {

    tmp <- get_archive_versions(pkg)

    if (nrow(tmp) > 0) {

      ver <- tmp[1, "Version"]
      archived <- TRUE

    }

  }

  if (!is.null(tst) && !is.na(tst) && cache == TRUE) {
    ret <- get_info_github(pkg, ver)

    if (is.null(ret)) {

      ret <- get_info_cran(pkg, ver)
    }

  } else {

      ret <- get_info_cran(pkg, ver)

  }

  if (is.null(archived)) {
    dm  <- cranlogs::cran_downloads(pkg, when = "last-month")
    ret$LastMonthDownloads <- sum(dm$count)

    cs <- github_packages(pkg)

    if (!is.na(cs))
      ret$Cached <- TRUE
    else
      ret$Cached <- FALSE

  } else {

    ret$Archived <- archived
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
        pat <- any(grepl(exppat, nm))
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




# Legacy ------------------------------------------------------------------



#' Extract Package Information
#'
#' This function extracts information from an R package.
#'
#' Generate package information from its build file.
#'
#' @param pkg The compressed (tar.gz) build file of an R package.
#' @param leaveRemains Keep decompressed package in temp directory.
#'
#' @return
#' \item{Package}{Package name}
#' \item{Version}{Version number}
#' \item{Imports}{Imported packages}
#' \item{Suggests}{Suggested packages}
#' \item{ImportedFunctions}{Functions imported from other packages}
#' \item{ExportedFunctions}{Functions exported from package}
#' \item{AllFunctions}{All defined functions}
#' \item{FormalArgs}{Function arguments}
# \item{Data}{Dimension information on data sets}
# \item{documentation}{Full package documentation}
# \item{source}{Files in src directory}
# \item{vignettes}{Files in vignettes directory}
# \item{tests}{Files in tests directory}
# \item{demo}{Files in demo directory}
#'
#' @noRd
# pkgInfo <- function(pkg, leaveRemains = FALSE) {
#   package <- unzipPackage(pkg)
#   bp <- basename(package)
#   dn <- dirname(package)
#   if(!leaveRemains) on.exit(unlink(dn, recursive = TRUE))
#   ## description
#   pd <- utils::packageDescription(bp, dn, c('Version', 'Imports', 'Suggests', 'Collate'))
#   vn <- pd$Version
#   imp <- gsub('\n', ' ', pd$Imports)
#   # imported packages should be loaded
#   if(!is.na(imp)) {
#     toload <- sub(' .*', '', strsplit(imp, ",[ ]?")[[1]])
#     srchpth <- basename(searchpaths())
#     toload <- setdiff(toload, srchpth)
#     for(i in seq_along(toload)) {
#       suppressMessages(didload <- require(toload[i], character.only = TRUE))
#       if(!didload) {
#         warning(sprintf('imported package failed to load: %s', toload[i]))
#       }
#     }
#     tounload <- setdiff(basename(searchpaths()), srchpth)
#     on.exit({
#       for(i in seq_along(tounload)) {
#         detach(paste0('package:', tounload[i]), character.only = TRUE)
#       }
#     }, add = TRUE)
#   }
#   sug <- gsub('\n', ' ', pd$Suggests)
#   coll <- pd$Collate
#   ## data
#   # dat <- utils::data(package = bp, lib.loc = dn)
#   # dsn <- unname(dat[['results']][,'Item'])
#   # if(length(dsn)) {
#   #   e <- new.env()
#   #   # a vector will only have `ncol`
#   #   ddf <- data.frame(data = dsn, nrow = NA, ncol = NA)
#   #   for(i in seq_along(dsn)) {
#   #     do.call(utils::data, list(dsn[i], package = bp, lib.loc = dn, envir = e))
#   #     dimval <- dim(e[[dsn[i]]])
#   #     if(is.null(dimval)) {
#   #       ddf[i,3] <- length(e[[dsn[i]]])
#   #     } else if(length(dimval) == 2) {
#   #       ddf[i,2:3] <- dimval
#   #     } else {
#   #       ddf[i,2:3] <- c(dimval[1], paste(dimval[-1], collapse = 'x'))
#   #     }
#   #   }
#   #   rm(e)
#   # } else {
#   #   ddf <- data.frame(data = NA, nrow = NA, ncol = NA)[FALSE,]
#   # }
#   ## all functions
#   code_files <- tools::list_files_with_type(file.path(package, 'R'), "code", full.names = TRUE)
#   if(!is.na(coll)) {
#     coll_order <- strsplit(coll, "[[:space:]]")[[1]]
#     coll_order <- file.path(package, 'R', gsub("'", '', coll_order))
#     code_files <- c(coll_order, setdiff(code_files, coll_order))
#   }
#   # what witchcraft is this?
#   e <- sourcerer(code_files)
#   le <- ls(envir = e)
#   obj <- vapply(le, function(z) is.function(e[[z]]), logical(1))
#   fun <- names(obj[obj])
#   var <- names(obj[!obj])
#   #   arg <- lapply(fun, function(z) methods::formalArgs(e[[z]]))
#   arg <- lapply(fun, function(z) names(formals(e[[z]])))
#   names(arg) <- fun
#   # read namespace file
#   nsf <- parseNamespaceFile(bp, dn)
#   ## exports
#   # can't easily distinguish function/class/method
#   # if exportClassPatterns is set, could easily over/under-reach
#   exp_list <- nsf[grep('export', names(nsf))]
#   patterns <- grep('Patterns', names(exp_list))
#   pat_list <- unname(unlist(exp_list[patterns]))
#   exp <- unname(unlist(exp_list[-patterns]))
#   if(nrow(nsf$S3methods)) {
#     s3_fun <- paste(nsf$S3methods[,1], nsf$S3methods[,2], sep = '.')
#     exp <- c(exp, s3_fun)
#   }
#   if(length(pat_list)) {
#     pat_fun <- fun[unlist(lapply(pat_list, grep, fun))]
#     exp <- c(exp, pat_fun)
#   }
#   exp <- sort(unique(exp))
#   ## imports
#   imp_list <- nsf[grep('import', names(nsf))]
#   all_imp <- lapply(imp_list, function(i) vapply(i, paste, character(1), collapse = '::'))
#   imp_fun <- unique(unname(unlist(all_imp)))
#   imp_fun <- imp_fun[order(grepl(':', imp_fun), imp_fun)]
#   ## documentation
#   # doc_files <- tools::list_files_with_type(file.path(package, 'man'), "docs", full.names = TRUE)
#   # docmac <- tools::loadPkgRdMacros(package)
#   # doctxt <- lapply(doc_files, function(d) {
#   #   rd <- tools::parse_Rd(d, macros = docmac)
#   #   paste(paste(utils::capture.output(tools::Rd2txt(rd, options = list(underline_titles = FALSE))), collapse = '\n'), '\n')
#   # })
#   # names(doctxt) <- sub('.Rd', '', basename(doc_files))
#   ## src
#   # src_patt <- "[.](c|cc|cpp|f|f90|f95|m|mm|h)$"
#   # src_files <- list.files(file.path(package, 'src'), pattern = src_patt, full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
#   # srctxt <- scanner(src_files) %!% NA
#   # ## vignettes
#   # vign_files <- lfwtr(file.path(package, 'vignettes'), "vignette")
#   # vigntxt <- scanner(vign_files) %!% NA
#   # ## tests
#   # test_files <- lfwtr(file.path(package, 'tests'), "code")
#   # testtxt <- scanner(test_files) %!% NA
#   # ## demo
#   # demo_files <- lfwtr(file.path(package, 'demo'), "demo")
#   # demotxt <- scanner(demo_files) %!% NA
#   # potential directories to check
#   # exec, exclude binary
#   # inst, exclude binary
#   x <- list(
#     Package = bp,
#     Version = vn,
#     Imports = imp,
#     Suggests = sug,
#     ImportedFunctions = imp_fun,
#     ExportedFunctions = exp,
#     AllFunctions = fun,
#     FormalArgs = arg #,
#     # Data = ddf,
#     # documentation = doctxt,
#     # source = srctxt,
#     # vignettes = vigntxt,
#     # tests = testtxt,
#     # demo = demotxt
#   )
#   class(x) <- 'pkgInfo'
#   x
# }



