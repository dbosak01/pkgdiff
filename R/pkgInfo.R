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
pkgInfo <- function(pkg, leaveRemains = FALSE) {
  package <- unzipPackage(pkg)
  bp <- basename(package)
  dn <- dirname(package)
  if(!leaveRemains) on.exit(unlink(dn, recursive = TRUE))
  ## description
  pd <- utils::packageDescription(bp, dn, c('Version', 'Imports', 'Suggests', 'Collate'))
  vn <- pd$Version
  imp <- gsub('\n', ' ', pd$Imports)
  # imported packages should be loaded
  if(!is.na(imp)) {
    toload <- sub(' .*', '', strsplit(imp, ",[ ]?")[[1]])
    srchpth <- basename(searchpaths())
    toload <- setdiff(toload, srchpth)
    for(i in seq_along(toload)) {
      suppressMessages(didload <- require(toload[i], character.only = TRUE))
      if(!didload) {
        warning(sprintf('imported package failed to load: %s', toload[i]))
      }
    }
    tounload <- setdiff(basename(searchpaths()), srchpth)
    on.exit({
      for(i in seq_along(tounload)) {
        detach(paste0('package:', tounload[i]), character.only = TRUE)
      }
    }, add = TRUE)
  }
  sug <- gsub('\n', ' ', pd$Suggests)
  coll <- pd$Collate
  ## data
  # dat <- utils::data(package = bp, lib.loc = dn)
  # dsn <- unname(dat[['results']][,'Item'])
  # if(length(dsn)) {
  #   e <- new.env()
  #   # a vector will only have `ncol`
  #   ddf <- data.frame(data = dsn, nrow = NA, ncol = NA)
  #   for(i in seq_along(dsn)) {
  #     do.call(utils::data, list(dsn[i], package = bp, lib.loc = dn, envir = e))
  #     dimval <- dim(e[[dsn[i]]])
  #     if(is.null(dimval)) {
  #       ddf[i,3] <- length(e[[dsn[i]]])
  #     } else if(length(dimval) == 2) {
  #       ddf[i,2:3] <- dimval
  #     } else {
  #       ddf[i,2:3] <- c(dimval[1], paste(dimval[-1], collapse = 'x'))
  #     }
  #   }
  #   rm(e)
  # } else {
  #   ddf <- data.frame(data = NA, nrow = NA, ncol = NA)[FALSE,]
  # }
  ## all functions
  code_files <- tools::list_files_with_type(file.path(package, 'R'), "code", full.names = TRUE)
  if(!is.na(coll)) {
    coll_order <- strsplit(coll, "[[:space:]]")[[1]]
    coll_order <- file.path(package, 'R', gsub("'", '', coll_order))
    code_files <- c(coll_order, setdiff(code_files, coll_order))
  }
  # what witchcraft is this?
  e <- sourcerer(code_files)
  le <- ls(envir = e)
  obj <- vapply(le, function(z) is.function(e[[z]]), logical(1))
  fun <- names(obj[obj])
  var <- names(obj[!obj])
  #   arg <- lapply(fun, function(z) methods::formalArgs(e[[z]]))
  arg <- lapply(fun, function(z) names(formals(e[[z]])))
  names(arg) <- fun
  # read namespace file
  nsf <- parseNamespaceFile(bp, dn)
  ## exports
  # can't easily distinguish function/class/method
  # if exportClassPatterns is set, could easily over/under-reach
  exp_list <- nsf[grep('export', names(nsf))]
  patterns <- grep('Patterns', names(exp_list))
  pat_list <- unname(unlist(exp_list[patterns]))
  exp <- unname(unlist(exp_list[-patterns]))
  if(nrow(nsf$S3methods)) {
    s3_fun <- paste(nsf$S3methods[,1], nsf$S3methods[,2], sep = '.')
    exp <- c(exp, s3_fun)
  }
  if(length(pat_list)) {
    pat_fun <- fun[unlist(lapply(pat_list, grep, fun))]
    exp <- c(exp, pat_fun)
  }
  exp <- sort(unique(exp))
  ## imports
  imp_list <- nsf[grep('import', names(nsf))]
  all_imp <- lapply(imp_list, function(i) vapply(i, paste, character(1), collapse = '::'))
  imp_fun <- unique(unname(unlist(all_imp)))
  imp_fun <- imp_fun[order(grepl(':', imp_fun), imp_fun)]
  ## documentation
  # doc_files <- tools::list_files_with_type(file.path(package, 'man'), "docs", full.names = TRUE)
  # docmac <- tools::loadPkgRdMacros(package)
  # doctxt <- lapply(doc_files, function(d) {
  #   rd <- tools::parse_Rd(d, macros = docmac)
  #   paste(paste(utils::capture.output(tools::Rd2txt(rd, options = list(underline_titles = FALSE))), collapse = '\n'), '\n')
  # })
  # names(doctxt) <- sub('.Rd', '', basename(doc_files))
  ## src
  # src_patt <- "[.](c|cc|cpp|f|f90|f95|m|mm|h)$"
  # src_files <- list.files(file.path(package, 'src'), pattern = src_patt, full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  # srctxt <- scanner(src_files) %!% NA
  # ## vignettes
  # vign_files <- lfwtr(file.path(package, 'vignettes'), "vignette")
  # vigntxt <- scanner(vign_files) %!% NA
  # ## tests
  # test_files <- lfwtr(file.path(package, 'tests'), "code")
  # testtxt <- scanner(test_files) %!% NA
  # ## demo
  # demo_files <- lfwtr(file.path(package, 'demo'), "demo")
  # demotxt <- scanner(demo_files) %!% NA
  # potential directories to check
  # exec, exclude binary
  # inst, exclude binary
  x <- list(
    Package = bp,
    Version = vn,
    Imports = imp,
    Suggests = sug,
    ImportedFunctions = imp_fun,
    ExportedFunctions = exp,
    AllFunctions = fun,
    FormalArgs = arg #,
    # Data = ddf,
    # documentation = doctxt,
    # source = srctxt,
    # vignettes = vigntxt,
    # tests = testtxt,
    # demo = demotxt
  )
  class(x) <- 'pkgInfo'
  x
}
