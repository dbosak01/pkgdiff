#' Message Suppressor
#'
#' Suppress messages and warnings.
#'
#' @keywords internal
#' @param cmd R command to run.
suppresser <- function(cmd) {
  suppressMessages(suppressWarnings(cmd))
}

#' Source Many R Scripts
#'
#' Evaluate R scripts in protected environment.
#'
#' This may fail if Collate is incorrect.
#'
#' @keywords internal
#' @param files Vector of file names.
#' @param envir Default environment.
sourcerer <- function(files, envir = NULL) {
  if(is.null(envir)) {
    e <- new.env()
  } else {
    e <- envir
  }
  badfiles <- character(0)
  for(i in seq_along(files)) {
    f <- files[i]
    if(file.exists(f)) {
      badlist <- character(0)
      repeat {
        # r is NULL if no error occurs or error could not be prevented
        # r equals 1 if error was prevented
        r <- tryCatch(suppresser(sys.source(f, e, keep.source = FALSE)), error = function(er) {
          if(grepl("could not find function", er)) {
            noFun <- sub('.*could not find function "(.*)".*', '\\1', er)
            e[[noFun]] <- function(...) NULL
            badlist[length(badlist)+1] <<- noFun
          } else if(grepl("object .* not found", er)) {
            noObj <- sub(".*object '(.*)' not found.*", '\\1', er)
            e[[noObj]] <- function(...) NULL
            badlist[length(badlist)+1] <<- noObj
          } else {
            badfiles[length(badfiles)+1] <<- f
            return(NULL)
          }
          1
        })
        if(is.null(r)) break
      }
      # remove from environment
      if(length(badlist)) {
        rm(list = badlist, envir = e)
      }
    }
  }
  fails <- logical(length(badfiles))
  for(i in seq_along(badfiles)) {
    pd <- parse(badfiles[i])
    for(j in seq_along(pd)) {
      tryCatch(eval(pd[j], envir = e), error = function(er) {
        fails[i] <<- TRUE
      })
    }
  }
  badfiles <- basename(badfiles[fails])
  if(length(badfiles)) {
    warning(sprintf('some files partially failed to parse: %s', paste(badfiles, collapse = ',')))
  }
  e
}

#' Scan List of Text Files
#'
#' Store text file as character string
#'
#' @keywords internal
#' @param files the names of files to read.
#' @param full.names Whether to use full file names.
scanner <- function(files, full.names = FALSE) {
  d <- lapply(files, function(t) {
    paste(scan(t, '', sep = '\n', quiet = TRUE), collapse = "\n")
  })
  if(!full.names) {
    files <- basename(files)
  }
  names(d) <- tools::file_path_sans_ext(files)
  d
}

#' Set or Replace
#'
#' Set or replace empty
#'
#' @keywords internal
#' @name setOrReplace
#' @param lhs thing to check
#' @param rhs thing to replace
`%!%` <- function(lhs, rhs) {
  if(length(lhs) == 0L) {
    rhs
  } else {
    lhs
  }
}

#' Recursively List Files With Type
#'
#' List files with type with recursive support
#'
#' @keywords internal
#' @param dir a character string with the path name to a directory.
#' @param \dots Additional arguments, passed to \code{\link{list_files_with_type}}.
lfwtr <- function(dir, ...) {
  lf1 <- list.files(dir, full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
  lf2 <- list.files(dir, full.names = TRUE, recursive = TRUE)
  dir_list <- c(dir, setdiff(lf1, lf2))
  unlist(lapply(dir_list, tools::list_files_with_type, ...))
}
