


#' @title Generate a Package Summary Report
#' @description
#' The \code{pkg_summary} function creates a package summary report.
#' The report aggregates some key information about the package
#' and a generates a chart showing breaking changes over time.
#' The function provides a nice snapshot that can help you get a feel for a package.
#' @details
#' The \code{pkg_summary} function generates an HTML
#' report that can be sent to the RStudio viewer.  It combines some information
#' from both the \code{pkg_info} and \code{pkg_stability} functions, and displays
#' it in a table and accompanying chart.
#'
#' The table shows a selection of fields
#' that can give you an overall sense of the package: its size, age, number
#' of releases, dependencies, etc.  The chart is a stacked bar chart
#' showing the number of breaking changes for each release.
#'
#' Here is an example of the package summary report:
#' \figure{summary1.png}
#' The chart shows each release issued by the package maintainer
#' over the last ~8.5 years. Releases prior to that are not displayed,
#' as they are not included in the stability score. The bars represent the
#' number of removed functions and removed parameters. The labels on each
#' bar show the release year, month and version.
#'
#' @seealso For additional information, see \code{\link{pkg_info}} and
#' \code{\link{pkg_stability}}.
#' @param pkg A quoted string containing the name of the package to report on.
#' This parameter is required.
#' @param releases An integer indicating the number of releases to collect
#' stability data for. For example, \code{releases = 10} will return stability
#' data for the last 10 releases of the package. Default is NULL, which means
#' the function will return data for all releases.
#' @param months An integer indicating the number of months back to collect
#' stability data for.  For example, \code{months = 24} will collect
#' stability data for the previous 2 years.  Default is NULL, meaning there is
#' no limitation on the number of release months, and the function will collect
#' data from all releases.
#' @param view Whether to display the report in the viewer.  By default,
#' the parameter is TRUE.
#' @param path A path and file name for the report.  If NULL, the function
#' will create a temporary file. Default is NULL.
#' @returns The path to the HTML report, invisibly.  If the \code{view} parameter is TRUE,
#' the function will send the report to the viewer.
#' @examples
#' # View summary report for "stringr" package
#' # - Set view = TRUE to see example
#' pkg_summary("stringr", view = FALSE)
#'
#' @import common
#' @import graphics
#' @export
pkg_summary <- function(pkg, releases = NULL, months = NULL, view = TRUE, path = NULL) {

  # Add parameters for months and releases
  # Automatic limit on 10 years.


  # Get temp directory
  td <- tempdir()

  # Construct temp path
  fpth <- file.path(td, paste0(pkg,  "_stability.html"))

  # Kill if exists
  if (file.exists(fpth)) {
    file.remove(fpth)
  }

  # Create image directory
  idir <- file.path(td, "images")
  if (!dir.exists(idir)) {
    dir.create(idir)
  }

  # Create image path
  ipth <-  file.path(idir, paste0(pkg,  "_stability.svg"))

  # Remove any existing files
  if (file.exists(ipth)) {
    file.remove(ipth)
  }

  # Get html page
  pth <- gen_html(pkg, fpth, ipth, releases, months)


  # Show in viewer
  if (view) {

    show_viewer(pth)

  }

  # Copy to user-requested path
  if (!is.null(path)) {

    # Create user file directory if needed
    udir <- dirname(path)
    if (!dir.exists(udir)) {
      dir.create(udir)
    }

    # Create user images directory if needed
    uidir <- file.path(udir, "images")
    if (!dir.exists(uidir)) {
      dir.create(uidir)
    }

    # Prepare target paths
    ufpth <- file.path(udir, basename(path))
    uipth <- file.path(uidir, basename(ipth))

    # Copy files from temp to user targets
    file.copy(fpth, ufpth)
    file.copy(ipth, uipth)

    pth <- path
  }


  invisible(pth)
}

#' @import grDevices
#' @noRd
gen_chart <- function(pkg, dat, fpth, sbttl) {

  # Extract stability data
  if (!is.null(dat) && nrow(dat) > 0) {

    # Filter for last ~8.5 years
    fltr <- (Sys.Date() - 3080) < as.Date(dat$Release)
    dat <- dat[fltr, ]

    # Get y scale
    bh <- dat$RF + dat$RP
    mx <- max(bh)
    ylm <- c(0, 10)
    if (mx > 10) {
      ylm <- c(0, ceiling(mx / 5) * 5)

    }

    # Create label columns
    dat$Label <- paste0(substring(dat$Release, 3, 7), " v", dat$Version)

    # Sort ascending
    # dat <- sort_by(dat, dat$Release)
    dat <- sort_data_frame(dat, by = "Release")

    # Reorder Releases chronologically (oldest to newest)
    # dat$Release <- factor(dat$Release, levels = dat$Release)

    # Matrix for barplot
    mat <- t(as.matrix(dat[, c("RF", "RP")]))

    # Capture current par settings
    oldmar <- par()$mar

    # Assign output path
    svg(fpth, height = 4, width = 12, pointsize = 12, family = "sans")

    # Set margins
    par(mar = c(5, 4, 4, 4))   # bottom, left, top, right

    # Create title and subtitle
    ttls <- "Breaking Changes by Release"
    if (sbttl != "") {
      ttls <- append(ttls, sbttl)
    }

    # Base R stacked barplot
    barplot(
      mat,
      beside = FALSE,
      col = c("steelblue", "tomato"),
      names.arg = dat$Label,
      las = 2,                        # vertical labels
      cex.names = 0.7,
     # cey.axis = 1.5,
      main = ttls,
      ylab = "Count",
      ylim = ylm
    )

    legend("topright",
           legend = c("Removed Functions", "Removed Parameters"),
           fill = c("steelblue", "tomato"),
           cex = 0.9)


    # Close device context
    dev.off()

    # Restore par
    par(mar = oldmar)

    # Get svg xml
    lns <- readLines(fpth, encoding = "UTF-8")

    # Remove the first line
    # XML definition is not needed and will mess up the HTML
    if (length(lns) > 2) {
      # ret <- paste('<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"',
      #           'width="100%" height="100%" viewBox="0 0 100 100" >')

      lns <- lns[seq(2, length(lns))]
    } else {
      lns <- lns
    }

    # Kill temp file
    if (file.exists(fpth)) {

      file.remove(fpth)
    }

    # Write modified lines
    writeLines(lns, con = fpth)

    ret <- fpth
  } else {

    ret <- ""
  }

  return(ret)

}


#' @noRd
gen_html <- function(pkg, fpth, ipth, rlses, mths) {


  # Get info data
  info <- pkg_info(pkg)

  if (!is.null(info)) {

    # Get stability data
    stb <- pkg_stability(pkg, releases = rlses, months = mths)

    # Modify image path for img tag
    ipth_mod <- file.path("./images", basename(ipth))

    # Construct subtitle
    sttl <- ""
    if (!is.null(rlses)) {
      sttl <- paste0("Releases = ", rlses)
    }
    if (!is.null(mths)) {
      if (sttl != "") {
        sttl <- paste0(sttl, " | ", "Months = ", mths)
      } else {
        sttl <- paste0("Months = ", mths)
      }
    }

    # Get chart path
    cpth <- gen_chart(pkg, stb$StabilityData, ipth, sttl)

    # Construct HTML
    lns <- c()

    lns[length(lns) + 1] <- "<!DOCTYPE html>"
    lns[length(lns) + 1] <- "<html>"
    lns[length(lns) + 1] <- get_styles()
    lns[length(lns) + 1] <- "<body>"
    lns[length(lns) + 1] <- paste0("<h2>", "Package Summary Report", "</h2>")
    lns[length(lns) + 1] <- get_table(info, stb)
    if (cpth != "") {
      lns[length(lns) + 1] <- paste0("<img src=\"", ipth_mod, "\"",
                                     " width=\"100%\" alt=\"Stability Chart\">")
    }
    lns[length(lns) + 1] <- "</body>"
    lns[length(lns) + 1] <- "</html>"


    # Write html file
    writeLines(lns, fpth)

  } else {


  }

  return(fpth)
}

# Construct style sheet
get_styles <- function() {

  ret <- '<style>
  body {font-family:Arial;
        font-size:11pt}
  </style>'

  return(ret)
}


get_table <- function(info, stab) {

  tblo <- '<table width="100%">'

  # Prepare row1
  scr <- sprintf("%.1f", stab$StabilityScore * 100)
  r1 <- paste0('<tr><td><b>Package Name:</b> ', info$PackageName, '</td>
         <td><b>Current Version:</b> ', info$Version, '</td>
         <td><b>Stability:</b> ', paste0(scr, " (", stab$Assessment, ")"), '</td></tr>')

  # Prepare row2
  r2 <- paste0('<tr><td><b>Maintainer:</b> ', info$Maintainer, '</td>
         <td colspan="2"><b>Title:</b> ', info$Title, '</td></tr>')

  # Prepare row3
  r3 <- paste0('<tr><td><b>Age:</b> ', stab$PackageAge, '</td>
         <td><b>First Release:</b> ', stab$FirstRelease, '</td>
         <td><b>Last Release:</b> ', stab$LastRelease, '</td></tr>')

  # Prepare row4
  r4 <- paste0('<tr><td><b>License:</b> ', info$License, '</td>
         <td><b>Num Releases:</b> ', stab$NumReleases, '</td>
         <td><b>Breaking Releases:</b> ', stab$BreakingReleases, '</td>
         </tr>')

  # Prepare row5
  r5 <- paste0('<tr><td><b>Depends:</b> ', info$Depends, '</td>
         <td><b>Num Functions:</b> ', length(info$Functions), '</td>
         <td><b>Downloads/month:</b> ', format(info$LastMonthDownloads, big.mark = ","), '</td></tr>')

  # Prepare row6
  r6 <- paste0('<tr><td colspan="3"><b>Imports:</b> ', info$Imports, '</td></tr>')

  # Prepare row7
  r7 <- paste0('<tr><td colspan="3"><b>Suggests:</b> ', info$Suggests, '</td></tr>')


  tblc <- '</table>'


  ret <- paste0(tblo, r1, r2, r3, r4, r5, r6, r7, tblc, collapse = "\n")

  return(ret)
}



# # Your data
# dat <- data.frame(
#   Release = c("2025-07-29", "2025-06-25", "2025-01-15", "2024-06-17", "2024-06-07",
#               "2024-03-05", "2024-02-02", "2023-12-16", "2023-10-18",
#               "2023-10-06", "2023-09-25", "2023-09-12", "2023-07-06",
#               "2023-06-09", "2023-04-25", "2023-03-14", "2023-03-08",
#               "2022-12-23", "2022-12-06", "2022-10-14", "2022-10-07",
#               "2022-09-29", "2022-09-21", "2022-09-05", "2022-07-18",
#               "2022-05-31", "2022-02-17"),
#   RF = c(0,0,2,0,0,0,0,15,0,0,0,5,0,8,0,0,7,0,8,0,0,0,0,32,0,13,0),
#   RP = c(0,0,10,0,8,0,0,2,0,0,0,0,0,2,0,0,4,0,0,0,0,0,0,2,0,4,0)
# )

