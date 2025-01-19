
#' @import rvest
#' @noRd
get_latest_version <- function(pkgname,
                                 baseurl = "https://cran.r-project.org/web/packages") {

  if (is.null(pkgname)) {
    stop("pkgname parameter cannot be NULL.")
  }

  # Concat url
  url <- file.path(baseurl, pkgname, "index.html")

  # Read and parse page into data frame
  page <- read_html(url)
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

  ret <- data.frame(Version = ver, FileName = src, "Date" = as.Date(date), "Size" = "")

  return(ret)
}


#' @import rvest
#' @import common
#' @noRd
get_archive_versions <- function(pkgname,
                              baseurl = "https://cran.r-project.org/src/contrib/Archive") {

  if (is.null(pkgname)) {
    stop("pkgname parameter cannot be NULL.")
  }

  # Concat url
  url <- file.path(baseurl, pkgname)

  # Read and parse page into data frame
  page <- read_html(url)
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
  ret <- subset(table1, table1$Name != "Parent Directory" & table1$Name != "")

  # Convert dates
  ret[["Last modified"]] <- as.Date(ret[["Last modified"]])

  # Extract package version
  vers <- get_version(ret$Name)

  # Create final data frame
  ret <- data.frame(Version = vers, FileName = ret$Name,
                    Date = ret[["Last modified"]],
                    Size = ret$Size)

  # Sort descending
  ret <- sort(ret, by = "Date", ascending = FALSE)

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






