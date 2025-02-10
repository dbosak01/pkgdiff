
# Introductory Documentation ----------------------------------------------

#' @title ards: A package for creating Analysis Results Datasets
#'
#' @description An Analysis Results Dataset (ARDS) is commonly used in the pharma-biotech industry
#' to capture the results of an analysis in a tabular data structure.  The \strong{ards}
#' package helps create the ARDS. \strong{ards} functions can be called from inside 
#' a report program or a data preparation program.  The functions use a 
#' "bucketing" approach, whereby data can be added to the ARDS in multiple 
#' calls and from multiple intermediate data sets.    
#'     
#' @details 
#' The \strong{ards} package allows you to easily create an ARDS dataset
#' in a standard clinical reporting or data preparation program.  The 
#' \strong{ards} package contains only three functions, and each will
#' be used to create the ARDS dataset.  The functions will be called in the 
#' following order, and for the described purpose:
#' \itemize{
#'   \item \code{\link{init_ards}}: Initialize an ARDS dataset
#'   \item \code{\link{add_ards}}: Add data to an ARDS dataset
#'   \item \code{\link{get_ards}}: Extract the completed ARDS
#' }
#' Click on the links above for more information about each function.
#' 
#' The following figure describes the structure of the ARDS dataset.  This
#' structure is recommended by CDISC.
#' \figure{structure.png}
#' 
#' @docType package
#' @keywords internal
#' @name ards
NULL

# Globals -----------------------------------------------------------------



ardsenv <- new.env()

ardsenv$ards <- NULL
ardsenv$template <- NULL



# Functions ---------------------------------------------------------------



#' @title Initialize the Analysis Results Dataset
#' @description A function to initialize the Analysis Results Dataset (ARDS).
#' This function will
#' first create a data template in the desired structure, and then
#' populate common values across the dataset from that template.  
#' These common values will be
#' repeated on each row of the analysis data frame for subsequent inserts
#' from the \code{\link{add_ards}} function.
#' @param studyid The study for which the analysis was performed. 
#' This parameter is optional.
#' @param tableid A table identifier to use for the results. This value 
#' identifies the table within the study.  Optional string
#' value.
#' @param adsns A vector of source dataset names.  This parameter is used to 
#' identify the input data for the analysis.  This parameter is optional.
#' @param population A description of the analysis population.  This parameter
#' is used to identify the population for analysis.  This parameter is optional.
#' @param time A description of the time frame used in the analysis.  For example, 
#' in a clinical study, the "time" value may identify the visit on which the
#' analysis is based.
#' @param where An optional description of the criteria used to subset the 
#' data for analysis.
#' @param reset If true, clears out the existing ARDS dataset and replaces with
#' an empty template.  Otherwise, just assign new parameter values to the 
#' existing template.  The default
#' value is TRUE, meaning the ARDS in memory will be cleared every time 
#' \code{init_ards}
#' is called.  If you wish to assign new initialization values, but 
#' keep appending to the existing ARDS dataset, set this parameter to FALSE.  
#' This feature is used when you are creating two different tables in the 
#' same program.
#' @return The initialized analysis dataset.
#' @family ards
#' @examples 
#' library(ards)
#' library(dplyr)
#' 
#' # Initialize the ARDS
#' # - These values will be common through the dataset
#' init_ards(studyid = "MTCARS",
#'           tableid = "01", adsns = "mtcars",
#'           population = "all cars",
#'           time = "1973")
#' 
#' # Perform analysis on MPG
#' # - Using cylinders as a by-group
#' analdf <- mtcars |> 
#'   select(cyl, mpg) |> 
#'   group_by(cyl) |> 
#'   summarize(n = n(),
#'             mean = mean(mpg),
#'             std = sd(mpg),
#'             min = min(mpg),
#'             max = max(mpg))
#' 
#' # View analysis data
#' analdf
#' #     cyl     n  mean   std   min   max
#' #   <dbl> <int> <dbl> <dbl> <dbl> <dbl>
#' # 1     4    11  26.7  4.51  21.4  33.9
#' # 2     6     7  19.7  1.45  17.8  21.4
#' # 3     8    14  15.1  2.56  10.4  19.2
#' 
#' # Add analysis data to ARDS
#' # - These values will be unique per row
#' add_ards(analdf, 
#'          statvars = c("n", "mean", "std", "min", "max"),
#'          anal_var = "mpg", trtvar = "cyl")
#' 
#' 
#' # Get the ARDS
#' ards <- get_ards() 
#' 
#' # Uncomment to view ards
#' # View(ards)
#' @export
init_ards <- function(studyid = NA,
                      tableid = NA, adsns = NA,
                      population = NA, time = NA, where = NA, reset = TRUE) {


  if (reset) {
    if (!is.null(ardsenv$ards)) {
      ardsenv$ards <- NULL
    }
  }
  
  # Collapse analysis data
  if (all(is.na(adsns) == FALSE))
    adsns <- paste(adsns, sep ="", collapse = "|")

  # Create template record
  ardsenv$template <- data.frame(studyid = studyid, resultid = 0,
                                 tableid = tableid,
                                 adsns = adsns,
                                 population = population, time = time,
                                 where = where, byvar1 = NA, byvar2 = NA,
                                 byvar3 = NA, byvar4 = NA, byvar5 = NA,
                                 byvar6 = NA, byvar7 = NA, byvar8 = NA,
                                 byvar9 = NA, byval1 = NA, byval2 = NA,
                                 byval3 = NA, byval4 = NA, byval5 = NA,
                                 byval6 = NA, byval7 = NA, byval8 = NA,
                                 byval9 = NA, trtvar = NA, trtval = NA,
                                 paramcd = NA, anal_var = NA, anal_val = NA,
                                 statname = NA, statval = NA, statdesc = NA)

  if (reset) {
    # Assign empty template to ards
    ardsenv$ards <- ardsenv$template[0, ]
  }

  return(ardsenv$template)

}



#' @title Adds data to an Analysis Results Dataset
#' @description The \code{add_ards} function dumps data from an input analysis 
#' dataset
#' to the ARDS dataset.  The function is designed to be pipe-friendly, and will
#' return the input dataset unaltered.  The parameters on the function
#' define how to extract the desired data from the analysis dataset.
#' The "statvars" parameter defines which columns contain desired
#' analysis results.  The values in these columns will be used to populate the
#' "statval" variable in the output dataset.  Other parameters are used to
#' define identifying information for the statistics values, and are optional.
#' 
#' The \code{add_ards} function should be called immediately after any
#' calculations, while the analysis results are still in numeric form.  This 
#' recommendation is to ensure that the ARDS will contain full precision of the
#' analysis values.  Once the analysis values are dumped into the ARDS, you
#' may proceed to transform and format your analysis data, without affecting
#' the values captured in the ARDS. 
#' @param data The input data to create analysis results for.  This parameter
#' is required.
#' @param statvars  A vector of column names that identify the desired results.
#' Statvar columns must be numeric.  This parameter is required.
#' @param statdesc A vector of values or a column name that identifies a description 
#' for each statvar. If passed as a vector of values, the number of values
#' should correspond to the number of 'statvar' variables.  
#' @param byvars A vector of column names to use for by variables.
#' @param trtvar A column name to use for the treatment variable.
#' @param paramcd A character string that describes the analysis parameter 
#' code or column name that contains the parameter code. If supplied as a
#' column name, the function will populate the 'paramcd' column in the ARDS
#' with the value of the 'paramcd' column.
#' @param anal_var A column name for the analysis variable or a string
#' that identifies the analysis variable.
#' @param anal_val The analysis variable value.  Can be identified by a column
#' name or a vector of values. By default, the analysis values will be taken
#' from the values of the variable passed in 'anal_var'.  This parameter 
#' exists so that you may pass in the values from a different variable, if desired.
#' @family ards
#' @return The input data frame, unaltered.
#' @examples 
#' library(ards)
#' library(dplyr)
#' 
#' # Initialize the ARDS
#' init_ards(studyid = "MTCARS",
#'           tableid = "01", adsns = "mtcars",
#'           population = "all cars",
#'           time = "1973")
#' 
#' # Perform analysis on MPG
#' # - Add to ARDS from within continuous variable pipeline
#' mpgdf <- mtcars |> 
#'   select(cyl, mpg) |> 
#'   group_by(cyl) |> 
#'   summarize(n = n(),
#'             mean = mean(mpg),
#'             std = sd(mpg),
#'             min = min(mpg),
#'             max = max(mpg)) |> 
#'   add_ards(statvars = c("n", "mean", "std", "min", "max"),
#'          anal_var = "mpg", trtvar = "cyl")
#'             
#' # Perform analysis on GEAR
#' # - Add to ARDS from within categorical variable pipeline
#' geardf <- mtcars |> 
#'   mutate(denom = n()) |> 
#'   select(cyl, gear, denom) |> 
#'   group_by(cyl, gear) |> 
#'   summarize(cnt = n(), 
#'             denom = max(denom)) |>
#'   mutate(pct = cnt / denom * 100) |> 
#'   add_ards(statvars = c("cnt", "pct", "denom"),
#'          anal_var = "gear", trtvar = "cyl")
#' 
#' # Get the ARDS
#' ards <- get_ards() 
#' 
#' # Uncomment to view ards
#' # View(ards)
#' @export
add_ards <- function(data, statvars, statdesc = NULL,
                     byvars = NULL, trtvar = NULL, paramcd = NULL,
                     anal_var = NULL, anal_val = NULL) {

  #browser()
  
  if (is.null(ardsenv$ards)) {
    
    stop("ARDS dataset is not initialized.") 
  }

  nms <- names(data)

  for (i in seq_len(length(statvars))) {

    # Start with template and turn into list
    ret <- unclass(ardsenv$template)

    # Populate statistics value
    ret$statval <- data[[statvars[[i]]]]

    # Populate statistic name from variable name
    ret$statname <- statvars[[i]]

    # Determine starting point of ResultID
    if (nrow(ardsenv$ards) == 0) {
      strt <- 1
    } else {
      strt <- max(ardsenv$ards$resultid) + 1
    }

    # Populate ResultID
    ret$resultid <- seq(strt, strt + nrow(data) - 1)

    # Populate Stat Description
    if (!is.null(statdesc)) {
      if (all(statdesc %in% nms)) {
        if (length(statdesc) == 1) {

          ret$statdesc <- data[[statdesc]]
        } else if (length(statdesc) >= i) {
          ret$statdesc <- data[[statdesc[[i]]]]
        }
      } else {
        if (length(statdesc) == 1) {
          ret$statdesc <- statdesc
        } else if (length(statdesc) >= i) {
          ret$statdesc <- statdesc[[i]]
        }
      }
    }

    # Populate By variables and values
    if (!is.null(byvars)) {
      for (j in seq_along(length(byvars))) {
        ret[[paste0("byvar", j)]] <- byvars[[j]]

        if (!is.null(data[[byvars[[j]]]]))
          ret[[paste0("byval", j)]] <- data[[byvars[[j]]]]

      }
    }

    # Populate Analysis Variable and Value
    if (!is.null(anal_var)) {
      ret[["anal_var"]] <- anal_var
      if (is.null(anal_val)) {
        if (all(anal_var %in% nms)) {

          ret[["anal_val"]] <- data[[anal_var]]
        }
      } else if (all(anal_val %in% nms)) {
        ret[["anal_val"]] <- data[[anal_val]]
      } else {

        ret[["anal_val"]] <- anal_var
      }
    }


    # Populate Treatment Variable
    if (!is.null(trtvar)) {
      if (trtvar %in% nms) {

        ret[["trtvar"]] <- trtvar
        ret[["trtval"]] <- data[[trtvar]]

      } else {

        ret[["trtval"]] <- trtvar
      }
    }
    
    # Populate PARAM Variable
    if (!is.null(paramcd)) {
      if (paramcd %in% nms) {
        
        ret[["paramcd"]] <- paramcd
        ret[["paramcd"]] <- data[[paramcd]]
        
      } else {
        
        ret[["paramcd"]] <- paramcd
      }
    }



    # Convert to data frame and recycle template values.
    ret <- data.frame(ret)

    # Append to ards
    ardsenv$ards <- rbind(ardsenv$ards, ret)

  }

  return(data)

}

#' @title Returns the current Analysis Results Dataset
#' @description The \code{get_ards} function returns the current state
#' of the Analysis Results Dataset (ARDS) as an R data frame. 
#' This data frame may be saved to disk, saved in
#' a database, or examined from code.  The function takes no parameters.
#' @return A data frame of the current analysis results.
#' @family ards
#' @examples 
#' library(ards)
#' library(dplyr)
#' 
#' # Initialize the ARDS
#' # - These values will be common through the dataset
#' init_ards(studyid = "IRIS",
#'           tableid = "01", adsns = "iris",
#'           population = "all flowers",
#'           time = "1973")
#' 
#' # Perform analysis on Petal.Length
#' # - Using Species as a by-group
#' analdf1 <- iris |> 
#'   select(Petal.Length, Species) |> 
#'   group_by(Species) |> 
#'   summarize(n = n(),
#'             mean = mean(Petal.Length),
#'             std = sd(Petal.Length),
#'             min = min(Petal.Length),
#'             max = max(Petal.Length)) |> 
#'   add_ards(statvars = c("n", "mean", "std", "min", "max"),
#'            statdesc = c("Count", "Mean", "STD", "Minimum", "Maximum"),
#'            anal_var = "Petal.Length", trtvar = "Species")
#'            
#' # Perform analysis on Petal.Width
#' # - Using Species as a by-group
#' analdf2 <- iris |> 
#'   select(Petal.Width, Species) |> 
#'   group_by(Species) |> 
#'   summarize(n = n(),
#'             mean = mean(Petal.Width),
#'             std = sd(Petal.Width),
#'             min = min(Petal.Width),
#'             max = max(Petal.Width)) |> 
#'   add_ards(statvars = c("n", "mean", "std", "min", "max"),
#'            statdesc = c("Count", "Mean", "STD", "Minimum", "Maximum"),
#'            anal_var = "Petal.Width", trtvar = "Species")
#' 
#' # Get the ARDS
#' ards <- get_ards() 
#' 
#' # Uncomment to view ards
#' # View(ards)
#' @export
get_ards <- function() {

 return(ardsenv$ards)

}

