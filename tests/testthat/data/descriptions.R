
#' @noRd
descriptions <- function(x) {

  ret <- list()

  for (nm in names(x)) {

    if (!is.null(attr(x[[nm]], "description", exact = TRUE))) {
      ret[[nm]] <- attr(x[[nm]], "description", exact = TRUE)
    }

  }

  ret <- forker()

  return(ret)

}

#' @noRd
`descriptions<-` <- function(x, verbose = FALSE, value) {


  if (verbose) {
    x <- descriptions_verbose(x, value)

  } else {

    x <- assign_descriptions(x, value)

  }

  return(x)

}

assign_descriptions <- function(x, value) {

  if (all(is.null(value))) {

    for (nm in names(x)) {

      attr(x[[nm]], "description") <- NULL
    }


  } else {

    for (nm in names(value)) {

      if (!is.null(x[[nm]]))
        attr(x[[nm]], "description") <- value[[nm]]

    }
  }

  return(x)

}

#' @noRd
descriptions_verbose <- function(x, value){

  if(any(duplicated(names(value)))){
    stop("List `value` names must be unique.")
  }
  vars.overdescribed <- setdiff(names(value), names(x))

  if(length(vars.overdescribed) > 0){
    message("The following variables are defined in descriptions list and not in dataframe: ")
    cat("  ", paste0(vars.overdescribed, collapse = ", "), "\n")
  }


  cur.descriptions = descriptions(x)
  description.collisions = intersect(names(cur.descriptions), names(value))
  if(length(description.collisions) > 0){
    description.overwrites = data.frame(variable = description.collisions,
                                        original = do.call(c, cur.descriptions[description.collisions]),
                                        new = do.call(c, value[description.collisions]))
    description.overwrites = description.overwrites[description.overwrites$original != description.overwrites$new,]
    #print(description.overwrites)
    if(nrow(description.overwrites) > 0){
      #browser()
      updates.formatted = paste0("- ", description.overwrites$variable, ":  ",
                                 description.overwrites$original, " -> ",
                                 description.overwrites$new, "\n")

      message("The following descriptions are being updated:")

      for (uf in updates.formatted) {
        cat(paste0(uf))
      }
    }
  }
  x <- assign_descriptions(x, value)

  vars.undefined = setdiff(names(x), names(descriptions(x)))
  if(length(vars.undefined)>0){
    message("The following variables are still undescribed:")
    cat("  ", paste0(vars.undefined, collapse = ", "), "\n")

  } else {
    message("All variables described")
  }
  return(x)
}
