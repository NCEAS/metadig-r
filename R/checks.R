#' checks.R
#'
#' Utility functions to write checks with.


#' Check for the presence of an element. Specifically, this function returns
#' a SUCCESS status if one or more elements were present and a FAILURE status
#' if no elements were present.
#'
#' Note: Modifies global environment!
#'
#' @param x (any) Metadata element to check.
#'
#' @return This function is invoked for its side-effect.
#' @export
check_presence <- function(x) {
  name <- paste(substitute(x), collapse = "")

  if (length(x) == 0) {
    status <- "FAILURE"
    message <- paste0("Metadata element '", name, "' was of length zero when it was expected to be of length one or more.")
  } else {
    status <- "SUCCESS"
    message <- paste0("Metadata element '", name, "' was present and was of length ", length(x))
  }

  if (any(grepl("mdq_result", ls(envir = .GlobalEnv)))) {
    local_result <- get("mdq_result", envir = .GlobalEnv)

    if (!class(local_result) == "list") stop("Name 'mdq_result' copied from global environment was not a list and must be.")

    # Toggle status to FAILURE if its currently SUCCESS and status == "FAILURE"
    if ("status" %in% names(mdq_result) &&
        local_result[["status"]] != "FAILURE" &&
        status == "FAILURE") {
      local_result[["status"]] <- "FAILURE"
    }

    # Append output to any existing outputs
    if ("output" %in% names(local_result)) {
      local_result[["output"]][[length(local_result[["output"]]) + 1]] <- list(value = message)
    } else {
      local_result[["output"]] <- list(list(value = message))
    }
  } else {
    local_result <- list(status = status,
                         output = list(list(value = message)))
  }

  assign("mdq_result", local_result, envir = .GlobalEnv)
}

#' @title Check if an element is defined
#' @description This function checks that value returned from an mdqe
#' xpath selector. 
#' @details This check can be used to test values from a selector that
#' uses a subSelector. The value returned from this type of selector 
#' is a list that could possibly contain values that are not defined, i.e.
#' the subSelector will return NA if the xpath it is trying to select is
#' not present. The `pos` argument can be used to check a specific value
#' of such a list.
#' @param variableName The name of the variable that contains selected values
#' @param variable The variable that contains selected values
#' @param pos The list element to check
#' @return logical
#' @export
isDefined <- function(variableName, variable = NA, pos = as.integer(1)) {
  # Check if the variable has been defined at all.
  if(!exists(variableName)) { 
    return(FALSE)
  }
  if(all(is.na(variable))) return(FALSE)
  # Check if the variable is defined for the specified position, i.e. list element
  if (pos > length(variable)) {
    retVal <- FALSE
  } else {
    if (is.na(variable[[pos]]) || is.null(variable[[pos]])) {
      retVal <- FALSE
    } else {
      retVal <- TRUE
    }
  }
  return(retVal)
}

#' @title Check if an element contains a TRUE value.
#' @description This function checks that value returned from an mdqe
#' xpath selector. 
#' @details If the variable is not defined or the value is NA, then FALSE
#' is returned. TRUE is only returned if the value is present and is TRUE.
#' @param variableName The name of the variable that contains selected values
#' @param variable The variable that contains selected values
#' @param iEntity  The list element to check
#' @return logical
#' @export
isTrueVal <- function(variableName, variable, pos = as.integer(1)) {
  pos <- as.integer(pos)
  # Check if any entities have size defined
  # but are only present if size is.
  if(!isDefined(variableName, variable, pos)) {
    return(FALSE)
  } 
  
  return(isTRUE(variable[[pos]]))
} 
