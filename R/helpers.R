#' helpers.R
#'
#' General helpers for the check writing that are not explicitly checks.


#' Set the status in a check.
#'
#' Repeated calls to this function within a single check has special behavior:
#'
#'   - If the status is ever set to FAILURE, it cannot be set back to SUCCESS
#'   - If the status is ever set to SUCCESS, it can always be set to FAILURE
#'
#' @param status (character) The status message to set.
#'
#' @return This function is invoked for its side-effect.
#' @export
set_status <- function(status) {
  allowed_statuses <- c("SUCCESS", "FAILURE")

  if (!(status %in% allowed_statuses)) {
    stop(paste0("The given type, ", status, " was not allowed: ", paste0(allowed_statuses, collapse = ", "), "."))
  }

  if (any(grepl("mdq_result", ls(envir = .GlobalEnv)))) {
    local_result <- get("mdq_result", envir = .GlobalEnv)

    if (!class(local_result) == "list")
      stop("Name 'mdq_result' copied from global environment was not a list and must be.")

    if (!("status" %in% names(local_result))) {
      local_result[["status"]] <- status
    } else {
      # Only set the status if we aren't trying to go from FAILURE back to SUCCESS
      if (status == "SUCCESS" && local_result[["status"]] != "FAILURE") {
        local_result[["status"]] <- status
      } else if (status == "FAILURE") {
        local_result[["status"]] <- status
      }
    }
  } else {
    local_result <- list(status = status)
  }

  assign("mdq_result", local_result, envir = .GlobalEnv)
}


#' Set the status to SUCCESS and, optionally, set an output message.
#'
#' @param message (character) Optional. A message to set as an output.
#'
#' @return This function is invoked for its side-effect.
#' @export
success <- function(message=NULL) {
  set_status("SUCCESS")
  if (current_status() != "FAILURE" && !is.null(message)) save_output(message)
}


#' Set the status to FAILURE and, optionally, set an output message.
#'
#' @param message (character) Optional. A message to set as an output.
#'
#' @return This function is invoked for its side-effect.
#' @export
#'
failure <- function(message=NULL) {
  set_status("FAILURE")
  if (!is.null(message)) save_output(message)
}


#' Save an object as an output.
#'
#' Note: Modifies global environment!
#'
#' @param x (any) Object to save as output.
#' @param type (character) The mime type to save the object as.
#'
#' @return This function is invoked for its side-effect.
#' @export
save_output <- function(x, type="text") {
  name <- paste(substitute(x), collapse = "")

  # Handle NULL specifically
  if (is.null(x)) {
    x <- "NULL"
  }

  if (length(x) == 0) {
    stop(paste0("'", name, "' was length zero, when length one is required."))
  }

  allowed_types <- c("text", "png")

  if (!(type %in% allowed_types)) {
    stop(paste0("The given type, ", type, " was not allowed: ", paste0(allowed_types, collapse = ", "), "."))
  }

  # Prepare output
  new_output <- list(value = paste0(as.character(x), collapse = "\n"), type = type)

  if (any(grepl("mdq_result", ls(envir = .GlobalEnv)))) {
    local_result <- get("mdq_result", envir = .GlobalEnv)

    if (!class(local_result) == "list") stop("Name 'mdq_result' copied from global environment was not a list and must be.")

    # Append output to any existing outputs
    if ("output" %in% names(local_result)) {
      local_result[["output"]][[length(local_result[["output"]]) + 1]] <- new_output
    } else {
      local_result[["output"]] <- list(new_output)
    }
  } else {
    local_result <- list(output = list(new_output))
  }

  assign("mdq_result", local_result, envir = .GlobalEnv)
}


current_status <- function() {
  if (any(grepl("mdq_result", ls(envir = .GlobalEnv))) && "status" %in% names(mdq_result)) {
      return(mdq_result[["status"]])
  } else {
    return(NULL)
  }
}