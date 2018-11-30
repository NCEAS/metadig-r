 # Functions related to downloading and caching objects


#' Get the file path for an object
#'
#' Get the file path for an object specified by its URL. If the content at the
#' given URL has not been cached previously, the bytes are retrieved and cached.
#'
#' @param url (character) Any URL.
#'
#' @return (character) The path to the file.
#'
#' @importFrom digest digest
#' @importFrom httr GET content
#'
#' @export
mdq_get <- function(url) {
  stopifnot(is.character(url), nchar(url) > 0)

  temp_dir = Sys.getenv("MDQE_CACHE_DIR", tempdir())
  if (temp_dir == "") stop("MDQE_CACHE_DIR was not set.")
  if (!file.exists(temp_dir))
    stop(paste0("MDQE_CACHE_DIR was set to a path that does not exist: ", temp_dir))

  cache_dir = file.path(temp_dir, "mdq-cache")
  if (!file.exists(cache_dir)) dir.create(cache_dir)
  stopifnot(file.exists(cache_dir))

  key = digest::digest(url, algo = "sha256", ascii = TRUE, serialize = FALSE)
  file_path = file.path(cache_dir, key)

  if (!file.exists(file_path)) {
    request <- httr::GET(url)
    writeLines(httr::content(request, as = "text"), con = file_path)
  }

  stopifnot(file.exists(file_path))

  file_path
}
