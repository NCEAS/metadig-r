 # Functions related to downloading and caching objects


#' Get the file path for an object
#'
#' Get the file path for an object specified by its URL. If the content at the
#' given URL has not been cached previously, the bytes are retrieved and cached.
#'
#' @param url (character) Any URL to an object.
#'
#' @return (character) The path to the file.
#'
#' @importFrom digest digest
#' @importFrom httr GET content
#'
#' @export
mdq_get <- function(url) {
  stopifnot(is.character(url), nchar(url) > 0)

  sys_url <- gsub("/object/", "/meta/", url)

  x <- xml2::read_xml(sys_url)
  fname <- xml2::xml_text(xml2::xml_find_all(x, "fileName"))
  fext <- tools::file_ext(fname)

  temp_dir = Sys.getenv("MDQE_CACHE_DIR", tempdir())
  if (temp_dir == "") stop("MDQE_CACHE_DIR was not set.")
  if (!file.exists(temp_dir))
    stop(paste0("MDQE_CACHE_DIR was set to a path that does not exist: ", temp_dir))

  cache_dir = file.path(temp_dir, "mdq-cache")
  if (!file.exists(cache_dir)) dir.create(cache_dir)
  stopifnot(file.exists(cache_dir))

  key = digest::digest(url, algo = "sha256", ascii = TRUE, serialize = FALSE)
  file_path = file.path(cache_dir, paste(key, fext, sep = "."))

  if (!file.exists(file_path)) {
    request <- httr::GET(url)
    writeLines(httr::content(request, as = "text"), con = file_path)
  }

  stopifnot(file.exists(file_path))

  file_path
}
