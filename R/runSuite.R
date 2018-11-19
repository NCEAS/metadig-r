#' Execute a quality suite for a metadata document
#'
#' This function runs a suite of quality checks based on an XML document
#' that conforms to the format specified with the reference implementation
#' of the metadata quality engine: <https://github.com/NCEAS/mdqengine>.
#'
#' @param suiteXML (character) A filepath for the quality suite document.
#' @param dirXML (character) A filepath for the directory storing quality check documents.
#' @param metadataXML (character) A filepath for the metadata document to check.
#'
#' @return A named list of results.
#'
#' @import xml2
#'
#' @export
#'
#' @examples
#' \dontrun{
#' metadataXML <- system.file("extdata/example_EML.xml", package = "metadig")
#' suiteXML <- system.file("extdata/example_suite.xml", package = "metadig")
#' dirXML <- systemfile("extdata", package = "metadig")
#'
#' results <- runSuite(suiteXML, dirXML, metadataXML)
#' }
runSuite <- function(suiteXML, dirXML, metadataXML) {
  stopifnot(is.character(suiteXML), length(suiteXML) == 1, nchar(suiteXML) > 0)
  stopifnot(is.character(dirXML), length(dirXML) == 1, nchar(dirXML) > 0)
  stopifnot(is.character(metadataXML), length(metadataXML) == 1, nchar(metadataXML) > 0)

  # Parse check IDs
  suite <- read_xml(suiteXML)
  ids <- xml_find_all(suite, "check")
  ids <- xml_text(ids)

  # Translate IDs to list of check XML filepaths
  checks <- list.files(dirXML, full.names = TRUE)
  all <- lapply(checks, read_xml)
  names(all) <- checks
  all <- lapply(all, xml_find_all, "id")
  all <- lapply(all, xml_text)
  run <- all[which(all %in% ids)]
  suite <- names(run)

  # Use runCheck() and iterate over checks in suite using the same metadata file
  results <- lapply(suite, runCheck, metadataXML)
  names(results) <- basename(suite)
  results
}
