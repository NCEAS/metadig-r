#' Execute a quality check for a metadata document
#'
#' This function runs a quality check based on an XML document
#' that conforms to the format specified with the reference implementation
#' of the metadata quality engine: <https://github.com/NCEAS/metadig-engine>. The function
#' extracts values from the metadata document as specified in the quality check document
#' and runs the check R code to examine these values. The check returns a list providing information
#' about the success or failure of the check.
#'
#' @param checkXML (character) The filepath for a quality check document.
#' @param metadataXML (character) The filepath for a metadata document to check.
#' @param checkFunction (not yet implemented)
#'
#' @return A named list of check results.
#'
#' @import xml2
#'
#' @export
#'
#' @examples
#' checkXML <- system.file("extdata/dataset_title_length-check.xml", package = "metadig")
#' metadataXML<- system.file("extdata/example_EML.xml", package = "metadig")
#' result <- runCheck(checkXML, metadataXML)
runCheck <- function(checkXML, metadataXML, checkFunction) {
  stopifnot(is.character(checkXML), length(checkXML) == 1, nchar(checkXML) > 0)
  stopifnot(is.character(metadataXML), length(metadataXML) == 1, nchar(metadataXML) > 0)

  # Read in the metadata document that will be checked.
  metadataDoc <- read_xml(metadataXML)
  # Create a copy of the document that is not namespace aware (i.e. namespace definitions stripped).
  # This document will be used for selectors that don't have namespaces defined, and therefore use
  # local XML paths.
  metadataDocNoNS <- read_xml(metadataXML)
  # nsList contains a list of namespace prefixes and associated URIs
  nsList <- xml_ns(metadataDoc)
  nsPrefixes <- names(nsList)
  if (length(nsPrefixes) > 0) {
    for (thisNsPrefix in nsPrefixes) {
      # Don't remove the XMLSchema-instance namespace
      if (thisNsPrefix == "xsi") next
      metadataDocNoNS <- ns_strip(metadataDocNoNS, thisNsPrefix)
    }
  }

  # Read in the XML for the check
  checkDoc <- read_xml(checkXML)
  # Check that the metadata document is a supported dialect for the
  # check. If the dialect isn't present, then all dialects are
  # supported by this check.
  if (!isCheckValid(checkDoc, metadataDoc)) {
    checkId <- xml_text(xml_find_first(checkDoc, "/mdq:check/id"))
    message(sprintf("Check %s is not valid for metadata document %s", checkId, metadataXML))
    return()
  }

  # Apply each selector to the document. The values extracted from the document
  # by the selector will be used to define variables in the check environment with the
  # name of the variable.
  selectors <- xml_find_all(checkDoc, ".//selector")
  variables <- ""
  codeEnv <- new.env()
  # Extract values from the metadata document as specified in the check selectors.
  # Each selector will return a single object, which may be a single value, a list,
  # or a list of lists. It is assumed that the check code will know how to handle
  # whatever value is passed in the variable.
  if (length(selectors) > 0) {
    for (thisSelector in selectors) {
      # cat(sprintf("selector: %s\n", xml_child(thisSelector, "name")))
      # cat(sprintf("Selector name: %s\n", selectorName))
      selectorXpath <- xml_text(xml_child(thisSelector, "xpath"))
      # cat(sprintf("Selector xpath: %s\n", selectorXpath))
      # Accumulate this selectors namespaces, if any are present.
      selectorNamespaces <- vector()
      class(selectorNamespaces) <- "xml_namespace"
      namespaces <- xml_find_first(thisSelector, "namespaces")
      if (length(namespaces) > 0) {
        # Now retrieve all the namespaces
        namespaces <- xml_children(namespaces)
        for (iNamespace in seq.int(1, length(namespaces), length.out = length(namespaces))) {
          thisNS <- namespaces[[iNamespace]]
          thisPrefix <- xml_attr(thisNS, "prefix")
          thisURI <- xml_text(thisNS)
          thisAttrs <- attributes(thisNS)
          # Add this selector namespace to the list
          selectorNamespaces[[thisPrefix]] <- thisURI
        }
      }
      # See if the check author specified that this selector should be namespace aware, i.e.
      # the selector has namespaces defined which it will use when extracting nodes from the document.
      nsNode <- xml_child(thisSelector, "namespaceAware")
      selectorNsAware <- FALSE
      if (class(nsNode) != "xml_missing") {
        selectorNsAware <- as.logical(xml_text(nsNode))
      }

      if (selectorNsAware) {
        metadataDocToUse <- metadataDoc
      } else {
        metadataDocToUse <- metadataDocNoNS
      }

      variableList <- selectNodes(metadataDocToUse, thisSelector, selectorNamespaces)
      variableValue <- unlist(variableList)
      # For each variable value, place it in the environment that the
      # check code will execute in. The variable that holds the value
      # is named for the '<name>' child element of the selector.
      # We need to use a separate environment that is not contaminated
      # with the current environment values.
      # If the selector didn't select anything, then the value will be
      # NULL.
      assign(xml_text(xml_child(thisSelector, "name")), value = variableValue, envir = codeEnv)
    }
  } else {
    stop("No selectors are defined for this check.")
  }

  if (!missing(checkFunction)) {
    # Call the check function that has been defined locally
    # in the current R environment.
    # assign("dataset.title.length", checkFunction, envir=codeEnv)
    # result <- runFunc(checkFunction, myEnv = new.env())
    # result <- do.call(what=checkFunction, args=list(), envir=new.env())
  } else {
    # Call the check function that was extracted from the check XML
    code <- xml_text(xml_child(checkDoc, "code"))
    codeCon <- textConnection(code, "r")
    result <- source(codeCon, local = codeEnv)
    close(codeCon)
  }

  # Remove temporary environment
  rm(codeEnv)

  return(result)
}


#' Select data values from a metadata document
#'
#' Select data values from a metadata document as specified in a check's selectors.
#'
#' @param contextNode The metadata document xml2 node to apply the selector to.
#' @param selector The check document xml2 node of a selector to apply.
#' @param selectorNamespace The XML namespaces that are defined for a selector.
#'
#' @import xml2
#'
#' @return An xml2 nodeset of the selected values (or xml2 nodes) from the metadata document.
#'
#' @noRd
selectNodes <- function(contextNode, selectorContext, selectorNamespaces) {
  if (length(selectorContext) == 0) return(list())
  values <- list()
  selectorName <- xml_text(xml_child(selectorContext, "name"))
  selectorXpath <- xml_text(xml_child(selectorContext, "xpath"))
  # Have to use xml_find_first here instead of xml_find_all, because xml_find_all
  # returns an internal error if the node doesn't evaluate to text, for example
  # the selector 'boolean(/eml/dataset/title)' causes an internal error.
  selectedNodeset <- xml_find_first(contextNode, selectorXpath, selectorNamespaces)
  # Return if selector didn't select anything
  if (length(selectedNodeset) == 0) return(values)
  # If the xpath expression evaluations to a number, logical or character, then
  # one of these types will be returned, otherwise an "xml_node" will be returned.
  if (class(selectedNodeset) != "xml_node") {
    if (class(selectedNodeset) %in% c("logical", "numeric", "character")) {
      values[[length(values) + 1]] <- selectedNodeset
    } else {
      warning(sprintf("Unknown data type returned by selector: %s.", class(selectedNodeset)))
    }
  } else {
    selectedNodeset <- xml_find_all(contextNode, selectorXpath, selectorNamespaces)
    # Apply the subSelector, if present, to each node in the nodeset.
    if (length(selectedNodeset) > 0) {
      for (iNode in seq_along(selectedNodeset)) {
        # cat(sprintf("iNode: %s\n", iNode))
        # cat(sprintf("value: %s\n", xml_text(selectedNodeset[[iNode]])))
        thisNode <- selectedNodeset[[iNode]]
        subSelector <- xml_child(selectorContext, "subSelector")
        # If a subselector is present, then use the current node as a context
        # and apply the subselector to it. There can be unlimited levels of subselectors,
        # so recurse. This could get really confusing, and I really don't think anyone will
        # use this "feature".
        if (length(subSelector) > 0) {
          #cat(sprintf("recursing with selector xpath: %s\n", xml_text(xml_child(subSelector, "xpath"))))
          value <- selectNodes(thisNode, subSelector, selectorNamespaces)
          values[[length(values) + 1]] <- value
        } else {
          # TODO: This might actually be a nodeset, so have to handle that case.
          # Try to coerce the value to an R type, if this hasn't been done by xml_find_first already.
          textVal <- xml_text(thisNode)
          suppressWarnings(value <- as.numeric(textVal))
          if (is.na(value)) {
            suppressWarnings(value <- as.logical(textVal))
          }
          if (is.na(value)) {
            value <- textVal
          }
          values[[length(values) + 1]] <- value
        }
      }
    }
  }

  return(values)
}


# Helper function to check if a check can be run against a document
isCheckValid <- function(checkDoc, metadataDoc) {
  dialectNodes <- xml_find_all(checkDoc, "dialect")
  if (length(dialectNodes) > 0) {
    for (iDialect in seq_along(dialectNodes)) {
      thisDialectNode <- dialectNodes[[iDialect]]
      if (length(thisDialectNode) > 0) {
        dialectName <- xml_text(xml_child(thisDialectNode, "name"))
        dialectXpath <- xml_text(xml_child(thisDialectNode, "xpath"))
        # The xpath for dialect should have been written as a boolean, so that
        # an R logical will be returned.
        dialectMatchNode <- xml_find_first(metadataDoc, dialectXpath)
        if (class(dialectMatchNode) != "logical") {
          message("Skipping dialect name: %s, xpath: %s", dialectName, dialectXpath)
          message("The xpath should be written as an xpath boolean expression.")
          next
        } else {
          if (dialectMatchNode) return(TRUE)
        }
      }
    }
  } else {
    # If no <dialect> specified in check, then this check is valid for all dialects.
    return(TRUE)
  }

  return(FALSE)
}


#' Strip the specified namespace from a document
#'
#' Strip the specified namespace from a document.
#'
#' @param x The document to strip.
#' @param nsPrefix The namespace prefix that identifies the namespace of interest.
#'
#' @import xml2
#'
#' @noRd
ns_strip <- function(x, nsPrefix) {
  xpathStr <- sprintf("//namespace::*[name()='%s']/parent::*", nsPrefix)
  namespace_element_nodes <- xml_find_all(x, xpathStr)
  # message(sprintf("stripping %s from %d document nodes...", nsPrefix, length(namespace_element_nodes)))
  if (length(namespace_element_nodes) > 0) {
    # Build the namespace declaration string
    nsDecl <- sprintf("xmlns:%s", nsPrefix)
    xml_attr(namespace_element_nodes, nsDecl) <- NULL
  }

  invisible(x)
}
