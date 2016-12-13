#
#   This work was created by participants in the Metadata Improvement and
#   Guidance Project (MetaDIG) project, and is jointly copyrighted by
#   participating institutions in MetaDIG.
#
#     Copyright 2016
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#' @title Execute a quality check for a metadata document.
#' @description The Metadata Improvement and Guidance (MetaDIG) project has defined a methodology
#' for running tests to evaluate the conformance of a metadata document to a set of quality metrics,
#' using an automated metadata quality engine that runs a suite of checks for a single or collection of metadata documents.
#' The reference implementation of this quality engine has been written in Java. The source
#' repository for this engine is \url{https://github.com/NCEAS/mdqengine}. A data repository that
#' uses this quality engine is located at \url{https://arcticdata.io}.
#'
#' The \emph{metadig} R package
#' is a partial implementation of this quality engine in R, intended to aid in authoring metadata
#' checks in R, executing (aka "running") single quality checks and check suites locally in the R
#' environment. When complete and tested, these checks can then be including in quality suites
#' running the Java reference implementation of the quality engine.
#' The \code{runCheck} function runs a single quality check for a specified metadata document.
#' @details The quality check is an XML document that conforms to the format specified with
#' the reference implementation of the metadata quality engine: \url{https://github.com/NCEAS/mdqengine}.
#' \code{runCheck} extracts values from the metadata document as specified in the quality check document
#' and runs the check R code to examine these values. The check returns a list providing information
#' about the sucess or failure of the check.
#' @aliases runCheck
#' @param check a character string that contains the filename of a check document
#' @param metadata a character string that contains the filename of a metdata document to check
#' @param checkFunction (not yet implemented)
#' @return The check results, as a named list
#' @seealso \code{\url{https://github.com/NCEAS/mdqengine}}
#' @export
#' @keywords quality check
#' @examples
#' checkXML <- system.file("extdata/dataset_title_length-check.xml", package="metadig")
#' metadataXML<- system.file("extdata/example_eml.xml", package="metadig")
#' result <- runCheck(checkXML, metadataXML)
#' @import xml2
#' @export
runCheck <- function(checkXML, metadataXML, checkFunction) {
  # Read in the metadata document that will be checked.
  metadataDoc <- read_xml(metadataXML)
  # Create a copy of the document that is not namespace aware (i.e. namespace definitions stripped)
  # This document will be used for selectors that don't have namespaces defined, and therefor use
  # local XML paths.
  metadataDocNoNS <- read_xml(metadataXML)
  # nsList contains a list of namespace prefixes and associated URIs
  nsList <- xml_ns(metadataDoc)
  nsPrefixes <- names(nsList)
  if(length(nsPrefixes) > 0) {
    for (thisNsPrefix in nsPrefixes) {
      # Don't remove the XMLSchema-instance namespace
      if(thisNsPrefix == "xsi") next
      metadataDocNoNS <- ns_strip(metadataDocNoNS, thisNsPrefix)
    }
  }

  # Read in the XML for the check
  checkDoc <- read_xml(checkXML)
  # Check that the metadata document is a supported dialect for the
  # check. If the dialect isn't present, then all dialects are
  # supported by this check.
  if(!isCheckValid(checkDoc, metadataDoc)) {
    checkId <- xml_text(xml_find_first(checkDoc, "/mdq:check/id"))
    message(sprintf("Check %s is not valid for metadata document %s", checkId, metadataXML))
    return()
  }

  # Apply each selector to the document. The values extracted from the document
  # by the select wil be used to define variables in the check environment with the
  # name of the variable
  selectors <- xml_find_all(checkDoc, ".//selector")
  variables <- ""
  codeEnv <- new.env()
  # Extract values from the metadata document as specified in the check selectors.
  # Each selector will return a single object, which may be a single value, a list,
  # or a list of lists. It is assumed that the check code will know how to handle
  # whatever value is passed in the variable.
  if(length(selectors) > 0) {
    for (thisSelector in selectors) {
      #cat(sprintf("selector: %s\n", xml_child(thisSelector, "name")))
      #cat(sprintf("Selector name: %s\n", selectorName))
      selectorXpath <- xml_text(xml_child(thisSelector, "xpath"))
      #cat(sprintf("Selector xpath: %s\n", selectorXpath))o
      # Accumulate this selectors namespaces, if any are present.
      selectorNamespaces <- vector()
      class(selectorNamespaces) <- "xml_namespace"
      namespaces <- xml_find_first(thisSelector, "namespaces")
      if(length(namespaces) > 0) {
        # Now retrieve all the namespaces
        namespaces <- xml_children(namespaces)
        for(iNamespace in seq.int(1, length(namespaces), length.out=length(namespaces))) {
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
      if(class(nsNode) != "xml_missing") {
        selectorNsAware <- as.logical(xml_text(nsNode))
      }

      if(selectorNsAware) {
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
      assign(xml_text(xml_child(thisSelector, "name")), value=variableValue, envir=codeEnv)
    }
  } else {
    stop("No selectors are defined for this check.")
  }

  if(!missing(checkFunction)) {
    # Call the check function that has been defined locally
    # in the current R environment.
    #assign("dataset.title.length", checkFunction, envir=codeEnv)
    result <- runFunc(checkFunction, myEnv=new.env())
    #result <- do.call(what=checkFunction, args=list(), envir=new.env())
  } else {
    # Call the check function that was extracted from the check XML
    code <- xml_text(xml_child(checkDoc, "code"))
    codeCon <- textConnection(code, "r")
    result <- source(codeCon, local=codeEnv)
    close(codeCon)
  }
  # Cleanup - remove temporary environment.
  rm(codeEnv)
  return(result)
}

#' Select data values from a metadata document as specified in a checks selectors.
#' @param contextNode the metadata document xml2 node to apply the selector to.
#' @param selector the check document xml2 node of a selector to apply
#' @param selectorNamespace the XML namespaces that are defined for a selector
#' @return an XML2 nodeset of the selected values (or XML2 nodes) from the metadata document
selectNodes <- function(contextNode, selectorContext, selectorNamespaces) {
  if(length(selectorContext) == 0) return(list())
  values <- list()
  selectorName <- xml_text(xml_child(selectorContext, "name"))
  selectorXpath <- xml_text(xml_child(selectorContext, "xpath"))
  # Have to use xml_find_first here instead of xml_find_all, because xml_find_all
  # returns an internal error if the node doesn't evaluate to text, for example
  # the selector 'boolean(/eml/dataset/title)' causes an internal error.
  selectedNodeset <- xml_find_first(contextNode, selectorXpath, selectorNamespaces)
  # Return if selector didn't select anything
  if(length(selectedNodeset) == 0) return(values)
  # If the xpath expression evaluations to a number, logical or character, then
  # one of these types will be returned, otherwise an "xml_node" will be returned.
  if(class(selectedNodeset) != "xml_node") {
    if (class(selectedNodeset) %in% c("logical", "numeric", "character")) {
      values[[length(values)+1]] <- selectedNodeset
    } else {
      warning(sprintf("Unknown data type returned by selector: %s.", class(selectedNodeset)))
    }
  } else {
    selectedNodeset <- xml_find_all(contextNode, selectorXpath, selectorNamespaces)
    # Apply the subSelector, if present, to each node in the nodeset.
    if(length(selectedNodeset) > 0) {
      for (iNode in 1:length(selectedNodeset)) {
        #cat(sprintf("iNode: %s\n", iNode))
        #cat(sprintf("value: %s\n", xml_text(selectedNodeset[[iNode]])))
        thisNode <- selectedNodeset[[iNode]]
        subSelector <- xml_child(selectorContext, "subSelector")
        # If a subselector is present, then use the current node as a context
        # and apply the subselector to it. There can be unlimited levels of subselectors,
        # so recurse. This could get really confusing, and I really don't think anyone will
        # use this "feature".
        if(length(subSelector) > 0) {
          #cat(sprintf("recursing with selector xpath: %s\n", xml_text(xml_child(subSelector, "xpath"))))
          value <- selectNodes(thisNode, subSelector, selectorNamespaces)
          values[[length(values)+1]] <- value
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
          values[[length(values)+1]] <- value
        }
      }
    }
  }
  return(values)
}

#' Check if a check can be run against a document.
isCheckValid <- function(checkDoc, metadataDoc) {
  dialectNodes <- xml_find_all(checkDoc, "dialect")
  if(length(dialectNodes) > 0) {
    for (iDialect in 1:length(dialectNodes)) {
      thisDialectNode <- dialectNodes[[iDialect]]
      if(length(thisDialectNode) > 0) {
        dialectName <- xml_text(xml_child(thisDialectNode, "name"))
        dialectXpath <- xml_text(xml_child(thisDialectNode, "xpath"))
        # The xpath for dialect should have been written as a boolean, so that
        # an R logical will be returned.
        dialectMatchNode <- xml_find_first(metadataDoc, dialectXpath)
        if(class(dialectMatchNode) != "logical") {
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
#' @param x The document to strip
#' @param nsPrefix The namespace prefix that identifies the
ns_strip <- function (x, nsPrefix) {
  xpathStr <- sprintf("//namespace::*[name()='%s']/parent::*", nsPrefix)
  namespace_element_nodes <- xml_find_all(x, xpathStr)
  #message(sprintf("stripping %s from %d document nodes...", nsPrefix, length(namespace_element_nodes)))
  if(length(namespace_element_nodes) > 0) {
    # Build the namespace declaration string
    nsDecl <- sprintf("xmlns:%s", nsPrefix)
    xml_attr(namespace_element_nodes, nsDecl) <- NULL
  }
  invisible(x)
}
