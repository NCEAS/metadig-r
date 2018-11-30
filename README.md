# metadig

The *metadig* R package provides functions that assist in authoring and running metadata
checks in R to be used by the [Java metadata quality engine](https://github.com/NCEAS/metadig-engine)
created by the Metadata Improvement and Guidance project (MetaDIG), a National Science Foundation
funded project (grant #1443062). When complete and tested, these checks can then be included in quality suites
running the Java reference implementation of the quality engine.

The MetaDIG project has defined a methodology for running tests to evaluate the conformance of a
metadata document to a set of quality metrics, using an automated metadata quality engine that runs
a suite of checks for a single or collection of metadata documents. A data repository that
uses this quality engine is located at <https://arcticdata.io>.

## Installation

Install the package using:

```{r}
# install.packages("remotes")
remotes::install_github("NCEAS/metadig-r")
```

## Usage

### `check_presence(name)`

Checks if the given `name` is non-`NULL` and sets an appropriate status and 
output.

**Example:**
Given name `title` (`/eml/dataset/title`): 'My dataset'

```{r}
check_presence(title)
```

- Status: SUCCESS
- Output:
    - "Object 'title' was present and was of length 10."


### `success(message)` & `failure(message)`

Sets the status for a check and, optionally, creates a new output 
with your message. Once a check's status is set to FAILURE, it cannot be set
back to SUCCESS.

**Example:**

Given name `title` (`/eml/dataset/title`)

```{r}
if (nchar(title) > 100) {
  success("Title was long enough.")
} else {
  failure("Title was too short.")
}
```

- Status: SUCCESS
- Output:
    - "Title was long enough."


### `runCheck(checkXML, metadataXML)`

Quality checks authored for the Java quality engine can be tested in the R environment using this package.
Checks must be written in an XML format designed for the quality engine, 
which includes the source code of the check.

Examples of quality checks written in R are available in this package within the "inst/extdata" folder.
The following example runs a quality check in the R environment, using an example metadata file that is
also provided by the package:

```{r}
library(metadig)
library(xml2)

checkFile <- system.file("extdata/dataset_title_length-check.xml", package = "metadig")
metadataFile <- system.file("extdata/example_EML.xml", package = "metadig")

results <- runCheck(checkFile, metadataFile)
```

The returned `results` object provides a status of the check which can include
SUCCESS, FAILURE, or SKIP as well as output describing the results of the check.
