# metadig

The `metadig-r` package provides functions that assist in authoring 
checks to be used by the [Java metadata quality engine](https://github.com/NCEAS/mdqengine)
created by the Metadata Improvement and Guidance project (MetaDIG), a National Science Foundation
funded project (grant #1443062).

## Usage

### `check_presence(name)`

Checks if the given `name` is non-NULL and sets an appropriate status and 
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

Quality checks authored for the Java quality engine can be tested in the R environment using the
`metadig`. Checks must be written in an XML format, including the source code of the check.

Examples of quality checks written in R are available in the `metadig` package. The following
example runs a quaility check in the R environment, using an example metadata file that is
also provided by the package:

```{r}
library(metadig)
library(xml2)
checkFile <- system.file("extdata/entity_attributes_sufficient_check.xml", package="metadig")
metadataFile <- system.file("extdata/example_eml.xml", package="metadig")
results <- runCheck(checkFile, metadataFile)
str(results)
```

The returned `results` provides a status of the check which can include "PASS", "FAIL" or "INFO" and output describing
the results of the check.

## Installation

```{r}
devtools::install_github("NCEAS/metadig-r")
```