# metadig

Helper package for metadig. Write checks more easily.

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


## Installation

```{r}
devtools::install_github("NCEAS/metadig-r")
```