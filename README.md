# RMapDB-shiny
![Build Status](https://github.com/Bishop-Laboratory/RMapDB-shiny/workflows/build/badge.svg)

## Quick-Start

To launch the RMapDB app, the easiest approach will be to do the following:

First, clone the repository and `cd` into it:

```bash
git clone https://github.com/Bishop-Laboratory/RMapDB-shiny.git
cd RMapDB-shiny/
```

Then install all dependencies:

```R
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
options(repos = c(RSPM = "https://packagemanager.rstudio.com/all/latest"))
renv::restore()
```

Finally, launch the server:

```R
library(shiny)
runApp()
```

## Issues and Bug Reports

Please open an issue if you find any errors or unexpected behavior. Please make sure to document:

1. What you tried
2. What the result was
3. What you expected the result to be
4. Steps (if any) which you took to resolve the issue and their outcomes.
