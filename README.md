# RLBase
![Build Status](https://github.com/Bishop-Laboratory/RLBase/workflows/tests/badge.svg)

## Quick-Start

RLBase can be accessed here: https://gccri.bishop-lab.uthscsa.edu/rlbase/

Please see the "Documentation" section in RLBase for additional details.

## Issues and Bug Reports

Please open an issue if you find any errors or unexpected behavior. Please make sure to document:

1. What you tried
2. What the result was
3. What you expected the result to be
4. Steps (if any) which you took to resolve the issue and their outcomes.


### Deploy RLBase

To launch the RLBase app, the easiest approach will be to do the following:

1. Clone the repository and `cd` into it:

```shell
git clone https://github.com/Bishop-Laboratory/RLBase.git
cd RLBase/
```

2. Create the environment (requires `renv` R package installed):

```shell
R -e "renv::restore()"
```

3. Configure awscli (only admin can do this)

```shell
aws configure
```

4. Retrieve non-conda dependencies 

```shell
R -e "install.packages(c('ggprism', 'shinyvalidate', 'prompter', 'valr', 'caretEnsemble'), repos='http://cran.us.r-project.org')"
R -e "BiocManager::install(version='devel', ask=FALSE)"
R -e "remotes::install_github('Bishop-Laboratory/RLHub', dependencies = TRUE, force=TRUE)"
R -e "remotes::install_github('Bishop-Laboratory/RLSeq', dependencies = FALSE, force=TRUE)"
```

5. Finally, launch the server:

```R
Rscript runApp.R 6868 # port number
```