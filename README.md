# RLBase
![Build Status](https://github.com/Bishop-Laboratory/RLBase/workflows/tests/badge.svg)

## Quick-Start

### Installation

To launch the RLBase app, the easiest approach will be to do the following:

1. Clone the repository and `cd` into it:

```shell
git clone https://github.com/Bishop-Laboratory/RLBase.git
cd RLBase/
```

2. Create the environment (requires conda installed):

```shell
conda install -c conda-forge mamba -y
mamba env create -f rlbase.yml --force
conda activate rlbase
```

3. Retrieve non-conda dependencies 

```shell
R -e "install.packages('ggprism', repos='http://cran.us.r-project.org')"
R -e "remotes::install_github("Bishop-Laboratory/RLHub", dependencies = TRUE, force=TRUE)"
R -e "remotes::install_github("Bishop-Laboratory/RLSeq", dependencies = TRUE, force=TRUE)"
```

Finally, launch the server:

```R
Rscript runApp.R
```

## Issues and Bug Reports

Please open an issue if you find any errors or unexpected behavior. Please make sure to document:

1. What you tried
2. What the result was
3. What you expected the result to be
4. Steps (if any) which you took to resolve the issue and their outcomes.
