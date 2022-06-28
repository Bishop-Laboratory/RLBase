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


### Launch RLBase locally

To launch the RLBase app, the easiest approach will be to do the following:

1. Clone the repository and `cd` into it:

```shell
git clone https://github.com/Bishop-Laboratory/RLBase.git rlbase
cd rlbase
```

2. Install R v4.2.0+ and get the `renv` package

```R
R -e 'if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")'
```

3. Install `getSysReqs`

```shell
R -e 'if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")'
R -e 'remotes::install_github("mdneuzerling/getsysreqs", force=TRUE)'
```

4. Install system reqs (requires sudo)

```shell
REQS=$(Rscript -e 'options(warn = -1); cat(getsysreqs::get_sysreqs("renv.lock"))' | sed s/"WARNING: ignoring environment value of R_HOME"//) \
  && echo $REQS \
  && sudo apt-get install -y $REQS
```

5. Create the environment (requires `renv` R package installed):

```shell
R -e "renv::restore()"
```

6. Configure awscli (required for Analyze feature to work; need admin credentials)

```shell
aws configure
```

7. Finally, launch the server:

```R
Rscript app.R
```
