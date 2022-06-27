FROM rocker/shiny:4.2.0

RUN R -e "install.packages('renv')"

WORKDIR /srv/shiny-server/rlbase/

COPY renv.lock renv.lock
COPY renv/ renv/

ENV RENV_CONFIG_REPOS_OVERRIDE https://packagemanager.rstudio.com/cran/latest


RUN sudo apt-get update

RUN R -e 'if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")'
RUN R -e 'remotes::install_github("mdneuzerling/getsysreqs", force=TRUE)'
RUN REQS=$(Rscript -e 'options(warn = -1); cat(getsysreqs::get_sysreqs("renv.lock"))' | sed s/"WARNING: ignoring environment value of R_HOME"//) \
    && echo $REQS && sudo apt-get install -y $REQS

RUN R -e "renv::restore()"

COPY . .
