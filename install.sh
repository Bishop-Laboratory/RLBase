#!/bin/bash
sudo apt install -y libfontconfig1-dev
R -e "install.packages(c('shiny','DT','plotly','aws.s3','shinycssloaders','shinyvalidate','futile.logger','kernlab','randomForest','rpart','MASS','prompter','shinyWidgets','pbapply','callr','VennDiagram','uuid','tidyverse','kableExtra','pheatmap', 'ggprism', 'bslib', 'RColorBrewer', 'BiocManager'), repos = 'https://packagemanager.rstudio.com/all/latest')"
R -e "options(repos = c(RSPM = 'https://packagemanager.rstudio.com/all/latest')); BiocManager::install(c('RLSeq'))"

