#!/bin/bash
sudo apt install -y libfontconfig1-dev
R -e "install.packages(c('shiny','DT','plotly','aws.s3','shinycssloaders','shinyvalidate','futile.logger','kernlab','randomForest','rpart','MASS','prompter','shinyWidgets','pbapply','callr','VennDiagram','uuid','tidyverse','kableExtra','pheatmap', 'ggprism', 'bslib', 'RColorBrewer', 'BiocManager'), repos = 'http://cran.rstudio.com/')"
R -e "BiocManager::install(c('RLSeq'))"

