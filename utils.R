pcaPlotDataFromCorr <- function(corr_data) {
  
  # Heavily adapted from DESeq2 plotPCA
  # https://github.com/mikelove/DESeq2/blob/a5941b305b597556d47c68df0b922a0a20b41fb6/R/plots.R
  
  # perform a PCA on the data in assay(x) for the selected genes
  pca <- prcomp(corr_data)
  
  # the contribution to the total variance for each component
  percentVar <- pca$sdev^2 / sum( pca$sdev^2 )
  
  # assembly the data for the plot
  d <- data.frame(PC1=pca$x[,1], PC2=pca$x[,2], 
                  id=colnames(corr_data))
  return(list(
    'pcData' = d,
    'percentVar' = round(percentVar[1:2] * 100)
  ))
  
}


cleanAnnoCorrNow <- function(annoCorrNow) {
  # Select isControl if there's a good reason to
  if (any(annoCorrNow$is_ctrl)) {
    annoCorrNow$is_ctrl <- as.factor(annoCorrNow$is_ctrl)
  } else {
    annoCorrNow <- annoCorrNow[,-which(colnames(annoCorrNow) == "is_ctrl")]
  }
  
  # Select pred_ctrl 
  if (any(annoCorrNow$pred_ctrl)) {
    annoCorrNow$pred_ctrl <- as.factor(annoCorrNow$pred_ctrl)
  } else {
    annoCorrNow <- annoCorrNow[,-which(colnames(annoCorrNow) == "pred_ctrl")]
  }
  
  annoCorrNow
}

#' Makes a gene cards link for an official gene symbol
makeGeneCards <- function(x) {
  GENECARDS_BASE <- "https://www.genecards.org/cgi-bin/carddisp.pl?gene="
  as.character(a(
    href=paste0(GENECARDS_BASE, x),
    target="_blank",
    x
  ))
}


#' Make RLoop consensus view in genome browser
makeRLConsensusGB <- function(x) {
  BASE_URL1 <- "http://genome.ucsc.edu/s/millerh1%40livemail.uthscsa.edu/RLoop_consensus?position="
  as.character(a(
    href=paste0(BASE_URL1, x),
    target="_blank",
    x
  ))
}

