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


