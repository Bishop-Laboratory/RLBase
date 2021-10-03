pcaPlotDataFromCorr <- function(corr_data) {
  
  # Heavily adapted from DESeq2 plotPCA
  # https://github.com/mikelove/DESeq2/blob/a5941b305b597556d47c68df0b922a0a20b41fb6/R/plots.R
  
  # perform a PCA on the data in assay(x) for the selected genes
  pca <- prcomp(corr_data)
  
  # the contribution to the total variance for each component
  percentVar <- pca$sdev^2 / sum( pca$sdev^2 )
  
  # assembly the data for the plot
  d <- data.frame(PC1=pca$x[,1], PC2=pca$x[,2], 
                  rlsample=colnames(corr_data))
  return(list(
    'pcData' = d,
    'percentVar' = round(percentVar[1:2] * 100)
  ))
  
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

#' Makes the global data for the app
makeGlobalData <- function(APP_DATA) {
    
  # Get membership matrix
  memMat <- parallel::mclapply(
    rlsamples$rlsample,
    function(rlsample) {
      tibble(
        membership = grepl(rlregions$samples, pattern = rlsample, perl = TRUE)
      ) %>%
        rename(!!quo_name(rlsample) := membership)
    }, mc.cores = 20
  )
  rlMembershipMatrix <- bind_cols(rlregions$rlregion, memMat) %>%
    column_to_rownames("...1") %>%
    as.matrix() %>% 
    Matrix::Matrix(sparse=TRUE)
  
  # Pick an hg38 and an mm10 and get plotting data
  featPlotData <- lapply(list("mm10"="mm10","hg38"="hg38"), function(gen) {
    message(gen)
    rlr <- aws.s3::s3readRDS(bucket = RLSeq:::RLBASE_S3, object = rlsamples$rlranges_rds_s3[rlsamples$genome == gen][1])
    
    # No split
    return(
      list(
        "none" = plotEnrichment(rlr, rlbaseRes=rlbaseRes, rlsamples=rlsamples, returnData = TRUE),
        "prediction" = plotEnrichment(rlr, splitby = "prediction", label_POS_only = FALSE,
                                      returnData = TRUE, pred_POS_only = FALSE,
                                      rlbaseRes=rlbaseRes, rlsamples=rlsamples),
        "label" = plotEnrichment(rlr, splitby = "label", 
                                 label_POS_only = FALSE, pred_POS_only = FALSE,
                                 returnData = TRUE,
                                 rlbaseRes=rlbaseRes, rlsamples=rlsamples) 
      )
    )
  })
  # Get the correlation data
  heatData <- corrHeatmap(
    aws.s3::s3readRDS(bucket = RLSeq:::RLBASE_S3, object = rlsamples$rlranges_rds_s3[rlsamples$genome == "hg38"][1]),
    returnData = TRUE
  )
  
  save(rlsamples, rlfsres, rlbaseRes, gss,
       rlregions, 
       featPlotData, heatData, rlMembershipMatrix,
       file = APP_DATA, compress = "gzip")
}




#' Make downloads for a specific sample
sampleDownloads <- function(sample, rlsamples) {
  tagList(
    fluidRow(
      column(
        width = 6,
        h4(paste0(current_samp(), " - Downloads")),
        hr()
      )
    ),
    fluidRow(
      column(
        width = 4,
        a(
          class="btn btn-default shiny-download-link",
          target="_blank",
          href=paste0(baseURLBW, snamebw, ".bw"),
          icon("download"),
          "Coverage (.bw)"
        )
      ),
      column(
        width = 4,
        a(
          class="btn btn-default shiny-download-link",
          target="_blank",
          href=paste0(baseURLPEAKS, snamepeak, ".unstranded.broadPeak"),
          icon("download"),
          "Peaks (.broadPeak)"
        )
      )
    )
  )
} 
