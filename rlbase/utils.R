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

#' Make SRA links
makeSRALinks <- function(x) {
  SRA_BASE <- "https://www.ncbi.nlm.nih.gov/sra/"
  as.character(a(
    href=paste0(SRA_BASE, x),
    target="_blank",
    x
  ))
}

#' Make pubmed links
makePubMedLinks <- function(x) {
  PUBMED_BASE <- "https://pubmed.ncbi.nlm.nih.gov/"
  as.character(a(
    href=paste0(PUBMED_BASE, x),
    target="_blank",
    x
  ))
}


#' Makes the global data for the app
makeGlobalData <- function(APP_DATA) {
  
  rlsamples <- RLHub::rlbase_samples()
  rlfsres <- RLHub::rlfs_res()
  rlbaseRes <- RLHub::feat_enrich_samples()
  gss <- RLHub::gs_signal()
  rlregions <- RLHub::rlregions_meta()
  tmp <- tempfile()
  download.file(file.path(RLSeq:::RLBASE_URL, "RLHub/tpm_rl_exp.rda"), destfile = tmp)
  load(tmp)
  # Get membership matrix
  memMat <- parallel::mclapply(
    rlsamples$rlsample,
    function(rlsample) {
      tibble(
        membership = grepl(rlregions$samples, pattern = rlsample, perl = TRUE)
      ) %>%
        dplyr::rename(!!quo_name(rlsample) := membership)
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
  
  # Get bucket sizes
  bucketsize <- function(name) {
    res <- system(paste0("aws s3 ls --summarize --human-readable --recursive s3://rlbase-data/", name), intern = TRUE)
    gsub(res[length(res)], pattern = "   Total Size: ([0-9\\.]+ [a-zA-Z]+)", replacement = "\\1")
  }
  bucks <- list(
    bam_stats="bam_stats",
    fastq_stats="fastq_stats",
    coverage="coverage",
    peaks="peaks",
    quant="quant",
    reports="reports",
    rlranges="rlranges",
    RLHub="RLHub"
  )
  bucket_sizes <- pblapply(bucks, bucketsize)
  save(rlsamples, rlfsres, rlbaseRes, gss,
       rlregions, tpm_rl_exp, bucket_sizes,
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


#' Helper function for running RLSeq
runrlseq <- function(inputs) {
  message("Beginning analysis...")
  timestamp()
  a_ <- lapply(names(inputs), function(x){message(x, ": ", inputs[[x]])})
  message("[[1]] Building RLRanges")
  current_step <- "Building RLRanges [1/3]"
  a_ <- knitr::knit(input = "www/rlseq_html/rlseq_inprogress.Rhtml", output = inputs$tmpHTML1, quiet = TRUE)
  aws.s3::put_object(file = inputs$tmpHTML1, object = file.path(inputs$runID, "res_index.html"), bucket = inputs$USERDATA_S3, acl = "public-read")
  rlr <- RLSeq::RLRanges(
    peaks = inputs$userPeaks$datapath,
    genome = inputs$userGenome,
    mode = inputs$userMode,
    label = inputs$userLabel,
    sampleName = inputs$userSample,
    quiet = FALSE
  )
  message("[[2]] Running RLSeq")
  current_step <- "Running RLSeq [2/3]"
  a_ <- knitr::knit(input = "www/rlseq_html/rlseq_inprogress.Rhtml", output = inputs$tmpHTML1, quiet = TRUE)
  aws.s3::put_object(file = inputs$tmpHTML1, object = file.path(inputs$runID, "res_index.html"), bucket = inputs$USERDATA_S3, acl = "public-read")
  rlr <- RLSeq::RLSeq(rlr, quiet = FALSE)
  message("[[3]] Building Report")
  current_step <- "Knitting Report [3/3]"
  a_ <- knitr::knit(input = "www/rlseq_html/rlseq_inprogress.Rhtml", output = inputs$tmpHTML1, quiet = TRUE)
  aws.s3::put_object(file = inputs$tmpHTML1, object = file.path(inputs$runID, "res_index.html"), bucket = inputs$USERDATA_S3, acl = "public-read")
  RLSeq:::report(rlr, reportPath = inputs$report)
  message("[[4]] Uploading to AWS")
  Sys.sleep(3)
  a_ <- knitr::knit(input = "www/rlseq_html/rlseq_upload.Rhtml", output = inputs$tmpHTML1, quiet = TRUE)
  aws.s3::put_object(file = inputs$tmpHTML1, object = file.path(inputs$runID, "res_index.html"), bucket = inputs$USERDATA_S3, acl = "public-read")
  Sys.sleep(5)
  message("Saving RDS to AWS...")
  aws.s3::s3saveRDS(x = rlr, compress = "xz", verbose=TRUE, show_progress = TRUE,
                    object = file.path(inputs$runID, "rlranges.rds"),
                    bucket = inputs$USERDATA_S3, acl = "public-read")
  Sys.sleep(3)
  message("Saving report to AWS...")
  aws.s3::put_object(file = inputs$report, verbose=TRUE, show_progress = TRUE,
                     object = file.path(inputs$runID, "report.html"),
                     bucket = inputs$USERDATA_S3, acl = "public-read")
  message("Done!")
  timestamp()
}
