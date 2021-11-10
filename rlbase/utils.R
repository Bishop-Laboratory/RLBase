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

# UI function to make ? button
helpButton <- function(message, position="right") {
  return(
    add_prompt(
      ui_element = span(
        HTML('<i class="fa fa-question-circle"></i>')),
      message = message, position = position
    )
  )
  
}

#' Make headers
makeHeaders <- function(title, message, fs=1.3, position="right") {
  tagList(
    span(span(title, style=paste0("font-size: ", fs, "em;")), helpButton(message, position=position))
  )
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
  BASE_URL1 <- "http://genome.ucsc.edu/s/millerh1%40livemail.uthscsa.edu/RLBase?position="
  paste0("<a href=\"", BASE_URL1, x, "\" target=\"_blank\">", x, "</a>")
}

#' Make SRA links
makeSRALinks <- function(x) {
  SRA_BASE <- "https://trace.ncbi.nlm.nih.gov/Traces/sra/?study="
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
  
  # Get the rlregion plot data
  dir.create("misc/rlranges/")
  if (! file.exists("misc/rlranges/ERX2277510_hg38.rds")) {
    system("aws s3 sync s3://rlbase-data/rlranges/ misc/rlranges/")
  }
  hg38samps <- rlsamples$rlsample[rlsamples$genome == "hg38"]
  hg38sampsfls <- setNames(lapply(hg38samps, function(x) {file.path("misc", "rlranges", paste0(x, "_hg38.rds"))}), nm = hg38samps)
  hg38sampsfls <- hg38sampsfls[sapply(hg38sampsfls, file.exists)]
  rlregionPltData <- parallel::mclapply(hg38sampsfls, function(fl) {
    rlr <- readRDS(file = fl)
    RLSeq::plotRLRegionOverlap(rlr, returnData = TRUE, rlregions_table = rlregions)
  }, mc.cores = 44)
  
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
       rlregionPltData,
       rlregions, tpm_rl_exp, bucket_sizes,
       featPlotData, heatData, rlMembershipMatrix,
       file = APP_DATA, compress = "xz")
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
runrlseq <- function(inputs, awstry_put, awstry_saverds) {
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
    sampleName = inputs$userSample
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
  awstry_put(file = inputs$tmpHTML1, object = file.path(inputs$runID, "res_index.html"), bucket = inputs$USERDATA_S3, acl = "public-read")
  Sys.sleep(5)
  message("Saving RDS to AWS...")
  awstry_saverds(x = rlr, compress = "xz",
                 object = file.path(inputs$runID, "rlranges.rds"),
                 bucket = inputs$USERDATA_S3, acl = "public-read")
  Sys.sleep(3)
  message("Saving report to AWS...")
  awstry_put(file = inputs$report,
             object = file.path(inputs$runID, "report.html"),
             bucket = inputs$USERDATA_S3, acl = "public-read")
  message("Done!")
  timestamp()
}


# Using try as a way of allowing multiple attempts
# Sometimes it will randomly error, so we need to allow for multiple attempts
awstry_put <- function(file, object, bucket, acl) {
  attempt <- 5
  success <- FALSE
  while (attempt > 0 & ! success) {
    message("Attempt: ", attempt)
    res <- try(aws.s3::put_object(file = file, object = object, verbose = TRUE,
                                  show_progress = TRUE, bucket = bucket, acl = acl), silent = TRUE)
    if ("try-error" %in% class(res)) {
      attempt <- attempt - 1
      Sys.sleep(3)
    } else {
      success <- TRUE      
    }
  }
  if (success) {
    return(success)
  }
  stop("Upload failed. Please notify RLBase maintainer.")
}

# Using try as a way of allowing multiple attempts
# Sometimes it will randomly error, so we need to allow for multiple attempts
awstry_saverds <- function(x, compress, object, bucket, acl) {
  attempt <- 5
  success <- FALSE
  while (attempt > 0 & ! success) {
    message("Attempt: ", attempt)
    res <- try(aws.s3::s3saveRDS(x=x, compress = compress, object = object, verbose = TRUE,
                                  show_progress = TRUE, bucket = bucket, acl=acl), silent = TRUE)
    if ("try-error" %in% class(res)) {
      attempt <- attempt - 1
      Sys.sleep(3)
    } else {
      success <- TRUE      
    }
  }
  if (success) {
    return(success)
  }
  stop("Upload failed. Please notify RLBase maintainer.")
}

rlseqbg <- function(inputs, runrlseq, awstry_put=awstry_put, 
                    awstry_saverds=awstry_saverds) {
  message("STARTING")
  failed <- TRUE
  attempts <- 3
  while(failed & attempts > 0) {
    message("attempt left: ", attempts)
    fail <- try({
      callr::r(func = runrlseq, args=list(inputs=inputs,
                                          awstry_put=awstry_put, 
                                          awstry_saverds=awstry_saverds),
               user_profile = FALSE, timeout = 300,
               stdout = inputs$log, stderr = inputs$log,
               poll_connection = FALSE, show = TRUE)
      failed <- FALSE
    },
    silent = TRUE
    )
    timeoutfail <- c("Error in get_result(output = out, options) : callr timed out\n",
                     "Error in get_result(output = out, options) : \n  callr subprocess failed: could not start R, exited with non-zero status, has crashed or was killed\n")
    message(fail)
    attempts <- attempts - 1
    if ("try-error" %in% class(fail) & ! fail %in% timeoutfail) {
      failed <- FALSE
    } else if (fail %in% timeoutfail & attempts > 0) {
      a_ <- knitr::knit(input = "www/rlseq_html/rlseq_error_timeout.Rhtml", output = inputs$tmpHTML1, quiet = TRUE)
      aws.s3::put_object(file = inputs$tmpHTML1,
                         object = file.path(inputs$runID, "res_index.html"),
                         bucket = inputs$USERDATA_S3, acl = "public-read")
      Sys.sleep(5)
    }
  }
  message(fail)
  message("OUT OF TRY")
  if ("try-error" %in% class(fail)) readr::write_lines(c(readr::read_lines(inputs$log), fail), file = inputs$log)
  aws.s3::put_object(file = inputs$log, object = file.path(inputs$runID, "log.txt"), bucket = inputs$USERDATA_S3, acl = "public-read")
  if ("try-error" %in% class(fail)) {
    a_ <- knitr::knit(input = "www/rlseq_html/rlseq_error.Rhtml", output = inputs$tmpHTML1, quiet = TRUE)
    aws.s3::put_object(file = inputs$tmpHTML1,
                       object = file.path(inputs$runID, "res_index.html"),
                       bucket = inputs$USERDATA_S3, acl = "public-read")
  } else {
    a_ <- knitr::knit(input = "www/rlseq_html/rlseq_done.Rhtml", output = inputs$tmpHTML1, quiet = TRUE)
    aws.s3::put_object(file = inputs$tmpHTML1, object = file.path(inputs$runID, "res_index.html"),
                       bucket = inputs$USERDATA_S3, acl = "public-read")
  }
}

