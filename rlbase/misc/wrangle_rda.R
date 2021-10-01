# For wrangling the report rda files
library(tidyverse)

list.files("misc/report_rda/", pattern = "*.rda", full.names = TRUE) %>%
  lapply(
    function(file_now) {
      
      message(file_now)
      file_next <- gsub(file_now, pattern = ".+/([ES]RX[0-9]+_.+)", replacement = "misc/report_rda_small/\\1")
      
      suppressWarnings(load(file_now))
      
      # Remove the expensive / unnecessary parts
      data_list[["peak_ol"]] <- NULL
      data_list$rlfs_data[[1]][[1]]$randomize.function <- NULL
      data_list$corr_data <- NULL
      data_list$read_qc_data <- NULL
      data_list$read_qc_data$read1_before_filtering$kmer_count <- NULL
      data_list$read_qc_data$read1_after_filtering$kmer_count <- NULL
      
      # Resave
      save(data_list, file = file_next, compress = "xz")
      
    }
  )

