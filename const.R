## MAGIC ##
baseURLBW <- "https://rmapdb-data.s3.us-east-2.amazonaws.com/bigwigs/rseq-coverage-unstranded/"
baseURLPEAKS <- "https://rmapdb-data.s3.us-east-2.amazonaws.com/macs2-peaks-unstranded/"

  
CORR_DATA <- "data/annoCorr__corr_data.rda"
torm <- "ERX2277510_E-MTAB-6318DRIP_mOHT"
name <- paste0(torm, "_hg38")
INPUT_CORR <- paste0("misc/report_rda/", name, ".QC_report.rda")
# Get corr dataset & wrangle
if (! file.exists(CORR_DATA)) {
  load(INPUT_CORR)
  keep <- which(! colnames(data_list$corr_data$corMat) %in% torm)
  corr_data <- data_list$corr_data$corMat[keep, keep]
  annoCorr <- data_list$corr_data$annoNow[colnames(corr_data),] %>%
    filter(Source == "RMapDB")
  newlabs <- gsub(rownames(annoCorr), pattern = ".+_([ES]RX[0-9]+)$", replacement = "\\1")
  rownames(annoCorr) <- colnames(corr_data) <- rownames(corr_data) <- newlabs
  annoCorr <- annoCorr %>%
    rownames_to_column(var = "id") %>%
    select(-Source) %>%
    left_join(
      dataLst %>%
        pluck("rmap_samples") %>% 
        mutate(pred_ctrl = prediction == "Control") %>%
        select(id, is_rnh_like, pred_ctrl)
    ) %>%
    column_to_rownames(var = "id") %>%
    mutate(is_ctrl = ifelse(is.na(is_rnh_like), TRUE, is_rnh_like)) %>%
    select(-is_rnh_like)
  save(annoCorr, corr_data, file = CORR_DATA)
} else {
  if (! all(c("annoCorr", "corr_data") %in% names(globalenv()))) {
    load(CORR_DATA)
  }
}
  

# Get annotations
if (! "anno_data" %in% names(globalenv())) {
  anno_data <- dataLst %>%
    pluck("sample_quality_characteristics") %>%
    filter(grepl(char_type, pattern = "__")) %>%
    mutate(Annotation = gsub(char_type, pattern = "(.+)__(.+)", replacement = "\\1"),
           data_type = gsub(char_type, pattern = "(.+)__(.+)", replacement = "\\2")) %>%
    select(-char_type) %>%
    inner_join(y = data.frame(
      "Annotation" = c('Intergenic', 'Simple_repeat', 'Satellite', 'Promoter', 'pseudo',
                       'Intron', 'TTS', 'LINE', 'LTR', 'SINE', 'DNA', 'CpG-Island', 'ncRNA',
                       'Low_complexity', 'Exon', 'snRNA', 'Retroposon', '5UTR', '3UTR', 'srpRNA', 
                       'tRNA', 'RC', 'scRNA', 'miRNA', 'RNA', 'snoRNA'),
      "annotate_type" = c("gene", "rep", "rep", "gene", "RNA", "gene", "gene", 
                          "rep", "rep", "rep", "rep", "gene", "RNA", "rep",
                          "gene", "RNA", "rep", "gene", "gene", "RNA",
                          "RNA", "RNA", "RNA", "RNA", "RNA", "RNA"), stringsAsFactors = FALSE
    ), by = "Annotation") %>%
    pivot_wider(id_cols = c(id, Annotation, annotate_type), names_from = data_type, values_from = value) %>%
    inner_join(y = pluck(dataLst, "rmap_samples"), by = "id")
  
  # Values for plotting
  annoPlot_boxFills <- c('gene' = 'firebrick', 'RNA' = 'goldenrod', 'rep' = 'forestgreen')
  annoPlot_titles <- c('gene' = 'Genomic Features', 'RNA' = 'ncRNAs', 'rep' = 'Repetitive Elements')
  annoPlot_genelvls <- c("CpG-Island",
                         "Promoter",
                         "5UTR",
                         "Exon",
                         "Intron",
                         "3UTR",
                         "TTS",
                         "Intergenic")
}

  
# For show
rltabShow <- rltab %>%
  mutate(avgSignalValue = round(avgSignalValue, 3),
         avgQVal = round(avgQVal, 3)) %>%
  select("RL Region" = id,
         "Location" = location,
         "Genes" = genes,
         "Type" = type,
         -origName, -is_rlfs,
         "# of Studies" = numStudies,
         "# of Samples" = numSamps,
         "# of Tissues" = numTissues, 
         "# of Modes" = numModes, 
         Modes,
         "Mean Signal" = avgSignalValue,
         "Mean FDR" = avgQVal,
         "Mean RLCounts" = avgNormCounts) %>%
  arrange(desc(`# of Studies`), desc(`Mean Signal`)) 
  

  
# Summary quality characteristics
# TODO: Need to find a non-hardcoded way to get this
qualCol <-  list(
  tibble(
    "descript" = rep(c('low', 'high'), 3),
    "type" = rep(c("error", "warning", "ok"), each = 2),
    'vals' = c(c(0, 1500), c(1500, 3000), c(3000, Inf)),
    "name" = 'MACS2__total_peaks'
  ),
  tibble(
    "descript" = rep(c('low', 'high'), 3),
    "type" = rep(c("error", "warning", "ok"), each = 2),
    'vals' = c(c(0, 1.3), c(1.3, 2), c(2, Inf)),
    "name" = 'rlfs_pval'
  ),
  # tibble(
  #   "descript" = rep(c('low', 'high'), 3),
  #   "type" = rep(c("error", "warning", "ok"), each = 2),
  #   'vals' = c(c(-Inf, 5), c(5, 15), c(15, Inf)),
  #   "name" = "rlfs_max"
  # ),
  tibble(
    "descript" = rep(c('low', 'high'), 3),
    "type" = rep(c("error", "warning", "ok"), each = 2),
    'vals' = c(c(0, 60), c(60, 80), c(80, 100)),
    "name" = "pct_aligned"
  ),
  tibble(
    "descript" = rep(c('low', 'high'), 3),
    "type" = rep(c("error", "warning", "ok"), each = 2),
    'vals' = c(c(0, 80), c(80, 90), c(90, 100)),
    "name" = "percent_passing"
  )
) %>% 
  bind_rows() %>%
  select(-descript) %>%
  group_by(name, type) %>%
  summarise(vals = list(vals)) %>%
  pivot_wider(id_cols = name, names_from = type, values_from = vals)

qualColors <- tibble(
  'name' = c("error", "warning", "ok"),
  'color' = c("red", "orange", "green")
)


