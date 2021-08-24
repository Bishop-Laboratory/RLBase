## MAGIC ##
baseURLBW <- "https://rmapdb-data.s3.us-east-2.amazonaws.com/bigwigs/rseq-coverage-unstranded/"
baseURLPEAKS <- "https://rmapdb-data.s3.us-east-2.amazonaws.com/macs2-peaks-unstranded/"

# Get the data
if (! "dataLst" %in% names(globalenv())) {
  load('data/dataLst.rda')
}
load("data/rmapfftsmall.rda")

# Get Blacklisted genome regions
BLACKLIST <- "data/ENCFF356LFX.bed"
bl <- regioneR::toGRanges(BLACKLIST)

# Get repeat masker
RMSK <- "data/hg38_repeatmasker.tsv.xz"
if (! "rmskgr" %in% names(globalenv())) {
  rmsk <- read_tsv(RMSK)
  rmskgr <- rmsk %>%
    # Remove the classes that are too ubiquitous 
    filter(! repClass %in% c("SINE", "LINE", "Simple_repeat", "DNA", "LTR", "Low_complexity"),
           ! grepl(repClass, pattern = ".+\\?.*")) %>%
    dplyr::select(
      seqnames = genoName, 
      start = genoStart, 
      end = genoEnd,
      repClass
    ) %>%
    as.data.frame() %>%
    GenomicRanges::makeGRangesFromDataFrame(keep.extra.columns = TRUE) 
}
  
# Get Centromeres
if (! "ctrmgr" %in% names(globalenv())) {
  CTRM <- "data/centromeres.tsv"
  ctrm <- read_tsv(CTRM)
  ctrmgr <- ctrm %>%
    dplyr::select(-`#bin`) %>%
    as.data.frame() %>%
    GenomicRanges::makeGRangesFromDataFrame(keep.extra.columns = TRUE) +
    1E6  # Expand centromere regions by 1 Mb
}

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

# Get RLoops
if (! file.exists("data/rltab.rda")) {
  
  # Get pathways from msigdbr to clean up the genes we use
  c("C2", "C5", "H", "C8") %>%
    lapply(msigdbr::msigdbr, species = "Homo sapiens") %>%
    bind_rows() %>%
    pull(gene_symbol) %>%
    unique() -> annoGenes
  
  rmap_samps <- dataLst %>% pluck('rmap_samples')
  
  # Join RMap Samples and RL Signal
  rmap_sig <- dataLst %>%
    pluck("rloop_signal") %>%
    filter(numOlap > 0) %>%
    left_join(select(rmap_samps, id, study_id, tissue, treatment, mode, is_rnh_like), by = c("rmap_sample_id" = "id"))
  
  ## Standardize? 
  # YES. This is clearly a per-sample and per-study issue...
  rmap_sig %>%
    filter(rloop_id %in% c(paste0("RL", 1:100))) %>%
    ggplot(aes(x = rmap_sample_id, y = log2(norm_counts + 1), fill = study_id)) +
    geom_boxplot()
  
  # Standardization applied -- looks better, though this would definitely be a good future direction to consider
  rmap_sig %>%
    group_by(rmap_sample_id) %>%
    mutate(norm_countsSC = scale(norm_counts)) %>%
    filter(rloop_id %in% c(paste0("RL", 1:100))) %>%
    ggplot(aes(x = rmap_sample_id, y = log2(norm_countsSC + 1), fill = study_id)) +
    geom_boxplot()
  
  rltab <- rmap_sig %>%
    group_by(rmap_sample_id) %>%
    mutate(norm_countsSC = scale(norm_counts)) %>%
    group_by(rloop_id) %>%
    summarise(samples=list(rmap_sample_id),
              numStudies = list(study_id),
              numTissues = list(tissue),
              numModes = list(mode),
              avgNormCounts = mean(norm_countsSC),
              avgQVal = mean(qVal),
              medQVal = median(qVal),
              avgSignalValue = mean(signalVal),
              medSignalValue = median(signalVal)) %>%
    mutate(numSamps = map_int(samples, function(x) {length(unique(x))}),
           numStudies = map_int(numStudies, function(x) {length(unique(x))}),
           numTissues = map_int(numTissues, function(x) {length(unique(x))}),
           Modes = map_chr(numModes, function(x) {
             unlist(x) %>% unique() %>% paste0(collapse = "\n")
           }),
           numModes = map_int(numModes, function(x) {length(unique(x))})) 
  rltab <- left_join(dataLst$rloops, y = rltab, by = c("id" = "rloop_id")) %>%
    mutate(location = gsub(location, pattern = "\\:\\*$", replacement = ""))
  
  # Get RL <--> Gene mapping
  rlGeneOl <- full_join(rltab, dataLst$gene_rl_overlap, by = c("id" = "rloop_id")) %>%
    left_join(select(dataLst$genes, gene_id = id, gene_symbol=symbol), by = c("gene_id")) %>%
    group_by(id) %>%
    summarise(
      genes = list(gene_symbol)
    )
  
  # Add genes back to rltab
  rltab <- left_join(rltab, rlGeneOl, by = c("id"))
  
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
  
  rltabShow <- rltabShow %>%
    mutate(GenesNat = map_chr(`Genes`, function(x) {
      unlist(x) %>% paste0(collapse = "\n")
    })) %>% 
    mutate(GenesFix = map_chr(`Genes`, function(x) {
      genes <- unlist(x)
      genes <- genes[genes %in% annoGenes]
      paste0(genes, collapse = "\n")
    }))
  
  
  rlgr <- rltabShow %>%
    select(names = `RL Region`, Location) %>%
    mutate(seqnames = gsub(Location, pattern = "(.+):(.+)-(.+)", replacement = "\\1"),
           start = as.numeric(gsub(Location, pattern = "(.+):(.+)-(.+)", replacement = "\\2")),
           end = as.numeric(gsub(Location, pattern = "(.+):(.+)-(.+)", replacement = "\\3"))) %>%
    select(-Location) %>%
    column_to_rownames(var = "names") %>%
    na.omit() %>%
    GenomicRanges::makeGRangesFromDataFrame()
  
  # Remove BL
  rangesBL <- GenomicRanges::findOverlaps(rlgr, bl) %>%
    as.data.frame() %>% pull(queryHits)
  
  # Remove repeats
  rangesRep <- GenomicRanges::findOverlaps(rlgr, rmskgr) %>%
    as.data.frame() %>% pull(queryHits)
  
  # Remove pericentromeric regions
  rangesCTRM <- GenomicRanges::findOverlaps(rlgr, ctrmgr) %>%
    as.data.frame() %>% pull(queryHits)
  
  # RLs which are not in BL or REP
  RLToKeep <- rlgr[
    -unique(c(rangesBL, rangesRep, rangesCTRM))
  ] %>%
    GenomeInfoDb::keepStandardChromosomes(pruning.mode = "coarse") %>%
    names()
  
  # Add match as column
  rltabShow <- rltabShow %>%
    mutate(repeats = ! `RL Region` %in% RLToKeep) 
  
  ### Match with expression ##
  
  # Due to mismatch in GSEs/SRPs, need to modify
  # TODO: Fix pipe to obviate the need for this
  dataLst$gene_exp_samples$condition <- dataLst$gene_exp_samples %>%
    pull(condition) %>%
    ifelse(grepl(x = ., pattern = "SRP058310"), 
           gsub(., pattern = "SRP[0-9]+(_.+)", replacement = "SRP058311\\1"),
           .) %>%
    ifelse(grepl(x = ., pattern = "SRP193695"), 
           gsub(., pattern = "SRP[0-9]+(_.+)", replacement = "GSE130242\\1"),
           .) 
  
  # Summarize RL signal to the condition level as well & scale
  rmap_sigSum <- rmap_sig %>%
    group_by(rmap_sample_id) %>%
    mutate(norm_countsSC = scale(norm_counts)) %>%
    ungroup()
  rmap_sigSum <- rmap_sigSum %>%
    mutate(
      condition = paste0(study_id, "_", tissue, "_", treatment)
    ) %>%
    group_by(rloop_id, condition) %>%
    summarise(
      qVal = mean(qVal),
      signalVal = mean(signalVal),
      normCounts = mean(norm_countsSC)
    )
  rmap2 <- rmap_samps %>%
    mutate(
      condition = paste0(study_id, "_", tissue, "_", treatment)
    )
  
  # Summarize gene Expression to the  condition level
  gene_expSum <- dataLst$gene_expression %>%
    inner_join(dataLst$gene_exp_samples, by = c("exp_sample_id" = "gene_exp_sample_id")) %>%
    group_by(gene_id, condition) %>%
    summarise(counts = mean(counts),
              log2tpm = mean(log2tpm),
              vst = mean(vst))
  
  # Match up to the R-loop level and convert to symbol
  rl_to_geneExp <- inner_join(dataLst$gene_rl_overlap, gene_expSum, by = c("gene_id")) %>%
    mutate(rl_geneID = paste0(rloop_id, "_", gene_id)) %>%
    select(-rloop_id, -gene_id)
  
  # Add genes to RL 
  rmap_sigSum_withGenes <- rmap_sigSum %>%
    ungroup() %>%
    inner_join(dataLst$gene_rl_overlap, by = "rloop_id") %>%
    mutate(rl_geneID = paste0(rloop_id, "_", gene_id)) %>%
    select(-rloop_id, -gene_id)
  
  # Finally, combine the data on RLoop, Condition, and Gene
  expSum_rlSum <- inner_join(rl_to_geneExp, rmap_sigSum_withGenes, by = c("rl_geneID", "condition")) %>%
    mutate(rloop_id = gsub(rl_geneID, pattern = "(.+)_(.+)", replacement = "\\1"),
           gene_id = gsub(rl_geneID, pattern = "(.+)_(.+)", replacement = "\\2")) %>%
    select(-rl_geneID) %>%
    inner_join(
      select(dataLst$genes, gene_id = id, gene_symbol = symbol)
    ) %>%
    select(-gene_id) 
  
  # Make a filtered version
  expSum_rlSum_geneNeat <- expSum_rlSum %>%
    filter(gene_symbol %in% annoGenes)
  
  # Gene level summary of R-loop signal
  rlExpCondLvl <- expSum_rlSum %>% select(-rloop_id) %>%
    group_by(gene_symbol, counts, log2tpm, vst, condition) %>%
    summarise(
      qVal = mean(qVal),
      signalVal = mean(signalVal),
      normCounts = mean(normCounts)
    ) %>%
    ungroup()
    
  runLst <- rlExpCondLvl %>%
    filter(gene_symbol %in% annoGenes) %>%
    group_by(gene_symbol) %>%
    {setNames(group_split(.), group_keys(.)[[1]])} 
  
  MINNUM <- 30
  YVAL <- "vst"
  xvals <- c("qVal"
             # "signalVal", "normCounts"
             )
  
  xtest <- lapply(
    xvals, 
    function(XVAL) {
      message(XVAL)
      corDF <- lapply(names(runLst), function(nx) {
        
        y <- runLst[[nx]]
        x <- select(y, all_of(c(XVAL, YVAL))) %>% unique()
        
        
        if (length(unique(y$condition)) > MINNUM) {
          corr <- cor.test(x = pull(x, !! XVAL), y = pull(x, !! YVAL),
                           method = "spearman")
        } else {
          corr <- NA
        }
        
        corrs <- ifelse(length(unique(y$condition)) > MINNUM,
                        corr$estimate,
                        0)
        pval <- ifelse(length(unique(y$condition)) > MINNUM,
                       corr$p.value,
                       1)
        
        tibble(
          'gene_symbol' = nx,
          'corr' = corrs,
          'pval' = pval
        )
        
      }) %>% bind_rows()
    }
  ) 
  names(xtest) <- xvals
  xtest <- lapply(names(xtest), function(x) {
    xtest[[x]]$type <- x
    xtest[[x]]
  })
  
  # Based on this, I think we should just use QVal
  # bind_rows(xtest) %>%
  #   select(-pval) %>%
  #   pivot_wider(id_cols = symbol, names_from = type, values_from = corr) %>%
  #   column_to_rownames("symbol") %>%
  #   na.omit() %>%
  #   cor() %>%
  #   pheatmap::pheatmap()
  # 
  # lapply(
  #   xvals, function(XVAL) {
  #     rlExpCondLvl %>%
  #       filter(gene_symbol == "PCDHGB4") %>%
  #       inner_join(
  #         rmap2, by = "condition"
  #       ) %>% 
  #       select(vst, signalVal, counts, normCounts, qVal, normCountsVST, log2tpm, mode, condition, study_id) %>%
  #       ggplot(., aes_string(x = XVAL, y = YVAL, text="mode", group="condition", color = "study_id")) +
  #           geom_point()
  #   }
  # ) %>% ggpubr::ggarrange(plotlist = .)
  
  xadj <- xtest %>%
    pluck(1) %>%
    filter(corr != 0) %>%
    mutate(corrpadj = p.adjust(pval, method = "fdr")) %>%
    dplyr::rename(corrpval = pval)
  
  xadj %>%
    filter(corr > 0 & corrpadj < .05) %>%
    write_csv("data/exp_rl_corResults_UP.csv")
  xadj %>%
    filter(corr < 0 & corrpadj < .05) %>%
    write_csv("data/exp_rl_corResults_DN.csv")
  
  rlExpCondLvlByGene <- left_join(rlExpCondLvl, xadj, by = "gene_symbol")
  
  # Gene level summary of R-loop signal
  rlExpCondLvlByRL <- expSum_rlSum %>% select(-gene_symbol) %>%
    group_by(rloop_id, counts, log2tpm, vst, condition) %>%
    summarise(
      qVal = mean(qVal),
      signalVal = mean(signalVal),
      normCounts = mean(normCounts)
    ) %>%
    ungroup()
  
  runLst <- rlExpCondLvlByRL %>%
    group_by(rloop_id) %>%
    {setNames(group_split(.), group_keys(.)[[1]])} 
  
  # Get the correlation for each R-loop with expression
  MINNUM <- 30
  YVAL <- "vst"
  xvals <- c("qVal")
  xtest <- lapply(
    xvals, 
    function(XVAL) {
      message(XVAL)
      corDF <- lapply(names(runLst), function(nx) {
        
        y <- runLst[[nx]]
        x <- select(y, all_of(c(XVAL, YVAL))) %>% unique()
        
        
        if (length(unique(y$condition)) > MINNUM) {
          corr <- cor.test(x = pull(x, !! XVAL), y = pull(x, !! YVAL),
                           method = "spearman")
        } else {
          corr <- NA
        }
        
        corrs <- ifelse(length(unique(y$condition)) > MINNUM,
                        corr$estimate,
                        0)
        pval <- ifelse(length(unique(y$condition)) > MINNUM,
                       corr$p.value,
                       1)
        
        tibble(
          'rloop_id' = nx,
          'corr' = corrs,
          'pval' = pval
        )
        
      }) %>% bind_rows()
    }
  ) 
  
  xadj <- xtest %>%
    pluck(1) %>%
    filter(corr != 0) %>%
    mutate(corrpadj = p.adjust(pval, method = "fdr")) %>%
    dplyr::rename(corrpval = pval)
    
  rlExpCondLvlByRL <- left_join(rlExpCondLvlByRL, xadj, by = "rloop_id")
  
  # Clean up
  rmap_sampsCond <- rmap_samps %>%
    filter(! is_rnh_like) %>%
    mutate(condition = paste0(study_id, "_", tissue, "_", treatment)) %>%
    select(condition, tissue, genotype, treatment, mode, study_id) %>%
    unique()
  rlExpCondLvlByRL <- rlExpCondLvlByRL %>%
    left_join(
      rmap_sampsCond
    )
  rlExpCondLvlByGene <- rlExpCondLvlByGene %>%
    left_join(
      rmap_sampsCond
    )
  rltabShow <- rltabShow %>%
    left_join(
      rlExpCondLvlByRL %>% select(rloop_id, corr, corrpval, corrpadj) %>% unique(),
      by = c("RL Region" = "rloop_id")
    )
  
  save(rltab, rltabShow, rlExpCondLvlByGene, rlExpCondLvlByRL, annoGenes, file = "data/rltab.rda", compress = "xz")
} else {
  if (! all(c("rltab", "rltabShow", "rlExpCondLvlByGene", "rlExpCondLvlByRL", "annoGenes") %in% names(globalenv()))) {
    load("data/rltab.rda")
  }
}

  

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


