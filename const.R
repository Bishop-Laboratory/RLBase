library(shiny)
library(DT)
library(plotly)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(ggplot2)
library(bslib)
library(RColorBrewer)

# Get the data
if (! "dataLst" %in% names(globalenv())) {
  load('dataLst.rda')
}

# Get corr dataset & wrangle
torm <- "ERX2277510_E-MTAB-6318DRIP_mOHT"
load(paste0("misc/report_rda/", torm, "_hg38.QC_report.rda"))
keep <- which(! colnames(data_list$corr_data$corMat) %in% torm)
corr_data <- data_list$corr_data$corMat[keep, keep]
annoCorr <- data_list$corr_data$annoNow[colnames(corr_data),]
newlabs <- gsub(rownames(annoCorr), pattern = ".+_([ES]{1}RX[0-9]+)$", replacement = "\\1")
rownames(annoCorr) <- colnames(corr_data) <- rownames(corr_data) <- newlabs
annoCorr <- annoCorr %>%
  rownames_to_column(var = "id") %>%
  select(-Source) %>%
  left_join(
    dataLst %>%
      pluck("rmap_samples") %>% 
      mutate(isControl = is_rnh_like | is_input) %>%
      select(id, isControl)
  ) %>%
  column_to_rownames(var = "id") %>%
  mutate(isControl = ifelse(is.na(isControl), TRUE, isControl))

# Get annotations
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

# Mode colors
# From https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
modes <- dataLst %>%
  pluck("rmap_samples") %>%
  pull(mode) %>%
  unique() 
modeCols <- gg_color_hue(
  length(
    modes
  )
)
set.seed(42); names(modeCols) <- sample(modes, size = length(modes))

# Values for annotation heatmap
heatSampCol <- c("grey", "firebrick")
names(heatSampCol) <- c("", "selected")
pheatColLst <- list(
  "sample" = heatSampCol,
  "Mode" = modeCols,
  "isControl" = c("TRUE" = "forestgreen", "FALSE" = "grey")
)

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




