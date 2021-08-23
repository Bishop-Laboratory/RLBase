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
set.seed(5)
names(modeCols) <- sample(modes, size = length(modes))

# Values for annotation heatmap
sampleCol <- c("grey", "firebrick")
names(sampleCol) <- c("", "selected")
sampleSize <- c(5, 15)
names(sampleSize) <- c("", "selected")
# ColList
colList <- list(
  "mode" = modeCols,
  "is_ctrl" = c("TRUE" = "#4d2121", "FALSE" = "#c4c4c4"),
  "pred_ctrl" =  c("TRUE" = "#264157", "FALSE" = "#c4c4c4"),
  "sample" = sampleCol
)

sizeList <- list(
  "sample" = sampleSize
)

# Violin colors for annoPlot
vCols <- list(
  "gene" = "#EBC7C7",
  "RNA" = "#b3a150",
  "rep" = "#71b350"
)
boxCols <- list(
  "gene" = "#CE7474",
  "RNA" = "#a18600",
  "rep" = "#36a100"
)
annoFillSplit <- list(
  "is_ctrl" = c("TRUE" = "#d483a3", "FALSE" = "#c0d483"),
  "pred_ctrl" = c("TRUE" = "purple", "FALSE" = "orange")
)

#' Scatter plots for RMapDB
#' @param ... additional arguments to geom_point()
rmap_scatter <- function(colorBy, sizeBy, ...) {
  pltLst <- list(
    geom_point(...),
    theme_prism(base_size = 18),
    scale_size_manual(values = sizeList[["sample"]]),
    theme(legend.key.width = unit(3, 'cm'),
          legend.title = element_text(size=18),
          legend.key.height = unit(1, "cm"),
          legend.text = element_text(size=14))
  )
  
  if (! is.null(colorBy) & colorBy %in% names(colList)) {
    print(colList[[colorBy]])
    # Get colors
    pltLst <- c(pltLst, 
                list(scale_color_manual(values = colList[[colorBy]])))
  }
  
  pltLst
}

