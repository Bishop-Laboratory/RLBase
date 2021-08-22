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
sampleCol <- c("grey", "firebrick")
names(sampleCol) <- c("", "selected")
sampleSize <- c(5, 15)
names(sampleSize) <- c("", "selected")
# ColList
colList <- list(
  "mode" = modeCols,
  "is_ctrl" = c("TRUE" = "forestgreen", "FALSE" = "grey"),
  "pred_ctrl" =  c("TRUE" = "goldenrod", "FALSE" = "grey"),
  "selected" = sampleCol
)

sizeList <- list(
  "sample" = sampleSize
)



#' Scatter plots for RMapDB
#' @param ... additional arguments to geom_point()
#' @example ggplot(mtcars, aes(x = wt, y = mpg, color = cyl)) + rmap_scatter("mode")
rmap_scatter <- function(colorBy, sizeBy, ...) {
  pltLst <- list(
    geom_point(...),
    theme_prism(base_size = 16),
    scale_size_manual(values = sizeList[["sample"]])
  )
  
  if (! is.null(colorBy) & colorBy %in% names(colList)) {
    # Get colors
    pltLst <- c(pltLst, 
                list(scale_color_manual(values = colList[[colorBy]])))
  }
  
  pltLst
}




