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


#' Scatter plots for RMapDB
#' @param ... additional arguments to geom_point()
#' @example ggplot(mtcars, aes(x = wt, y = mpg, color = cyl)) + rmap_scatter("mode")
rmap_scatter <- function(colorBy, ...) {
  
  # Get colors
  colorsNow <- switch(
    colorBy,
    "mode" = modeCols
  )
  
  list(
    geom_point(...),
    scale_color_manual(values = colorsNow)
  )
  
}




