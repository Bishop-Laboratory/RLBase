#' Scatter plots for RMapDB
#' @param ... additional arguments to geom_point()
rmap_scatter <- function(cols, ...) {
  pltLst <- list(
    geom_point(...),
    theme_prism(base_size = 18),
    scale_size_manual(values = sizeList[["sample"]]),
    theme(legend.key.width = unit(3, 'cm'),
          legend.title = element_text(size=18),
          legend.key.height = unit(1, "cm"),
          legend.text = element_text(size=14))
  )
  
  if (! is.na(cols)) {
    # Get colors
    pltLst <- c(pltLst, 
                list(scale_color_manual(values = cols,
                                        drop=TRUE)))
  }
  
  pltLst
}

corrHeatmapShiny <- function() {
  # Get dots -- get values used by RLBase
  # Provides RLBase with a speed boost by pre-supplying
  # data from memory when plotting. Not intended for regular use.
  dots <- list(...)
  selected <- NULL
  rlsamples <- NULL
  if (length(dots) > 0) {
    selected <- dots$selected
    rlsamples <- dots$rlsamples
  }
  
  # Get RLBase samples
  suppressMessages({
    if (is.null(rlsamples)) rlsamples <- RLHub::rlbase_samples() 
  })
  
  # Get the correlation matrix
  corrRes <- rlresult(object, resultName = "correlationMat")
  
  # Get the mode and prediction and label
  prediction <- rlresult(object, resultName = "predictRes")
  
  # Wrangle the annotation data
  rlsamples <- rlsamples[rlsamples$rlsample != object@metadata$sampleName,]
  annoCorr <- rlsamples %>%
    dplyr::mutate(group = "RLBase") %>%
    dplyr::select(
      .data$rlsample, .data$mode,
      # .data$label,
      .data$prediction, .data$group
    ) %>%
    dplyr::bind_rows(
      dplyr::tibble(
        rlsample = object@metadata$sampleName,
        mode = object@metadata$mode,
        # label = object@metadata$label,
        prediction = prediction$prediction,
        group = object@metadata$sampleName
      )
    ) %>%
    dplyr::distinct(.data$rlsample, .keep_all = TRUE)
  annoCorr <- as.data.frame(annoCorr)
  rownames(annoCorr) <- annoCorr$rlsample
  annoCorr <- annoCorr[, -which(colnames(annoCorr) == "rlsample")]
  
  # Filter for available / desired samples
  toSelect <- colnames(corrRes)
  if (!is.null(selected)) {
    toSelect <- intersect(selected, toSelect)
  }
  corrNow <- corrRes[toSelect, toSelect]
  annoCorr <- annoCorr[toSelect, ]
  
  # Pallete
  paletteLength <- 100
  myColor <- grDevices::colorRampPalette(
    rev(RColorBrewer::brewer.pal(n = 7, name = "RdBu"))
  )(paletteLength)
  # length(breaks) == length(paletteLength) + 1
  # use floor and ceiling to deal with even/odd length pallettelengths
  myBreaks <- c(
    seq(min(corrNow), 0, length.out = ceiling(paletteLength / 2) + 1),
    seq(max(corrNow) / paletteLength, max(corrNow),
        length.out = floor(paletteLength / 2)
    )
  )
  
  # Wrangle colors
  mode_cols <- auxdata$mode_cols$col
  names(mode_cols) <- auxdata$mode_cols$mode
  cond_cols <- auxdata$label_cols$col
  names(cond_cols) <- auxdata$label_cols$label
  verd_cols <- auxdata$prediction_cols$col
  names(verd_cols) <- auxdata$prediction_cols$prediction
  group_cols <- stats::setNames(c(
    auxdata$heat_cols$col[auxdata$heat_cols$selected == "user_selected"],
    auxdata$heat_cols$col[auxdata$heat_cols$selected == "RLBase"]
  ), nm = c(object@metadata$sampleName, "RLBase"))
  cat_cols <- list(
    "mode" = mode_cols,
    # "label" = c(cond_cols, "grey"),
    "prediction" = verd_cols,
    "group" = group_cols
  )
  cat_cols$mode <- cat_cols$mode[names(cat_cols$mode) %in% annoCorr$mode]
  
  # Build heatmap
  pheatmap::pheatmap(
    corrRes, 
    color = myColor, breaks = myBreaks,
    annotation_col = annoCorr[,c(3, 2, 1)], 
    annotation_colors = cat_cols,
    show_colnames = FALSE, 
    show_rownames = FALSE,
    silent = TRUE
  )
  
  return(hm)
}
