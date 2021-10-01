# Libraries
library(shiny)
library(RLSeq)
library(RLHub)
library(DT)
library(tidyverse)
library(kableExtra)
library(plotly)
library(pheatmap)
library(ggprism)
library(bslib)
library(RColorBrewer)

# Get constants
source("utils.R")
APP_DATA <- "misc/app_data.rda"
if (! file.exists(APP_DATA)) {
  makeGlobalData(APP_DATA)
  load(APP_DATA)
} else {
  load(APP_DATA)
  rlMemMat <- as.matrix(rlMembershipMatrix)  # Decompress
}
source("const.R")
source("ui_globals.R")
source("plots.R")


# Define UI for application that draws a histogram
ui <- function(request) {
  tagList(
    # For stick footer    
    tags$head(
      tags$style(
        HTML(headerHTML())
      )
    ),
    navbarPage(
      title = "RLBase",
      id = "rlbase",
      theme = bslib::bs_theme(bootswatch = "flatly"),
      
      # Pages
      tabPanel(
        title = "Home", 
        id = "home-tab",
        value = "aboutTab",
        icon = icon("home"),
        fluidPage(
          br(),
          includeHTML("www/home.html")
        )
      ),
      tabPanel(
        title = "Samples",
        id = "samples-tab",
        icon = icon('vials'),
        SamplesPageContents()
      ),
      tabPanel(
        title = "R-Loop DB",
        id = "rloops-tab",
        icon = icon('database'),
        RLoopsPageContents()
      ),
      tabPanel(
        title = "Download",
        id = "download-tab",
        icon = icon('download'),
        DownloadPageContents()
      ),
      tabPanel(
        title = "Documentation",
        id = "docs-tab",
        icon = icon('file-alt'),
        # From https://stackoverflow.com/questions/43393310/include-markdown-with-options-in-shiny
        tags$iframe(src = './documentation.html', # put myMarkdown.html to /www
                    width = '100%', height = '800px',
                    frameborder = 0, scrolling = 'auto'
        )
      )
    ),
    tags$footer(
      HTML(footerHTML())
    )
  )
}


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # ### Sample Page ###
  
  # # TODO: Should be sorted
  # # TODO: Should contain link that will open the details modal
  # # TODO: Should include UI to add hyperlink for clicking SRX
  # # TODO: Should have better color for the selection of rows
  rmapSampsRV <- reactive({
    rlsamples %>%
      filter(.data$genome == input$selectGenome,
             .data$label != "NEG" | input$select_label_NEG,
             .data$prediction != "NEG" | input$select_prediction_NEG,
             .data$mode %in% input$selectMode) %>%
      pull(rlsample)
  })
  observe({
    print(input$select_label_NEG)
  })

  current_samp <- reactive({
    # Get selected row from datatable
    selectedRow <- ifelse(is.null(input$rmapSamples_rows_selected),
                          1,
                          input$rmapSamples_rows_selected)

    # Get current sample ID
    current_samp <- rlsamples %>%
      filter(rlsample %in% rmapSampsRV()) %>%
      filter(row_number() == selectedRow) %>%
      pull(rlsample)

    current_samp
  })
  
  current_gen <- reactive({
    rlsamples$genome[rlsamples$rlsample == current_samp()]
  })
  
  output$rmapSamples <- renderDT(server = FALSE, {
      rlsamples %>%
        filter(rlsample %in% rmapSampsRV()) %>%
        select(Sample=rlsample, Study=study, Mode = mode, Tissue=tissue, Condition = condition,
               prediction = prediction, Genome = genome, Genotype = genotype, Other = other) %>%
        datatable(selection = list(mode = "single", selected = 1), rownames = FALSE,
                  options = list(pageLength = 10, scrollX = TRUE))
    }) %>% bindCache(rmapSampsRV())

  
  ## Panel for RLFS analysis ##
  
  # Z-score plot
  output$zScorePlot <- renderPlot({
    plotRLFSRes(rlfsres[[current_samp()]]$rlfsData, plotName = current_samp())
  }) %>% bindCache(current_samp())

  # Z-score plot (FFT)
  output$FFTPlot <- renderPlot({
    plotRLFSRes(rlfsres[[current_samp()]]$rlfsData, plotName = current_samp(), fft = TRUE)
  }) %>% bindCache(current_samp())

  # P-val plot
  output$pValPlot <- renderPlot({
    regioneR:::plot.permTestResults(rlfsres[[current_samp()]]$rlfsData$perTestResults[["regioneR::numOverlaps"]])
  }) %>%  bindCache(current_samp())

  # HTML summary
  output$RLFSOutHTML <- renderUI({
    rlfsRes <- rlfsres %>% pluck(current_samp(), "rlfsData")
    list(rlfs_pval=-log10(rlfsRes$perTestResults$`regioneR::numOverlaps`$pval),
         MACS2__total_peaks=rlsamples$numPeaks[rlsamples$rlsample == current_samp()],
         label=rlsamples$label[rlsamples$rlsample == current_samp()],
         condition=rlsamples$condition[rlsamples$rlsample == current_samp()],
         prediction=rlsamples$prediction[rlsamples$rlsample == current_samp()]) %>% RLFSTagList()
  })
  
  ## Annotation plots ##
  annodbs <- reactive({
    names(featPlotData[[input$selectGenome]]$none)
  })
  output$annoPlots <- renderUI({
    tabs <- lapply(annodbs(), function(plt) {
      tabPanel(title = plt, br(), plotOutput(outputId = plt))
    })
    do.call(tabsetPanel, c(tabs, id="tabAnno"))
  })
  observe({
    lapply(annodbs(), function(plt) {
      output[[plt]] <- renderPlot({
        RLSeq:::feature_ggplot(
          x = featPlotData[[current_gen()]][[input$splitby]][[plt]],
          limits = c(-10, 15), 
          splitby = input$splitby,
          usamp = current_samp()
        )
      })
    })
  })
  
  
  ## Summary page ##
  
  # Heatmap
  output$heatmap <- renderPlot({
    toshow <- rmapSampsRV()[which(rmapSampsRV() %in% rownames(heatData$corrRes))]
    corrRes <- heatData$corrRes[toshow, toshow]
    annoCorr <- heatData$annoCorr[toshow,]
    annoCorr$group <- ifelse(rownames(annoCorr) == current_samp(), current_samp(), "Unselected")
    names(heatData$cat_cols$group)[1] <- current_samp()
    names(heatData$cat_cols$group)[2] <- "Unselected"
    pheatmap(corrRes, color = heatData$pheatmap_color, breaks = heatData$pheatmap_breaks,
             annotation_col = annoCorr[,c(4, 3, 2, 1)], annotation_colors = heatData$cat_cols,
             show_colnames = FALSE, show_rownames = FALSE, silent = FALSE, fontsize = 15)
  }) %>% bindCache(rmapSampsRV(), current_samp())
  
  # PCA
  output$rmapPCA <- renderPlot({
    # Filter for current samples selected
    toshow <- rmapSampsRV()[which(rmapSampsRV() %in% rownames(heatData$corrRes))]
    corrRes <- heatData$corrRes[toshow, toshow]
    annoCorr <- heatData$annoCorr[toshow,]
    annoCorr$group <- ifelse(rownames(annoCorr) == current_samp(), "Selected", "Unselected")
    # Get the PCA data
    pcd <- pcaPlotDataFromCorr(corrRes)
    toPlt <- pcd[["pcData"]] %>%
      right_join(rownames_to_column(annoCorr, var = "rlsample"), by = "rlsample")
    ggplot(
      toPlt,
      aes_string(x = "PC1", y = "PC2", color = input$PCA_colorBy,
                 shape = input$PCA_shapeBy, size = "group")
    ) +
      rlbase_scatter(sizes = c("Selected" = 10, "Unselected" = 3),
                     cols = heatData$cat_cols$mode,
                     shapes = c("POS" = 19, "NEG" = 4)) +
      guides(colour = guide_legend(override.aes = list(size=4), ncol = 2),
             shape = guide_legend(override.aes = list(size=4))) +
      xlab(paste0("PC1 (", pcd$percentVar[1], "%)")) +
      ylab(paste0("PC2 (", pcd$percentVar[2], "%)")) +
      ggtitle("RLBase PCA Plot", subtitle = current_samp())
  }) %>%
    bindCache(rmapSampsRV(), current_samp(), input$PCA_shapeBy, input$PCA_colorBy)

  ## R-loop summary ##
  
  # R-loop table
  output$RLoopsPerSample <- renderDT({

    # Get the R-loops for the current sample
    sampRLMem <- rlMemMat[,current_samp()]
    to2show <- names(sampRLMem[which(sampRLMem)])
    rltabNow <- rlregions[rlregions$rlregion %in% to2show,]

    # Show the R-loops within that sample
    if (! input$showRepSamp) rltabNow <- dplyr::filter(rltabNow, ! is_repeat)
    if (input$showCorrSamp) rltabNow <- filter(rltabNow, ! is.na(corrR)) %>% arrange(corrPAdj)
    rltabNow$Genes <- rltabNow$mainGenes
    if (input$showAllGenesRLSamp) rltabNow$Genes <- rltabNow$allGenes
    rltabNow %>%
      select(`RL Region` = rlregion, Location = location, Genes, `Mean Signal` = avgSignalVal,
             `Mean FDR` = avgQVal, `# of Studies` = nStudies, `# of Samples` = nSamples,
             `# of Tissues` = nTissues, `# of Modes` = nModes) %>% 
      relocate(Genes, .after = Location) %>% arrange(desc(`Mean FDR`)) %>%
      DT::datatable(extensions = 'Buttons', selection = list(mode = "none"), rownames = FALSE,
                    options = list(scrollX = TRUE, server=FALSE, pageLength = 6))
  })
  
  ## Downloads ##
  output$downloadsForSample <- function() {
    # From https://cran.r-project.org/web/packages/kableExtra/vignettes/use_kable_in_shiny.html
    currentrlsample <- rlsamples[rlsamples$rlsample == current_samp(),] 
    currentrlsample <- mutate(currentrlsample, across(contains("_S3"), function(x) {
      paste0("<a href='", file.path(RLSeq:::RLBASE_URL, x), "' target='_blank' >",
             gsub(x, pattern = ".+/", replacement = ""), "</a>")
    }))
    tribble(
      ~Item, ~URL,
      "Peaks (.broadPeak)", currentrlsample$peaks_s3,
      "Coverage (.bw)", currentrlsample$coverage_s3,
      "RLSeq RLRanges object (.rds)", currentrlsample$rlranges_rds_s3,
      "RLSeq report (.html)", currentrlsample$report_html_s3,
      "FASTQ stats from fastp (.json)", currentrlsample$fastq_stats_s3,
      "BAM stats from samtools (.txt)", currentrlsample$bam_stats_s3
    ) %>%
      knitr::kable("html", escape = FALSE) %>%
      kable_styling("hover", full_width = F) %>%
      add_header_above(set_names(2, nm = current_samp()), align = "left")
  }
  
  # ### RLoops Page ###
  # # Get RLoops dataset
  # rloops <- reactive({
  #   rltabShowNow <- rltabShow %>%
  #     mutate(
  #       across(
  #         corr:corrpadj, ~ signif(.x, digits = 4)
  #       )
  #     ) 
  #   
  #   if (input$showAllGenesRL) {
  #     rltabNow <- rltabShowNow %>%
  #       select(-Genes, -GenesFix) %>%
  #       dplyr::rename(Genes = GenesNat)
  #   } else {
  #     rltabNow <- rltabShowNow %>%
  #       select(-Genes, -GenesNat) %>%
  #       dplyr::rename(Genes = GenesFix) 
  #   }
  #   
  #   if (! input$showRep) {
  #     rltabNow <- filter(rltabNow, ! repeats)
  #   }
  #   
  #   if (input$showCorr) {
  #     rltabNow <- rltabNow %>%
  #       filter(! is.na(corr)) %>%
  #       arrange(corrpadj) 
  #   }
  #   
  #   rltabNow
  # }) %>% bindCache(input$showAllGenesRL, input$showRep, input$showCorr)
  # 
  # # Make DataTable
  # output$rloops <- renderDT({
  #   rloops() %>%
  #     select(-Type, -Modes, -`Mean RLCounts`, -repeats) %>%
  #     relocate(Genes, .after = Location) 
  # }, rownames = FALSE, escape = FALSE, 
  # selection = list(mode = "single",
  #                  selected = 1),
  # options = list(
  #   pageLength = 5,
  #   scrollX = TRUE
  # )) 
  # 
  # # Current selected RL from DT
  # current_rl <- reactive({
  #   # Get selected row from datatable
  #   selectedRow <- ifelse(is.null(input$rloops_rows_selected), 
  #                         1, 
  #                         input$rloops_rows_selected)
  #   
  #   # Get current sample
  #   rloops() %>%
  #     filter(row_number() == selectedRow) %>%
  #     pull(`RL Region`)
  # }) %>%
  #   bindCache(rloops(), input$rloops_rows_selected)
  # 
  # # Make summary page
  # NA_LINK <- "<a href=\"https://www.genecards.org/cgi-bin/carddisp.pl?gene=NA\" target=\"_blank\">NA</a>"
  # output$RLoopsSummary <- renderUI({
  #   rloopsNow <- rloops() %>%
  #     filter(`RL Region` == current_rl()) %>%
  #     left_join(
  #       select(
  #         rltab, `RL Region`=id, confidence_level, is_rlfs, samples, genes
  #       ),
  #       by = "RL Region"
  #     ) %>%
  #     mutate(Genes = map_chr(
  #       Genes, function(x) {
  #         paste0(sapply(unique(unlist(strsplit(Genes, split = "\n"))), makeGeneCards), collapse = "\n")
  #       }
  #     )) %>%
  #     mutate(Genes = ifelse(Genes == NA_LINK, NA, Genes)) %>%
  #     select(-genes) %>%
  #     relocate(Genes, .before = Type) %>%
  #     select(-`Mean RLCounts`, -repeats, -corrpval, -Type)
  #   rloopsNow %>%
  #     mutate(Location = makeRLConsensusGB(Location)) %>%
  #     mutate(samples = paste0(unique(unlist(samples)), collapse = "\n")) %>%
  #     t() %>%
  #     kableExtra::kbl(format = "html", escape = FALSE) %>%
  #     kableExtra::kable_styling() %>%
  #     HTML()
  # })
  # 
  # output$RLvsExpbySample <- renderPlot({
  #   rloopsNow <- rloops() %>%
  #     filter(`RL Region` == current_rl())
  #   
  #   # Get the corr and pval
  #   corr <- pull(rloopsNow, "corr") %>% round(4)
  #   corrpadj <- pull(rloopsNow, "corrpadj") %>% round(4)
  #   
  #   # Get the color
  #   rlExpCondLvlByRL %>%
  #     dplyr::filter(rloop_id == current_rl()) %>%
  #     dplyr::rename(
  #       Treatment = treatment,
  #       Mode = mode,
  #       Tissue = tissue,
  #       Study = study_id,
  #       Condition = condition
  #     ) %>%
  #     ggplot(aes_string(x = "vst", y = "qVal", color = "Mode")) +
  #     geom_point() +
  #     ggtitle(current_rl(), subtitle = "Expression vs. R-Loop Intensity") +
  #     theme_bw(base_size = 14) +
  #     annotate(geom = 'text', 
  #              label = paste0("Rho: ", corr, "; Padj: ", corrpadj), 
  #              x = -Inf, y = Inf, hjust = -.20, vjust = 3)
  #   
  # }) %>% bindCache(current_rl(), rloops())
}

# TODO: Need URL cleaner
# Run the application 
graphics.off()
shinyApp(ui, server, enableBookmarking = "url")
