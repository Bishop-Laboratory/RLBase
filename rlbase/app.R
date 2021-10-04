# Libraries
library(shiny)
library(RLSeq)
library(RLHub)
library(DT)
library(shinyvalidate)
library(aws.s3)
library(future)
library(pbapply)
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
} 
load(APP_DATA)
rlMemMat <- as.matrix(rlMembershipMatrix)  # Decompress
rlsamples <- rlsamples %>%
  mutate(rlsampleLink = map_chr(rlsample, makeSRALinks),
         study = map_chr(study, makeSRALinks),
         PMID= map_chr(PMID, makePubMedLinks))
source("const.R")
source("ui_globals.R")
source("plots.R")
rltabShow <- rlregions %>%
  arrange(desc(nStudies), desc(nModes), desc(pct_case)) %>%
  mutate(avgSignalVal = signif(avgSignalVal, 4),
         avgQVal = signif(10^(-1*avgQVal), 4)) %>%
  select(`RL Region` = rlregion, Location = location, `# of Studies` = nStudies,
         `# of Modes` = nModes, `Mean Signal` = avgSignalVal, `Mean FDR` = avgQVal,
         `# of Samples` = nSamples, `# of Tissues` = nTissues, `Source type`=source,
         contains("corr"), allGenes, mainGenes, is_repeat, samples) %>%
  mutate(allGenes = gsub(allGenes, pattern = ",", replacement = " ", perl = TRUE),
         mainGenes = gsub(mainGenes, pattern = ",", replacement = " ", perl = TRUE),
         Location = gsub(Location, pattern = ":\\.$", replacement = ""))

# Define UI for application that draws a histogram
ui <- function(request) {
  tagList(
    tags$head(tags$style(HTML(headerHTML()))), # For sticky footer    
    navbarPage(
      title = "RLBase",
      id = "rlbase",
      theme = bslib::bs_theme(bootswatch = "flatly"),
      tabPanel(title = "Home", id = "home-tab", value = "aboutTab", icon = icon("home"),
               fluidPage(
                 br(), 
                 includeHTML("www/home.html"),
               )),
      tabPanel(title = "Samples", id = "samples-tab", icon = icon('vials'),
               SamplesPageContents(rlsamples)),
      tabPanel(title = "R-Loop Regions", id = "rloops-tab", icon = icon('map'),
               RLoopsPageContents()),
      tabPanel(title = "Analyze", id = "analyze-tab", icon = icon('server'),
               AnalyzePageContents(rlsamples)),
      tabPanel(title = "Download", id = "download-tab", icon = icon('download'),
               DownloadPageContents(bucket_sizes, rlsamples)),
      tabPanel(title = "Documentation", id = "docs-tab", icon = icon('file-alt'),
               tags$iframe(src = './documentation.html', width = '100%', height = '800px',
                           frameborder = 0, scrolling = 'auto'))
    ), 
    tags$footer(HTML(footerHTML()))
  )
}


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ### Sample Page ###
  rmapSampsRV <- reactive({
    rlsamples %>%
      filter(.data$genome == input$selectGenome,
             .data$label != "NEG" | input$select_label_NEG,
             .data$prediction != "NEG" | input$select_prediction_NEG,
             .data$mode %in% input$selectMode) %>%
      pull(rlsample)
  })

  current_samp <- reactive({
    # Get selected row from datatable
    selectedRow <- ifelse(is.null(input$rmapSamples_rows_selected),
                          1,
                          input$rmapSamples_rows_selected)
    current_samp <- rlsamples %>%
      filter(rlsample %in% rmapSampsRV()) %>%
      filter(row_number() == selectedRow) %>%
      pull(rlsample)
    current_samp
  })
  
  current_gen <- reactive(rlsamples$genome[rlsamples$rlsample == current_samp()])
  output$rmapSamples <- renderDT(server = FALSE, {
      rlsamples %>%
        filter(rlsample %in% rmapSampsRV()) %>%
        select(Sample=rlsampleLink, Study=study, Mode = mode, Tissue=tissue, Condition = condition,
               PMID, prediction = prediction, Genome = genome, Genotype = genotype, Other = other) %>%
        datatable(selection = list(mode = "single", selected = 1), rownames = FALSE, escape = FALSE,
                  options = list(pageLength = 10, scrollX = TRUE))
    }) %>% bindCache(rmapSampsRV())
  
  ## Summary panel ##
  
  # Sample summary
  output$sampleSummary <- renderUI({
    currentrlsample <- rlsamples[rlsamples$rlsample == current_samp(),] 
    currentrlsample <- mutate(currentrlsample, across(contains("_S3"), function(x) {
      paste0("<a href='", file.path(RLSeq:::RLBASE_URL, x), "' target='_blank' >",
             gsub(x, pattern = ".+/", replacement = ""), "</a>")
    }))
    fluidRow(
      column(
        width = 8, offset = 2,
        tribble(
          ~Feature, ~Value, ~message,
          "Mode", currentrlsample$mode, "The type of R-loop mapping used in this sample. See the documentation for more info.",
          "Condition", currentrlsample$condition, "The condition of the sample. See the documentation for more info.",
          "Label", paste0("<strong style='color: ",
                          ifelse(currentrlsample$label == "POS", "#165566", "#8a2c2c"),"'>", 
                          currentrlsample$label, "</strong>"), "The label 'POS' or 'NEG' referencing whether the sample was expected to map R-loops.",
          "Prediction", paste0("<strong style='color: ",ifelse(currentrlsample$prediction == "POS", "#165566", "#8a2c2c"), "'>",
                               currentrlsample$label, "</strong>"), "The prediction ('POS' or 'NEG') made by the quality model (see details).",
          "RLSeq report", currentrlsample$report_html_s3, "The HTML analysis report for this sample, provided by the RLSeq R package."
        ) %>%
          mutate(Details = cell_spec('<i class="fa fa-question-circle"></i>', tooltip = spec_tooltip(title = message), escape = FALSE)) %>%
          dplyr::select(-message) %>%
          knitr::kable("html", escape = FALSE) %>%
          kable_styling("hover", full_width = F) %>%
          add_header_above(set_names(3, nm = currentrlsample$rlsample), align = "left") %>%
          HTML()
      )
    )
  })
  
  # Update the plot_ly plots
  donuts <- reactive({
    agsmall <- RLSeq:::available_genomes %>% select(genome=UCSC_orgID, Organism=organism)
    rlsamplesNow <- rlsamples %>% filter(rlsample %in% rmapSampsRV())
    modeDat <-  dplyr::mutate(rlsamplesNow, Mode = ifelse(mode %in% RLSeq:::auxdata$mode_cols$mode, mode, "misc")) %>%
      group_by(Mode) %>% tally()
    labelDat <- dplyr::rename(rlsamplesNow, Label = label) %>% group_by(Label) %>% tally()
    predDat <- dplyr::rename(rlsamplesNow, Prediction = prediction) %>% group_by(Prediction) %>% tally()
    datList <- list("Mode"=modeDat, "Label"=labelDat, "Prediction"=predDat)
    pltLst <- lapply(names(datList), function(dat) {
      dataNow <- datList[[dat]]
      plot_ly(type = "pie") %>%
        add_pie(data=dataNow, labels = dataNow[,dat,drop=T], values = ~n, textinfo='label+value',
                marker = list(colors = heatData$cat_cols[[tolower(dat)]][dataNow[,dat,drop=T]]), 
                insidetextorientation='horizontal', hole=.6, rotation=250) %>%
        layout(showlegend = FALSE, title=list(text = dat, x=0.15), margin = list(l = 50, r = 50),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    names(pltLst) <- names(datList)
    pltLst
  }) %>% bindCache(rmapSampsRV())
  
  # Donuts
  output$modeDonut <- renderPlotly(donuts()$Mode) %>% bindCache(donuts())
  output$labelDonut <- renderPlotly(donuts()$Label) %>% bindCache(donuts())
  output$predictionDonut <- renderPlotly(donuts()$Prediction) %>% bindCache(donuts())
  
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
  featPlotDataNow <- reactive({
    featPlotData[[input$selectGenome]]$prediction <- lapply(featPlotData[[input$selectGenome]]$prediction, function(x) {
      x[x$experiment %in% rmapSampsRV(),]
    })
    featPlotData
  })
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
          x = featPlotDataNow()[[current_gen()]]$prediction[[plt]],
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
    pheatmap(corrRes, color = heatData$pheatmap_color, main = current_samp(), breaks = heatData$pheatmap_breaks,
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
      guides(colour = guide_legend(override.aes = list(size=4), ncol = 1),
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
    rltabNow <- rltabShow[rltabShow$`RL Region` %in% to2show,]

    # Show the R-loops within that sample
    if (! input$showRepSamp) rltabNow <- dplyr::filter(rltabNow, ! is_repeat)
    if (input$showCorrSamp) rltabNow <- filter(rltabNow, ! is.na(corrR) & corrPVal < .05) %>% arrange(corrPAdj)
    rltabNow$Genes <- rltabNow$mainGenes
    if (input$showAllGenesRLSamp) rltabNow$Genes <- rltabNow$allGenes
    rltabNow %>% select(-samples) %>%
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
  
  ### RLoops Page ###
  
  # Get RLoops dataset
  rloops <- reactive({
    rltabShowNow <- mutate(rltabShow, across(contains("corr"), ~ signif(.x, digits = 4)))
    rltabNow <- select(rltabShowNow, -allGenes) %>% rename(Genes = mainGenes)
    if (input$showAllGenesRL) rltabNow <- select(rltabShowNow, -mainGenes) %>% rename(Genes = allGenes)
    if (! input$showRep) rltabNow <- filter(rltabNow, ! is_repeat)
    if (input$showCorr) rltabNow <- filter(rltabNow, ! is.na(corrR) & corrPVal < .05) %>% arrange(corrPAdj)
    rltabNow
  }) %>% bindCache(input$showAllGenesRL, input$showRep, input$showCorr)

  # Make DataTable
  output$rloops <- renderDT({
    relocate(rloops(), Genes, .after = Location) %>% select(-samples)
  }, rownames = FALSE, escape = FALSE, selection = list(mode = "single", selected = 1),
  options = list(pageLength = 6, scrollX = TRUE))

  # Current selected RL from DT
  current_rl <- reactive({
    selectedRow <- ifelse(is.null(input$rloops_rows_selected), 1, input$rloops_rows_selected)
    filter(rloops(), row_number() == selectedRow) %>% pull(`RL Region`)
  }) %>% bindCache(rloops(), input$rloops_rows_selected)

  # Make summary page
  NA_LINK <- "<a href=\"https://www.genecards.org/cgi-bin/carddisp.pl?gene=NA\" target=\"_blank\">NA</a>"
  output$RLoopsSummary <- renderUI({
    rloopsNow <- filter(rloops(), `RL Region` == current_rl()) %>%
      mutate(Genes = map_chr(Genes, function(x) {paste0(sapply(unique(unlist(strsplit(Genes, split = " "))), makeGeneCards), collapse = "\n")})) %>%
      mutate(Genes = ifelse(Genes == NA_LINK, NA, Genes)) %>%
      mutate(Samples = map_chr(Genes, function(x) {paste0(sapply(unique(unlist(strsplit(samples, split = ","))), makeSRALinks), collapse = "\n")})) %>%
      select(-is_repeat, -samples, -contains("corr"))
    mutate(rloopsNow, Location = makeRLConsensusGB(Location)) %>% t() %>%
      kableExtra::kbl(format = "html", escape = FALSE) %>%
      kableExtra::kable_styling() %>% HTML()
  })

  output$RLvsExpbySample <- renderPlot({
    rloopsNow <- filter(rloops(), `RL Region` == current_rl())
    # Get the corr and pval
    corrR <- pull(rloopsNow, "corrR") %>% signif(4)
    corrPAdj <- pull(rloopsNow, "corrPAdj") %>% signif(4)
    # Get the color
    dplyr::filter(tpm_rl_exp, rlregion == current_rl()) %>%
      inner_join(dplyr::filter(rlsamples, prediction == "POS"), by = "exp_matchCond") %>%
      dplyr::rename(Other = other, Mode = mode, Tissue = tissue,
        Study = study, Condition = condition) %>%
      ggplot(aes_string(x = "exp", y = "rl", color = "Mode")) +
      geom_point() +
      ggtitle(current_rl(), subtitle = "Expression vs. R-Loop Intensity") +
      theme_bw(base_size = 14) +
      annotate(geom = 'text',
               label = paste0("Rho: ", corrR, "; Padj: ", corrPAdj),
               x = -Inf, y = Inf, hjust = -.20, vjust = 3)
  }) %>% bindCache(current_rl(), rloops())
  
  ### User-submit sample ###
  
  # Form validation
  iv <- InputValidator$new()
  iv$add_rule("userGenome", sv_required())
  iv$add_rule("userPeaks", sv_required())
  iv$add_rule("privacyStatement", sv_equal(TRUE))
  iv$enable()
  
  # Form check
  observeEvent(input$userUpload, {
    
    print(input$privacyStatement)
    print(input$userTitle)
    print(input$userGenome)
    print(input$userMode)
    print(input$userLabel)
    print(input$userPeaks)
    print(input$userEmail)
    print(input$privacyStatement)
    
    validate(
      need(input$userGenome, message = "No  entered."),
      need(input$userPeaks, message = "No peaks provided."),
      need(input$privacyStatement, message = "Privacy Agreement not acknowledged.")
    )
    shinyWidgets::sendSweetAlert(session = session, title = "Running.", html = TRUE,  type = "info",
                                 text = "You report will be available here when ready: www.google.com")
    
    
  })
  
  ### Downloads ###
  output$rlsamplesDownloadFiles <- renderDT({
    rlsamples %>% 
      select(Sample=rlsampleLink, Study=study, Mode = mode, Tissue=tissue, Condition = condition,
             PMID, prediction = prediction, Genome = genome, Genotype = genotype, Other = other,
             peaks_s3, coverage_s3, rlranges_rds_s3,
             report_html_s3, fastq_stats_s3, bam_stats_s3) %>%
      dplyr::rename(
        "Peaks (.broadPeak)"=peaks_s3,
        "Coverage (.bw)"=coverage_s3,
        "RLRanges (.rds)"=rlranges_rds_s3,
        "RLSeq report (.html)"=report_html_s3,
        "FASTQ stats (.json)"=fastq_stats_s3,
        "BAM stats (.txt)"=bam_stats_s3
      ) %>%
      mutate(across(contains("("), function(x) {
        paste0("<a href='", file.path(RLSeq:::RLBASE_URL, x),
               "' target='_blank' download><i class='fa fa-download'></i>  ",
               toupper(gsub(basename(x), pattern = "^.+\\.([a-zA-Z0-9]+)$",
                    replacement = "\\1", perl = TRUE)), "</a>")
      })) %>%
      DT::datatable(rownames = FALSE, escape = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE),
                    selection = "none")
  })
  
}

# TODO: Need URL cleaner
# Run the application 
graphics.off()
shinyApp(ui, server, enableBookmarking = "url")
