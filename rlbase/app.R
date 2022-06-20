# Libraries
library(shiny)
library(RLSeq)
library(RLHub)
library(DT)
library(shinyvalidate)
library(aws.s3)
library(shinycssloaders)
library(prompter)
library(shinyWidgets)
library(pbapply)
library(callr)
library(VennDiagram)
library(uuid)
library(tidyverse)
library(kableExtra)
library(plotly)
library(pheatmap)
library(ggprism)
library(bslib)
library(RColorBrewer)

# Increase upload size to 300MB
options(shiny.maxRequestSize=300*1024^2)
# Get constants
source("utils.R")
APP_DATA <- "misc/app_data.rda"
if (! file.exists(APP_DATA)) {
  makeGlobalData(APP_DATA)
} 
if (! "rlregions" %in% ls()) {
  load(APP_DATA)
}
rlMemMat <- as.matrix(rlMembershipMatrix)  # Decompress
rlsamples <- rlsamples %>%
  mutate(rlsampleLink = map_chr(rlsample, makeSRAExperimentLinks),
         study = map_chr(study, makeSRAStudyLinks),
         PMID= map_chr(PMID, makePubMedLinks))
source("const.R")
source("ui_globals.R")
source("plots.R")
BASE_URL1 <- "http://genome.ucsc.edu/s/millerh1%40livemail.uthscsa.edu/RLBase?position="
rltabShow <- rlregions %>%
  arrange(desc(confidence_score)) %>%
  mutate(avgSignalVal = signif(avgSignalVal, 4),
         avgQVal = 10^(-1*avgQVal),
         confidence_score = signif(confidence_score, 4)) %>%
  dplyr::select(`RL Region` = rlregion, Location = location,
                `Confidence Score` = confidence_score,
                `# of Studies` = nStudies,
         `# of Modes` = nModes, `Mean Signal` = avgSignalVal, `Mean FDR` = avgQVal,
         `# of Samples` = nSamples, `# of Tissues` = nTissues, `Source type`=source,
         contains("corr"), allGenes, mainGenes, is_repeat, samples) %>%
  mutate(allGenes = gsub(allGenes, pattern = ",", replacement = " ", perl = TRUE),
         mainGenes = gsub(mainGenes, pattern = ",", replacement = " ", perl = TRUE),
         Location = gsub(Location, pattern = ":\\.$", replacement = "")) %>%
  mutate(Location = paste0("<a href=\"", BASE_URL1, Location, "\" target=\"_blank\">", Location, "</a>"))

# Define UI for application that draws a histogram
ui <- function(request) {
  tagList(
    tags$head(
      tags$style(HTML(headerHTML())),
      tags$link(rel="shortcut icon", href="https://rlbase-data.s3.amazonaws.com/misc/assets/rlbase_icon.png"),
      tags$script(src="https://kit.fontawesome.com/5071e31d65.js", crossorigin="anonymous"),
      tags$link(rel="stylesheet", type="text/css", href="https://cdnjs.cloudflare.com/ajax/libs/cookieconsent2/3.1.1/cookieconsent.min.css")
    ),
    tags$body(
      tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/cookieconsent2/3.1.1/cookieconsent.min.js", `data-cfasync`="false"),
      tags$script(src="cookie_consent.js")
    ),
    use_prompt(),
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
               tags$iframe(src = './documentation.html', 
                           style = "display:block; width:100%; height:86vh;",
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
      dplyr::filter(.data$genome == input$selectGenome,
             .data$label != "NEG" | input$select_label_NEG,
             .data$prediction != "NEG" | input$select_prediction_NEG,
             .data$mode %in% input$selectMode) %>%
      pull(rlsample)
  })

  current_samp <- reactive({
    req(length(rmapSampsRV()) > 0)
    # Get selected row from datatable
    selectedRow <- ifelse(is.null(input$rmapSamples_rows_selected),
                          1,
                          input$rmapSamples_rows_selected)
    current_samp <- rlsamples %>%
      dplyr::filter(rlsample %in% rmapSampsRV()) %>%
      dplyr::filter(row_number() == selectedRow) %>%
      pull(rlsample)
    current_samp
  })
  
  output$rmapSamples <- renderDT(server = FALSE, {
    validate(
      need(length(rmapSampsRV()) > 0, message = "No samples available within selected criteria.")
    )
    rlsamples %>%
      dplyr::filter(rlsample %in% rmapSampsRV()) %>%
      dplyr::select(Sample=rlsampleLink, Study=study, Mode = mode, Tissue=tissue, Condition = condition,
             PMID, prediction = prediction, Genome = genome, Genotype = genotype, Other = other) %>%
      datatable(selection = list(mode = "single", selected = 1), rownames = FALSE, escape = FALSE,
                options = list(pageLength = 10, scrollX = TRUE))
    }) %>% bindCache(rmapSampsRV())
  
  ## Summary panel ##
  
  # Sample summary
  output$sampleSummary <- renderUI({
    validate(
      need(length(rmapSampsRV()) > 0, 
           message = "No samples available within selected criteria.")
    )
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
                               currentrlsample$prediction, "</strong>"), "The prediction ('POS' or 'NEG') made by the quality model (see details).",
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
    req(length(rmapSampsRV()) > 0)
    agsmall <- RLSeq:::available_genomes %>% dplyr::select(genome=UCSC_orgID, Organism=organism)
    rlsamplesNow <- rlsamples %>% dplyr::filter(rlsample %in% rmapSampsRV())
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
    req(! is.na(rlfsres[[current_samp()]]))
    plotRLFSRes(rlfsres[[current_samp()]]$rlfsData, plotName = current_samp())
  }) %>% bindCache(current_samp())

  # Z-score plot (FFT)
  output$FFTPlot <- renderPlot({
    req(! is.na(rlfsres[[current_samp()]]))
    plotRLFSRes(rlfsres[[current_samp()]]$rlfsData, plotName = current_samp(), fft = TRUE)
  }) %>% bindCache(current_samp())

  # P-val plot
  output$pValPlot <- renderPlot({
    req(! is.na(rlfsres[[current_samp()]]))
    regioneR:::plot.permTestResults(rlfsres[[current_samp()]]$rlfsData$perTestResults[["regioneR::numOverlaps"]])
  }) %>%  bindCache(current_samp())

  # HTML summary
  output$RLFSOutHTML <- renderUI({
    rlfsRes <- rlfsres %>% pluck(current_samp(), "rlfsData")
    validate(need(! is.na(rlfsRes), message = "RLFS results unavailable for this sample."))
    list(rlfs_pval=-log10(rlfsRes$perTestResults$`regioneR::numOverlaps`$pval),
         MACS2__total_peaks=rlsamples$numPeaks[rlsamples$rlsample == current_samp()],
         label=rlsamples$label[rlsamples$rlsample == current_samp()],
         condition=rlsamples$condition[rlsamples$rlsample == current_samp()],
         prediction=rlsamples$prediction[rlsamples$rlsample == current_samp()]) %>% RLFSTagList()
  })
  
  ## Annotation plots ##
  featPlotDataNow <- reactive({
    req(input$selectGenome %in% c("hg38", "mm10"))
    featPlotData[[input$selectGenome]]$prediction <- lapply(featPlotData[[input$selectGenome]]$prediction, function(x) {
      x[x$experiment %in% rmapSampsRV(),]
    })
    featPlotData
  })
  annodbs <- reactive({
    req(input$selectGenome %in% c("hg38", "mm10"))
    names(featPlotData[[input$selectGenome]]$none)
  })
  output$annoPlots <- renderUI({
    validate(
      need(input$selectGenome %in% c("hg38", "mm10"),
           message = "This feature is only available for 'hg38' and 'mm10'")
    )
    tabs <- lapply(annodbs(), function(plt) {
      tabPanel(title = plt, br(), plotOutput(outputId = plt))
    })
    do.call(tabsetPanel, c(tabs, id="tabAnno"))
  })
  observe({
    req(input$selectGenome %in% c("hg38", "mm10"))
    lapply(annodbs(), function(plt) {
      output[[plt]] <- renderPlot({
        RLSeq:::feature_ggplot(
          x = featPlotDataNow()[[input$selectGenome]]$prediction[[plt]],
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
    validate(
      need(input$selectGenome == "hg38", message = "This feature is only available for 'hg38' samples.")
    )
    toshow <- rmapSampsRV()[which(rmapSampsRV() %in% rownames(heatData$corrRes))]
    corrRes <- heatData$corrRes[toshow, toshow]
    annoCorr <- heatData$annoCorr[toshow,]
    annoCorr$Selected <- ifelse(rownames(annoCorr) == current_samp(), current_samp(), "Unselected")
    names(heatData$cat_cols$Selected)[1] <- current_samp()
    names(heatData$cat_cols$Selected)[2] <- "Unselected"
    pheatmap(corrRes, color = heatData$pheatmap_color, main = current_samp(), breaks = heatData$pheatmap_breaks,
             annotation_col = annoCorr[,c(4, 3, 2, 1)], annotation_colors = heatData$cat_cols,
             show_colnames = FALSE, show_rownames = FALSE, silent = FALSE, fontsize = 14)
  }) %>% bindCache(rmapSampsRV(), current_samp())
  
  # PCA
  output$rmapPCA <- renderPlot({
    validate(
      need(input$selectGenome == "hg38", message = "This feature is only available for 'hg38' samples.")
    )
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
      aes_string(x = "PC1", y = "PC2", color = "Mode",
                 alpha = "group",
                 shape = input$PCA_shapeBy, size = "group")
    ) +
      rlbase_scatter(sizes = c("Selected" = 10, "Unselected" = 2.5),
                     alphas = c("Selected" = 1, "Unselected" = 0.6),
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
  
  # R-loop venn
  output$rlVenn <- renderPlot({
    validate(
      need(input$selectGenome == "hg38", message = "This feature is only available for 'hg38' samples.")
    )
    
    futile.logger::flog.threshold(futile.logger::ERROR,
                                  name = "VennDiagramLogger")
    VennDiagram::venn.diagram(
      x = rlregionPltData[[current_samp()]]$pltdata,
      filename = NULL,
      main = "RL-Region Overlap",
      sub = rlregionPltData[[current_samp()]]$subtitle,
      fill = c("#9ad9ab", "#9aa0d9"),
      main.cex = 2,
      cat.pos = c(-60, 60),
      cat.dist=.02,
      margin = .1
    ) %>%
      grid::grid.draw() %>%
      ggplotify::grid2grob() %>%
      ggplotify::as.ggplot()
  }) %>% bindCache(current_samp())
  
  # R-loop table
  output$RLoopsPerSample <- renderDT({
    
    validate(
      need(input$selectGenome == "hg38", message = "This feature is only available for 'hg38' samples.")
    )

    # Get the R-loops for the current sample
    sampRLMem <- rlMemMat[,current_samp()]
    to2show <- names(sampRLMem[which(sampRLMem)])
    rltabNow <- rltabShow[rltabShow$`RL Region` %in% to2show,]

    # Show the R-loops within that sample
    if (! input$showRepSamp) rltabNow <- dplyr::filter(rltabNow, ! is_repeat)
    if (input$showCorrSamp) rltabNow <- dplyr::filter(rltabNow, ! is.na(corrR) & corrPVal < .05) %>% arrange(corrPAdj)
    rltabNow$Genes <- rltabNow$mainGenes
    if (input$showAllGenesRLSamp) rltabNow$Genes <- rltabNow$allGenes
    rltabNow <- dplyr::select(rltabNow, -allGenes, -mainGenes, -is_repeat)
    rltabNow %>% dplyr::select(-samples) %>% dplyr::relocate(Genes, .after = `Confidence Score`) %>%
      DT::datatable(extensions = 'Buttons', selection = list(mode = "none"), rownames = FALSE,
                    options = list(scrollX = TRUE, server=FALSE, pageLength = 6), escape = FALSE)
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
      ~Item, ~URL, ~message,
      "Peaks (.broadPeak)", currentrlsample$peaks_s3, "Peaks called with macs3 in broadPeak format",
      "Coverage (.bw)", currentrlsample$coverage_s3, "Un-normalized read alignment coverage calculated with deepTools (10bp bins)",
      "RLSeq RLRanges object (.rds)", currentrlsample$rlranges_rds_s3, "Sample in RLRanges format with full results available. See RLSeq documentation.",
      "RLSeq report (.html)", currentrlsample$report_html_s3, "RLSeq HTML report for this sample.",
      "FASTQ stats from fastp (.json)", currentrlsample$fastq_stats_s3, "Read statistics generated by running the fastp program (see RLPipes)",
      "BAM stats from samtools (.txt)", currentrlsample$bam_stats_s3, "Alignment statistics from samtools (see RLPipes)"
    ) %>%
      mutate(Details = cell_spec('<i class="fa fa-question-circle"></i>', tooltip = spec_tooltip(title = message), escape = FALSE)) %>%
      dplyr::select(-message) %>%
      knitr::kable("html", escape = FALSE) %>%
      kable_styling("hover", full_width = F) %>%
      add_header_above(set_names(3, nm = current_samp()), align = "left")
  }
  
  ### RLoops Page ###
  
  # Get RLoops dataset
  rloops <- reactive({
    rltabShowNow <- mutate(rltabShow, across(contains("corr"), ~ signif(.x, digits = 4)))
    rltabNow <- dplyr::select(rltabShowNow, -allGenes) %>% dplyr::rename(Genes = mainGenes)
    if (input$showAllGenesRL) rltabNow <- dplyr::select(rltabShowNow, -mainGenes) %>% dplyr::rename(Genes = allGenes)
    if (! input$showRep) rltabNow <- dplyr::filter(rltabNow, ! is_repeat)
    if (input$showCorr) rltabNow <- dplyr::filter(rltabNow, ! is.na(corrR) & corrPVal < .05) %>% arrange(corrPAdj)
    rltabNow
  }) %>% bindCache(input$showAllGenesRL, input$showRep, input$showCorr)

  # Make DataTable
  output$rloops <- renderDT({
    relocate(rloops(), Genes, .after = Location) %>% dplyr::select(-samples)
  }, rownames = FALSE, escape = FALSE, selection = list(mode = "single", selected = 1),
  options = list(pageLength = 6, scrollX = TRUE))

  # Current selected RL from DT
  current_rl <- reactive({
    selectedRow <- ifelse(is.null(input$rloops_rows_selected), 1, input$rloops_rows_selected)
    dplyr::filter(rloops(), row_number() == selectedRow) %>% pull(`RL Region`)
  }) %>% bindCache(rloops(), input$rloops_rows_selected)

  # Make summary page
  NA_LINK <- "<a href=\"https://www.genecards.org/cgi-bin/carddisp.pl?gene=NA\" target=\"_blank\">NA</a>"
  output$RLoopsSummary <- renderUI({
    rloopsNow <- dplyr::filter(rloops(), `RL Region` == current_rl()) %>%
      mutate(Genes = map_chr(Genes, function(x) {paste0(sapply(unique(unlist(strsplit(Genes, split = " "))), makeGeneCards), collapse = "\n")})) %>%
      mutate(Genes = ifelse(Genes == NA_LINK, NA, Genes)) %>%
      mutate(Samples = map_chr(Genes, function(x) {paste0(sapply(unique(unlist(strsplit(samples, split = ","))), makeSRAExperimentLinks), collapse = "\n")})) %>%
      dplyr::select(-is_repeat, -samples, -contains("corr"))
    rloopsNow %>%
      t() %>%
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      select(Feature, Value=V1) %>%
      
      mutate(message = c(
        "R-loop region ID",
        "Genomic location of R-Loop region. Click link to view the Genome Browser session for RLBase.",
        "Confidence score derived from a combination of features indicating the robustness of the region (see Documentation for more detail).",
        "Number of studies in which this RL Region was identified by at least one sample.",
        "Number of modes in which this RL Region was identified by at least one sample.",
        "Average R-loop signal (7th column of broadPeak) for RLBase samples in this RL Region",
        "Average adjusted pvalue (9th column of broadPeak) for RLBase samples in this RL Region",
        "Number of samples in which this RL Region was identified (i.e., sample with peaks that overlap with this RL Region)",
        "Number of tissues in which this RL Region was identified by at least one sample",
        paste0("The source type. RL Regions were calculated separately for S9.6 and dRNH-based modalities.",
               " The source type indicates whether the RL Region was found by one or both types."),
        "The genes which overlap with this R-loop. Clicking the link opens the GeneCards page for that gene.",
        "The samples in which this R-loop was identified. Clicking the links will open the corresponding SRA sample page."
      )) %>%
      mutate(Details = cell_spec('<i class="fa fa-question-circle"></i>', tooltip = spec_tooltip(title = message), escape = FALSE)) %>%
      dplyr::select(-message) %>%
      kableExtra::kbl(format = "html", escape = FALSE, digits = 5, format.args = list(scientific = TRUE)) %>%
      kableExtra::kable_styling("hover") %>% 
      HTML()
  })

  output$RLvsExpbySample <- renderPlot({
    rloopsNow <- dplyr::filter(rloops(), `RL Region` == current_rl())
    validate(
      need(! is.na(rloopsNow$corrR), message = "No correlation available for selected R-loop.")
    )
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
      ylab("R-loop signal (log2(RLRPM + 1))") +
      xlab("Expression (log2(TPM + 1))") +
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
  rprocessX <- reactiveVal(NULL)
  reportlink <- reactiveVal(NULL)
  observeEvent(input$userUpload, {
    # Validate inputs
    validate(
      need(input$userGenome, message = "No  entered."),
      need(input$userPeaks, message = "No peaks provided."),
      need(input$privacyStatement, message = "Privacy Agreement not acknowledged."),
      need(is.null(rprocessX()), message = "RLSeq is currently running.")
    )
    # If everything passed, generate path and upload dummy
    inputs <- list()
    USERDATA_S3_HTTPS <- "https://rlbase-userdata.s3.amazonaws.com"
    USERDATA_S3 <- "s3://rlbase-userdata"
    hash <- uuid::UUIDgenerate()
    fls <- setNames(as.list(file.path("/tmp", hash, c("report.html", "index.html", "log.txt", "rlranges.rds", "log.debug.txt"))), 
                    nm = c("report", "index", "log", "rlranges", "logdebug"))
    # inputs <- c(fls, setNames(list(hash, "test", "hg38", "DRIP", "POS", list(name="www/SRX3581345_hg38.broadPeak",
    #                                                                          datapath="www/SRX3581345_hg38.broadPeak"), USERDATA_S3),
    #                           nm = c("runID", "userSample", "userGenome", "userMode", "userLabel", "userPeaks", "USERDATA_S3")))
    inputs <- c(fls, setNames(list(hash, input$userSample, input$userGenome, 
                                   input$userMode, input$userLabel, input$userPeaks, USERDATA_S3),
                              nm = c("runID", "userSample", "userGenome", "userMode", "userLabel", "userPeaks", "USERDATA_S3")))
    tmpHTML1 <- tempfile(fileext = ".html")
    inputs$tmpHTML1 <- tmpHTML1
    current_step <- " "
    a_ <- knitr::knit(input = "www/rlseq_html/rlseq_inprogress.Rhtml", output = tmpHTML1, quiet = TRUE)
    aws.s3::put_object(file = tmpHTML1, object = file.path(hash, "res_index.html"), 
                       bucket = USERDATA_S3, acl = "public-read")
    shinyWidgets::sendSweetAlert(
      session = session, title = "Running.", html = TRUE,  type = "info",
      text = tags$span("You results will be available here when ready:",
                       tags$a("Link", href=file.path(USERDATA_S3_HTTPS, hash, "res_index.html"), target="_blank"))
    )
    reportlink(file.path(USERDATA_S3_HTTPS, hash, "res_index.html"))
    dir.create(dirname(inputs$log), showWarnings = FALSE)
    dir.create(dirname(inputs$logdebug), showWarnings = FALSE)
    message(inputs$logdebug)
    rprocess <- callr::r_bg(
      user_profile = FALSE,
      poll_connection = FALSE,
      args = list(inputs=inputs, runrlseq=runrlseq, 
                  awstry_put=awstry_put, awstry_saverds=awstry_saverds),
      stdout = inputs$logdebug, stderr = inputs$logdebug,
      func = rlseqbg
    )
    rprocessX(rprocess)
    updateActionButton(session, inputId = "userUpload", label = "Running...",
                       icon = icon("hourglass-half"))
  })
  
  # Updates report link
  output$analysisResults <- renderUI({
    req(reportlink())
    if (! is.null(rprocessX())) {
      span("In progress ⌛️: ", a(href=reportlink(), target="_blank", "Link"), style="font-size: 1.3em;")
    } else {
      span("Results ready 🔥: ", a(href=reportlink(), target="_blank", "Link"),  style="font-size: 1.3em;")
    }
  })
  
  # Checks if the results are done -- and kills and hanging processes
  observe({
    invalidateLater(3000)
    rp <- rprocessX()
    req(! is.null(rp))
    dt <- difftime(rp$get_start_time(), Sys.time(), units = "s")
    if (dt[[1]] < -300 & rp$format() != "PROCESS 'R', finished.\n") {
      # Kill the R process if hanging
      # TODO: Figure out a robust way to accomplish this
      rp$interrupt()
      rprocessX(NULL)
      shinyWidgets::sendSweetAlert(
        session = session, title = "Error in RLSeq.", html = TRUE,  type = "error",
        text = tags$span(
          "Your RLSeq run has timed out. This is an error which can occur intermittently due to connectivity problems.",
          tags$em("Please try again."),
          tags$strong("If you have recieved this error previously, please submit an issue to the ",
                      tags$a(href='https://github.com/Bishop-Laboratory/RLBase/issues', "RLBase repo"),
                      ' and please include a link to the peaks file you are using along with a screenshot of your "Analyze" page inputs.'),
          " and consider using the ",
          tags$a("R package implementation",
          href='https://bishop-laboratory.github.io/RLSeq/',
          target='_blank'), "of RLSeq.")
      )
      updateActionButton(session, inputId = "userUpload", label = "Start",
                         icon = icon("plane-departure"))
    } else if (rp$format() == "PROCESS 'R', finished.\n") {
      rprocessX(NULL)
      shinyWidgets::sendSweetAlert(
        session = session, title = "RLSeq finished.", html = TRUE,  type = "success",
        text = tags$span(
          "Your RLSeq run has completed! View results here: ",
          tags$a("Link", href=reportlink(), target="_blank")
      ))
      updateActionButton(session, inputId = "userUpload", label = "Start",
                         icon = icon("plane-departure"))
    }
  })
  
  ### Downloads ###
  output$rlsamplesDownloadFiles <- renderDT({
    rlsamples %>% 
      dplyr::select(Sample=rlsampleLink, Study=study, Mode = mode, Tissue=tissue, Condition = condition,
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
