# Libraries
library(shiny)
library(DT)
library(plotly)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(ggprism)
library(readr)
library(ggplot2)
library(bslib)
library(RColorBrewer)

# Get constants
source("const.R")
source("ui_globals.R")
source("utils.R")
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
            title = "RMapDB",
            id = "rmapdb",
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
                title = "About",
                value = "aboutTab",
                id = "about-tab",
                icon = icon('info-circle'),
                fluidPage(
                    br(),
                    includeHTML("www/about.html")
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
                p("Hello world")
                # includeHTML("www/documentation.html")
            )
        ),
        tags$footer(
            HTML(footerHTML())
        )
    )
}


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    ### Sample Page ###
    # TODO: Should be sorted
    # TODO: Should contain link that will open the details modal
    # TODO: Should include UI to add hyperlink for clicking SRX
    # TODO: Should have better color for the selection of rows
    rmapSampsRV <- reactive({
        dataLst %>%
            pluck("rmap_samples") %>%
            mutate(pred_ctrl = prediction == "Control") %>%
            filter(genome == input$selectGenome, 
                   is_rnh_like %in% c(FALSE, input$selectRNH),
                   pred_ctrl %in% c(FALSE, input$selectCTRL),
                   mode %in% input$selectMode) %>%
            pull(id)
    })
    
    current_samp <- reactive({
        # Get selected row from datatable
        selectedRow <- ifelse(is.null(input$rmapSamples_rows_selected), 
                              1, 
                              input$rmapSamples_rows_selected)
        
        # Get current sample ID
        current_samp <- dataLst %>%
            pluck("rmap_samples") %>%
            filter(id %in% rmapSampsRV()) %>%
            filter(row_number() == selectedRow) %>%
            pull(id)
        
        current_samp
    })
    
    output$rmapSamples <- renderDT(
        server = FALSE, {
            dataLst %>%
                pluck("rmap_samples") %>%
                filter(id %in% rmapSampsRV()) %>%
                mutate(Pred_CTRL = prediction == 'Control') %>%
                select(Sample=id, 
                       Study=study_id,
                       Mode = mode,
                       Tissue=tissue,
                       Condition = condition,
                       Pred_CTRL,
                       Genome = genome,
                       Genotype = genotype,
                       Treatment = treatment) %>%
                datatable(
                    selection = list(mode = "single",
                                     selected = 1),
                    rownames = FALSE, 
                    options = list(
                        pageLength = 10,
                        scrollX = TRUE
                    )
                )
        }
    ) %>%
        bindCache(rmapSampsRV())
    
    output$zScorePlot <- renderPlot({
      
        RLFS_LOC <- "../RSeq-supplemental-analysis/misc/datasets_for_fft_testing/rlfs/"
        
        # Get file to load from
        current_file <-  current_samp() %>%
            list.files(RLFS_LOC, 
                       pattern = .,
                       full.names = TRUE)
        
        # Load from file
        # TODO: This is really not an good way to get this data...
        suppressWarnings(load(current_file)) 
        
        # Get LZ
        lz <- rlfsRes %>%
            pluck("Z-scores", 
                  "regioneR::numOverlaps")
        
        # Plot
        data.frame("zscore" = lz$shifted.z.scores,
                   "shift" = lz$shifts) %>%
            ggplot(aes(y = zscore, x = shift)) +
            geom_vline(color = "firebrick", xintercept = 0, linetype = "dashed") +
            geom_line(size = 1) +
            ggtitle('ZScore around RLFS', subtitle = current_samp()) +
            ylab("Peak Enrichment (Z-Score)") +
            scale_y_continuous(expand = c(0,0)) +
            xlab("Distance to RLFS (bp)") +
            theme_prism(base_size = 15)
    }) %>%
        bindCache(rmapSampsRV(), current_samp())
    
    output$RLFSOutHTML <- renderUI({
      
      RLFS_LOC <- "../RSeq-supplemental-analysis/misc/datasets_for_fft_testing/rlfs/"
      
      # Get file to load from
      current_file <-  current_samp() %>%
        list.files(RLFS_LOC, 
                   pattern = .,
                   full.names = TRUE)
      
      # Load from file
      # TODO: This is really not an good way to get this data...
      suppressWarnings(load(current_file)) 
        
        # Get the data for this sample
        vals <- c(
            dataLst %>%
                pluck("sample_quality_characteristics") %>%
                filter(char_type %in% c("id", "rlfs_pval", "MACS2__total_peaks"),
                       id %in% current_samp()) %>%
                select(-id) %>%
                deframe(),
            dataLst %>%
                pluck("rmap_samples") %>%
                filter(id %in% current_samp()) %>%
                mutate(is_ctrl = is_rnh_like,
                       pred_ctrl = prediction == "Control") %>%
                select(is_ctrl, pred_ctrl) %>%
                pivot_longer(cols = everything()) %>%
                deframe()
        )
        
        vals['rlfs_pval'] <- -log10(rlfsRes$perTestResults$`regioneR::numOverlaps`$pval)
        
        tagList(
            div(
                class="col d-flex justify-content-center",
                div(
                    class = "card",
                    div(
                        class = "card-body",
                        h5(
                            class = "card-title",
                            "RLFS analysis results"
                        ),
                        p(
                            class = "card-text",
                            HTML(paste0("RLFS-PVAL (min = 0.002): ", span(strong(
                                style=paste0("color: ", ifelse(
                                    vals[["rlfs_pval"]] > 1.6, 
                                    "green", 
                                    ifelse(
                                        vals[["rlfs_pval"]] > 1.3,
                                        "orange", "red"
                                    ))),
                                signif(10^(-1*vals[["rlfs_pval"]]), 3)
                            ))))
                        ),
                        p(
                            class = "card-text",
                            HTML(paste0("Num. Peaks Available: ", span(strong(
                                style=paste0("color: ", ifelse(
                                    vals[["MACS2__total_peaks"]] > 3000, 
                                    "green", 
                                    ifelse(
                                        vals[["MACS2__total_peaks"]] > 1500,
                                        "orange", "red"
                                    ))),
                                round(vals[["MACS2__total_peaks"]])            
                            ))))
                        ),
                        p(
                            class = "card-text",
                            HTML(paste0("Control Sample (labeled): ", span(strong(
                                style=paste0("color: ", ifelse(
                                    vals[["is_ctrl"]] == 0, 
                                    "green", "red"
                                )),
                                vals[["is_ctrl"]] == 1
                            ))))
                        ),
                        p(
                            class = "card-text",
                            HTML(paste0("Control Sample (predicted): ", span(strong(
                                style=paste0("color: ", ifelse(
                                    vals[["pred_ctrl"]] == 0, 
                                    "green", "red"
                                )),
                                vals[["pred_ctrl"]] == 1
                            ))))
                        )
                    )
                )
            )
        )
    })
    
    output$FFTPlot <- renderPlot({
        
        RLFS_LOC <- "../RSeq-supplemental-analysis/misc/datasets_for_fft_testing/rlfs/"
        
        # Get file to load from
        current_file <-  current_samp() %>%
          list.files(RLFS_LOC, 
                     pattern = .,
                     full.names = TRUE)
        
        # Load from file
        # TODO: This is really not an good way to get this data...
        suppressWarnings(load(current_file)) 
        
        # Get LZ
        lz <- rlfsRes %>%
          pluck("Z-scores", 
                "regioneR::numOverlaps")
        
        # Plot
        data.frame("fftval" = Re(fft(lz$shifted.z.scores)),
                   "freq" = seq(lz$shifts)) %>%
            ggplot(aes(y = fftval, x = freq)) +
            geom_hline(color = "firebrick", yintercept = 0, linetype = "dashed") +
            geom_line(size = 1) +
            ggtitle('Fourier Transform of ZScore around RLFS', subtitle = current_samp()) +
            ylab("FT-ZScore (Real Component)") +
            xlab("Relative Frequency") +
            theme_prism(base_size = 15)
    }) %>%
        bindCache(rmapSampsRV(), current_samp())
    
    
    output$pValPlot <- renderPlot({
        # Get file to load from
      RLFS_LOC <- "../RSeq-supplemental-analysis/misc/datasets_for_fft_testing/rlfs/"
      
      # Get file to load from
      current_file <-  current_samp() %>%
        list.files(RLFS_LOC, 
                   pattern = .,
                   full.names = TRUE)
      
      # Load from file
      # TODO: This is really not an good way to get this data...
      suppressWarnings(load(current_file)) 
      
      # Get LZ
      pt <- rlfsRes %>%
        pluck("perTestResults", 
              "regioneR::numOverlaps")
        
        # Plot
        regioneR:::plot.permTestResults(pt)
        
    }) %>%
        bindCache(rmapSampsRV(), current_samp())
    
    output$sampleAnnotationPlot <- renderPlot({
        
        opt <- input$chooseAnnoPlotData
        
        MIN_ALLOW <- ifelse(opt == "Log2 Ratio (obs/exp)", -7.5, 
                            ifelse(opt == "LogP enrichment (+values depleted)", 
                                   -1000, -Inf))
        MAX_ALLOW <- ifelse(opt == "Log2 Ratio (obs/exp)", 7.5, 
                            ifelse(opt == "LogP enrichment (+values depleted)", 
                                   1000, Inf))
        minplt <- (min(na.omit(anno_data)[, opt]) * 1.05 )%>% ifelse(. < MIN_ALLOW, MIN_ALLOW, .)
        maxplt <- (max(na.omit(anno_data)[, opt]) * 1.05) %>% ifelse(. > MAX_ALLOW, MAX_ALLOW, .)
        
        suppressWarnings(lapply(unique(anno_data$annotate_type), function(annoNow) {
            toPlt <- anno_data %>%
                rename(is_ctrl = is_rnh_like) %>%
                mutate(pred_ctrl = prediction == "Control") %>%
                filter(annotate_type == !!annoNow,
                       id %in% rmapSampsRV()
                       ) %>%
                mutate(Annotation = if (!! annoNow == "gene") 
                    factor(Annotation, levels =  annoPlot_genelvls) 
                    else Annotation) %>%
                mutate(sample_now = id == !! current_samp()) %>%
                fill(everything(0)) %>%
                arrange(sample_now) 
            
            if (input$splitAnnoBy == "None") {
                plt <- ggplot(toPlt, mapping = aes(x = Annotation, 
                                                   y = !! sym(opt))) +
                    geom_hline(yintercept = 0, linetype = "dashed") +
                    geom_violin(
                        # draw_quantiles = c(.50), 
                        trim = FALSE,  position = position_dodge(.9),
                        fill = vCols[[annoNow]]) +
                    geom_boxplot(width=.12, color = "black",position = position_dodge(.9),
                                 fill = boxCols[[annoNow]],
                                 alpha = 1) +
                    geom_point(alpha=ifelse(toPlt$sample_now, 1, 0), size = 4, color = "black", 
                    shape=23, stroke = 2, fill = "#32889c") 
            } else {
                if (input$splitAnnoBy != "mode") {
                    plt <- ggplot(toPlt, mapping = aes(x = Annotation, 
                                                       fill = !! sym(input$splitAnnoBy),
                                                       y = !! sym(opt))) +
                        geom_hline(yintercept = 0, linetype = "dashed") +
                        geom_violin(
                            trim = FALSE,  position = position_dodge(.9)) 
                } else {
                    plt <- ggplot(toPlt, mapping = aes(x = Annotation, 
                                                       fill = !! sym(input$splitAnnoBy),
                                                       y = !! sym(opt))) +
                        geom_hline(yintercept = 0, linetype = "dashed") 
                }
                
                # So that only the colors in the factor level appear in the legend
                cols <- annoFillSplit[[input$splitAnnoBy]][unique(as.character(as.data.frame(toPlt)[,input$splitAnnoBy]))]
                
                # Add box and jitter
                plt <- plt + geom_boxplot(width=ifelse(input$splitAnnoBy != "mode", .12, .85), 
                                   color = "black",
                                   position = position_dodge(.9),
                                   alpha = 1) +
                    geom_jitter(alpha=ifelse(toPlt$sample_now, 1, 0), 
                                size = 4, color = "black",
                                position = position_jitterdodge(dodge.width = .9,
                                                                jitter.width = 0, 
                                                                jitter.height = 0),
                                shape=23, stroke = 2) +
                    scale_fill_manual(
                        values = cols, drop=TRUE
                    )
               
            }
            
            plt <- plt +
                ggpubr::rremove("legend") +
                ylab(opt) +
                xlab(NULL) +
                labs(title = annoPlot_titles[annoNow]) +
                theme_prism(base_size = 16) +
                theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                                 hjust = 1)) +
                theme(legend.title = element_text(size=18),
                      legend.text = element_text(size=14))
                
        })) %>%
            ggpubr::ggarrange(plotlist = ., nrow = 3, ncol = 1, align = "hv")
    }) %>%
        bindCache(input$chooseAnnoPlotData, input$splitAnnoBy, rmapSampsRV(), current_samp())
    
    
    output$rmapHeatmap <- renderPlot({
        # Filter for current samples selected
        annoCorrNow <- annoCorr[rmapSampsRV(),]
        corrNow <- corr_data[rmapSampsRV(),rmapSampsRV()]
        
        # From: https://stackoverflow.com/questions/31677923/set-0-point-for-pheatmap-in-r
        annoCorrNow$sample <- as.factor(ifelse(rownames(annoCorrNow) != current_samp(), "", "selected"))
        
        # Match up columns
        annoCorrNow <- cleanAnnoCorrNow(annoCorrNow)
        
        # Pallete
        paletteLength <- 100
        myColor <- colorRampPalette(rev(brewer.pal(n = 7, name = "RdBu")))(paletteLength)
        # length(breaks) == length(paletteLength) + 1
        # use floor and ceiling to deal with even/odd length pallettelengths
        myBreaks <- c(seq(min(corrNow), 0, length.out=ceiling(paletteLength/2) + 1), 
                      seq(max(corrNow)/paletteLength, max(corrNow), length.out=floor(paletteLength/2)))
        pheatColLst <- colList[which(names(colList) %in% colnames(annoCorrNow))]
        pheatColLst$Mode <- colList$mode[which(names(colList$mode) %in% annoCorrNow$Mode)]
        pheatmap::pheatmap(corrNow, show_rownames = FALSE, 
                           main = paste0("RMapDB Corr Heatmap\n",
                                         current_samp()
                                         ), fontsize = 13.5,
                           show_colnames = FALSE, silent = TRUE,
                           annotation_colors = pheatColLst, 
                           color = myColor, breaks = myBreaks,
                           annotation_col = annoCorrNow) %>%
            pluck(4) %>%
            ggplotify::as.ggplot()
        
    }) %>%
        bindCache(rmapSampsRV(), current_samp())
    
    
    output$rmapPCA <- renderPlot({
        # Filter for current samples selected
        annoCorrNow <- annoCorr[rmapSampsRV(),] %>%
            na.omit()
        corrNow <- corr_data[rownames(annoCorrNow), rownames(annoCorrNow)]
        
        # Get the PCA data
        pcd <- pcaPlotDataFromCorr(corrNow)
        
        # From: https://stackoverflow.com/questions/31677923/set-0-point-for-pheatmap-in-r
        annoCorrNow$sample <- as.factor(ifelse(rownames(annoCorrNow) != current_samp(), "", "selected"))
        
        # Get the plot
        toPlt <- pcd %>%
            pluck("pcData") %>%
            inner_join(dataLst %>%
                           pluck("rmap_samples"),
                       by = "id") %>%
            
            right_join(rownames_to_column(annoCorrNow, var = "id"), by = "id") %>%
            mutate(selected = as.factor(ifelse(id != current_samp(),
                                               "", "selected"))) %>%
            mutate(
                across(any_of(names(colList)), as.factor)
            )
        
        if (input$PCA_colorBy %in% names(colList)) {
            cols <- colList[[input$PCA_colorBy]][unique(as.character(toPlt[,input$PCA_colorBy]))]
        } else {
            cols <- NA
        }
        
        ggplot(
            toPlt,
            aes_string(x = "PC1", y = "PC2", color = input$PCA_colorBy, 
                       shape = input$PCA_shapeBy, size = "selected")
        ) +
            rmap_scatter(cols) +
            guides(colour = guide_legend(override.aes = list(size=9)),
                   shape = guide_legend(override.aes = list(size=9))) +
            xlab(paste0("PC1 (", pcd$percentVar[1], "%)")) +
            ylab(paste0("PC2 (", pcd$percentVar[2], "%)")) + 
            ggtitle("RMapDB PCA Plot", subtitle = current_samp()) 
            
    }) %>%
        bindCache(rmapSampsRV(), current_samp(), input$PCA_shapeBy, input$PCA_colorBy)
    
    
    output$sumStats <- renderDT({
        
        # TODO: REFACTOR THIS!
        
        dataLst %>%
            pluck("sample_quality_characteristics") %>%
            filter(id == current_samp()) %>%
            left_join(y = qualCol, by = c("char_type" = "name")) %>%
            pivot_longer(
                cols = all_of(c("error", "ok", "warning")), names_repair = "minimal", values_to = "vals"
            ) %>%
            mutate(is_nl = map_lgl(
                vals,
                function(x) {is.null(x)}
            )) %>%
            filter(! is_nl) -> dd
        dd$intervalTest <- sapply(rownames(dd), function(x) {
            x <- as.numeric(x)
            intv <- dd$vals[x] %>% unlist()
            lab <- dd$name[x]
            val <- dd$value[x]
            return(findInterval(val, intv) == 1L)
        })
        dd %>%
            filter(intervalTest) %>%
            left_join(qualColors) %>%
            mutate(value = paste0(
                "<p style='color:", color,"'>", round(value, 3), "</p>"
            )) %>%
            select("QC Metric" = char_type, value = value)
    }, escape=FALSE, server=FALSE, rownames = FALSE, options = list(dom = "t")) %>%
        bindCache(rmapSampsRV(), current_samp())
    
    output$RLoopsPerSample <- renderDT({
        
        # Get the R-loops for the current sample
        to2show <- dataLst %>%
            pluck("rloop_signal") %>%
            filter(rmap_sample_id == current_samp(),
                   numOlap > 0) %>%
            pull(rloop_id)
            
    
        # Show the R-loops within that sample    
        rltabShowNow <- rltabShow %>%
            filter(`RL Region` %in% to2show) %>%
            mutate(
                across(
                    corr:corrpadj, ~ signif(.x, digits = 4)
                )
            )
        
        if (input$showAllGenesRLSamp) {
            rltabNow <- rltabShowNow %>%
                select(-Genes, -GenesFix) %>%
                dplyr::rename(Genes = GenesNat)
        } else {
            rltabNow <- rltabShowNow %>%
                select(-Genes, -GenesNat) %>%
                dplyr::rename(Genes = GenesFix) 
        }
        
        if (! input$showRepSamp) {
            rltabNow <- dplyr::filter(rltabNow, ! repeats)
        } 
        
        if (input$showCorrSamp) {
            rltabNow <- rltabNow %>%
                filter(! is.na(corr)) %>%
                arrange(corrpadj) 
        }
        
        rltabNow %>%
            select(c("RL Region", "Location", "Genes", "Mean Signal", "Mean FDR", 
                     "# of Studies", "# of Samples", "# of Tissues", "# of Modes")) %>%
            relocate(Genes, .after = Location) %>%
            arrange(desc(`Mean FDR`)) %>%
            DT::datatable(
                extensions = 'Buttons',
                options = list(
                    scrollX = TRUE,
                    server=FALSE,
                    pageLength = 6
                ), selection = list(mode = "none")
            )
    }) 
    
    
    output$downloadsForSample <- renderUI({
        
        snamebw <- rmapfftsmall %>%
            filter(id == current_samp()) %>%
            mutate(sname = paste0(sample_name, ".", genome)) %>%
            pull(sname)
        snamepeak <- rmapfftsmall %>%
            filter(id == current_samp()) %>%
            mutate(sname = paste0(sample_name, "_", genome)) %>%
            pull(sname)
        
        tagList(
            fluidRow(
                column(
                    width = 6,
                    h4(paste0(current_samp(), " - Downloads")),
                    hr()
                )
            ),
            fluidRow(
                column(
                    width = 4, 
                    a(
                        class="btn btn-default shiny-download-link",
                        target="_blank",
                        href=paste0(baseURLBW, snamebw, ".bw"),
                        icon("download"),
                        "Coverage (.bw)"
                    )
                ),
                column(
                    width = 4, 
                    a(
                        class="btn btn-default shiny-download-link",
                        target="_blank",
                        href=paste0(baseURLPEAKS, snamepeak, ".unstranded.broadPeak"),
                        icon("download"),
                        "Peaks (.broadPeak)"
                    )
                )
            )
        )
    })
    
    ### RLoops Page ###
    # Get RLoops dataset
    rloops <- reactive({
        rltabShowNow <- rltabShow %>%
            mutate(
                across(
                    corr:corrpadj, ~ signif(.x, digits = 4)
                )
            ) 
        
        if (input$showAllGenesRL) {
            rltabNow <- rltabShowNow %>%
                select(-Genes, -GenesFix) %>%
                dplyr::rename(Genes = GenesNat)
        } else {
            rltabNow <- rltabShowNow %>%
                select(-Genes, -GenesNat) %>%
                dplyr::rename(Genes = GenesFix) 
        }
        
        if (! input$showRep) {
            rltabNow <- filter(rltabNow, ! repeats)
        }
        
        if (input$showCorr) {
            rltabNow <- rltabNow %>%
                filter(! is.na(corr)) %>%
                arrange(corrpadj) 
        }
        
        rltabNow
    }) %>% bindCache(input$showAllGenesRL, input$showRep, input$showCorr)
    
    # Make DataTable
    output$rloops <- renderDT({
        rloops() %>%
            select(-Type, -Modes, -`Mean RLCounts`, -repeats) %>%
            relocate(Genes, .after = Location) 
    }, rownames = FALSE, escape = FALSE, 
    selection = list(mode = "single",
                     selected = 1),
    options = list(
        pageLength = 5,
        scrollX = TRUE
    )) 
    
    # Current selected RL from DT
    current_rl <- reactive({
        # Get selected row from datatable
        selectedRow <- ifelse(is.null(input$rloops_rows_selected), 
                              1, 
                              input$rloops_rows_selected)
        
        # Get current sample
        rloops() %>%
            filter(row_number() == selectedRow) %>%
            pull(`RL Region`)
    }) %>%
        bindCache(rloops(), input$rloops_rows_selected)
    
    # Make summary page
    NA_LINK <- "<a href=\"https://www.genecards.org/cgi-bin/carddisp.pl?gene=NA\" target=\"_blank\">NA</a>"
    output$RLoopsSummary <- renderUI({
        rloopsNow <- rloops() %>%
            filter(`RL Region` == current_rl()) %>%
            left_join(
                select(
                    rltab, `RL Region`=id, confidence_level, is_rlfs, samples, genes
                ),
                by = "RL Region"
            ) %>%
            mutate(Genes = map_chr(
                Genes, function(x) {
                    paste0(sapply(unique(unlist(strsplit(Genes, split = "\n"))), makeGeneCards), collapse = "\n")
                }
            )) %>%
            mutate(Genes = ifelse(Genes == NA_LINK, NA, Genes)) %>%
            select(-genes) %>%
            relocate(Genes, .before = Type) %>%
            select(-`Mean RLCounts`, -repeats, -corrpval, -Type)
        rloopsNow %>%
            mutate(Location = makeRLConsensusGB(Location)) %>%
            mutate(samples = paste0(unique(unlist(samples)), collapse = "\n")) %>%
            t() %>%
            kableExtra::kbl(format = "html", escape = FALSE) %>%
            kableExtra::kable_styling() %>%
            HTML()
    })
    
    output$RLvsExpbySample <- renderPlot({
        rloopsNow <- rloops() %>%
            filter(`RL Region` == current_rl())
        
        # Get the corr and pval
        corr <- pull(rloopsNow, "corr") %>% round(4)
        corrpadj <- pull(rloopsNow, "corrpadj") %>% round(4)
        
        # Get the color
        rlExpCondLvlByRL %>%
            dplyr::filter(rloop_id == current_rl()) %>%
            dplyr::rename(
                Treatment = treatment,
                Mode = mode,
                Tissue = tissue,
                Study = study_id,
                Condition = condition
            ) %>%
            ggplot(aes_string(x = "vst", y = "qVal", color = "Mode")) +
            geom_point() +
            ggtitle(current_rl(), subtitle = "Expression vs. R-Loop Intensity") +
            theme_bw(base_size = 14) +
            annotate(geom = 'text', 
                     label = paste0("Rho: ", corr, "; Padj: ", corrpadj), 
                     x = -Inf, y = Inf, hjust = -.20, vjust = 3)
        
    }) %>% bindCache(current_rl(), rloops())
}
    
# TODO: Need URL cleaner
# Run the application 
graphics.off()
shinyApp(ui, server, enableBookmarking = "url")
