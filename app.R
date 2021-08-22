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
                SamplesPageContents()
            ),
            tabPanel(
                title = "R-Loops", 
                id = "rloops-tab",
                RLoopsPageContents()
            )
        ),
        tags$footer(
            HTML(footerHTML())
        )
    )
}


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # Need to exclude the buttons from themselves being bookmarked
    setBookmarkExclude(c("samplesbookmark", "rloopsbookmark"))
    
    # Trigger bookmarking with either button
    observeEvent(input$samplesbookmark, {
        session$doBookmark()
    })
    observeEvent(input$rloopsbookmark, {
        session$doBookmark()
    })
    
    ### Sample Page ###
    # TODO: Should be sorted
    # TODO: Should contain link that will open the details modal
    # TODO: Should include UI to add hyperlink for clicking SRX
    # TODO: Should have better color for the selection of rows
    rmapSampsRV <- reactive({
        dataLst %>%
            pluck("rmap_samples") %>%
            filter(genome == input$selectGenome, 
                   is_rnh_like %in% c(FALSE, input$selectRNH),
                   is_ctrl %in% c(FALSE, input$selectCTRL),
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
            print("DATATABLE RMAP")
            dataLst %>%
                pluck("rmap_samples") %>%
                filter(id %in% rmapSampsRV()) %>%
                select(Sample=id, 
                       Study=study_id,
                       Mode = mode,
                       Tissue=tissue,
                       Condition = condition,
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
        
        # Get file to load from
        current_file <-  current_samp() %>%
            list.files('misc/report_rda_small/', 
                       pattern = .,
                       full.names = TRUE)
        
        # Load from file
        # TODO: This is really not an good way to get this data...
        suppressWarnings(load(current_file)) 
        
        # Get LZ
        lz <- data_list %>%
            pluck("rlfs_data", 
                  2,
                  "regioneR::numOverlaps")
        
        # Plot
        data.frame("zscore" = lz$shifted.z.scores,
                   "shift" = lz$shifts) %>%
            ggplot(aes(y = zscore, x = shift)) +
            geom_line() +
            labs(title = paste0(current_samp, ' RLFS analysis')) +
            ylab("Peak Enrichment (Z-Score)") +
            xlab("Distance to RLFS (bp)") +
            theme_bw(base_size = 15)
    }) %>%
        bindCache(rmapSampsRV(), current_samp())
    
    
    output$pValPlot <- renderPlot({
        # Get file to load from
        current_file <-  current_samp() %>%
            list.files('misc/report_rda_small/', 
                       pattern = .,
                       full.names = TRUE)
        
        # Load from file
        # TODO: This is really not an good way to get this data...
        suppressWarnings(load(current_file)) 
        
        # Get pt
        pt <- data_list %>%
            pluck("rlfs_data", 
                  1,
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
            anno_data %>%
                filter(annotate_type == !!annoNow,
                       id %in% rmapSampsRV()) %>%
                mutate(Annotation = if (!! annoNow == "gene") 
                    factor(Annotation, levels =  annoPlot_genelvls) 
                    else Annotation) %>%
                mutate(sample_now = id == !! current_samp) %>%
                fill(everything(0)) %>%
                arrange(sample_now) %>%
                ggplot(mapping = aes(x = Annotation, 
                                     y = .data[[opt]])) +
                geom_boxplot(fill = annoPlot_boxFills[annoNow],
                             outlier.shape = NA) +
                geom_jitter(mapping = aes(
                    size = sample_now, 
                    color = sample_now,
                    alpha = sample_now, 
                    ),
                    width = 0.1) +
                ggpubr::rremove("legend") +
                ylab(opt) +
                xlab(NULL) +
                labs(title = annoPlot_titles[annoNow]) +
                theme_bw() +
                scale_color_manual(values = c(
                    "TRUE" = "#2F3940",
                    "FALSE" = "grey"
                )) +
                scale_alpha_manual(values = c(
                    "TRUE" = 1,
                    "FALSE" = .5
                )) +
                scale_size_manual(values = c(
                    "TRUE" = 5,
                    "FALSE" = .6
                )) +
                ggpubr::rremove("legend") +
                theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                                 hjust = 1)) +
                ylim(minplt, maxplt)
        })) %>%
            ggpubr::ggarrange(plotlist = ., nrow = 2, align = "hv")
    }) %>%
        bindCache(input$chooseAnnoPlotData, rmapSampsRV(), current_samp())
    
    
    output$rmapHeatmap <- renderPlot({
        # Filter for current samples selected
        annoCorrNow <- annoCorr[rmapSampsRV(),]
        corrNow <- corr_data[rmapSampsRV(),rmapSampsRV()]
        
        # From: https://stackoverflow.com/questions/31677923/set-0-point-for-pheatmap-in-r
        annoCorrNow$sample <- as.factor(ifelse(rownames(annoCorrNow) != current_samp(), "", "selected"))
        
        # Select isControl if there's a good reason to
        if (any(annoCorrNow$isControl)) {
            annoCorrNow$isControl <- as.factor(annoCorrNow$isControl)
        } else {
            annoCorrNow <- annoCorrNow[,-which(colnames(annoCorrNow) == "isControl")]
        }
        
        paletteLength <- 100
        myColor <- colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(paletteLength)
        # length(breaks) == length(paletteLength) + 1
        # use floor and ceiling to deal with even/odd length pallettelengths
        myBreaks <- c(seq(min(corrNow), 0, length.out=ceiling(paletteLength/2) + 1), 
                      seq(max(corrNow)/paletteLength, max(corrNow), length.out=floor(paletteLength/2)))
        pheatColLst$Mode <- pheatColLst$Mode[which(names(pheatColLst$Mode) %in% annoCorrNow$Mode)]
        pheatmap::pheatmap(corrNow, show_rownames = FALSE, 
                           main = paste0("RMapDB Corr Heatmap\n", current_samp()),
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
        annoCorrNow <- annoCorr[rmapSampsRV(),]
        
        # Get the PCA data
        pcd <- pcaPlotDataFromCorr(corrNow)
        
        # From: https://stackoverflow.com/questions/31677923/set-0-point-for-pheatmap-in-r
        annoCorrNow$sample <- as.factor(ifelse(rownames(annoCorrNow) != current_samp(), "", "selected"))
        
        # Select isControl if there's a good reason to
        if (any(annoCorrNow$isControl)) {
            annoCorrNow$isControl <- as.factor(annoCorrNow$isControl)
        } else {
            annoCorrNow <- annoCorrNow[,-which(colnames(annoCorrNow) == "isControl")]
        }
        
        # Get the plot
        pcd %>%
            pluck("pcData") %>%
            inner_join(dataLst %>%
                           pluck("rmap_samples"),
                       by = "id") %>%
            mutate(selected = as.factor(ifelse(id != current_samp(),
                                               "", "selected"))) %>%
            ggplot(
                aes_string(x = "PC1", y = "PC2", color = input$PCA_colorBy, 
                           shape = "selected", size = "selected")
            ) +
            rmap_scatter(colorBy = input$PCA_colorBy)
    }) %>%
        bindCache(rmapSampsRV(), current_samp())
    
    
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
        rltabShow %>%
            filter(`RL Region` %in% to2show)
            mutate(
                across(
                    corr:corrpadj, ~ signif(.x, digits = 4)
                )
            ) %>%
            select(-Type, -Modes, -`Mean RLCounts`, -repeats) %>%
            relocate(Genes, .after = Location) 
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
    })
    
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
    
    output$RLvsExpbySample <- renderPlot({
        
        # Get selected row from datatable
        selectedRow <- ifelse(is.null(input$rloops_rows_selected), 
                              1, 
                              input$rloops_rows_selected)
        
        # Get current sample
        current_rl <- rloops() %>%
            filter(row_number() == selectedRow)
        
        # Get the corr and pval
        corr <- pull(current_rl, "corr") %>% round(4)
        corrpadj <- pull(current_rl, "corrpadj") %>% round(4)
        
        # Get the color
        rlExpCondLvlByRL %>%
            dplyr::filter(rloop_id == current_rl$`RL Region`) %>%
            dplyr::rename(
                Treatment = treatment,
                Mode = mode,
                Tissue = tissue,
                Study = study_id,
                Condition = condition
            ) %>%
            ggplot(aes_string(x = "vst", y = "qVal", color = "Mode")) +
            geom_point() +
            ggtitle(current_rl, subtitle = "Expression vs. R-Loop Intensity") +
            theme_bw(base_size = 14) +
            annotate(geom = 'text', 
                     label = paste0("Rho: ", corr, "; Padj: ", corrpadj), 
                     x = -Inf, y = Inf, hjust = -.20, vjust = 3)
        
    })
}

# TODO: Need URL cleaner
# Run the application 
graphics.off()
shinyApp(ui, server, enableBookmarking = "url")
