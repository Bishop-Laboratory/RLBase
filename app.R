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

# Get constants
source("const.R")

# Define UI for application that draws a histogram
ui <- function(request) {
    tagList(
        
        # For stick footer    
        tags$head(
            tags$style(HTML(
                "
            html {
             position: relative;
             min-height: 100%;
           }
           body {
             margin-bottom: 60px; /* Margin bottom by footer height */
           }
           .footer {
             position: absolute;
             bottom: 0;
             width: 100%;
             height: 60px; /* Set the fixed height of the footer here */
             background-color: #2C3E50;
           }
                "
                
                
                )
            )
        ),
        
        navbarPage(
            title = "RMapDB",
            id = "rmapdb",
            theme = bslib::bs_theme(bootswatch = "flatly"),
            
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
                fluidPage(
                    title = "RMapDB Samples",
                    fluidRow(
                        column(
                            width = 4,
                            fluidRow(
                                column(
                                    width = 6,
                                    selectInput(
                                        inputId = "selectGenome", 
                                        label = "Genome",
                                        multiple = FALSE,
                                        selected = "hg38",
                                        choices = unique(dataLst$rmap_samples$genome),
                                    ),
                                ),
                                column(
                                    width = 6,
                                    selectInput(
                                        inputId = "selectMode", 
                                        label = "Mode",
                                        multiple = TRUE,
                                        selected = c("DRIP", "DRIPc", "sDRIP", "qDRIP"),
                                        choices = unique(dataLst$rmap_samples$mode)
                                    ),
                                )
                            ),
                            fluidRow(
                                column(
                                    width = 6,
                                    checkboxInput(
                                        inputId = "selectCond", 
                                        label = "Show Controls (e.g., 'RNH')",
                                        value = FALSE
                                    )
                                ),
                                column(
                                    width = 6,
                                    bookmarkButton(label = "Share",
                                                   id = "samplesbookmark")
                                )
                            ),
                            hr(),
                            fluidRow(
                                column(
                                    width = 12,
                                    DTOutput('rmapSamples')
                                )
                            )
                       ),
                       column(
                           width = 8,
                           column(
                               width = 12,
                               tabsetPanel(
                                   id = "rmapSampsTabset",
                                   tabPanel(
                                       title = "QC",
                                       # TODO: Need icon for QC
                                       icon = icon('home'),
                                       fluidRow(
                                           column(
                                               width = 4,
                                               # fluidRow(
                                               #     column(
                                               #         width = 12,
                                               #         plotOutput('heatDistPlot')
                                               #     )
                                               # ),
                                               fluidRow(
                                                   column(
                                                       width = 12,
                                                       hr(),
                                                       tabsetPanel(
                                                           id = "qualCharTabs",
                                                           tabPanel(
                                                               title = "Summary",
                                                               icon = icon('home'),
                                                               DTOutput('sumStats')
                                                           ),
                                                           tabPanel(
                                                               title = "Reads",
                                                               icon = icon('home'),
                                                               tableOutput('fqStats')
                                                           ),
                                                           tabPanel(
                                                               title = "Alignment",
                                                               icon = icon('home'),
                                                               tableOutput('bamStats')
                                                           ),
                                                           tabPanel(
                                                               title = "Peaks",
                                                               icon = icon('home'),
                                                               tableOutput('pkStats')
                                                           )
                                                       )
                                                   )
                                               )
                                           ),
                                           column(
                                               width = 8, 
                                               plotOutput('rmapHeatmap',
                                                          height = "500px")
                                           )
                                       ),
                                       fluidRow(
                                           column(
                                               width = 4,
                                               br(),
                                               plotOutput('zScorePlot', 
                                                          height = "300px")
                                           ),
                                           column(
                                               width = 4,
                                               br(),
                                               plotOutput('pValPlot',
                                                          height = "300px")
                                           ),
                                           column(
                                               width = 4,
                                               br(),
                                               plotOutput('FFTAnalysis')
                                           )
                                       )
                                   ),
                                   tabPanel(
                                       title = "Annotation",
                                       icon = icon("home"),
                                       fluidRow(
                                           column(
                                               width = 12,
                                               selectInput(
                                                   inputId = 'chooseAnnoPlotData',
                                                   label = "Select Data Type",
                                                   choices = c("Log2 Ratio (obs/exp)",
                                                               "Number of peaks", 
                                                               "Total size (bp)",
                                                               "LogP enrichment (+values depleted)"), 
                                                   selected = "Log2 Ratio (obs/exp)"
                                               ),
                                               plotOutput('sampleAnnotationPlot')
                                           )
                                       )
                                   ),
                                   tabPanel(
                                       title = "Expression",
                                       # TODO: Need icon
                                       icon = icon('home'),
                                       fluidRow(
                                           column(
                                               width = 6,
                                               fluidRow(
                                                   column(
                                                       width = 12,
                                                       plotlyOutput('rlVsExp')
                                                   )
                                               ),
                                               fluidRow(
                                                   selectInput(
                                                       inputId = "selectExpType",
                                                       label = "Select Normalization",
                                                       choices = c("TPM", "VST", "Log2-counts"),
                                                       selected = "TPM"
                                                   )
                                               )
                                           ),
                                           column(
                                               width = 6,
                                               plotlyOutput('expPCA')
                                           )
                                       )
                                   ),
                                   tabPanel(
                                       title = "Downloads",
                                       # TODO: Need icon
                                       icon = icon('home'),
                                       downloadButton(
                                           outputId = "downloadCoverage",
                                           label = "Coverage"
                                       ),
                                       downloadButton(
                                           outputId = "downloadPeaks",
                                           label = "Peaks"
                                       )
                                   )
                               )
                           )
                       )
                    )
                    
                )
            ),
            tabPanel(
                title = "R-Loops", 
                id = "rloops-tab",
                fluidPage(
                    title = "R-Loops",
                    fluidRow(
                        column(
                            width = 12,
                            DTOutput('rloops')
                        )
                    )
                )
            )
        ),
        tags$footer(
            HTML("
                    <footer class='footer'>
                    <div class='footer-copyright text-center py-3'><span style='color:white'>© 2021 Copyright:</span>
                    <a href='https://gccri.uthscsa.edu/lab/bishop/' target='_blank'> Bishop Laboratory</a>
                    </div>
                    </footer>")
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
    # TODO: Should contain quality score
    # TODO: Should contain link that will open the details modal
    # TODO: Should include UI to add hyperlink for clicking SRX
    # TODO: Should have better color for the selection of rows
    rmapSampsRV <- reactive({
        dataLst %>%
            pluck("rmap_samples") %>%
            filter(genome == input$selectGenome, 
                   is_rnh_like %in% c(FALSE, input$selectCond),
                   mode %in% input$selectMode) %>%
            pull(id)
    })
        
    output$rmapSamples <- renderDT(
        server = FALSE, {
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
        
        # Get file to load from
        current_file <-  current_samp %>%
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
        bindCache(input$rmapSamples_rows_selected, rmapSampsRV())
    
    
    output$pValPlot <- renderPlot({
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
        
        # Get file to load from
        current_file <-  current_samp %>%
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
        bindCache(input$rmapSamples_rows_selected, rmapSampsRV())
    
    output$sampleAnnotationPlot <- renderPlot({
        
        opt <- input$chooseAnnoPlotData
        
        MIN_ALLOW <- ifelse(opt == "Log2 Ratio (obs/exp)", -7.5, 
                            ifelse(opt == "LogP enrichment (+values depleted)", 
                                   -1000, -Inf))
        MAX_ALLOW <- ifelse(opt == "Log2 Ratio (obs/exp)", 7.5, 
                            ifelse(opt == "LogP enrichment (+values depleted)", 
                                   1000, Inf))
        
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
            ggpubr::ggarrange(plotlist = ., nrow = 1, align = "h")
    }) %>%
        bindCache(input$rmapSamples_rows_selected, input$chooseAnnoPlotData, rmapSampsRV())
    
    
    output$rmapHeatmap <- renderPlot({
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
        
        # Filter for current samples selected
        annoCorrNow <- annoCorr[rmapSampsRV(),]
        corrNow <- corr_data[rmapSampsRV(),rmapSampsRV()]
        
        # From: https://stackoverflow.com/questions/31677923/set-0-point-for-pheatmap-in-r
        annoCorrNow$sample <- as.factor(ifelse(rownames(annoCorrNow) != current_samp, "", "selected"))
        
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
                           main = paste0("RMapDB Corr Heatmap\n", current_samp),
                           show_colnames = FALSE, silent = TRUE,
                           annotation_colors = pheatColLst, 
                           color = myColor, breaks = myBreaks,
                           annotation_col = annoCorrNow) %>%
            pluck(4) %>%
            ggplotify::as.ggplot()
        
    }) %>%
        bindCache(input$rmapSamples_rows_selected, rmapSampsRV())
    
    
    
    output$sumStats <- renderDT({
        
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
        
        # TODO: REFACTOR THIS!
        
        dataLst %>%
            pluck("sample_quality_characteristics") %>%
            filter(id == current_samp) %>%
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
                "<p style='color:", color,"'>", round(value, 4), "</p>"
            )) %>%
            select("QC Metric" = char_type, value = value)
    }, escape=FALSE, rownames = FALSE, options = list(dom = "t")) 
    
    ### RLoops Page ###
    output$rloops <- renderDT({
        rltab
    })
}

# TODO: Need URL cleaner
# Run the application 
graphics.off()
shinyApp(ui, server, enableBookmarking = "url")
