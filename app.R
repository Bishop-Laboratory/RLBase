library(shiny)
library(DT)
library(plotly)
library(tidyverse)
library(bslib)

# Get the data
if (! "dataLst" %in% names(globalenv())) {
    load('dataLst.rda')
}

# Get ammptatopms
anno_data <- dataLst %>%
    pluck("sample_quality_characteristics") %>%
    filter(grepl(char_type, pattern = "__")) %>%
    mutate(Annotation = gsub(char_type, pattern = "(.+)__(.+)", replacement = "\\1"),
           data_type = gsub(char_type, pattern = "(.+)__(.+)", replacement = "\\2")) %>%
    select(-char_type) %>%
    inner_join(y = data.frame(
        "Annotation" = c('Intergenic', 'Simple_repeat', 'Satellite', 'Promoter', 'pseudo',
                         'Intron', 'TTS', 'LINE', 'LTR', 'SINE', 'DNA', 'CpG-Island', 'ncRNA',
                         'Low_complexity', 'Exon', 'snRNA', 'Retroposon', '5UTR', '3UTR', 'srpRNA', 
                         'tRNA', 'RC', 'scRNA', 'miRNA', 'RNA', 'snoRNA'),
        "annotate_type" = c("gene", "rep", "rep", "gene", "RNA", "gene", "gene", 
                            "rep", "rep", "rep", "rep", "gene", "RNA", "rep",
                            "gene", "RNA", "rep", "gene", "gene", "RNA",
                            "RNA", "RNA", "RNA", "RNA", "RNA", "RNA"), stringsAsFactors = FALSE
    ), by = "Annotation") %>%
    pivot_wider(id_cols = c(id, Annotation, annotate_type), names_from = data_type, values_from = value) %>%
    inner_join(y = pluck(dataLst, "rmap_samples"), by = "id")

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
                            width = 12,
                            bookmarkButton(label = "Share",
                                           style="float:right",
                                           id = "samplesbookmark")
                        )
                    ),
                    fluidRow(
                        column(
                            width = 5,
                            DTOutput('rmapSamples')
                        ),
                        column(
                            width = 7,
                            tabsetPanel(
                                id = "rmapSampsTabset",
                                tabPanel(
                                    title = "QC",
                                    # TODO: Need icon for QC
                                    icon = icon('home'),
                                    fluidRow(
                                        column(
                                            width = 12,
                                            selectInput(
                                                inputId = 'chooseAnnoPlotData',
                                                label = "Select Data Type",
                                                choices = c("Log2 Ratio (obs/exp)",
                                                            "LogP enrichment (+values depleted)"), 
                                                selected = "Log2 Ratio (obs/exp)"
                                            ),
                                            plotOutput('sampleAnnotationPlot')
                                        )
                                    ),
                                    fluidRow(
                                        column(
                                            width = 6,
                                            plotOutput('zScorePlot')
                                        ),
                                        column(
                                            width = 6,
                                            plotOutput('rmapHeatmap')
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
                    ),
                    
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
                    ), 
                    fluidRow(
                        column(
                            width = 12,
                            bookmarkButton(label = "permalink",
                                           id = "rloopsbookmark")
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
    output$rmapSamples <- renderDT(
        server = FALSE, {
            dataLst %>%
                pluck("rmap_samples") %>%
                select(Sample=id, 
                       Study=study_id,
                       Mode = mode,
                       Genome = genome,
                       Condition = condition,
                       Tissue=tissue,
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
    )
    
    output$zScorePlot <- renderPlot({
        
        # Get selected row from datatable
        selectedRow <- ifelse(is.null(input$rmapSamples_rows_selected), 
                              1, 
                              input$rmapSamples_rows_selected)
        
        # Get current sample ID
        current_samp <- dataLst %>%
            pluck("rmap_samples") %>%
            filter(row_number() == selectedRow) %>%
            pull(id)
        
        # Get file to load from
        current_file <-  current_samp %>%
            list.files('misc/report_rda_small/', 
                       pattern = .,
                       full.names = TRUE)
        
        # Load from file
        # TODO: This is not an optimal way to get this data...
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
    })
    
    
    output$sampleAnnotationPlot <- renderPlot({
        
        opt <- input$chooseAnnoPlotData
        
        MIN_ALLOW <- ifelse(opt == "Log2 Ratio (obs/exp)", -7.5, -300)
        MAX_ALLOW <- ifelse(opt == "Log2 Ratio (obs/exp)", 7.5, 300)
        
        print(MAX_ALLOW)
        
        # Get selected row from datatable
        selectedRow <- ifelse(is.null(input$rmapSamples_rows_selected), 
                              1, 
                              input$rmapSamples_rows_selected)
        
        # Get current sample ID
        current_samp <- dataLst %>%
            pluck("rmap_samples") %>%
            filter(row_number() == selectedRow) %>%
            pull(id)
        
        
        minplt <- (min(na.omit(anno_data)[, opt]) * 1.05 )%>% ifelse(. < MIN_ALLOW, MIN_ALLOW, .)
        maxplt <- (max(na.omit(anno_data)[, opt]) * 1.05) %>% ifelse(. > MAX_ALLOW, MAX_ALLOW, .)
        
        print(maxplt)
        
        boxFills <- c('gene' = 'firebrick', 'RNA' = 'goldenrod', 'rep' = 'forestgreen')
        titles <- c('gene' = 'Genomic Features', 'RNA' = 'ncRNAs', 'rep' = 'Repetitive Elements')
        genelvls <- c("CpG-Island",
                      "Promoter",
                      "5UTR",
                      "Exon",
                      "Intron",
                      "3UTR",
                      "TTS",
                      "Intergenic")
        
        lapply(unique(anno_data$annotate_type), function(annoNow) {
            anno_data %>%
                filter(annotate_type == !!annoNow) %>%
                mutate(Annotation = if (!! annoNow == "gene") 
                    factor(Annotation, levels =  genelvls) 
                    else Annotation) %>%
                mutate(sample_now = id == !! current_samp) %>%
                fill(everything(0)) %>%
                arrange(sample_now) %>%
                ggplot(mapping = aes(x = Annotation, 
                                     y = .data[[opt]])) +
                geom_boxplot(fill = boxFills[annoNow],
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
                labs(title = titles[annoNow]) +
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
        }) %>%
            ggpubr::ggarrange(plotlist = ., nrow = 1, align = "h")
    })
}

# TODO: Need URL cleaner
# Run the application 
shinyApp(ui, server, enableBookmarking = "url")
