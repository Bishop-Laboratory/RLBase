library(shiny)
library(DT)
library(plotly)
library(tidyverse)
library(bslib)

# Get the data
if (! "dataLst" %in% names(globalenv())) {
    load('dataLst.rda')
}

# Get the data for the RegioneR plots
load("misc/report_rda/ERX2277510_E-MTAB-6318DRIP_mOHT_hg38.QC_report.rda")
data_list$rlfs_data[[2]]$`regioneR::numOverlaps`$shifts


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
                                            plotlyOutput('sampleAnnotationPlot')
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
            list.files('misc/report_rda/', 
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
            labs(title = "Peak enrichment around RLFS") +
            ylab("Peak Enrichment (Z-Score)") +
            xlab("Distance to RLFS (bp)") +
            theme_bw(base_size = 15)
    })
    
    
    
    
    
    
    
}

# TODO: Need URL cleaner
# Run the application 
shinyApp(ui, server, enableBookmarking = "url")
