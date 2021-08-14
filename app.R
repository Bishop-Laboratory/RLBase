library(shiny)
library(DT)
library(plotly)
library(tidyverse)
library(bslib)

# Define UI for application that draws a histogram
ui <- function(request) {
    tagList(
        
        # For stick footer    
        tags$head(
            tags$style(HTML(
                "html {
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
           }")
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
                            bookmarkButton(label = "permalink",
                                           style="float:right",
                                           id = "samplesbookmark")
                        )
                    ),
                    fluidRow(
                        column(
                            width = 6,
                            dataTableOutput('rmap-samples')
                        ),
                        column(
                            width = 6,
                            dataTableOutput('rmap-sample-rloops')
                        )
                    ),
                    fluidRow(
                        hr(),
                        column(
                            width = 12,
                            tabsetPanel(
                                id = "rmapsamps-tabset",
                                tabPanel(
                                    title = "QC",
                                    # TODO: Need icon for QC
                                    icon = icon('home'),
                                    fluidRow(
                                        column(
                                            width = 6,
                                            plotlyOutput('zscore-plot')
                                        ),
                                        column(
                                            width = 6,
                                            plotOutput('heatmap')
                                        )
                                    ),
                                    fluidRow(
                                        column(
                                            width = 12,
                                            plotlyOutput('annotation-plot')
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
                                                    plotlyOutput('rl-vs-exp')
                                                )
                                            ),
                                            fluidRow(
                                                selectInput(
                                                    inputId = "select-tpm-vst",
                                                    label = "Select Normalization",
                                                    choices = c("TPM", "VST", "Log2-counts"),
                                                    selected = "TPM"
                                                )
                                            )
                                        ),
                                        column(
                                            width = 6,
                                            plotlyOutput('exp-pca')
                                        )
                                    )
                                ),
                                tabPanel(
                                    title = "Downloads",
                                    # TODO: Need icon
                                    icon = icon('home'),
                                    downloadButton(
                                        outputId = "download-coverage",
                                        label = "Coverage"
                                    ),
                                    downloadButton(
                                        outputId = "download-peaks",
                                        label = "Peaks"
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
                            dataTableOutput('rloops')
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
        
    
    
    
    
}

# Run the application 
shinyApp(ui, server, enableBookmarking = "url")
