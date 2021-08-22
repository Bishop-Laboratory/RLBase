SamplesPageContents <- function() {
  fluidPage(
    title = "RMapDB Samples",
    fluidRow(
      column(
        width = 4,
        RMapSamplesTable_panel()
      ),
      column(
        width = 8,
        RMapSamplesOutput_tabset()
      )
    )
  )
}


RMapSamplesTable_panel <- function() {
  tagList(
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
        width = 4,
        checkboxInput(
          inputId = "selectRNH", 
          label = "Show controls (e.g., RNH)",
          value = FALSE
        )
      ),
      column(
        width = 4,
        checkboxInput(
          inputId = "selectCTRL", 
          label = "Show predicted controls",
          value = TRUE
        )
      ),
      column(
        width = 4,
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
  )
}


RMapSamplesOutput_tabset <- function() {
  column(
    width = 12,
    tabsetPanel(
      id = "rmapSampsTabset",
      tabPanel(
        title = "Summary",
        # TODO: Need icon for QC
        icon = icon('home'),
        Summary_panel()
      ),
      tabPanel(
        title = "Annotation",
        icon = icon("home"),
        Annotation_panel()
      ),
      tabPanel(
        title = "RLFS",
        # TODO: Need icon for QC
        icon = icon('home'),
        RLFS_panel()
      ),
      tabPanel(
        title = "R-loops",
        # TODO: Need icon
        icon = icon('home'),
        RLoops_Panel()
      ),
      tabPanel(
        title = "Downloads",
        # TODO: Need icon
        icon = icon('home'),
        Downloads_panel()
      )
    )
  )
}


RLFS_panel <- function() {
  tagList(
    fluidRow(
      column(
        width = 6,
        htmlOutput(outputId = "RLFSOutHTML")
      ),
      column(
        width = 6,
        plotOutput('zScorePlot')
      )
    ),
    fluidRow(
      column(
        width = 6,
        plotOutput('pValPlot')
      ),
      column(
        width = 6,
        plotOutput('FFTAnalysis')
      )
    )
  )
}


Annotation_panel <- function() {
  tagList(
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
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        plotOutput("sampleAnnotationPlot")
      )
    )
  )
}


Summary_panel <- function() {
  tabsetPanel(
    id = "RMapSamplesSummary",
    type = "pills",
    tabPanel(
      title = "Heatmap",
      column(
        width = 8, 
        plotOutput('rmapHeatmap',
                   height = "500px")
      )
    ),
    tabPanel(
      title = "PCA",
      column(
        width = 8, 
        plotOutput('rmapPCA')
      )
    )
  )
}


RLoops_Panel <- function() {
  fluidRow(
    column(
      12, 
      DTOutput("RLoopsPerSample")
    )
  )
}


Downloads_panel <- function() {
  tagList(
    downloadButton(
      outputId = "downloadCoverage",
      label = "Coverage"
    ),
    downloadButton(
      outputId = "downloadPeaks",
      label = "Peaks"
    )
  )
}


RLoopsPageContents <- function() {
  fluidPage(
    title = "R-Loops",
    fluidRow(
      column(
        width = 2,
        h4("Display Controls")
      ),
      column(
        width = 1,
        checkboxInput(inputId = "showAllGenesRL",
                      label = "All genes", 
                      value = FALSE)  
      ),
      column(
        width = 1,
        checkboxInput(inputId = "showRep",
                      label = "Repetitive", 
                      value = FALSE)  
      ),
      column(
        width = 2,
        checkboxInput(inputId = "showCorr",
                      label = "Correlated with expression", 
                      value = FALSE)  
      )
    ),
    fluidRow(
      column(
        width = 7,
        DTOutput('rloops')
      ),
      column(
        width = 5,
        tabsetPanel(
          id = "rloopStats",
          tabPanel(
            title = "Summary",
            icon = icon("home"),
            h4("Summary goes here!")
          ),
          tabPanel(
            title = "Expression",
            icon = icon("home"),
            selectInput(
              inputId = "colorExpvRlBy",
              label = "Color",
              multiple = FALSE,
              selected = "Tissue",
              choices = c("Tissue", "Mode", "Study", "Condition")
            ),
            plotOutput(outputId = "RLvsExpbySample")
          ),
          tabPanel(
            title = "Genomic Features",
            icon = icon("home"),
            ## Genomic features content
            ## Comparison with RLFS
          ),
          tabPanel(
            title = "Downloads",
            icon = icon("home"),
            ## Download the R-Loops table
          )
        )
      )
    )
  )
}


headerHTML <- function() {
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
}


footerHTML <- function() {
  "
                    <footer class='footer'>
                    <div class='footer-copyright text-center py-3'><span style='color:white'>© 2021 Copyright:</span>
                    <a href='https://gccri.uthscsa.edu/lab/bishop/' target='_blank'> Bishop Laboratory</a>
                    </div>
                    </footer>"
}
