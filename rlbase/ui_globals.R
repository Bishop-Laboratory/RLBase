PAGE_PLOT_WIDTH = "96%"
PAGE_PLOT_HEIGHT = "600px"
ANNO_PLOT_HEIGHT = "1000px"

SamplesPageContents <- function() {
  fluidPage(
    title = "RLBase Samples",
    fluidRow(
      column(
        width = 5,
        RMapSamplesTable_panel()
      ),
      column(
        width = 7,
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
          choices = unique(rlsamples$genome),
        ),
      ),
      column(
        width = 6,
        selectInput(
          inputId = "selectMode", 
          label = "Mode",
          multiple = TRUE,
          selected = c("DRIP", "DRIPc", "sDRIP", "qDRIP"),
          choices = unique(rlsamples$mode)
        ),
      )
    ),
    fluidRow(
      column(
        width = 6,
        checkboxInput(
          inputId = "select_label_NEG", 
          label = "Show labeled controls (e.g., 'RNaseH1-treated')",
          value = FALSE
        )
      ),
      column(
        width = 6,
        checkboxInput(
          inputId = "select_prediction_NEG", 
          label = "Show predicted controls (i.g., predicted 'CTRL')",
          value = TRUE
        )
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
        icon = icon('list'),
        br(),
        Summary_panel()
      ),
      tabPanel(
        title = "Annotation",
        icon = icon("paint-brush"),
        Annotation_panel()
      ),
      tabPanel(
        title = "RLFS",
        # TODO: Need icon for QC
        icon = icon('laptop-code'),
        RLFS_panel()
      ),
      tabPanel(
        title = "R-loops",
        # TODO: Need icon
        icon = icon('table'),
        RLoops_Panel()
      ),
      tabPanel(
        title = "Downloads",
        # TODO: Need icon
        icon = icon('download'),
        Downloads_panel()
      )
    )
  )
}


RLFS_panel <- function() {
  tagList(
    br(),
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
        plotOutput('FFTPlot')
      )
    )
  )
}


Annotation_panel <- function() {
  tagList(
    br(),
    fluidRow(
      column(
        width = 6,
        selectInput(
          inputId = "splitby",
          label = "Split",
          choices = c("none", "prediction", "label"),
          selected = "none"
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        uiOutput(outputId = "annoPlots")
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
      plotOutput('heatmap',  
                 height = PAGE_PLOT_HEIGHT, 
                 width = PAGE_PLOT_WIDTH)
    ),
    tabPanel(
      title = "PCA",
      fluidRow(
        column(
          width = 6,
          selectInput(
            inputId = "PCA_colorBy",
            choices = c("mode", "study_id", "tissue"),
            label = "Color"
          )
        ),
        column(
          width = 6,
          selectInput(
            inputId = "PCA_shapeBy",
            choices = c("label", "prediction"),
            selected = "prediction",
            label = "Shape"
          )
        )
      ),
      column(
        width = 12, 
        plotOutput('rmapPCA', 
                   height = PAGE_PLOT_HEIGHT, 
                   width = PAGE_PLOT_WIDTH)
      )
    )
  )
}


RLoops_Panel <- function() {
  tagList(
    br(),
    fluidRow(
      column(
        width = 2,
        checkboxInput(inputId = "showAllGenesRLSamp",
                      label = "All genes", 
                      value = FALSE)  
      ),
      column(
        width = 2,
        checkboxInput(inputId = "showRepSamp",
                      label = "Repetitive", 
                      value = FALSE)  
      ),
      column(
        width = 3,
        checkboxInput(inputId = "showCorrSamp",
                      label = "Correlated with expression", 
                      value = FALSE)  
      )
    ),
    fluidRow(
      column(
        12, 
        DTOutput("RLoopsPerSample")
      )
    )
  )
  
}


Downloads_panel <- function() {
  tableOutput("downloadsForSample")
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
            icon = icon("list"),
            uiOutput("RLoopsSummary")
          ),
          tabPanel(
            title = "Expression",
            icon = icon("dna"),
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
            title = "Downloads",
            icon = icon("download"),
            ## Download the R-Loops table
          )
        )
      )
    )
  )
}


HelpPageContents <- function() {
  list(
    h1("Help page")
  )
}


DownloadPageContents <- function() {
  list(
    h1("Downloads page")
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
      <div class='footer-copyright text-center py-3'><span style='color:white'>RLBase © 2021 Copyright:</span>
        <a href='https://gccri.uthscsa.edu/lab/bishop/' target='_blank'> Bishop Laboratory</a> 
        <span>&nbsp</span>
        <a href='https://github.com/Bishop-Laboratory/' target='_blank'> 
          <img src='GitHub-Mark-Light-64px.png' height='20'>
        </a>
      </div>
    </footer>"
}


RLFSTagList <- function(vals) {
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
            HTML(paste0("Labeled Condition: ", span(strong(
              style=paste0("color: ", ifelse(
                vals[["label"]] == "POS",
                "green", "red"
              )),
              vals[["condition"]]
            ))))
          ),
          p(
            class = "card-text",
            HTML(paste0("Predicted Condition: ", span(strong(
              style=paste0("color: ", ifelse(
                vals[["prediction"]] == "POS",
                "green", "red"
              )),
              vals[["prediction"]]
            ))))
          )
        )
      )
    )
  )
}

