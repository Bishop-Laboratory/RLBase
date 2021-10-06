PAGE_PLOT_WIDTH = "96%"
PAGE_PLOT_HEIGHT = "600px"
ANNO_PLOT_HEIGHT = "1000px"

SamplesPageContents <- function(rlsamples) {
  fluidPage(
    title = "RLBase Samples",
    fluidRow(
      column(
        width = 5,
        RMapSamplesTable_panel(rlsamples)
      ),
      column(
        width = 7,
        RMapSamplesOutput_tabset()
      )
    )
  )
}


RMapSamplesTable_panel <- function(rlsamples) {
  tagList(
    fluidRow(
      column(width = 12,
             h3("RLBase Samples"),
             hr())
    ),
    fluidRow(
      column(
        width = 12,
        makeHeaders(
          title = "Table Controls ",
          message=paste0("Controls the data displayed in the 'RLBase Samples Table' and the ",
                         "plots/data displayed on the right side of the screen.")
        )
      )
    ),
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
          selected = c("DRIP", "DRIPc", "sDRIP", "qDRIP", "ssDRIP"),
          choices = unique(rlsamples$mode)
        ),
      )
    ),
    fluidRow(
      column(
        width = 6,
        checkboxInput(
          inputId = "select_label_NEG", 
          label = "Show labeled controls",
          value = TRUE
        )
      ),
      column(
        width = 6,
        checkboxInput(
          inputId = "select_prediction_NEG", 
          label = "Show predicted controls",
          value = TRUE
        )
      )
    ),
    hr(),
    fluidRow(
      column(
        width = 12,
        makeHeaders(
          title = "RLBase Samples Table ",
          message=paste0("R-loop mapping samples in RLBase and their associated metadata.",
                         " The samples in this table correspond to the plots and data",
                         " displayed on the right side of the screen. Use the 'Table",
                         " Controls' to change the displayed samples.")
        ),
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
        icon=icon('chart-pie'),
        Summary_panel()
      ),
      tabPanel(
        title = "Sample-sample comparison",
        icon = icon('check-double'),
        br(),
        Sample_Sample_panel()
      ),
      tabPanel(
        title = "Annotation",
        icon = icon("paint-brush"),
        Annotation_panel()
      ),
      tabPanel(
        title = "RLFS",
        icon = icon('wave-square'),
        RLFS_panel()
      ),
      tabPanel(
        title = "RL Regions",
        icon = icon('map'),
        br(),
        RLoops_Panel()
      ),
      tabPanel(
        title = "Downloads",
        icon = icon('download'),
        br(),
        fluidRow(
          column(
            width = 6, offset = 3,
            hr(),
            makeHeaders(
              title = "Sample downloads ",
              message = paste0("Sample downloads list. Hover over help icons for information about each.", 
                               " See 'Download' for more detail.")
            ),
            hr(),
            Downloads_panel()
          )
        )
      )
    )
  )
}


RLFS_panel <- function() {
  tagList(
    fluidRow(
      column(
        width = 6,
        hr(),
        makeHeaders(
          title = "R-loop forming sequences (RLFS) analysis results ",
          message = paste0("R-loop forming sequences (RLFS) analysis summary.",
                           " For details on each row, hover over the corresponding",
                           " help icon.")
        ),
        hr(),
        htmlOutput(outputId = "RLFSOutHTML")
      ),
      column(
        width = 6,
        hr(),
        makeHeaders(
          title = "Z-score distribution plot ",
          message = paste0("Plot showing the enrichment of sample peaks within RLFS. See Documentation for details.")
        ),
        hr(),
        plotOutput('zScorePlot')
      )
    ),
    fluidRow(
      column(
        width = 6,
        hr(),
        makeHeaders(
          title = "Permutation test plot ",
          message = paste0("Plot showing the enrichment of permutation testing as part of RLSeq. ",
                           "Green bar shows actual number of overlaps in comparison with the random distribution.",
                           " See Documentation for details.")
        ),
        hr(),
        plotOutput('pValPlot')
      ),
      column(
        width = 6,
        hr(),
        makeHeaders(
          title = "Fourier transform plot ",
          message = paste0("Plot of the Fourier transform of the Z-score distribution.",
                           " See Documentation for details.")
        ),
        hr(),
        plotOutput('FFTPlot')
      )
    )
  )
}


Annotation_panel <- function() {
  tagList(
    fluidRow(
      column(
        width = 12,
        hr(),
        makeHeaders(
          title = "Sample annotations ",
          message = paste0("Sample annotation plots show the enrichment of genomic features within the RLBase samples. ",
                           "The &#9670; shows the location of the select sample in 'RLBase Samples Table'. ",
                           "For more information, read the Documentation.")
        ),
        hr()
      )
    ),
    fluidRow(
      column(
        width = 6,
        selectInput(
          inputId = "splitby",
          label = "Split",
          choices = c("prediction", "label", "none"),
          selected = "prediction"
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
  list(
    fluidRow(
      column(
        width = 6,
        hr(),
        makeHeaders(
          title = "Sample summary ",
          message=paste0("A high-level summary for the sample that is selected in the 'Sample Table'.",
                         " Mouse over the help icons for row to learn more.")
        ),
        hr(),
        uiOutput("sampleSummary")
      ),
      column(
        width = 6,
        hr(),
        makeHeaders(
          title = "R-loop mapping modalities ",
          message=paste0("Representation of R-loop mapping modalities in",
                         " selected data. Use the table controls to adjust ",
                         "this. See documentation for further details.")
        ),
        hr(),
        plotlyOutput("modeDonut")
      )
    ),
    fluidRow(
      column(
        width = 6,
        hr(),
        makeHeaders(
          title = "Sample labels ",
          message=paste0("Representation of sample labels among the",
                         " selected data. 'POS' indicates a sample which was ",
                         " expected to map R-loops (e.g., 'S9.6 -RNH1' in DRIP-Seq) and ",
                         " 'NEG' indicates a sample which was not expected to map",
                         " R-loops (e.g., 'S9.6 +RNH1' or 'Genomic Input').",
                         " See documentation for details.")
        ),
        hr(),
        plotlyOutput("labelDonut")
      ),
      column(
        width = 6,
        hr(),
        makeHeaders(
          title = "Sample quality prediction ",
          message=paste0("Representation of sample quality predictions among the",
                         " selected data. 'POS' indicates a sample which was ",
                         " predicted by the RLSeq quality model to map R-loops robustly and ",
                         " 'NEG' indicates a sample which was predicted to map R-loops poorly.",
                         " See documentation for details.")
        ),
        hr(),
        plotlyOutput("predictionDonut")
      )
    )
  )
}


Sample_Sample_panel <- function() {
  tabsetPanel(
    id = "RMapSamplesSummary",
    type = "pills",
    tabPanel(
      title = "Heatmap", icon = icon("fire-alt"),
      hr(),
      makeHeaders(
        title = "Sample Heatmap ",
        message=paste0("The sample heatmap displays the sample-sample pearson ",
                       "correlation around gold-standard R-loop sites (see Documentation.)",
                       "The 'group' annotation displays the location of the sample selected in",
                       " the 'RLBase Samples Table'.")
      ),
      hr(),
      plotOutput('heatmap',  
                 height = PAGE_PLOT_HEIGHT, 
                 width = PAGE_PLOT_WIDTH)
    ),
    tabPanel(
      title = "PCA", icon = icon("ruler-combined"),
      fluidRow(
        column(
          width = 12,
          hr(),
          makeHeaders(
            title = "Sample PCA ",
            message=paste0("The sample PCA plot displays the sample-sample variance ",
                           "based on the correlation around gold-standard R-loop sites (see Documentation.)",
                           "The 'group' annotation displays the location of the sample selected in",
                           " the 'RLBase Samples Table'. Controls allow for tuning of plot parameters.")
          ),
          hr()
        )
      ),
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
    tabsetPanel(
      id = "rlsampleRLRegions",
      type = "pills",
      tabPanel(
        title = "Enrichment",
        icon = icon("adjust"),
        fluidRow(
          column(
            width = 8, offset = 2,
            hr(),
            makeHeaders(
              title = "Overlap of sample peaks and RL Regions ",
              message = paste0("The R-loop regions (RL Regions) set was overlapped",
                               "with the peaks derived from the selected sample. The ",
                               "plot shows the degree of overlap and significance (Fisher's exact test).",
                               " See Documentation for details.")
            ),
            hr()
          )
        ),
        fluidRow(
          column(
            width = 8, offset = 2,
            plotOutput("rlVenn")
          )
        )
      ),
      tabPanel(
        title = "Table",
        icon = icon("table"),
        fluidRow(
          column(
            width = 12,
            hr(),
            makeHeaders(
              title = "R-loop regions in selected sample ",
              message = paste0("The R-loop regions (RL Regions) found in the sample selected in ",
                               "'RLBase Samples Table'. Controls filter the RL Regions table. ",
                               "Select 'All genes' to show every gene symbol. Select 'Repetitive' to",
                               " show RL Regions overlapping with repetitive elements. Select ",
                               "'Correlated with Expression' to show only the RL Regions with are",
                               " significantly correlated with gene expression.",
                               " See Documentation for details.")
            ),
            hr()
          )
        ),
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
    )
  )
}


Downloads_panel <- function() {
  tableOutput("downloadsForSample")
}


RLoopsPageContents <- function() {
  fluidPage(
    title = "RL Regions",
    fluidRow(
      column(
        width = 7,
        fluidRow(
          column(
            width = 12,
            h3("R-Loop Regions"),
            hr()
          )
        ),
        fluidRow(
          column(
            width = 12,
            makeHeaders(
              title = "Table Controls ",
              message =  paste0("The R-loop regions (RL Regions) found in the sample selected in ",
                                "'RLBase Samples Table'. Controls filter the RL Regions table. ",
                                "Select 'All genes' to show every gene symbol. Select 'Repetitive' to",
                                " show RL Regions overlapping with repetitive elements. Select ",
                                "'Correlated with Expression' to show only the RL Regions with are",
                                " significantly correlated with gene expression.",
                                " See Documentation for details.")
            )
          )
        ),
        fluidRow(
          column(
            width = 2,
            checkboxInput(inputId = "showAllGenesRL",
                          label = "All genes", 
                          value = FALSE)  
          ),
          column(
            width = 2,
            checkboxInput(inputId = "showRep",
                          label = "Repetitive", 
                          value = FALSE)  
          ),
          column(
            width = 4,
            checkboxInput(inputId = "showCorr",
                          label = "Correlated with expression", 
                          value = FALSE)  
          ),
          hr()
        ),
        fluidRow(
          column(
            width = 12,
            makeHeaders(
              title = "RL Regions Table ",
              message = paste0("The R-loop regions are derived from a consesnsus ",
                               "analysis of the high-quality ('POS' prediction) samples ",
                               "in RLBase. Selecting rows in this table will affect the output",
                               " on the right side of the screen. For more details, see Documentation.")
            ),
            DTOutput('rloops')
          )
        )
      ),
      
      column(
        width = 5,
        tabsetPanel(
          id = "rloopStats",
          tabPanel(
            title = "Summary",
            icon = icon("list"),
            fluidRow(
              column(
                width = 12,
                hr(),
                makeHeaders(
                  title = "RL Region Summary ",
                  message = paste0("Summary information about the selected RL Region. ",
                                   "For more details, hover over the help modal on each row.")
                ),
                hr()
              )
            ),
            fluidRow(
              column(
                width = 12,
                uiOutput("RLoopsSummary")
              )
            )
          ),
          tabPanel(
            title = "Expression",
            icon = icon("dna"),
            fluidRow(
              column(
                width = 12,
                hr(),
                makeHeaders(
                  title = "RL Region expression correlation plot ",
                  message = paste0("Plots showing the relationship between normalized ",
                                   "abundance of R-loop and RNA-Seq reads within an RL-Region ",
                                   "across samples with paired R-loop/Expression data.",
                                   "For more details, see Documentation.")
                ),
                hr()
              )
            ),
            fluidRow(
              column(
                width=12,
                plotOutput(outputId = "RLvsExpbySample")
              )
            )
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

AnalyzePageContents <- function(rlsamples) {
  list(
    fluidRow(
      column(
        width = 6,
        h3("Analyze R-loop data"),
        hr()
      )
    ),
    fluidRow(
      column(
        width = 6,
        makeHeaders(
          title = "Enter sample info ",
          message = paste0("Enter the information describing your sample and upload peaks. ",
                           "For more details, see Documentation.")
        ),
        hr(),
        fluidRow(
          column(
            width = 6,
            textInput(inputId = "userSample", label = "Sample name"),
            selectInput(inputId = "userGenome", label = "Genome", selected = "hg38",
                        choices = RLSeq:::available_genomes$UCSC_orgID),
            selectInput(inputId = "userMode", label = "Mode", selected = "DRIP",
                        choices = unique(rlsamples$mode)),
            selectInput(inputId = "userLabel", label = "Label", 
                        choices = c("POS", "NEG"))
          ),
          column(
            width = 6,
            fileInput("userPeaks", label = "Peaks (broadPeak format)",
                      accept = c(".broadPeak", ".narrowPeak", ".bed")),
            span(strong("Privacy statement"),"I understand that my RLSeq report and submitted data",
                 " will be posted on a publicly-accessible AWS S3 bucket and will NOT be kept private."),
            br(),
            checkboxInput(inputId = "privacyStatement", 
                          label = "I have read and understood the privacy statement.", 
                          value = FALSE),
            actionButton(inputId = "userUpload", label = "Start", icon = icon("rocket")),
          )
        ),
        hr(),
        br(),
        uiOutput("analysisResults")
      )
    )
  )
}

DownloadPageContents <- function(bucket_sizes, rlsamples) {
  md <- "
  ## RLBase Downloads
  <hr>
  
  *RLBase* provides access to the raw and processed data sets which were generated
  as part of the *RLSuite* project. With the exception of raw `.bam` files, these
  data are stored on the publicly-avialable *RLBase-data* AWS bucket (`s3://rlbase-data/`).
  
  For **bulk access** to *RLBase-data* (**67.8 GB**), please use <a href='https://anaconda.org/conda-forge/awscli' target='_blank'>*AWS CLI*</a>:
  
  ```shell
  # conda install -c conda-forge awscli
  aws s3 sync --no-sign-request s3://rlbase-data/ rlbase_data/  # Downloads all RLBase-data
  ```
  For **fine-grained access** to specific resources, please see the following guides:
  <br>
  "
  list(
    shiny::markdown(md),
    tabsetPanel(
      id = "downloads",
      tabPanel(
        title = "Processed data files",
        icon = icon("table"),
        processedDataDownloads(bucket_sizes, rlsamples)
      ),
      tabPanel(
        title = "RLHub downloads",
        icon = icon("database"),
        br(),
        rlhubDownloads(bucket_sizes)
      ),
      tabPanel(
        title = "Raw and misc data",
        icon = icon("dna"),
        br(),
        rawDataDownloads()
      )
    ),
    br()
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

# TODO: Put this in a kable
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


#' Downloads for all
rlhubDownloads <- function(bucket_sizes) {
  
  md <- paste0("
  ### RData objects via RLHub
  
  Processed RData objects are provided via the <a href='https://github.com/Bishop-Laboratory/RLHub' target='_blank'>*RLHub*</a> R package (part of the *RLSuite*).
  A full description of the data is provided in the table below. 
  
  <details>
  <summary><strong>Data Access</strong> Details</summary>
  
  <br>
  
  To access these data, there are several options:
  
  * **RLHub** (preferred)
    - Download the `RLHub` R package via `remotes` (requires Bioconductor 3.14): 
    
    ```r
    if (!requireNamespace('BiocManager', quietly = TRUE))
      install.packages('BiocManager', version='devel')
  
    remotes::install_github('Bishop-Laboratory/RLHub')
    ```
    - Access data using the functions shown in the table below. For example, to access 'GS-Signal':
    
    ```r
    gssignal <- RLHub::gs_signal()
    ```
    - For further details, please see the <a href='https://rlbase-data.s3.amazonaws.com/misc/rlhub_vignette.html' target='_blank'>RLHub vignette</a>.
    
  <br>
  
  * **Direct download**
    - All files are in `.rda` (RData) format and have a direct download link.
    - For example, to download and load `annotations_primary_hg38` in R:
    
    ```r
    tmp <- tempfile()
    download.file('https://rlbase-data.s3.amazonaws.com/RLHub/annotations_primary_hg38.rda', destfile=tmp)
    load(tmp)
    ```
  * **AWS CLI**:
    - Files can also be synced from AWS using the AWS CLI.
    - To download the entire RLHub bucket (", bucket_sizes$RLHub,"), for example:
    
    ```shell
    # conda install -c conda-forge awscli
    aws s3 sync --no-sign-request s3://rlbase-data/RLHub RLHub/  # Downloads the entire folder
    ```
  </details>
  <br>
  
  ")
  tagList(
    fluidRow(
      column(
        width = 12,
        shiny::markdown(md),
        read_csv(system.file("extdata", "metadata.csv", package = "RLHub"), show_col_types = FALSE, progress = FALSE) %>% 
          mutate(
            Direct_Download = paste0("<a class='button' href='", file.path(RLSeq:::RLBASE_URL, RDataPath),
                                     "' target='_blank' download><i class='fa fa-download'></i> Link</a>"),
            RLHub_Accessor = paste0("<code>RLHub::", Tags, "()</code>")
          ) %>%
          dplyr::select(Title, Description, Genome, RDataClass, Direct_Download, RLHub_Accessor) %>%
          kableExtra::kable(format = "html", escape = FALSE) %>%
          kableExtra::kable_styling("hover") %>%
          HTML()
      )
    )
  )
}

processedDataDownloads <- function(bucket_sizes, rlsamples) {
  
  md <- paste0("
  ### Processed data files
  
  All data in *RLBase* were processed using the
  <a href='https://github.com/Bishop-Laboratory/RLPipes' target='_blank'>*RLPipes*</a>
  program (part of *RLSuite*). Peaks and coverage files were generated from genomic alignments, 
  and the <a href='https://github.com/Bishop-Laboratory/RLSeq' target='_blank'>*RLSeq*</a>
  analysis package (also part of *RLSuite*) was used to analyze the data and generate 
  an HTML report. *RLBase* provides both bulk and fine-grained access to these data.
  
  <details>
  <summary><strong>Data details</strong> (and bulk download instructions)</summary>
  
  <br>
  
  Data sets (below) can be downloaded in bulk using the AWS CLI.
  
  * **Peaks** (", bucket_sizes$peaks,")
    - Peaks were called from genomic alignments (`*.bam`) using <a href='https://github.com/macs3-project/MACS' target='_blank'>`macs3`</a>.
      When available, an input control was used. 
      See <a href='https://github.com/Bishop-Laboratory/RLPipes' target='_blank'>*RLPipes*</a>.
    - Files are uncompressed, in `.broadPeak` (<a href='https://genome.ucsc.edu/FAQ/FAQformat.html#format13' target='_blank'>broadPeak</a>) format.
    - AWS CLI: `aws s3 sync --no-sign-request s3://rlbase-data/peaks/ peaks/`
  * **Coverage** (", bucket_sizes$coverage,")
    - Coverage tracks were generated from genomic alignments (`*.bam`) with 
      <a href='https://deeptools.readthedocs.io/en/develop/' target='_blank'>`deepTools`</a>. 
      See <a href='https://github.com/Bishop-Laboratory/RLPipes' target='_blank'>*RLPipes*</a>.
    - Files are in `.bw` (<a href='https://genome.ucsc.edu/FAQ/FAQformat.html#format6.1' target='_blank'>bigWig</a>) format.
    - AWS CLI: `aws s3 sync --no-sign-request s3://rlbase-data/coverage/ coverage/`
  * **RLRanges** (from *RLSeq*) (", bucket_sizes$rlranges, ")
    - The *RLSeq* analysis package was used to analyze the peak and coverage tracks to assess quality, genomic annotation enrichment, 
      and other features of interest. The usage of *RLSeq* is found in the vignette 
      <a href='https://rlbase-data.s3.amazonaws.com/misc/analyzing-rloop-data-with-rlseq.html' target='_blank'>here</a>.
      See <a href='https://github.com/Bishop-Laboratory/RLSeq' target='_blank'>*RLSeq*</a>.
    - The files are compressed `.rds` files. They can be loaded with the `readRDS()` function in R. 
    - AWS CLI: `aws s3 sync --no-sign-request s3://rlbase-data/rlranges/ rlranges/`
  * ***RLSeq* Reports** (", bucket_sizes$reports,")
    - The *RLSeq* analysis package also generates quality and analysis reports of samples analyzed with it. 
      For each sample in *RLBase*, a report was generated (via the `RLSeq::report()` command).
      See <a href='https://github.com/Bishop-Laboratory/RLSeq' target='_blank'>*RLSeq*</a>.
    - The files are in uncompressed `*.html` format.
    - AWS CLI: `aws s3 sync --no-sign-request s3://rlbase-data/reports/ reports/`
  * **FASTQ Stats** (", bucket_sizes$fastq_stats, ")
    - Quality statistics for the raw reads were generated via the `fastp` program
      (<a href='https://github.com/OpenGene/fastp' target='_blank'>link</a>).
      See <a href='https://github.com/Bishop-Laboratory/RLPipes' target='_blank'>*RLPipes*</a>.
    - The files are in uncompressed `*.json` format.
    - AWS CLI: `aws s3 sync --no-sign-request s3://rlbase-data/fastq_stats/ fastq_stats/`
  * **BAM Stats** (", bucket_sizes$bam_stats, ")
    - Quality statistics for the genomic alignments (`*.bam` files) were generated via the
      `samtools` program (<a href='http://www.htslib.org/' target='_blank'>link</a>).
      See <a href='https://github.com/Bishop-Laboratory/RLPipes' target='_blank'>*RLPipes*</a>.
    - The files are in uncompressed `*.txt` format.
    - AWS CLI: `aws s3 sync --no-sign-request s3://rlbase-data/bam_stats/ bam_stats/`
  * **Quantified expression** (", bucket_sizes$quant, ")
    - Expression samples were quantified via *Salmon* `v1.5.2` (<a href='https://github.com/COMBINE-lab/salmon'>link</a>). 
      See <a href='https://github.com/Bishop-Laboratory/RLPipes' target='_blank'>*RLPipes*</a>.
    - The files are in compressed archive (`*.tar.xz`) format. The archive contains the output of salmon as described 
      in the *Salmon* documentation (<a href='https://salmon.readthedocs.io/en/latest/file_formats.html'>link</a>)
    - AWS CLI: `aws s3 sync --no-sign-request s3://rlbase-data/quant/ quant/`
      
  </details>
  <br>
  
  The **full list** of samples in *RLBase* and their corresponding download links are listed below:
    
  ")
  
  tagList(
    fluidRow(
      column(
        width = 12,
        shiny::markdown(md),
        dataTableOutput('rlsamplesDownloadFiles')
      )
    )
  )
}


rawDataDownloads <- function() {
  md <- "
  ### Raw data
  
  The raw data was downloaded from <a href='https://www.ncbi.nlm.nih.gov/sra' target='_blank'>SRA</a>
  programmatically as part of the <a href='https://github.com/Bishop-Laboratory/RLPipes' target='_blank'>*RLPipes*</a>
  processing pipeline. Raw reads were aligned to the genome using 
  <a href='https://github.com/bwa-mem2/bwa-mem2' target='_blank'>*bwa-mem2*</a> and uploaded
  to a publicly-accessible 
  <a href='https://uthscsa.box.com/s/529qtbh94z1zqs8rqtwb3gxabalzaya4' target='_blank'>Box folder</a> (1.4 TB).
  
  **Note**: You will be unable to download the entire contents in bulk without a paid Box account. If you need to 
  access these `*.bam` files in bulk, please simply follow the protocol outlined in
  the RLBase-data repository
  (<a href='https://github.com/Bishop-Laboratory/RLBase-data#run-rlpipes-on-all-public-samples' target='_blank'>link</a>). 
  If you are unable to do so, please contact the *RLBase* maintainer 
  (<a href='mailto:millerh1@uthscsa.edu' target='_blank'>Henry Miller</a>) and he will assist you in accessing the data.
  
  ### Other data
  
  Miscellaneous data which provide support to *RLBase* and the other software in *RLSuite* are also 
  available for download if desired. 
  
  * R-loop forming sequences (RLFS)
    - R-loop forming sequenes were discovered for each genome that has gene annotations
      (48 in total; see <a href='https://rlbase-data.s3.amazonaws.com/misc/available_genomes.tsv' target='_blank'>available genomes</a>) using the 
      <a href='https://github.com/piroonj/QmRLFS-finder' target='_blank'>*QmRLFS-finder*</a> program
      and converted to <a href='https://genome.ucsc.edu/FAQ/FAQformat.html#format1' target='_blank'>BED format</a>. 
    - They can be accessed in two main ways:
      * Bulk download: `aws s3 sync --no-sign-request s3://rlbase-data/rlfs-beds/ .`
      * Direct download of individual files (`https://rlbase-data.s3.amazonaws.com/rlfs-beds/<UCSC_GENOME>.rlfs.bed`). Where `UCSC_GENOME` is replaced by 
        the genome of interest. For example, 'hg38' would be `https://rlbase-data.s3.amazonaws.com/rlfs-beds/hg38.rlfs.bed`.
  * Quality ML Models
    - These models are used by <a href='https://github.com/Bishop-Laboratory/RLSeq' target='_blank'>*RLSeq*</a>
      to predict whether a sample robustly ('POS') or poorly ('NEG') maps R-loops. The full workflow by which 
      they are generated is found in the 
      <a href='https://github.com/Bishop-Laboratory/RLBase-data#build-discriminator-model' target='_blank'>RLBase-data repo</a>.
    - Download all files in builk via `aws s3 sync --no-sign-request s3://rlbase-data/misc/model/ .`
    - Download RData models via <a href='https://github.com/Bishop-Laboratory/RLHub' target='_blank'>*RLHub*</a> (does not include HTML report). See `?RLHub:::models`.
    - Model-building summary HTML report is available from direct download (<a href='https://rlbase-data.s3.amazonaws.com/misc/model/FFT-classifier.html' target='_blank'>link</a>).
  * Cohesin peaks
    - Manually-curated STAG2 and STAG1 ChIP-Seq data reprocessed by the RLHub authors. 
      They are the same STAG1 and STAG2 peaks described in <a href='https://academic.oup.com/nar/article/48/10/5639/5827199' taret='_blank'>*Pan et al., 2020*</a>.
    - The file format is uncompressed broadPeak (`*.broadPeak`). 
    - The processed form of these data is provided within <a href='https://github.com/Bishop-Laboratory/RLHub' target='_blank'>*RLHub*</a>. See `?RLHub::annotations`.
    - The steps used for processing are provided in the 
      <a href='https://github.com/Bishop-Laboratory/RLBase-data/blob/main/scripts/getGenomicFeatures.R#L559-L581' target='_blank'>RLBase-data repo</a>.
    - BroadPeak files can be downloaded in bulk `aws s3 sync --no-sign-request s3://rlbase-data/misc/cohesin_peaks/`.
    
  **Note**: Any other desired data will be provided upon reasonable request to the RLBase maintainer (<a href='mailto:millerh1@uthscsa.edu' target='_blank'>Henry Miller</a>).
  "
  
  tagList(
    fluidRow(
      column(
        width = 12,
        shiny::markdown(md)
      )
    )
  )
}
