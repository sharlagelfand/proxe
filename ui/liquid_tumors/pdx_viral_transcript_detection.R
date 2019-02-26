ui_liquid_tumors_pdx_viral_transcript_detection <- function() {
  tabPanel(
    "PDX Viral Transcript Detection",
    h1("Liquid Tumor PDX Viral Transcript Detection"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        radioButtons(
          inputId = "pdx_viral_transcript_measure",
          label = "Select a measure",
          choices = c(
            "log2(FPKM)" = "log2_fpkm",
            "FPKM" = "fpkm",
            "log(Counts)" = "log_counts",
            "Counts" = "counts"
          ),
          selected = "log2_fpkm"
        ),
        
        radioButtons(
          inputId = "pdx_viral_transcript_all_transcripts",
          label = "Select transcripts",
          choices = c("All", 
                      "By transcript" = "transcript",
                      "By virus" = "virus"),
          selected = "All"
        ),
        
        conditionalPanel(
          condition = "input.pdx_viral_transcript_all_transcripts == 'transcript'",
          selectizeInput(
            inputId = "pdx_viral_transcript_transcripts",
            label = "Transcript:",
            choices = sort(rownames(virusseq_fpkm_matrix)),
            selected = NULL,
            multiple = TRUE
          )
        ),
        
        conditionalPanel(
          condition = "input.pdx_viral_transcript_all_transcripts == 'virus'",
          checkboxGroupInput(
            inputId = "pdx_viral_transcript_virus",
            label = "",
            choices = c("HTLV1 transcripts" = "HTLV1",
                        "EBV transcripts" = "EBV", 
                        "Hep C transcripts" = "Hep C",
                        "HHV5 transcripts" = "HHV5",
                        "HPV transcripts" = "HPV",
                        "XMRV transcripts" = "XMRV"),
            selected = NULL
          )
        ),
        
        radioButtons(
          inputId = "pdx_viral_transcript_selection_method",
          label = "Type of sample input",
          choices = c(
            "All samples" = "all",
            "Select line names" = "line_name",
            "Select WHO Category" = "who_category",
            "Select WHO Classification" = "who_classification",
            "Click rows in Database Explorer" = "database_explorer"
          ),
          selected = "all"
        ),
        
        conditionalPanel(
          condition = "input.pdx_viral_transcript_selection_method == 'line_name'",
          selectInput(
            inputId = "pdx_viral_transcript_line_name",
            label = "Type or select line names",
            choices = colnames(virusseq_fpkm_matrix),
            selected = NULL,
            multiple = TRUE
          )
        ),
        
        conditionalPanel(
          condition = "input.pdx_viral_transcript_selection_method == 'who_category'",
          selectizeInput(
            inputId = "pdx_viral_transcript_who_category",
            label = "WHO Category",
            choices = unique(df[["WHO Category"]]),
            multiple = TRUE,
            selected = "AML"
          )
        ),
        
        conditionalPanel(
          condition = "input.pdx_viral_transcript_selection_method == 'who_classification'",
          selectizeInput(
            inputId = "pdx_viral_transcript_who_classification",
            label = "WHO Classification",
            choices = unique(df[["WHO Classification"]]),
            multiple = TRUE,
            selected = NULL
          )
        ),
        
        downloadLink("pdx_viral_transcript_download", "Download associated data")
      ),
      mainPanel(
        plotOutput(
          "plot_pdx_viral_transcript_detection",
          height = 1000,
          width = 1000
        )
      )
    )
  )
}
