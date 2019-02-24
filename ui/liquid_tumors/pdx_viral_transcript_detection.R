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
          label = "All transcripts? May be be useful for understanding overall distribution. Overrides whatever is below.",
          choices = c("No", "Yes"),
          selected = "No"
        ),

        selectizeInput(
          inputId = "pdx_viral_transcript_transcripts",
          label = "Select transcript",
          choices = sort(rownames(virusseq_fpkm_matrix)),
          selected = sort(rownames(virusseq_fpkm_matrix))[1:5],
          multiple = TRUE
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
        )
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
