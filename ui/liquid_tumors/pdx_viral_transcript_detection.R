ui_liquid_tumors_pdx_viral_transcript_detection <- function() {
  tabPanel(
    "PDX Viral Transcript Detection",
    h1("Liquid Tumor PDX Viral Transcript Detection"),
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          inputId = "pdx_viral_transcript_measure",
          label = "Select a measure",
          choices = c("log2(FPKM)" = "log2_fpkm",
                      "FPKM" = "fpkm",
                      "Counts" = "counts",
                      "log(Counts)" = "log_counts"),
          selected = "log2_fpkm"
        ),
        
        radioButtons(
          inputId = "pdx_viral_transcript_all_transcripts",
          label = "All transcripts? May be be useful for understanding overall distribution. Overrides whatever is below.",
          choices = c("no", "yes"),
          selected = "no"
        ),
        
        selectizeInput(
          inputId = "pdx_viral_transcript_transcripts",
          label = "Select transcript",
          choices = sort(rownames(virusseq_fpkm_matrix)),
          selected = sort(rownames(virusseq_fpkm_matrix))[1:5],
          multiple = TRUE
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
