ui_liquid_tumors_pdx_viral_transcript_detection <- function() {
  tabPanel(
    "PDX Viral Transcript Detection",
    h1("Liquid Tumor PDX Viral Transcript Detection"),
    sidebarLayout(
      sidebarPanel(
        selectizeInput(
          inputId = "pdx_viral_transcript_genes",
          label = "Select genes",
          choices = sort(rownames(virusseq_fpkm_matrix)),
          multiple = TRUE
        )
      ),
      mainPanel(
        plotOutput(
          "plot_pdx_viral_transcript_detection",
          height = 1300,
          width = 800
        )
      )
    )
  )
}
