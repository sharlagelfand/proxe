ui_liquid_tumors_pdx_viral_transcript_detection <- function() {
  tabPanel(
    "PDX Viral Transcript Detection",
    h1("Liquid Tumor PDX Viral Transcript Detection"),
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          inputId = "pdx_viral_transcript_plot",
          label = "Plot type",
          choices = c("Heatmap" = "heatmap",
                      "Distribution" = "distribution"),
          selected = "heatmap"
        ),
        
        radioButtons(
          inputId = "pdx_viral_transcript_measure",
          label = "Select a measure",
          choices = c("log2(FPKM)" = "log2_fpkm",
                      "FPKM" = "fpkm",
                      "Counts" = "counts"),
          selected = "log2_fpkm"
        ),
        
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
          height = 800,
          width = 800
        )
      )
    )
  )
}
