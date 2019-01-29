server_liquid_tumors_pdx_viral_transcript_detection <- function(input, output, server) {
  output$plot_pdx_viral_transcript_detection <- renderPlot({
    virusseq_fpkm_matrix_selected_genes <- virusseq_fpkm_matrix[rownames(virusseq_fpkm_matrix) %in% input$pdx_viral_transcript_genes, ]

    heatmap.2(log(virusseq_fpkm_matrix_selected_genes + 0.01, base = 2),
      main = "Log2( FPKM + 0.01 )",
      notecol = "black",
      density.info = "none",
      trace = "none",
      col = my_palette,
      keysize = 0.75,
      dendrogram = "both"
    )
  })
}
