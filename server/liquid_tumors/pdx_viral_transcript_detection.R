server_liquid_tumors_pdx_viral_transcript_detection <-
  function(input, output, server) {
    output$plot_pdx_viral_transcript_detection <- renderPlot({
      
      pdx_viral_transcript_detection_data <- {
        if (input$pdx_viral_transcript_measure == "log2_fpkm")
          log(virusseq_fpkm_matrix + 0.01, base = 2)
        else if (input$pdx_viral_transcript_measure == "fpkm")
          virusseq_fpkm_matrix
        else if (input$pdx_viral_transcript_measure == "counts")
          virusseq_counts_matrix
      }
      
      pdx_viral_transcript_detection_plot_title <- {
        if (input$pdx_viral_transcript_measure == "log2_fpkm")
          "Log2( FPKM + 0.01)"
        else if (input$pdx_viral_transcript_measure == "fpkm")
          "FPKM"
        else if (input$pdx_viral_transcript_measure == "counts")
          "Counts"
      }
      
      virusseq_matrix_selected_genes <-
        pdx_viral_transcript_detection_data[rownames(pdx_viral_transcript_detection_data) %in% input$pdx_viral_transcript_genes,]
      
      heatmap.2(
        virusseq_matrix_selected_genes,
        main = pdx_viral_transcript_detection_plot_title,
        notecol = "black",
        density.info = "none",
        trace = "none",
        margins = c(19, 16),
        col = my_palette,
        keysize = 0.75,
        dendrogram = "both"
      )
    })
  }
