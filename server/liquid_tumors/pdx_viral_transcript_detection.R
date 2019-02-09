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
        else if (input$pdx_viral_transcript_measure == "log_counts")
          log(virusseq_counts_matrix + 0.01)
      }
      
      pdx_viral_transcript_detection_plot_title <- {
        if (input$pdx_viral_transcript_measure == "log2_fpkm")
          "Log2( FPKM + 0.01)"
        else if (input$pdx_viral_transcript_measure == "fpkm")
          "FPKM"
        else if (input$pdx_viral_transcript_measure == "counts")
          "Counts"
        else if (input$pdx_viral_transcript_measure == "log_counts")
          "Log(Counts)"
      }
    
      virusseq_matrix_selected_transcripts <- pdx_viral_transcript_detection_data[rownames(pdx_viral_transcript_detection_data) %in% input$pdx_viral_transcript_transcripts,]
      
      heatmap.2(
        if(input$pdx_viral_transcript_all_transcripts == "no"){virusseq_matrix_selected_transcripts}else{pdx_viral_transcript_detection_data},
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