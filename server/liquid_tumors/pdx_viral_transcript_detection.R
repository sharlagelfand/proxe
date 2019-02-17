server_liquid_tumors_pdx_viral_transcript_detection <-
  function(input, output, server) {
    output$plot_pdx_viral_transcript_detection <- renderPlot({
      
      pdx_viral_transcript_detection_data <- switch(
        input$pdx_viral_transcript_measure,
        log2_fpkm = log(virusseq_fpkm_matrix + 0.01, base = 2),
        fpkm = virusseq_fpkm_matrix,
        counts = virusseq_counts_matrix,
        log_counts = log(virusseq_counts_matrix + 0.01)
      )
      
      pdx_viral_transcript_detection_plot_title <- switch(
        input$pdx_viral_transcript_measure,
        log2_fpkm = "Log2( FPKM + 0.01)",
        fpkm = "FPKM",
        counts = "Counts",
        log_counts = "Log(Counts)"
      )
      
      virusseq_matrix_selected_transcripts <- switch(
        input$pdx_viral_transcript_all_transcripts,
        no = pdx_viral_transcript_detection_data[rownames(pdx_viral_transcript_detection_data) %in% input$pdx_viral_transcript_transcripts,],
        yes = pdx_viral_transcript_detection_data)
      
      viral_transcripts_from_database_explorer <-
        function(rows_selected) {
          sample_names <- df[rows_selected, "PDX Name"]
          virusseq_matrix_selected_transcripts[, sample_names, drop = FALSE]
        }
      
      viral_transcripts_from_who_classification <-
        function(who_classification) {
          sample_names <- df[df[["WHO Category"]] %in% who_classification,
                             "PDX Name"]
          valid_sample_names <-
            sample_names[sample_names %in% colnames(virusseq_matrix_selected_transcripts)]
          virusseq_matrix_selected_transcripts[, valid_sample_names, drop = FALSE]
        }
      
      virusseq_matrix_selected_lines <- switch(
        input$pdx_viral_transcript_selection_method,
        all = virusseq_matrix_selected_transcripts,
        line_name = virusseq_matrix_selected_transcripts[, input$pdx_viral_transcript_line_name, drop = FALSE],
        # retain matrix structure
        database_explorer = viral_transcripts_from_database_explorer(input$table_rows_selected),
        who = viral_transcripts_from_who_classification(input$pdx_viral_transcript_who),
        virusseq_matrix_selected_transcripts
      )
      
      # TODO: heatmap.2 breaks if less than 2 lines and less than 2 transcripts are selected; other visualization? indication that 2 must always be selected?
      heatmap.2(
        virusseq_matrix_selected_lines,
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
