server_liquid_tumors_pdx_viral_transcript_detection <-
  function(input, output, server) {
    output$plot_pdx_viral_transcript_detection <- renderPlot({
      pdx_viral_transcript_detection_data <- {
        if (input$pdx_viral_transcript_measure == "log2_fpkm") {
          log(virusseq_fpkm_matrix + 0.01, base = 2)
        } else if (input$pdx_viral_transcript_measure == "fpkm") {
          virusseq_fpkm_matrix
        } else if (input$pdx_viral_transcript_measure == "counts") {
          virusseq_counts_matrix
        } else if (input$pdx_viral_transcript_measure == "log_counts") {
          log(virusseq_counts_matrix + 0.01)
        }
      }

      pdx_viral_transcript_detection_plot_title <- {
        if (input$pdx_viral_transcript_measure == "log2_fpkm") {
          "Log2( FPKM + 0.01)"
        } else if (input$pdx_viral_transcript_measure == "fpkm") {
          "FPKM"
        } else if (input$pdx_viral_transcript_measure == "counts") {
          "Counts"
        } else if (input$pdx_viral_transcript_measure == "log_counts") {
          "Log(Counts)"
        }
      }

      virusseq_matrix_selected_transcripts <- {
        if (input$pdx_viral_transcript_all_transcripts == "no") {
          pdx_viral_transcript_detection_data[rownames(pdx_viral_transcript_detection_data) %in% input$pdx_viral_transcript_transcripts, ]
        }
        else {
          pdx_viral_transcript_detection_data
        }
      }

      virusseq_matrix_selected_lines <- {
        if (input$pdx_viral_transcript_selection_method == "all") {
          virusseq_matrix_selected_transcripts
        }
        else if (input$pdx_viral_transcript_selection_method == "line_name") {
          virusseq_matrix_selected_transcripts[, input$pdx_viral_transcript_line_name, drop = FALSE] # retain matrix structure
        }
        else if (input$pdx_viral_transcript_selection_method == "database_explorer") {
          selected_ids <- input$table_rows_selected
          sample_names <- df[selected_ids, "PDX Name"]
          virusseq_matrix_selected_transcripts[, sample_names, drop = FALSE]
        }
        else if (input$pdx_viral_transcript_selection_method == "who") {
          sample_names <- df[
            df[["WHO Category"]] %in% input$pdx_viral_transcript_who,
            "PDX Name"
          ]
          valid_sample_names <- sample_names[sample_names %in% colnames(virusseq_matrix_selected_transcripts)]
          virusseq_matrix_selected_transcripts[, valid_sample_names, drop = FALSE]
        }
        else {
          virusseq_matrix_selected_transcripts
        }
      }

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
