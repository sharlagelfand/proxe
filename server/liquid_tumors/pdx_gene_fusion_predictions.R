server_liquid_tumors_pdx_gene_fusion_predictions <-
  function(input, output, server) {
    
    output$plot_pdx_gene_fusion_predictions <- renderPlot({
      
      fusion_inverses <- gene_fusion_predictions %>%
        distinct(fusion_name, inverse_exists, pair)
      
      gene_fusion_predictions_filter_fusions <- switch(
        input$pdx_gene_fusion_predictions_select_fusions,
        All = gene_fusion_predictions,
        fusion_name = gene_fusion_predictions[gene_fusion_predictions$fusion_name %in% input$pdx_gene_fusion_predictions_fusion_name, ],
        fusion_reads = gene_fusion_predictions[gene_fusion_predictions$spanning_frags >= input$pdx_gene_fusion_predictions_fusions_reads, ],
        fusion_junction_reads = gene_fusion_predictions[gene_fusion_predictions$spanning_frags >= input$pdx_gene_fusion_predictions_fusions_reads &
                                                          gene_fusion_predictions$junction_reads >= input$pdx_gene_fusion_predictions_junction_reads, ]
      )
      
      gene_fusion_predictions_filter_samples <- switch(
        input$pdx_gene_fusion_predictions_selection_method,
        
        All = gene_fusion_predictions_filter_fusions %>%
          complete(pdx_name, fusion_name, fill = list(total_reads = 0)),
        
        line_name = gene_fusion_predictions_filter_fusions[gene_fusion_predictions_filter_fusions$pdx_name %in% input$pdx_gene_fusion_predictions_line_name,],
        
        who_category = {
          sample_names <- df[df[["WHO Category"]] %in% input$pdx_gene_fusion_predictions_who_category,
                             "PDX Name"]
          gene_fusion_predictions_filter_fusions[gene_fusion_predictions_filter_fusions$pdx_name %in% sample_names,]
        },
        
        who_classification = {
          sample_names <- df[df[["WHO Classification"]] %in% input$pdx_gene_fusion_predictions_who_classification,
                             "PDX Name"]
          gene_fusion_predictions_filter_fusions[gene_fusion_predictions_filter_fusions$pdx_name %in% sample_names,]
        },
        
        database_explorer = {
          sample_names <- df[input$table_rows_selected, "PDX Name"]
          gene_fusion_predictions_filter_fusions[gene_fusion_predictions_filter_fusions$pdx_name %in% sample_names,]
        }
      )
      
      gene_fusion_predictions_filter_samples <- gene_fusion_predictions_filter_samples %>%
        left_join(fusion_inverses, by = "fusion_name", suffix = c("", "_filter")) %>%
        arrange(!inverse_exists_filter, pair_filter, total_reads)
      
      gene_fusion_predictions_matrix <-
        gene_fusion_predictions_filter_samples[, c("pdx_name", "fusion_name", "total_reads")] %>%
        spread(pdx_name, total_reads, fill = 0) %>%
        as.data.frame()
      
      row.names(gene_fusion_predictions_matrix) <- gene_fusion_predictions_matrix[["fusion_name"]]
      
      gene_fusion_predictions_matrix <- gene_fusion_predictions_matrix[, names(gene_fusion_predictions_matrix) != "fusion_name"] %>%
        as.matrix()
      
      gene_fusion_predictions_matrix <- log(gene_fusion_predictions_matrix + 1,
                                            base = 2)
      
      validate(
        need(
          nrow(gene_fusion_predictions_matrix) > 1 &
            ncol(gene_fusion_predictions_matrix) > 1,
          "Please select at least two fusions and two samples."
        )
      )
      
      # heatmap.2 breaks if all values are identical AND they are all zeros (does not happen if all identical and non-zero). The fix is to remove the colour key, see: https://stackoverflow.com/questions/9721785/r-trying-to-make-a-heatmap-from-a-matrix-all-the-values-in-the-matrix-are-the
      data_all_zeros <- all(unique(c(gene_fusion_predictions_matrix)) == 0)
      
      if (!data_all_zeros) {
        heatmap.2(
          gene_fusion_predictions_matrix,
          main = "log2(Total evidence + 1)",
          notecol = "black",
          density.info = "none",
          trace = "none",
          margins = c(19, 16),
          keysize = 0.75,
          dendrogram = "column",
          Rowv = FALSE
        )
      }
      else {
        heatmap.2(
          gene_fusion_predictions_matrix,
          main = "log2(Total evidence + 1)",
          notecol = "black",
          density.info = "none",
          trace = "none",
          margins = c(19, 16),
          key = FALSE,
          dendrogram = "column",
          Rowv = FALSE
        )
      }
    })
    
    output$pdx_viral_transcript_download <- downloadHandler(
      filename = function() {
        "liquid_tumor_pdx_gene_fusion_predictions.csv"
      },
      content = function(file) {
        write.csv(gene_fusion_predictions, file, row.names = FALSE)
      }
    )
    
    output$ui_plot_pdx_gene_fusion_predictions_detection <- renderUI({
      plotOutput("plot_pdx_gene_fusion_predictions",
                 height = input$pdx_gene_fusion_predictions_plot_height,
                 width = input$pdx_gene_fusion_predictions_plot_width
      )
    })
  }
