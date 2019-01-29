server_liquid_tumors_contingency_table <- function(input, output, session) {
  output$table_various <- renderTable({
    filtered_row_inds <- input$table_rows_all
    ctable_df <- df[filtered_row_inds, ]

    # remove mCLP and Luc lines
    to_remove <- c(
      grep("Luc", ctable_df$`PDX Name`),
      grep("mCLP", ctable_df$`PDX Name`)
    )
    ctable_df <- ctable_df[-to_remove, ]

    if (input$ctable_numcats == 1) {
      tmp_table <- table(ctable_df[, input$tablevar1])
      names(dimnames(tmp_table)) <- "category"
      tmp_table
    } else if (input$ctable_numcats == 2) {
      as.data.frame.matrix(table(ctable_df[, input$tablevar1], ctable_df[, input$tablevar2]))
    }
  }, striped = T, hover = T, bordered = T, rownames = T)
}
