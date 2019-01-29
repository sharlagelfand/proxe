server_liquid_tumors_database_explorer <- function(input, output, session) {

  # liquid data table
  output$table <- DT::renderDataTable({
    cols_selected <- c(input$check2_liquid_administrative, input$check2_liquid_patient, input$check2_liquid_tumor, input$check2_liquid_pdx)
    if (is.null(cols_selected)) {
      data.frame("no variables selected" = c("no variables selected"))
    } else {
      data <- df[, cols_selected, drop = FALSE]
      # todo: perhaps work on column sorting here.
      data
      # # note this code changes formatting of data table. Useful to play with later.
      # # this is a substitute for 'data' above.
      #   datatable(data) %>% formatStyle(
      #     'PDX Name',
      #     backgroundColor = styleInterval(3.4, c('gray', 'yellow'))
      #   )
    }
  },
  # class="cell-border stripe", # option to add thin vertical line between columns
  # caption="Note: table filtering (below) resets upon new column selections (left).",
  style = "default", # alternative: "bootstrap"
  escape = FALSE,
  # From SPK: Consider escaping only 'PDF' and future columns that contain hyperlinks via this tip:
  # Note: Besides TRUE and FALSE, you can also specify which columns you want to escape, e.g.
  # datatable(m, escape = 1)  # escape the first column
  # datatable(m, escape = 2)  # escape the second column
  # datatable(m, escape = c(TRUE, FALSE))  # escape the first column
  # colnames(m) = c('V1', 'V2')
  # datatable(m, escape = 'V1')
  filter = "top",
  server = FALSE, # note this means the entire dataframe is sent to user. Should be fine.
  rownames = FALSE,
  extensions = c("ColReorder"), # ,'Buttons'), #'ColVis',,,'Responsive'
  options = list(
    # set label for upper-right search box.
    language = list(
      search = "Search table:",
      info = "Showing _START_ to _END_ of _TOTAL_ PDX lines"
    ), # Other ideas: "Filter:", "Filter entire table:", "Search entire table:"
    # options related to extensions:
    dom = "Rlfrtip",
    colReorder = list(realtime = TRUE),
    # dom = 'C<"clear">lfrtip', # testing colVis
    # colVis = list(activate="mouseover"),#"click"), # testing colVis
    # TODO: the major questions with ColVis are whether we can...
    # 1) capture the hidden/not hidden column information
    # 2) format the dropdown to be pretty and more intuitive
    # 3) easily create a 'select all/none' -- old one might work too.
    # 4) create some kind of 'reset' (browser refresh might be good enough)
    # Update: ColVis status is 'retired' in favor of less-clear 'Buttons' But I think it's not in `DT`.
    # Buttons was just added but the development version breaks my table -- waiting... 3/18/16
    # dom = 'Bfrtip',
    # buttons = c('copy', 'excel', 'pdf', 'print', 'colvis'),

    # standard options:
    orderClasses = TRUE,
    searchHighlight = TRUE,
    pageLength = 5,
    lengthMenu = list(c(5, 10, 25, 50, 100, -1), c(5, 10, 25, 50, 100, "All"))
  )
  )

  # liquid plots based on filtering data table
  output$plot_various <- renderPlot({
    req(input$table_rows_all)
    filtered_row_inds <- input$table_rows_all

    # choose type of plot
    if (input$plotType == "hist") {
      hist_x <- df[filtered_row_inds, input$hist_var]
      hist_xlab <- input$hist_var
      if (input$hist_log == TRUE) {
        hist_x <- log10(hist_x)
        hist_xlab <- paste0(hist_xlab, " (log10)")
      }
      # find a way to use the table output by output$table above.
      if (input$breaks != "custom") {
        hist_breaks <- input$breaks
      } else {
        hist_breaks <- input$breakCount
      }
      hist(hist_x,
        breaks = hist_breaks,
        main = "Histogram of selected variable.\nUpdates based on filtering above.",
        xlab = hist_xlab,
        col = "skyblue", # rgb(66,139,202,maxColorValue = 255),
        border = "white"
      )
    } else if (input$plotType == "scatter") {
      scat_x <- df[filtered_row_inds, input$scat_var_x]
      scat_y <- df[filtered_row_inds, input$scat_var_y]
      plot(
        x = scat_x, y = scat_y,
        main = "Scatterplot of selected variables.\nUpdates based on filtering above.",
        xlab = input$scat_var_x, ylab = input$scat_var_y, col = "royalblue", pch = 19
      )
      # TODO goal: switch this with an interactive plot via rCharts or something else.
      # this url might help: https://gallery.shinyapps.io/095-plot-interaction-advanced/
    } else if (input$plotType == "bar") {
      bar_table <- table(df[filtered_row_inds, input$bar_var])
      op <- par(no.readonly = TRUE)
      # change settings
      par(mar = c(5, 18, 2, 2) + 0.1)
      num_vars <- length(levels(df[, input$bar_var]))
      par(cex = 1 + log(7 / num_vars, base = 100)) # simply scaling plot size to number of variables
      barplot(rev(bar_table),
        las = 2, horiz = TRUE, col = rainbow(length(bar_table)), border = "white", xlab = input$bar_var,
        main = "Barplot of counts in selected category.\nUpdates based on filtering above."
      )
      # reset settings
      par(op)
    } else if (input$plotType == "scatbox") {
      scatbox_df <- df[filtered_row_inds, ]
      scatbox_formula <- get(input$scatbox_num) ~ get(input$scatbox_cat)

      # change settings
      par(mar = c(5, 18, 3, 2) + 0.1)
      num_vars <- length(levels(df[, input$scatbox_cat]))
      par(cex = 1.3 + log(7 / num_vars, base = 100)) # scales plot to number of variables
      # Done: incorporate proportionality to number of variables into height of this (and barplot).
      # Failed avenues: par(din), dev.size(), 'height' parameter in different parts of UI.,
      # try: check other par() parameters
      # try: ?png() or ?x11()
      # best idea: TODO could perhaps still improve at the UI height specification.

      # TODO: adjust par(mar) according to length of variable names so they aren't cut off. (Also for barplot.)
      # or adjust long variable names to a maximum length.
      # now may need to incorporate num_vars or par(cex) into this calculation
      # reverse vertical order of categories
      scatbox_df[, input$scatbox_cat] <- with(scatbox_df, factor(get(input$scatbox_cat), levels = rev(levels(get(input$scatbox_cat)))))
      beeswarm(scatbox_formula,
        data = scatbox_df,
        log = FALSE, pch = 21, col = rainbow(5), bg = "gray", corral = "wrap",
        horizontal = TRUE, las = 2, ylab = "", xlab = input$scatbox_num,
        main = "1D-scatterplot + boxplot of counts in selected category.\nUpdates based on filtering above.",
        add = F
      )
      boxplot(scatbox_formula,
        data = scatbox_df,
        outline = FALSE, horizontal = TRUE, add = T, col = "#00000000", las = 2, xlab = input$scatbox_num
      )
      # reset settings
      par(op)
    } else if (input$plotType == "ctable_plot") {
      # TODO: add to ui.R
      tablefunc_df <- df[filtered_row_inds, ]
      # plot(table(tablefunc_df[,input$tablevar1],tablefunc_df[,input$tablevar2]))  # works, but ugly.
      mosaicplot(table(tablefunc_df[, input$ctable_plot_var1], tablefunc_df[, input$ctable_plot_var2]),
        color = rainbow(8), main = "Congingency table plot.\nUpdates based on filtering above",
        cex.axis = 0.7
      ) # same -- could optimize with color etc?
      # TODO: fix main and axis labels, maybe margins.
      # qplot(x=table(tablefunc_df[,input$tablevar1],tablefunc_df[,input$tablevar2]),
      # stat="summary")  # doesn't work.
      # ggplot(as.data.frame(table(tablefunc_df)), aes_string(x=input$tablevar1, fill = input$tablevar2)) +
      # geom_bar(stat="identity")  # doesn't work.
    } else {
      input$plotType <- plot(0)
    }

    ### TODO: I would like to switch this to ggplot/qplot, but it doesn't take variables where 'input$hist_var' is.
    # aes_string is a hint.
    ### issue: 'count' scale is off compared to base::hist method above.
    # qplot(input$hist_var, data=df[filtered_row_inds,], geom="histogram",
    # main="histogram of selected variable: \n updates based on selections above")
    #     m <- ggplot(df[filtered_row_inds,], aes_string(x=input$hist_var))
    #     m + geom_histogram(fill="blue") #input$hist_binwidth)
  })

  # download the filtered liquid table
  output$download_filtered <- downloadHandler("PRoXe_filtered.xlsx", content = function(filename) {
    filtered_row_inds <- input$table_rows_all
    dfxl <- df[filtered_row_inds, 1:(obInvisRet_ind - 1), drop = FALSE]
    #     WriteXLS(dfxl, ExcelFileName=filename)
    #     write.table(dfxl, filename, quote=FALSE,sep="|",na="",row.names=FALSE)
    write.xlsx(x = dfxl, file = filename, row.names = FALSE, showNA = FALSE)
  })
}
