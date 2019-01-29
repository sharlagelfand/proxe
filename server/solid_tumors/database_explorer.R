server_solid_tumors_database_explorer <- function(input, output, server) {
  output$solid_table <- DT::renderDataTable({
    cols_selected <- c(input$check2_solid_administrative, input$check2_solid_tumor, input$check2_solid_pdx) # note took out patient because no data yet.
    if (is.null(cols_selected)) {
      data.frame("no variables selected" = c("no variables selected"))
    } else {
      data <- solid[, cols_selected, drop = FALSE]
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

  # download solid filtered data
  output$solid_download_filtered <- downloadHandler("PRoXe_solid_filtered.xlsx", content = function(filename) {
    filtered_row_inds <- input$solid_table_rows_all
    dfxl <- solid[filtered_row_inds, , drop = FALSE]
    #     WriteXLS(dfxl, ExcelFileName=filename)
    #     write.table(dfxl, filename, quote=FALSE,sep="|",na="",row.names=FALSE)
    write.xlsx(x = dfxl, file = filename, row.names = FALSE, showNA = FALSE)
  })

  # lower plot for solid tumor Database Explorer
  output$solid_plot_various <- renderPlot({
    filtered_row_inds <- input$solid_table_rows_all

    # choose type of plot
    if (input$solid_plotType == "hist") {
      hist_x <- solid[filtered_row_inds, input$solid_hist_var]
      hist_xlab <- input$solid_hist_var
      if (input$solid_hist_log == TRUE) {
        hist_x <- log10(hist_x)
        hist_xlab <- paste0(hist_xlab, " (log10)")
      }
      # find a way to use the table output by output$table above.
      if (input$solid_breaks != "custom") {
        hist_breaks <- input$solid_breaks
      } else {
        hist_breaks <- input$solid_breakCount
      }
      # note hist() results in a transient error when app is first loaded bedcause input$table_rows_all is initially empty
      # could fix if desired, but doesn't affect how the app works.
      hist(hist_x,
        breaks = hist_breaks,
        main = "Histogram of selected variable.\nUpdates based on filtering above.",
        xlab = hist_xlab,
        col = "skyblue", # rgb(66,139,202,maxColorValue = 255),
        border = "white"
      )
    } else if (input$solid_plotType == "scatter") {
      scat_x <- solid[filtered_row_inds, input$solid_scat_var_x]
      scat_y <- solid[filtered_row_inds, input$solid_scat_var_y]
      plot(
        x = scat_x, y = scat_y,
        main = "Scatterplot of selected variables.\nUpdates based on filtering above.",
        xlab = input$solid_scat_var_x, ylab = input$solid_scat_var_y, col = "royalblue", pch = 19
      )
      # TODO goal: switch this with an interactive plot via rCharts or something else.
      # this url might help: https://gallery.shinyapps.io/095-plot-interaction-advanced/
    } else if (input$solid_plotType == "bar") {
      bar_table <- table(solid[filtered_row_inds, input$solid_bar_var])
      op <- par(no.readonly = TRUE)
      # change settings
      par(mar = c(5, 18, 2, 2) + 0.1)
      num_vars <- length(levels(solid[, input$solid_bar_var]))
      par(cex = 1 + log(7 / num_vars, base = 100)) # simply scaling plot size to number of variables
      barplot(bar_table,
        las = 2, horiz = TRUE, col = rainbow(length(bar_table)), border = "white", xlab = input$solid_bar_var,
        main = "Barplot of counts in selected category.\nUpdates based on filtering above."
      )
      # reset settings
      par(op)
    } else if (input$solid_plotType == "scatbox") {
      scatbox_df <- solid[filtered_row_inds, ]
      # print(str(scatbox_df))
      scatbox_formula <- get(input$solid_scatbox_num) ~ get(input$solid_scatbox_cat)
      # print("here8")
      # change settings
      par(mar = c(5, 18, 3, 2) + 0.1)
      num_vars <- length(levels(solid[, input$solid_scatbox_cat]))
      par(cex = 1 + log(7 / num_vars, base = 100)) # scales plot to number of variables
      # Done: incorporate proportionality to number of variables into height of this (and barplot).
      # Failed avenues: par(din), dev.size(), 'height' parameter in different parts of UI.,
      # try: check other par() parameters
      # try: ?png() or ?x11()
      # best idea: TODO could perhaps still improve at the UI height specification.

      # TODO: adjust par(mar) according to length of variable names so they aren't cut off. (Also for barplot.)
      # or adjust long variable names to a maximum length.
      # now may need to incorporate num_vars or par(cex) into this calculation
      if (input$solid_scatbox_log == "log") {
        scatbox_log <- TRUE
      } else {
        scatbox_log <- FALSE
      }

      beeswarm(scatbox_formula,
        data = scatbox_df,
        log = scatbox_log, pch = 21, col = rainbow(5), bg = "gray", corral = "wrap",
        horizontal = TRUE, las = 2, ylab = "", xlab = input$solid_scatbox_num,
        main = "1D-scatterplot + boxplot of counts in selected category.\nUpdates based on filtering above.",
        add = F
      )
      boxplot(scatbox_formula,
        data = scatbox_df, log = scatbox_log,
        outline = FALSE, horizontal = TRUE, add = T, col = "#00000000", las = 2, xlab = input$solid_scatbox_num
      )
      # reset settings
      par(op)
    } else if (input$solid_plotType == "ctable_plot") {
      # TODO: add to ui.R
      tablefunc_df <- solid[filtered_row_inds, ]
      # plot(table(tablefunc_df[,input$tablevar1],tablefunc_df[,input$tablevar2]))  # works, but ugly.
      mosaicplot(table(tablefunc_df[, input$solid_ctable_plot_var1], tablefunc_df[, input$solid_ctable_plot_var2]),
        color = rainbow(8), main = "Congingency table plot.\nUpdates based on filtering above",
        cex.axis = 0.7
      ) # same -- could optimize with color etc?
      # TODO: fix main and axis labels, maybe margins.
      # qplot(x=table(tablefunc_df[,input$tablevar1],tablefunc_df[,input$tablevar2]),
      # stat="summary")  # doesn't work.
      # ggplot(as.data.frame(table(tablefunc_df)), aes_string(x=input$tablevar1, fill = input$tablevar2)) +
      # geom_bar(stat="identity")  # doesn't work.
    } else {
      input$solid_plotType <- plot(0)
    }

    ### TODO: I would like to switch this to ggplot/qplot, but it doesn't take variables where 'input$hist_var' is.
    # aes_string is a hint.
    ### issue: 'count' scale is off compared to base::hist method above.
    # qplot(input$hist_var, data=df[filtered_row_inds,], geom="histogram",
    # main="histogram of selected variable: \n updates based on selections above")
    #     m <- ggplot(df[filtered_row_inds,], aes_string(x=input$hist_var))
    #     m + geom_histogram(fill="blue") #input$hist_binwidth)
  })
}
