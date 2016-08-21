# server.R

# note ./global.R is run before any of this.

library(shiny)
# library(WriteXLS)
library(xlsx)
library(ggplot2)
library(DT)
library(beeswarm)
library(stringr)
# library(rCharts)

op <- par(no.readonly = TRUE)

# warn if WHO_Classification is a factor -- this was a temporary fix in clean_data.R for production of a contingency table 2/2016.
if(class(df$`WHO Classification`)=="factor") warning("Reminder: WHO Classification is a factor even though it is highly variable.")

print("didcomehere4")
# Define a server for the Shiny app
shinyServer(function(input, output, session) {  #TODO: read on what 'session' means here.  
  # select/deselect all using action button
  observe({
    updateSelectizeInput(session,inputId="rna_genes",
                   choices=sort(rownames(rnamat_sub)), server=TRUE,
                   selected=c("BCL2","TP53","FLT3","MYC","JAK2"))
    updateSelectizeInput(session,inputId="bar_gene",
                         choices=sort(rownames(rnamat_sub)), server=TRUE,
                         selected=c("BCL2"))
    updateSelectizeInput(session,inputId="oncop_genes",
                          choices=sort(M2_genes), server=TRUE,
                          selected=c("TP53","KRAS","NRAS"))
    if(input$Request_link != 0) updateTabsetPanel(session, inputId="mainNavBar", selected="Line Request/Pricing")
    if(input$Methods_link != 0) updateTabsetPanel(session, inputId="mainNavBar", selected="Methods")

    ## solid tab
    # select all
    if (input$solid_selectall > 0) {
      if (input$solid_selectall %% 2 == 1){
        updateCheckboxGroupInput(session=session, 
          inputId="solid_show_vars",
          choices = names(solid),
          selected = c(names(solid)))
      } else {
        updateCheckboxGroupInput(session=session, 
          inputId="solid_show_vars",
          choices = names(solid),
          selected = c())
      }
    }
    # Line Request link button
    if(input$Request_link_solid != 0) updateTabsetPanel(session, inputId="mainNavBar", selected="Line Request/Pricing")

  })
  
  # Filter data based on selections
  output$table <- DT::renderDataTable({
    cols_selected <- c(input$check2_administrative,input$check2_patient,input$check2_tumor,input$check2_pdx)
    if (is.null(cols_selected)){
      data.frame("no variables selected" = c("no variables selected"))
    } else {
      data <- df[,cols_selected, drop=FALSE]
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
    style="default", # alternative: "bootstrap"
    escape= FALSE,
      # From SPK: Consider escaping only 'PDF' and future columns that contain hyperlinks via this tip:
      # Note: Besides TRUE and FALSE, you can also specify which columns you want to escape, e.g.
        # datatable(m, escape = 1)  # escape the first column
        # datatable(m, escape = 2)  # escape the second column
        # datatable(m, escape = c(TRUE, FALSE))  # escape the first column
        # colnames(m) = c('V1', 'V2')
        # datatable(m, escape = 'V1')
    filter="top",
    server=FALSE, # note this means the entire dataframe is sent to user. Should be fine.
    rownames=FALSE,
    extensions = c('ColReorder'),#,'Buttons'), #'ColVis',,,'Responsive'
    options = list(
      # set label for upper-right search box.
      language = list(
        search = 'Search table:',
        info = 'Showing _START_ to _END_ of _TOTAL_ PDX lines'
      ), # Other ideas: "Filter:", "Filter entire table:", "Search entire table:"
      # options related to extensions:
      dom = 'Rlfrtip',
      colReorder = list(realtime = TRUE), 
      # dom = 'C<"clear">lfrtip', # testing colVis
      # colVis = list(activate="mouseover"),#"click"), # testing colVis
        # TODO: the major questions with ColVis are whether we can...
          #1) capture the hidden/not hidden column information
          #2) format the dropdown to be pretty and more intuitive
          #3) easily create a 'select all/none' -- old one might work too.
          #4) create some kind of 'reset' (browser refresh might be good enough)
      # Update: ColVis status is 'retired' in favor of less-clear 'Buttons' But I think it's not in `DT`.
        # Buttons was just added but the development version breaks my table -- waiting... 3/18/16 
        # dom = 'Bfrtip', 
      # buttons = c('copy', 'excel', 'pdf', 'print', 'colvis'),
          
      # standard options:
      orderClasses = TRUE,
      searchHighlight = TRUE,
      pageLength = 5, 
      lengthMenu = list(c(5,10,25,50,100,-1),c(5,10,25,50,100,"All"))
    )
  )
  
  #   # create solid tumor data
  #   output$solid_table <- DT::renderDataTable({
  #     solid
  #   })
  
  output$plot_various <- renderPlot({
    req(input$table_rows_all)
    filtered_row_inds <- input$table_rows_all
      
    # choose type of plot
    if(input$plotType == "hist") {
      hist_x <- df[filtered_row_inds,input$hist_var]
      hist_xlab <- input$hist_var
      if(input$hist_log==TRUE) {
        hist_x <- log10(hist_x)
        hist_xlab <- paste0(hist_xlab, " (log10)")
      }
      # find a way to use the table output by output$table above.
      if(input$breaks != "custom") hist_breaks = input$breaks
      else hist_breaks = input$breakCount
      hist(hist_x,
           breaks= hist_breaks,
           main="Histogram of selected variable.\nUpdates based on filtering above.",
           xlab=hist_xlab, 
           col="skyblue",# rgb(66,139,202,maxColorValue = 255),
           border = "white")
    } else if (input$plotType == "scatter") {
      scat_x <- df[filtered_row_inds,input$scat_var_x]
      scat_y <- df[filtered_row_inds,input$scat_var_y]
      plot(x = scat_x, y = scat_y,
           main="Scatterplot of selected variables.\nUpdates based on filtering above.",
           xlab=input$scat_var_x,ylab=input$scat_var_y, col = "royalblue",pch=19)
      # TODO goal: switch this with an interactive plot via rCharts or something else.
        # this url might help: https://gallery.shinyapps.io/095-plot-interaction-advanced/
    } else if (input$plotType == "bar") {
      bar_table <- table(df[filtered_row_inds,input$bar_var])
      op <- par(no.readonly = TRUE)
      #change settings
      par(mar=c(5, 18, 2, 2) + 0.1)
      num_vars = length(levels(df[,input$bar_var]))
      par(cex=1+log(7/num_vars,base=100)) # simply scaling plot size to number of variables
      barplot(rev(bar_table),las=2,horiz=TRUE,col = rainbow(length(bar_table)), border = "white", xlab = input$bar_var,
              main="Barplot of counts in selected category.\nUpdates based on filtering above.")
      #reset settings
      par(op)
    } else if (input$plotType == "scatbox"){
      scatbox_df <- df[filtered_row_inds,]
      scatbox_formula <- get(input$scatbox_num) ~ get(input$scatbox_cat)
      
      #change settings
      par(mar=c(5, 18, 3, 2) + 0.1)
      num_vars = length(levels(df[,input$scatbox_cat]))
      par(cex=1.3+log(7/num_vars,base=100)) # scales plot to number of variables
      # Done: incorporate proportionality to number of variables into height of this (and barplot).
        # Failed avenues: par(din), dev.size(), 'height' parameter in different parts of UI.,
        # try: check other par() parameters
        # try: ?png() or ?x11()
        # best idea: TODO could perhaps still improve at the UI height specification.
      
      # TODO: adjust par(mar) according to length of variable names so they aren't cut off. (Also for barplot.)
        # or adjust long variable names to a maximum length. 
        # now may need to incorporate num_vars or par(cex) into this calculation
      # reverse vertical order of categories
      scatbox_df[,input$scatbox_cat] = with(scatbox_df, factor(get(input$scatbox_cat), levels = rev(levels(get(input$scatbox_cat)))))
      beeswarm(scatbox_formula, data = scatbox_df, 
               log = FALSE, pch = 21, col = rainbow(5), bg = "gray", corral = "wrap", 
               horizontal=TRUE, las=2, ylab="", xlab=input$scatbox_num, 
               main="1D-scatterplot + boxplot of counts in selected category.\nUpdates based on filtering above.",
               add=F)
      boxplot(scatbox_formula, data = scatbox_df,
              outline = FALSE, horizontal=TRUE, add=T, col="#00000000", las=2, xlab=input$scatbox_num)
      #reset settings
      par(op)
    } else if (input$plotType == "ctable_plot") {
      #TODO: add to ui.R
      tablefunc_df <- df[filtered_row_inds,]
      # plot(table(tablefunc_df[,input$tablevar1],tablefunc_df[,input$tablevar2]))  # works, but ugly.
      mosaicplot(table(tablefunc_df[,input$ctable_plot_var1],tablefunc_df[,input$ctable_plot_var2]),
                 color=rainbow(8), main = "Congingency table plot.\nUpdates based on filtering above",
                 cex.axis=0.7) # same -- could optimize with color etc?
      # TODO: fix main and axis labels, maybe margins.
      # qplot(x=table(tablefunc_df[,input$tablevar1],tablefunc_df[,input$tablevar2]),
            # stat="summary")  # doesn't work.
      # ggplot(as.data.frame(table(tablefunc_df)), aes_string(x=input$tablevar1, fill = input$tablevar2)) +
        # geom_bar(stat="identity")  # doesn't work.
    } else input$plotType <- plot(0)
    
    ### TODO: I would like to switch this to ggplot/qplot, but it doesn't take variables where 'input$hist_var' is.
      # aes_string is a hint.
      ### issue: 'count' scale is off compared to base::hist method above.
    # qplot(input$hist_var, data=df[filtered_row_inds,], geom="histogram",
          # main="histogram of selected variable: \n updates based on selections above")
    #     m <- ggplot(df[filtered_row_inds,], aes_string(x=input$hist_var))
    #     m + geom_histogram(fill="blue") #input$hist_binwidth)
  })
  
  # Solid tumor PDX table
  output$solid_table <- DT::renderDataTable({
    if (is.null(input$solid_show_vars)){
      data.frame("no variables selected" = c("no variables selected"))
    } else{
      data <- solid[,input$solid_show_vars, drop=FALSE]
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
    style="default", # alternative: "bootstrap"
    escape= FALSE,
    # From SPK: Consider escaping only 'PDF' and future columns that contain hyperlinks via this tip:
    # Note: Besides TRUE and FALSE, you can also specify which columns you want to escape, e.g.
    # datatable(m, escape = 1)  # escape the first column
    # datatable(m, escape = 2)  # escape the second column
    # datatable(m, escape = c(TRUE, FALSE))  # escape the first column
    # colnames(m) = c('V1', 'V2')
    # datatable(m, escape = 'V1')
    filter="top",
    server=FALSE, # note this means the entire dataframe is sent to user. Should be fine.
    rownames=FALSE,
    extensions = c('ColReorder'),#,'Buttons'), #'ColVis',,,'Responsive'
    options = list(
      #     # options related to extensions:
      dom = 'Rlfrtip',
      colReorder = list(realtime = TRUE), 
      #     # dom = 'C<"clear">lfrtip', # testing colVis
      #     # colVis = list(activate="mouseover"),#"click"), # testing colVis
      #       # TODO: the major questions with ColVis are whether we can...
      #         #1) capture the hidden/not hidden column information
      #         #2) format the dropdown to be pretty and more intuitive
      #         #3) easily create a 'select all/none' -- old one might work too.
      #         #4) create some kind of 'reset' (browser refresh might be good enough)
      #     # Update: ColVis status is 'retired' in favor of less-clear 'Buttons' But I think it's not in `DT`.
      #       # Buttons was just added but the development version breaks my table -- waiting... 3/18/16 
      #       # dom = 'Bfrtip', 
      # buttons = c('copy', 'excel', 'pdf', 'print', 'colvis'),
      #     
      #     # standard options:
      orderClasses = TRUE,
      searchHighlight = TRUE,
      pageLength = 5, 
      lengthMenu = list(c(5,10,25,50,100,-1),c(5,10,25,50,100,"All"))
    )
  )

  # download solid filtered data
  output$solid_download_filtered = downloadHandler("PRoXe_solid_filtered.xlsx", content = function(filename) {
    filtered_row_inds <- input$solid_table_rows_all
    dfxl <- solid[filtered_row_inds, , drop = FALSE] 
    #     WriteXLS(dfxl, ExcelFileName=filename)
    #     write.table(dfxl, filename, quote=FALSE,sep="|",na="",row.names=FALSE)
    write.xlsx(x=dfxl,file=filename,row.names=FALSE,showNA=FALSE)
  })
  
  # lower plot for solid tumor Database Explorer
  output$solid_plot_various <- renderPlot({
    filtered_row_inds <- input$solid_table_rows_all
    
    # choose type of plot
    if(input$solid_plotType == "hist") {
      hist_x <- solid[filtered_row_inds,input$solid_hist_var]
      hist_xlab <- input$solid_hist_var
      if(input$solid_hist_log==TRUE) {
        hist_x <- log10(hist_x)
        hist_xlab <- paste0(hist_xlab, " (log10)")
      }
      # find a way to use the table output by output$table above.
      if(input$solid_breaks != "custom") hist_breaks = input$solid_breaks
      else hist_breaks = input$solid_breakCount
      # note hist() results in a transient error when app is first loaded bedcause input$table_rows_all is initially empty
      # could fix if desired, but doesn't affect how the app works.
      hist(hist_x,
        breaks= hist_breaks,
        main="Histogram of selected variable.\nUpdates based on filtering above.",
        xlab=hist_xlab, 
        col="skyblue",# rgb(66,139,202,maxColorValue = 255),
        border = "white")
    } else if (input$solid_plotType == "scatter") {
      scat_x <- solid[filtered_row_inds,input$solid_scat_var_x]
      scat_y <- solid[filtered_row_inds,input$solid_scat_var_y]
      plot(x = scat_x, y = scat_y,
        main="Scatterplot of selected variables.\nUpdates based on filtering above.",
        xlab=input$solid_scat_var_x,ylab=input$solid_scat_var_y, col = "royalblue",pch=19)
      # TODO goal: switch this with an interactive plot via rCharts or something else.
      # this url might help: https://gallery.shinyapps.io/095-plot-interaction-advanced/
    } else if (input$solid_plotType == "bar") {
      bar_table <- table(solid[filtered_row_inds,input$solid_bar_var])
      op <- par(no.readonly = TRUE)
      #change settings
      par(mar=c(5, 18, 2, 2) + 0.1)
      num_vars = length(levels(solid[,input$solid_bar_var]))
      par(cex=1+log(7/num_vars,base=100)) # simply scaling plot size to number of variables
      barplot(bar_table,las=2,horiz=TRUE,col = rainbow(length(bar_table)), border = "white", xlab = input$solid_bar_var,
        main="Barplot of counts in selected category.\nUpdates based on filtering above.")
      #reset settings
      par(op)
    } else if (input$solid_plotType == "scatbox"){
      scatbox_df <- solid[filtered_row_inds,]
      # print(str(scatbox_df))
      scatbox_formula <- get(input$solid_scatbox_num) ~ get(input$solid_scatbox_cat)
      # print("here8")
      #change settings
      par(mar=c(5, 18, 3, 2) + 0.1)
      num_vars = length(levels(solid[,input$solid_scatbox_cat]))
      par(cex=1+log(7/num_vars,base=100)) # scales plot to number of variables
      # Done: incorporate proportionality to number of variables into height of this (and barplot).
      # Failed avenues: par(din), dev.size(), 'height' parameter in different parts of UI.,
      # try: check other par() parameters
      # try: ?png() or ?x11()
      # best idea: TODO could perhaps still improve at the UI height specification.
      
      # TODO: adjust par(mar) according to length of variable names so they aren't cut off. (Also for barplot.)
      # or adjust long variable names to a maximum length. 
      # now may need to incorporate num_vars or par(cex) into this calculation
      if(input$solid_scatbox_log == "log"){
        scatbox_log = TRUE
      } else {
        scatbox_log = FALSE
      }
      
      beeswarm(scatbox_formula, data = scatbox_df, 
        log = scatbox_log, pch = 21, col = rainbow(5), bg = "gray", corral = "wrap", 
        horizontal=TRUE, las=2, ylab="", xlab=input$solid_scatbox_num, 
        main="1D-scatterplot + boxplot of counts in selected category.\nUpdates based on filtering above.",
        add=F)
      boxplot(scatbox_formula, data = scatbox_df,log=scatbox_log,
        outline = FALSE, horizontal=TRUE, add=T, col="#00000000", las=2, xlab=input$solid_scatbox_num)
      #reset settings
      par(op)
      
    } else if (input$solid_plotType == "ctable_plot") {
      #TODO: add to ui.R
      tablefunc_df <- solid[filtered_row_inds,]
      # plot(table(tablefunc_df[,input$tablevar1],tablefunc_df[,input$tablevar2]))  # works, but ugly.
      mosaicplot(table(tablefunc_df[,input$solid_ctable_plot_var1],tablefunc_df[,input$solid_ctable_plot_var2]),
        color=rainbow(8), main = "Congingency table plot.\nUpdates based on filtering above",
        cex.axis=0.7) # same -- could optimize with color etc?
      # TODO: fix main and axis labels, maybe margins.
      # qplot(x=table(tablefunc_df[,input$tablevar1],tablefunc_df[,input$tablevar2]),
      # stat="summary")  # doesn't work.
      # ggplot(as.data.frame(table(tablefunc_df)), aes_string(x=input$tablevar1, fill = input$tablevar2)) +
      # geom_bar(stat="identity")  # doesn't work.
      
    } else input$solid_plotType <- plot(0)
    
    
    
    
    ### TODO: I would like to switch this to ggplot/qplot, but it doesn't take variables where 'input$hist_var' is.
    # aes_string is a hint.
    ### issue: 'count' scale is off compared to base::hist method above.
    # qplot(input$hist_var, data=df[filtered_row_inds,], geom="histogram",
    # main="histogram of selected variable: \n updates based on selections above")
    #     m <- ggplot(df[filtered_row_inds,], aes_string(x=input$hist_var))
    #     m + geom_histogram(fill="blue") #input$hist_binwidth)
  })

  # for plotting a contingency table in table format
  output$table_various <- renderTable({
    filtered_row_inds <- input$table_rows_all
    ctable_df <- df[filtered_row_inds,]
    
    # remove mCLP and Luc lines
    to_remove <- c(grep("Luc",ctable_df$`PDX Name`),
                   grep("mCLP",ctable_df$`PDX Name`))
    ctable_df <- ctable_df[-to_remove,]
    
    if (input$ctable_numcats == 1) {
      tmp_table <- table(ctable_df[,input$tablevar1])
      names(dimnames(tmp_table)) <- "category"
      tmp_table
    } else if (input$ctable_numcats == 2) {
      as.data.frame.matrix(table(ctable_df[,input$tablevar1],ctable_df[,input$tablevar2]))
    }
  },striped=T,hover=T,bordered=T,rownames=T)
  
  output$plot_rna <- renderPlot({
    
    # see this for making heatmaps
    # http://sebastianraschka.com/Articles/heatmaps_in_r.html
    
    if(input$expType == "heat"){
      # subset for only genes desired
      if(input$geneInput == "panels") {
        # use user-selected list from lists of genes to subset in RNA-seq clustering in app
        rnamat_sub2 <- rnamat_sub[rownames(rnamat_sub) %in% genesets_list[[input$rna_panel]] ,]
      } else if(input$geneInput == "indiv") {
        rnamat_sub2 <- rnamat_sub[rownames(rnamat_sub) %in% input$rna_genes,]
      }
      
      # subset for only samples desired
      df_idx <- input$table_rows_selected
      samp_names <- df$`PDX RNA-Seq Name`[df_idx]
      samp_names <- samp_names[complete.cases(samp_names)]
      rnamat_names <- as.character(dfr[dfr$`PDX RNA-Seq Name` %in% samp_names,]$PDX_RNA_abrv)
      # Note great: nothing extra happens if both samples that point to a single RNA-seq are selected.
    
      if(input$sampleInput == "all"){
        rnamat_sub3 <- rnamat_sub2
      } else if(input$sampleInput == "click"){
        rnamat_sub3 <- rnamat_sub2[,colnames(rnamat_sub2) %in% rnamat_names]
      }
      
      # print(paste("debug: input$rna_genes =",input$rna_genes))
      # weird output: `[1] "debug: input$rna_genes = AKT3" "debug: input$rna_genes = TP53" "debug: input$rna_genes = EGFR"`
        
      # normalize RNA-seq data various ways #TODO more
      rnamat_sub3log2 <- log(rnamat_sub3+0.01,2)
      
      print("didcomehere6")
      # plot
      # op <- par(no.readonly = T)
      # par(mar=c(50,5,5,6))
      heatmap.2(rnamat_sub3log2,
                # cellnote = toy2,  # same data set for cell labels
                main = "Log2( RPKM + 0.01 )", # heat map title
                notecol="black",      # change font color of cell labels to black
                density.info="none",  # turns off density plot inside color legend
                trace="none",         # turns off trace lines inside the heat map
                margins =c(19,16),     # widens margins around plot
                col=my_palette,       # use on color palette defined earlier 
                # breaks=col_breaks,    # enable color transition at specified limits
                keysize = 0.75,
                dendrogram="both")#,     # only draw a row dendrogram
      # Colv="NA")            # turn off column clustering
      # par(op)
    } else if (input$expType == "bar"){
      # subset rnamat_sub2 by gene, then transpose
      if(input$across_bar == "samples"){
        rnamat_onegene <- rnamat_sub[rownames(rnamat_sub) == input$bar_gene,]
        rnadf_onegene <- as.data.frame(rnamat_onegene)
        names(rnadf_onegene) <- "gene"
        rnadf_onegene$name <- rownames(rnadf_onegene)
        rnadf_onegene <- transform(rnadf_onegene, 
                                   name = reorder(name, gene))
        ggplot(rnadf_onegene,aes(x=name,y=gene)) + 
          geom_bar(stat='identity',fill="blue") + 
          ggtitle(paste(input$bar_gene,"expression levels (RPKM)")) + 
          xlab("sample") + coord_flip() + ylab(NULL)
      } else if (input$across_bar == "gene_set") {
        # for plotting one sample and a gene set on a barplot
        rnamat_geneset <- rnamat_sub[rownames(rnamat_sub) %in% genesets_list[[input$bar_rna_panel]] ,]
        rnamat_geneset <- rnamat_geneset[,colnames(rnamat_geneset) == input$bar_rna_sample]
        rnadf_geneset <- as.data.frame(rnamat_geneset)
        names(rnadf_geneset) <- "expr"
        rnadf_geneset$genename <- rownames(rnadf_geneset)
        rnadf_geneset$expr <- log2(rnadf_geneset$expr + 0.01)
        rnadf_geneset <- transform(rnadf_geneset, 
                                   name = reorder(genename, expr))
        ggplot(rnadf_geneset,aes(x=name,y=expr)) + 
          geom_bar(stat='identity',fill="blue") + 
          ggtitle(paste("log2( expression levels (RPKM) + 0.01) in",input$bar_rna_sample)) + 
          xlab(paste("Gene panel:",input$bar_rna_panel)) + coord_flip() +
          ylab(NULL)
      }
    }
  })
  
  output$plot_oncoprint <- renderPlot({
    
    # if desired, subset for only samples selected in Database Explorer
    if(input$oncop_sample_input == "click"){
      df_idx <- input$table_rows_selected
      samp_names <- df$`PDX RNA-Seq Name`[df_idx]
      samp_names <- samp_names[complete.cases(samp_names)]
      M2 <- M2[M2$PDX.RNA.Seq_Name %in% samp_names,]
    }
    
    # Subset for sample subtypes
    if (input$oncop_sample_type == "AML") {
      M2 <- M2[M2$type %in% c("AML","BP"),]
    } else if (input$oncop_sample_type == "BA") {
      M2 <- M2[M2$type %in% c("BA"),]
    } else if (input$oncop_sample_type == "TA") {
      M2 <- M2[M2$type %in% c("TA"),]
    }
    
    # "1" is somewhat validated mutations
    M3 <- cbind(M2[,1:3],
                as.data.frame(lapply(M2[4:ncol(M2)],grepl,pattern="1")))
    rownames(M3) <- M3$Line.Name.for.Figure
    #remove first three sorting columns from M3
    M3 <- M3[4:ncol(M3)]
    M3 <- t(M3)
    
    # subsetting genes by those relevant to cancer type
    GOIdf <- list(AML = c("CEBPA","DNMT3A","FLT3","IDH1","IDH2","KIT","KRAS","NPM1","NRAS",
                          "PTPN11","RUNX1","TET2","TP53","WT1"),
                  BA = c("ABL1","CDKN2A","CDKN2B","FLT3","IKZF1","IL7R","JAK1","JAK2","NRAS",
                         "PAX5","RB1","SH2B3","TP53"))
    # Note T-ALL not implemented yet.
    if(input$oncop_gene_input == "gene_sets"){
      if(input$oncop_gene_set != "all") {
        M3 <- M3[rownames(M3) %in% GOIdf[[input$oncop_gene_set]],]
      }
    }

    # subsetting genes individually
    if(input$oncop_gene_input == "indiv"){
      if(length(input$oncop_genes) >= 2){
        M3 <- M3[rownames(M3) %in% input$oncop_genes,]
      } else return(plot(0,main="Please select at least two genes"))
    }
    
    op <- par(no.readonly = T)
    par(mar=rep(5,4),cex.axis=0.7) # Mark, cex.axis changes the x and y axis label font sizes.
    oncoPrint(M3)
    par(op)
    
  })
  
  # download the filtered data
  output$download_filtered = downloadHandler("PRoXe_filtered.xlsx", content = function(filename) {
    filtered_row_inds <- input$table_rows_all
    dfxl <- df[filtered_row_inds, 1:(obInvisRet_ind-1), drop = FALSE] 
    #     WriteXLS(dfxl, ExcelFileName=filename)
    #     write.table(dfxl, filename, quote=FALSE,sep="|",na="",row.names=FALSE)
    write.xlsx(x=dfxl,file=filename,row.names=FALSE,showNA=FALSE)
  })
  
  # line report
  output$line_report <- DT::renderDataTable({
    # df_idx <- 4
    # print(df_idx)
    if(input$line_report_input_type == "click"){
      df_idx <- input$table_rows_selected
      if (length(df_idx) == 0) {
        return(data.frame(
          Instructions = "Select a line in the Database Explorer to see a report here.",
          row.names = ""))
      } else if (length(df_idx) > 1) {
        return(data.frame(
          Problem = "Please select only one row from Database Explorer",
          row.names = ""))
      } 
    #       else if (length(df_idx) == 1) {
    #         continue
    #       } 
    } else if (input$line_report_input_type == "dropdown"){
      df_idx <- which(df[,"PDX Name"] == input$line_report_name)
    }
    temp_df <- as.data.frame(t(df[df_idx,1:(obInvisRet_ind-1)]))
    colnames(temp_df) <- "sample"
    temp_df$PRoXe_Column_Header <- rownames(temp_df)
    rownames(temp_df) <- NULL
    temp_df <- temp_df[,2:1]
    
    # 1. reorder by column groupings
    m3 <- meta_gloss
    m3$PRoXe_Column_Header <- gsub("_"," ",m3$PRoXe_Column_Header)
    td2 <- merge(temp_df,m3,by="PRoXe_Column_Header",all.x=T,sort=F)
    td2$Column_Groupings <- factor(td2$Column_Groupings,levels =c("patient","tumor","pdx","administrative"))
    td3 <- td2[with(td2, order(Column_Groupings,Row_Order)),]
    #TODO below: remove columns that are NA as a temporary stopgap. Later get Mark to edit Glossary doc.
    if(anyNA(td3$Line_Report)){
      warning("These rows are not yet annotated for Line Report; removing:")
      warning(td3[is.na(td3$Line_Report),]$PRoXe_Column_Header)
      td3 <- td3[!is.na(td3$Line_Report),]
    }
    
    # 2. Remove/edit rows as marked in 'Line Report' and 'Line Report Notes' columns.
      # 0=do not include
      # 1=include
      # 2=do not include this column per se, but similar data from an external spreadsheet will be shown
        # TODO: determine what to do regarding this (see Mark), then include below.
      # 3=include in report conditionally on instructions in Line Report Notes column.
      
      # remove 0s
    td4 <- td3[td3$Line_Report!=0,]
    
      # TODO: discuss further with Mark re: 2s
        # the only columns that is 2 currently is 'Limited Distribution' (now Available for Distribution)
        # Mark says can read this in from a spreadsheet he's shared wiht me
        # but has not yet fully determined which spreadsheet that is
        # Maybe the BODFI tracking sheet or something like it.

      # conditionally reformat 3s
      # include FAB Classification only when WHO Cat == AML
    who_cat = td4[td4$PRoXe_Column_Header=="WHO Category","sample"]
    if(!is.na(who_cat)){
      if(who_cat != "AML"){
        td4 <- td4[!(td4$PRoXe_Column_Header=="FAB Classification"),]
        td4 <- td4[!(td4$PRoXe_Column_Header=="Cytogenetic Risk Category"),]        
      }
    }

      # combine into single row in order CD45,33,34,19,2,3, 
        # in format CD45(superscript + or -)CD33(etc. no spaces.
        # If PDX_Other_Immunophenotypes, add ", and" and append to end.
        # then remove combined rows.
    to_combine <- c("PDX CD45","PDX CD33","PDX CD34","PDX CD19",
      "PDX CD2","PDX CD3","PDX Other Immunophenotype") # note in correct order
    CDs <- to_combine[-length(to_combine)]
    temp <- as.character(td4[td4$PRoXe_Column_Header %in% to_combine,]$sample)
    temp <- gsub("Positive","<sup>+</sup> ",temp)
    temp <- gsub("Negative","<sup>-</sup> ",temp)
    temp <- gsub("Dim","<sup>Dim</sup> ",temp)
    temp <- gsub("Partial","<sup>Partial</sup> ",temp)
    names(temp) <- gsub("PDX ","",to_combine)
    temp <- temp[!is.na(temp)]
    if(!is.na(temp["Other Immunophenotype"])){
      oi <- temp["Other Immunophenotype"]
      temp <- temp[names(temp) != "Other Immunophenotype"]
    }
    temp2 <- paste0(names(temp),temp,collapse="")
    if(exists("oi")) temp2 <- paste0(temp2,", and ", oi)
    # remove unwanted rows and add new row
    td5 <- td4[!(td4$PRoXe_Column_Header %in% CDs),]
    td5[td5$PRoXe_Column_Header == "PDX Other Immunophenotype",1] <- "PDX Immunophenotype"
    td5$sample <- as.character(td5$sample)
    td5[td5$PRoXe_Column_Header == "PDX Immunophenotype","sample"] <- temp2

    # combine into single row: HLA Alleles TODO TODO currnelty not working
      # TODO: determine format with Mark
      # then remove combined rows.
    to_combine <- c("HLA-A Alleles", "HLA-B Alleles", "HLA-C Alleles")
    temp <- as.character(td5[td5$PRoXe_Column_Header %in% to_combine,]$sample)
    temp <- temp[!is.na(temp)]
    temp2 <- paste0(names(temp),temp,collapse=" ")
    # remove unwanted rows and add new row
    td5[td5$PRoXe_Column_Header == "HLA-A Alleles",1] <- "HLA Alleles"
    td6 <- td5[!(td5$PRoXe_Column_Header %in% to_combine),]
    td6$sample <- as.character(td6$sample)
    td6[td6$PRoXe_Column_Header == "HLA Alleles","sample"] <- temp2

    td8 <- td6[,c("PRoXe_Column_Header","sample","Column_Groupings")] 
    colnames(td8) <- c("Data Type",
      as.character(td8[td8$PRoXe_Column_Header=="PDX Name","sample"]),
      "Column_Groupings")
    td8 <- td8[,1:2]
    return(td8)
    },
    escape= FALSE,
    filter="top",
    server=FALSE, # note this means the entire dataframe is sent to user. Should be fine.
    rownames=FALSE,
    options = list(
      searchHighlight = TRUE,
      paging = FALSE
    )
  )
  
  output$line_report_FC <- renderUI({
    if(input$line_report_input_type == "click"){
      df_idx <- input$table_rows_selected
    } else if(input$line_report_input_type == "dropdown"){
      df_idx <- which(df[,"PDX Name"] == input$line_report_name)
    }
    fc_path <- df[df_idx,"Flow Cytometry PDF"]
    if(length(df_idx)<1){
      tags$p(" ")
    } else if(is.na(fc_path)){
      tags$p("There is no Flow Cytometry data for the selected line.")
    } else if (length(df_idx) > 1) {
      tags$p(" ")
    } else {
      outfile=str_extract(fc_path,"Flow_Cytometry/.*.pdf")
      tags$iframe(
        src=outfile,
        width="100%",
        height="800px")
    }
  })
  
  output$line_report_IHC <- renderUI({
    if(input$line_report_input_type == "click"){
      df_idx <- input$table_rows_selected
    } else if(input$line_report_input_type == "dropdown"){
      df_idx <- which(df[,"PDX Name"] == input$line_report_name)
    }
    fc_path <- df[df_idx,"IHC PDF"]
    if(length(df_idx)<1){
      tags$p(" ")
    } else if(is.na(fc_path)){
      tags$p("There is no IHC data for the selected line.")
    } else if (length(df_idx) > 1) {
      tags$p(" ")
    } else {
      outfile=str_extract(fc_path,"IHC/.*.pdf")
      tags$iframe(
        src=outfile,
        width="100%",
        height="800px")
    }
  })
  
  output$line_report_Path <- renderUI({
    if(input$line_report_input_type == "click"){
      df_idx <- input$table_rows_selected
    } else if(input$line_report_input_type == "dropdown"){
      df_idx <- which(df[,"PDX Name"] == input$line_report_name)
    }
    fc_path <- df[df_idx,"Path Report PDF"]
    if(length(df_idx)<1){
      tags$p(" ")
    } else if(is.na(fc_path)){
      tags$p("There is no Path Report for the selected line.")
    } else if (length(df_idx) > 1) {
      tags$p(" ")
    } else {
      outfile=str_extract(fc_path,"Pathology_Reports/.*.pdf")
      tags$iframe(
        src=outfile,
        width="100%",
        height="800px")
    }
  })
  
    # download the line report TODO: change filename to reflect line.
      # TODO: consider pre-processing all these if slow.
  #   output$download_line_report = downloadHandler("PRoXe_line_report.pdf", content = function(filename) {
  #     df_idx <- input$table_rows_selected
  #     library(knitr)
  #     library(rmarkdown)
  #     if (length(df_idx) == 1) {
  #       temp_df <- t(df[df_idx,1:(obInvisRet_ind-1)])
  #       colnames(temp_df) <- "sample"
  #       knitr::knit2pdf()
  #       # rmarkdown::
  #     }
  #   })
  
  # add glossary
  output$glossary <- DT::renderDataTable({
    #     # take visible columns' header and description
    #     meta3 <- meta2[meta2$Visible_Invisible != "ob_invis",c("PRoXe_Column_Header","Column_Description")]
    #     # reformat
    #     meta3 <- as.data.frame(lapply(meta3,gsub,pattern="_",replacement=" "),stringsAsFactors = F)
    #     names(meta3) <- gsub("_"," ",names(meta3))
    #     
    #     missing_names <- setdiff(names(df[1:(obInvisRet_ind-1)]), meta3$`PRoXe Column Header`)
    #     new_rows_df <- meta_gloss[meta_gloss$PRoXe_Column_Header %in% missing_names,
    #                               c("PRoXe_Column_Header","Column_Description")]
    #     names(new_rows_df) <- gsub("_"," ",names(new_rows_df))
    #     meta3 <- rbind(meta3,new_rows_df)
    #     # change order to same as Database Explorer
    #     meta3[match(meta3$`PRoXe Column Header`,names(df[1:(obInvisRet_ind-1)])),]
    meta3[,c("PRoXe Column Header","Column Description")]
    },
    filter="top",
    server=FALSE, # note this means the entire dataframe is sent to user. Should be fine.
    rownames=FALSE,
    options = list(
      
      searchHighlight = TRUE,
      paging=FALSE
    )
  )
  
  output$PDX_methods <- renderUI({
    # filename <- dir(path = "www/methods",pattern="_PDX_Methods_for_proxe.pdf",full.names=T)
    # TODO: later functionalize this
    filename <- dir(path = "www/methods",pattern="_PDX_Methods_for_proxe.pdf",full.names=T)
    if(length(filename) < 1) warning("Where is the methods pdf?")
    if(length(filename) > 1) {
      warning("> 1 methods PDFs in www/methods folder. Taking last saved.")
      tmp <- sapply(filename,function(i) file.info(i)$mtime)
      newest_file <- sort(tmp,decreasing=T)[1]
      filename <- names(newest_file)
    }
    filename <- gsub("www/","",filename)
    tags$iframe(
      src=filename,
      width="100%",
      height="800px")
  })
  
  output$Renal_methods <- renderUI({
    filename <- "methods/Renal_capsule_methods.pdf"
    tags$iframe(
      src=filename,
      width="100%",
      height="800px")
  })
  
  output$pricing <- DT::renderDataTable({
    pricing <- data.frame(
      "Service Name" = c("Per vial","Handling rate (per shipment)","Consulting (hourly)","Shipping (domestic)","Shipping (international)"),
      "DFCI Rate" = c("$385","$94","$125","varies","n/a"),
      "Academic Rate" = c("$519","$126","$169","by shipment zone","by shipment address"),
      "Corporate Rate" = "<a href=\"mailto:proxe.feedback@gmail.com?Subject=PRoXe%20corporate%20rates\" target=\"_top\">contact us</a>",
    row.names=NULL)
    # change colnames to remove automatic periods instead of spaces.
    colnames(pricing) <- c("Service Name","DFCI Rate","Academic Rate","Corporate Rate")
    pricing
    },
    escape= FALSE,
    rownames = FALSE,
    selection = "none",
    options = list(
      dom = 't',
      ordering = FALSE
    )
  )
  
  output$iLab_manual <- renderUI({
    filename <- "iLab_Customer_Manual_for_LLX.pdf"
    tags$iframe(
      src=filename,
      width="80%",
      height="800px")
  })
  
  ### testing dropdownMenu ###
  # Sorting asc

  observeEvent(input[["a2z_administrative"]], {
    updateCheckboxGroupInput(
      session = session, inputId = "check2_administrative",
      choices = sort(meta3[(meta3$`Column Groupings` == "administrative"),]$`PRoXe Column Header`),
      selected = input$check2_administrative
    )
  })
  
  observeEvent(input[["a2z_tumor"]], {
    updateCheckboxGroupInput(
      session = session, inputId = "check2_tumor",
      choices = sort(meta3[(meta3$`Column Groupings` == "tumor"),]$`PRoXe Column Header`),
      selected = input$check2_tumor
    )
  })
  
  observeEvent(input[["a2z_patient"]], {
    updateCheckboxGroupInput(
      session = session, inputId = "check2_patient",
      choices = sort(meta3[(meta3$`Column Groupings` == "patient"),]$`PRoXe Column Header`),
      selected = input$check2_patient
    )
  })
  
  observeEvent(input[["a2z_pdx"]], {
    updateCheckboxGroupInput(
      session = session, inputId = "check2_pdx",
      choices = sort(meta3[(meta3$`Column Groupings` == "pdx"),]$`PRoXe Column Header`),
      selected = input$check2_pdx
    )
  })
  
#   observeEvent(input[[paste0("a2z_",lab)]], {
#     updateCheckboxGroupInput(
#       session = session, inputId = paste0("check2_",lab),
#       choices = sort(meta3[(meta3$`Column Groupings` == lab),]$`PRoXe Column Header`),
#       selected = input[["check2_",lab]]
#     )
#   })
  
  # leaving in for testing:
#   output$res2 <- renderPrint({
#     c(input$check2_administrative,input$check2_tumor,input$check2_patient,input$check2_pdx)
#   })
  
  
  # TODO: functionalize call of observeEvent()s below via for loop or other mechanism
  if(F){ # Note this does not work. Determine why. # Test: does it work ok to access input as list like this?
    for(colgrp in levels(as.factor(meta3$`Column Groupings`))){
      observeEvent(input[[paste0("all_",colgrp)]], {
        if (all(meta3[meta3$`Column Groupings`==colgrp,]$`PRoXe Column Header` %in% input[[paste0("check2_",colgrp)]])) {
          updateCheckboxGroupInput(
            session = session, inputId = input[[paste0("check2_",colgrp)]], selected = ""
          )
        } else {
          updateCheckboxGroupInput(
            session = session, inputId = input[[paste0("check2_",colgrp)]], selected = names(df[1:(obInvisRet_ind-1)])
          )
        }
      })
    }
  } # end of if(F)
  
  # if(F){ # purpose -- commenting out this section below.
  # Select all / Unselect all
  observeEvent(input$all_administrative, {
    if (all(meta3[meta3$`Column Groupings`=="administrative",]$`PRoXe Column Header` %in% input$check2_administrative)) {
      updateCheckboxGroupInput(
        session = session, inputId = "check2_administrative", selected = ""
      )
    } else {
      updateCheckboxGroupInput(
        session = session, inputId = "check2_administrative", selected = names(df[1:(obInvisRet_ind-1)])
      )
    }
  })
  
  observeEvent(input$all_tumor, {
    if (all(meta3[meta3$`Column Groupings`=="tumor",]$`PRoXe Column Header` %in% input$check2_tumor)) {
      updateCheckboxGroupInput(
        session = session, inputId = "check2_tumor", selected = ""
      )
    } else {
      updateCheckboxGroupInput(
        session = session, inputId = "check2_tumor", selected = names(df[1:(obInvisRet_ind-1)])
      )
    }
  })
  
  observeEvent(input$all_patient, {
    if (all(meta3[meta3$`Column Groupings`=="patient",]$`PRoXe Column Header` %in% input$check2_patient)) {
      updateCheckboxGroupInput(
        session = session, inputId = "check2_patient", selected = ""
      )
    } else {
      updateCheckboxGroupInput(
        session = session, inputId = "check2_patient", selected = names(df[1:(obInvisRet_ind-1)])
      )
    }
  })
  
  observeEvent(input$all_pdx, {
    if (all(meta3[meta3$`Column Groupings`=="pdx",]$`PRoXe Column Header` %in% input$check2_pdx)) {
      updateCheckboxGroupInput(
        session = session, inputId = "check2_pdx", selected = ""
      )
    } else {
      updateCheckboxGroupInput(
        session = session, inputId = "check2_pdx", selected = names(df[1:(obInvisRet_ind-1)])
      )
    }
  })
  # } # end of if(F)
})

