# server.R

library(shiny)
# library(WriteXLS)
library(xlsx)
# library(ggplot2)
library(DT)
library(beeswarm)
# library(rCharts)

# setwd, load, and clean data
# source("clean_data.R")
# source("rna_seq.R")
loadedServer <- load("pre-compiled.RData")

print("didcomehere4")
# Define a server for the Shiny app
shinyServer(function(input, output, session) {  #TODO: read on what 'session' means here.  
  # select/deselect all using action button
  observe({
    if (input$selectall > 0) {
      if (input$selectall %% 2 == 1){
        updateCheckboxGroupInput(session=session, 
                                 inputId="show_vars",
                                 choices = names(df[1:obInvisRet_ind-1]),
                                 selected = c(names(df)))
      } else {
        updateCheckboxGroupInput(session=session, 
                                 inputId="show_vars",
                                 choices = names(df[1:obInvisRet_ind-1]),
                                 selected = c())
      }}
    updateSelectizeInput(session,inputId="rna_genes",
                   choices=sort(rownames(rnamat_sub)), server=TRUE,
                   selected=c("BCL2","TP53","FLT3","MYC","JAK2"))
    updateSelectizeInput(session,inputId="bar_gene",
                         choices=sort(rownames(rnamat_sub)), server=TRUE,
                         selected=c("BCL2"))
    updateSelectizeInput(session,inputId="oncop_genes",
                          choices=sort(M2_genes), server=TRUE,
                          selected=c("TP53","KRAS","NRAS"))
  })
  
  # Filter data based on selections
  output$table <- DT::renderDataTable({
    if (is.null(input$show_vars)){
      data.frame("no variables selected" = c("no variables selected"))
    } else{
      data <- df[,input$show_vars, drop=FALSE]
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
  filter="top",
  server=FALSE, # note this means the entire dataframe is sent to user. Should be fine.
  rownames=FALSE,
#   extensions = 'ColVis', # testing colVis
  options = list(
#     dom = 'C<"clear">lfrtip', # testing colVis
#     colVis = list(activate="click"),#mouseover"), # testing colVis
    searchHighlight = TRUE,
    pageLength = 5, 
    lengthMenu = list(c(5,10,25,50,100,-1),c(5,10,25,50,100,"All"))
    )
  )
  
  output$plot_various <- renderPlot({
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
      # note hist() results in a transient error when app is first loaded bedcause input$table_rows_all is initially empty
        # could fix if desired, but doesn't affect how the app works.
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
      barplot(bar_table,las=2,horiz=TRUE,col = "forestgreen", border = "white", xlab = input$bar_var,
              main="Barplot of counts in selected category.\nUpdates based on filtering above.")
      #reset settings
      par(op)
    } else if (input$plotType == "scatbox"){
      scatbox_df <- df[filtered_row_inds,]
      scatbox_formula <- get(input$scatbox_num) ~ get(input$scatbox_cat)
      op <- par(no.readonly = TRUE)
      #change settings
      par(mar=c(5, 18, 3, 2) + 0.1)
      num_vars = length(levels(df[,input$scatbox_cat]))
      par(cex=1+log(7/num_vars,base=100)) # scales plot to number of variables
      # Done: incorporate proportionality to number of variables into height of this (and barplot).
        # Failed avenues: par(din), dev.size(), 'height' parameter in different parts of UI.,
        # try: check other par() parameters
        # try: ?png() or ?x11()
        # best idea: TODO could perhaps still improve at the UI height specification.
      
      # TODO: adjust par(mar) according to length of variable names so they aren't cut off. (Also for barplot.)
        # or adjust long variable names to a maximum length. 
        # now may need to incorporate num_vars or par(cex) into this calculation
      beeswarm(scatbox_formula, data = scatbox_df, 
               log = FALSE, pch = 21, col = rainbow(5), bg = "gray", corral = "wrap", 
               horizontal=TRUE, las=2, ylab="", xlab=input$scatbox_num, 
               main="1D-scatterplot + boxplot of counts in selected category.\nUpdates based on filtering above.",
               add=F)
      boxplot(scatbox_formula, data = scatbox_df,
              outline = FALSE, horizontal=TRUE, add=T, col="#00000000", las=2, xlab=input$scatbox_num)
      #reset settings
      par(op)
      
    }
    
    
    
    
    ### TODO: I would like to switch this to ggplot/qplot, but it doesn't take variables where 'input$hist_var' is.
      # aes_string is a hint.
      ### issue: 'count' scale is off compared to base::hist method above.
    # qplot(input$hist_var, data=df[filtered_row_inds,], geom="histogram",
          # main="histogram of selected variable: \n updates based on selections above")
#     m <- ggplot(df[filtered_row_inds,], aes_string(x=input$hist_var))
#     m + geom_histogram(fill="blue") #input$hist_binwidth)
  })
  
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
      samp_names <- df$`PDX-RNA-Seq Name`[df_idx]
      samp_names <- samp_names[complete.cases(samp_names)]
      rnamat_names <- as.character(dfr[dfr$`PDX-RNA-Seq Name` %in% samp_names,]$PDX_RNA_abrv)
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
      samp_names <- df$`PDX-RNA-Seq Name`[df_idx]
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
    dfxl <- df[filtered_row_inds, 1:obInvisRet_ind-1, drop = FALSE] 
#     WriteXLS(dfxl, ExcelFileName=filename)
#     write.table(dfxl, filename, quote=FALSE,sep="|",na="",row.names=FALSE)
    write.xlsx(dfxl,filename,row.names=FALSE,showNA=FALSE)
  })
})

### --- Appendix of old code --- ###

# if (!is.null(input$pathDx) && input$pathDx != ""){
#   data <- data[data$Pathologic_Diagnosis == input$pathDx,]
# }
# #     if (input$m1 != "All"){
# #       data <- data[data$Molecular1 == input$m1,]
# #     }
# if (!is.null(input$karyotype) && input$karyotype != ""){
#   data <- data[data$Source_Karyotype_Simplified == input$karyotype,]
# }