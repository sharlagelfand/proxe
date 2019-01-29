server_liquid_tumors_glossary <- function(input, output, server) {
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
    meta3[, c("PRoXe Column Header", "Column Description")]
  },
  filter = "top",
  server = FALSE, # note this means the entire dataframe is sent to user. Should be fine.
  rownames = FALSE,
  options = list(
    searchHighlight = TRUE,
    paging = FALSE
  )
  )
}
