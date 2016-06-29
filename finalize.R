# final commands to run before saving image

## -- produce final meta-glossary df for app: meta3 -- ##
# take visible columns' header and description
if(T) {
  meta3 <- meta2[meta2$Visible_Invisible != "ob_invis",]
  # drop column
  meta3$Visible_Invisible_int <- NULL
  # reformat
  meta3 <- as.data.frame(lapply(meta3,gsub,pattern="_",replacement=" "),stringsAsFactors = F)
  names(meta3) <- gsub("_"," ",names(meta3))
  
  missing_names <- setdiff(names(df[1:(obInvisRet_ind-1)]), meta3$`PRoXe Column Header`)
  new_rows_df <- meta_gloss[meta_gloss$PRoXe_Column_Header %in% missing_names,]
  names(new_rows_df) <- gsub("_"," ",names(new_rows_df))
  meta3 <- rbind(meta3,new_rows_df)
  # change order to same as Database Explorer
  meta3 <- meta3[match(meta3$`PRoXe Column Header`,names(df[1:(obInvisRet_ind-1)])),]
}