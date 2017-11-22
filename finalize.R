# final commands to run before saving image

## -- produce final meta-glossary df for app: meta3 -- ##
# take visible columns' header and description
if(T) {
  meta3 <- meta2[meta2$Visible_Invisible != "ob_invis",]
  # drop column
  meta3$Visible_Invisible_int <- NULL
  # reformat
  if(F){
    # not sure why I had this -- reformats too many things!
    meta3 <- as.data.frame(lapply(meta3,gsub,pattern="_",replacement=" "),stringsAsFactors = F)
  }
  meta3$PRoXe_Column_Header <- gsub("_"," ",meta3$PRoXe_Column_Header)
  names(meta3) <- gsub("_"," ",names(meta3))
  
  missing_names <- setdiff(names(df[1:(obInvisRet_ind-1)]), meta3$`PRoXe Column Header`)
  new_rows_df <- meta_gloss[meta_gloss$PRoXe_Column_Header %in% missing_names,]
  new_rows_df$`In_PRIMAGRAFTS` <- NULL
  names(new_rows_df) <- gsub("_"," ",names(new_rows_df))
  meta3 <- rbind(meta3,new_rows_df)
  # change order to same as Database Explorer
  stopifnot(all(meta3$`PRoXe Column Header` %in% names(df[1:(obInvisRet_ind-1)])))
  meta3 <- meta3[match(meta3$`PRoXe Column Header`,names(df[1:(obInvisRet_ind-1)])),]
  
  # change order of both df and meta3 so ob_vis rows are at top
  ob_vis_inds <- which(meta3$`Visible Invisible` == "ob_vis")
  ob_vis_ind_diff <- diff(ob_vis_inds)
  if(any(ob_vis_ind_diff != 1)) {
    out_of_order_inds <- ob_vis_inds[which(ob_vis_ind_diff != 1)+1]

    ooo_names <- meta3$`PRoXe Column Header`[out_of_order_inds]
    #1. move ooos to top
    meta3 <- rbind(meta3[1:(condVis_ind-1),],meta3[out_of_order_inds,],meta3[-out_of_order_inds,][condVis_ind:nrow(meta3[-out_of_order_inds,]),])
    #2. change df column order to be same as meta
    df <- moveMe(
      data=df,
      tomove=ooo_names,
      where="before",
      names(df)[condVis_ind]
    )
    #3. increase condVis_ind. 
    condVis_ind = condVis_ind + length(ooo_names)
    # note obInvisRet_ind shouldn't need to change because these new columns will have been visible before.
  }
}

