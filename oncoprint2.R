
library(readxl)


# read in spreadsheet
variants.filename <- dir(data_outside_app_dir,pattern = glob2rx("20*_pdx_dna_variants.xlsx"))
if(length(variants.filename) != 1) stop("too few or too many _pdx_dna_variants.xlsx files in dropbox")
# var_native <- read_excel(file.path(data_outside_app_dir,variants.filename),sheet=1)
variants <- read_excel(file.path(data_outside_app_dir,variants.filename),sheet=1,col_types = "text")
variants <- as.data.frame(variants)
vs = variants


# notes:
# tumor_sample_name_new is PDX_Name -- it is truncated and becomes pdx_id in database
# for True Mutation: 0 = benign, 1 = pathologic, 2 = vus

# produce desired form:
cols_keep = c("tumor_sample_name_new",
  "Canonical_Variant_Classification", # table 1
  "Canonical_Hugo_Symbol", # table 1
  "BestEffect_Variant_Classification", #table 2
  "BestEffect_Hugo_Symbol", # table 2
  "True_Mutation", # want to show just 1s or 1s+2s
  "allele_fraction",
  "Coverage")

v = vs[cols_keep]
# v = v[v$True_Mutation != 0 & !is.na(v$True_Mutation),]
v = v[v$True_Mutation == 1 & !is.na(v$True_Mutation),]

v1 = v[c(1,4:8)]

#### --- recode *_Variant_Classification -- ####

# set up constants
snv=c("Missense_Mutation","Nonsense_Mutation","Nonstop_Mutation","Translation_Start_Site","Missense")
indel=c("Frame_Shift_Del","Frame_Shift_Ins","In_Frame_Del","In_Frame_Ins")
splice=c("Splice_Region","Splice_Site")
mut_types_keep = c(snv,indel,splice)

# - for v1 - #
# filter out terms that don't exist, like "0", "intron","intergenic_variant".
v1 <- v1[v1$BestEffect_Variant_Classification %in% mut_types_keep,]
# Create new columns that map these classifications to the above terms: snv, indel, splice
v1$class2 <- NA_character_
for(i in 1:nrow(v1)){
  class1 <- v1$BestEffect_Variant_Classification[i]
  if (class1 %in% snv){
    v1$class2[i] <- "snv"
  } else if (class1 %in% indel){
    v1$class2[i] <- "indel"
  } else if (class1 %in% splice){
    v1$class2[i] <- "splice"
  } else {
    stop("Encountered unexpected value.")
  }
}
v1$class2 <- as.factor(v1$class2)
table(v1$BestEffect_Variant_Classification,v1$class2)

# # - for v2 - #
# # filter out terms that don't exist, like "0", "intron","intergenic_variant".
# v2 <- v2[v2$BestEffect_Variant_Classification %in% mut_types_keep,]
# # Create new columns that map these classifications to the above terms: snv, indel, splice
# v2$class2 <- NA_character_
# for(i in 1:nrow(v2)){
#   class1 <- v2$BestEffect_Variant_Classification[i]
#   if (class1 %in% snv){
#     v2$class2[i] <- "snv"
#   } else if (class1 %in% indel){
#     v2$class2[i] <- "indel"
#   } else if (class1 %in% splice){
#     v2$class2[i] <- "splice"
#   } else {
#     stop("Encountered unexpected value.")
#   }
# }
# v2$class2 <- as.factor(v2$class2)
# table(v2$BestEffect_Variant_Classification,v2$class2)



##### --- pivot into genes x samples --- ####

# - v1 - #

library(dplyr)
v1a <- v1 %>%
  dplyr::group_by(tumor_sample_name_new,BestEffect_Hugo_Symbol) %>%
  dplyr::summarise(id = paste(class2, collapse = ";"))

# note this looks like: (TODO: is snv;snv;snv a problem?)
# A tibble: 858 x 3
# Groups:   tumor_sample_name_new [?]
# tumor_sample_name_new BestEffect_Hugo_Symbol          id
# <chr>                 <chr>       <chr>
#   1         CBAB-10855-V2                 EPHA6         snv
# 2         CBAB-10855-V2                 GATA2         snv
# 3         CBAB-10855-V2                  MLL3 snv;snv;snv
# 4         CBAB-10855-V2                  PAX5         snv
v1a <- as.data.frame(lapply(v1a,as.factor))
v1a$id <- as.character(v1a$id)

library(reshape2)
v1mat <- acast(v1a,BestEffect_Hugo_Symbol~tumor_sample_name_new, value.var="id")

# produce ComplexHeatmap

if(F){
  col = c(snv = "red", indel = "blue", splice = "yellow")
  png(filename = "tmp/test_oncoprint.png",width = 25, height= 20,units = "in",res=250)
  library(ComplexHeatmap)
  ComplexHeatmap::oncoPrint(v1mat, get_type = function(x) strsplit(x, ";")[[1]],
    alter_fun = list(
      snv = function(x, y, w, h) grid.rect(x, y, w*0.9, h*0.9, gp = gpar(fill = col["snv"], col = NA)),
      indel = function(x, y, w, h) grid.rect(x, y, w*0.9, h*0.4, gp = gpar(fill = col["indel"], col = NA)),
      splice = function(x, y, w, h) grid.rect(x, y, w*0.5, h*0.5, gp = gpar(fill = col["splice"], col = NA))
    ), col = col,
    show_column_names = TRUE)
  dev.off()
}




# - v2 - #

##TODO


# - add to dataframe - #

# Template: WT1 Missense NM_024426 c.679A>G p.S227G of 0.481013 in 158 reads
# <gene> <type> <transcriptid> <nt change> <AA change> of <A.F.> in <read support> reads.
# only keep True Mutation == 1

# remove NAs
vdf <- vs[!is.na(vs$True_Mutation),]
# filter for true mutations, desired columns
annot_cols = c("tumor_sample_name_new","BestEffect_Hugo_Symbol","BestEffect_Variant_Classification",
  "BestEffect_Refseq_mRNA_Id","BestEffect_cDNA_Change","BestEffect_Protein_Change","allele_fraction","Coverage")
vdf <- vdf[vdf$True_Mutation==1,annot_cols]
# filter out intergenic
vdf <- vdf[vdf$BestEffect_Variant_Classification!="intergenic_variant",]

# clean up data
vdf$BestEffect_Variant_Classification <- gsub("_"," ",vdf$BestEffect_Variant_Classification)
vdf$BestEffect_Variant_Classification <- gsub(" Mutation","",vdf$BestEffect_Variant_Classification)
vdf$BestEffect_Variant_Classification <- gsub("Del","Deletion",vdf$BestEffect_Variant_Classification)
vdf$BestEffect_Variant_Classification <- gsub("Ins","Insertion",vdf$BestEffect_Variant_Classification)
vdf$BestEffect_Variant_Classification <- gsub("Frame Shift","Frameshift",vdf$BestEffect_Variant_Classification)
vdf$BestEffect_Variant_Classification <- gsub("In Frame","In-Frame",vdf$BestEffect_Variant_Classification)

# filter out nonsensical allele_fractions
vdf$allele_fraction <- as.numeric(vdf$allele_fraction)
vdf <- vdf[between(vdf$allele_fraction,0,1),]

# filter out rows that are completely NA
vdf <- vdf[apply(vdf,1,function(row)sum(is.na(row)))!=ncol(vdf),]

attach(vdf)
library(scales)
tmp <- paste(BestEffect_Hugo_Symbol,BestEffect_Variant_Classification,BestEffect_Refseq_mRNA_Id,BestEffect_cDNA_Change,BestEffect_Protein_Change,"in",scales::percent(allele_fraction),"of",Coverage,"reads")
detach(vdf)
vdf2 <- vdf
vdf2$PDX_Molecular_Details <- tmp

# replace " NA " with " __ "
vdf2$PDX_Molecular_Details <- gsub(" NA "," __ ",vdf2$PDX_Molecular_Details)

vdf2$pdx_id <- substring(vdf2$tumor_sample_name_new,1,10)
vdf2 <- vdf2[vdf2$pdx_id %in% df$namenum,]


# group vdf2 by pdx_id and concatenate the following, pipe-separating:
  # 1. the BestEffect Hugo Symbol as PDX Molecular Alterations Positive, and
  # 2. the PDX Molecular Details as PDX_Molecular Details
vdf_summ <- vdf2 %>% 
  group_by(pdx_id) %>% 
  summarise("PDX Molecular Alterations Positive" = paste0(BestEffect_Hugo_Symbol, collapse = " | "),
            "PDX Molecular Details" = paste0(PDX_Molecular_Details,collapse = " | "))

# note: this code supersedes oncoprint.R, so I'm removing the need for that now in compile_upload.R
  # TODO later: git rm oncoprint.R, etc.

# complete the merge, moving columns and visibility indexes accordingly

  # merge
df <- merge(df,vdf_summ,by.x = "namenum",by.y = "pdx_id",all.x=T)
obInvisRet_ind = obInvisRet_ind + 1  # accounts for new 'namenum' at column 1 (was near end)

  # move 'name' columns from #1,2 out of visible range
cols_to_move = c( "PDX RNA-Seq Name","namenum")
df <- moveMe(df, cols_to_move, "after", names(df)[obInvisRet_ind])
obInvisRet_ind <- obInvisRet_ind - length(cols_to_move)

  # move new mutation columns into visible range in intuitive context
cols_to_move = c("PDX Molecular Alterations Positive","PDX Molecular Details")
df <- moveMe(df, cols_to_move ,"after", "PDX HemoSeq")
obInvisRet_ind <- obInvisRet_ind + length(cols_to_move)



# Appendix: template example used to build this document

if(F){
  mat = read.table(textConnection(
    ",s1,s2,s3
    g1,snv;indel,snv,indel
    g2,,snv;indel,snv
    g3,snv,,indel;snv"), row.names = 1, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  mat = as.matrix(mat)
  mat
  
  library(ComplexHeatmap)
  col = c(snv = "red", indel = "blue")
  ComplexHeatmap::oncoPrint(mat, get_type = function(x) strsplit(x, ";")[[1]],
    alter_fun = list(
      snv = function(x, y, w, h) grid.rect(x, y, w*0.9, h*0.9, gp = gpar(fill = col["snv"], col = NA)),
      indel = function(x, y, w, h) grid.rect(x, y, w*0.9, h*0.4, gp = gpar(fill = col["indel"], col = NA))
    ), col = col,
    show_column_names = TRUE)
  
}


# Ad hoc filter to say 'none' for known negative PDX Molecular Alterations

# pdx_hemo_none <- readxl::read_excel(file.path(data_outside_app_dir,"20180924_pdx_hemoseq_negative_mutations_or_vus.xlsx"))
# pdx_hemo_none <- pdx_hemo_none$`PDX Name`
# if( length(setdiff(pdx_hemo_none,df$`PDX Name`)) != 0 ){
#   stop("not all pdx hemoseq negative samples are in df. Probably a PRMS-mismatch.")
# }
# # 
# for (pdx in pdx_hemo_none) {
#   if(!is.na(df[df$`PDX Name`==pdx,"PDX Molecular Alterations Positive"]) | 
#     is.na(df[df$`PDX Name`==pdx,"PDX Molecular Details"])) {
#     print(paste(pdx,"has some Molecular Alteration info and shouldn't"))
#   }
# }

pdx_hemo_none <- df[df$`PDX HemoSeq` == "Complete" &
  !is.na(df$`PDX HemoSeq`) &
  is.na(df$`PDX Molecular Alterations Positive`) &
  is.na(df$`PDX Molecular Details`),]$`PDX Name`

for(pdx in pdx_hemo_none){
  df[df$`PDX Name`==pdx,"PDX Molecular Alterations Positive"] <- "none detected"
  df[df$`PDX Name`==pdx,"PDX Molecular Details"] <- "none detected"
}



