
library(readxl)


# read in spreadsheet
variants.filename <- dir("../data_outside_app/",pattern = glob2rx("20*_pdx_dna_variants.xlsx"))
if(length(variants.filename) != 1) stop("too few or too many _pdx_dna_variants.xlsx files in dropbox")
variants <- read_excel(paste0("../data_outside_app/",variants.filename),sheet=1,col_types = "text")
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
v = v[v$True_Mutation != 0 & !is.na(v$True_Mutation),]

v1 = v[c(1:3,6:8)]
v2 = v[c(1,4:8)]

# recode *_Variant_Classification
table(v1$Canonical_Variant_Classification)
snv=c("Missense_Mutation","Nonsense_Mutation","Nonstop_Mutation","Translation_Start_Site","Missense")
indel=c("Frame_Shift_Del","Frame_Shift_Ins","In_Frame_Del","In_Frame_Ins")
splice=c("Splice_Region","Splice_Site")

#TODO: continue here, creating new columns that map these classifications to the above terms: snv, indel, splice
# TODO: also make sure to filter out terms that don't exist, like "0", "intron","intergenic_variant".



table(v2$BestEffect_Variant_Classification)



# pivot into genes x samples


# produce ComplexHeatmap



# example
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
  oncoPrint(mat, get_type = function(x) strsplit(x, ";")[[1]],
    alter_fun = list(
      snv = function(x, y, w, h) grid.rect(x, y, w*0.9, h*0.9, gp = gpar(fill = col["snv"], col = NA)),
      indel = function(x, y, w, h) grid.rect(x, y, w*0.9, h*0.4, gp = gpar(fill = col["indel"], col = NA))
    ), col = col)
  
}
