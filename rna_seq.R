
library(readxl)
library(xlsx)
library(gplots)
library(RColorBrewer)
library(plyr)

# -- one-time conversion of data to CSV to avoid bugs: -- #
## for old data
# rnadf <- read_excel("data/PDX.rpkm.xlsx")
# write.table(rnadf,"data/PDX.rpkm.txt",sep = "\t",row.names = F,col.names = T)
## for new + old data as of 3/2017
# rnadf <- read_excel("../data_outside_app/Cuff_Gene_Counts.xlsx")
# rnadf[is.na(rnadf)] <- 0; names(rnadf)[1] <- "gene"
# cols_to_drop <- names(rnadf)[grep(pattern = "_STRND",names(rnadf))]
# rnadf[,cols_to_drop] <- NULL
# write.table(rnadf,"../data_outside_app/PDX.rpkm.201703.txt",sep = "\t",row.names = F,col.names = T)

#### read in RNA-seq data and produce figure, perhaps on second tab. ####
print("didcomehere00")
# read in RPKM data
# rnadf <- read.table("../data_outside_app/PDX.rpkm.txt",header=T,sep="\t")
# rnadf <- read.table("../data_outside_app/PDX.rpkm.201703.txt",header=T,sep="\t")
rnadf <- read.csv(
  file.path(data_outside_app_dir,"Cuff_Gene_Counts.csv"),
  header=T)

# old version, transposed:
  # rnadf <- read.table(file = "data/wl_tidy_rnaseq_transposed.txt",
  #                    header = T,sep = "\t",stringsAsFactors = F)

# turn gene names into rownames
rownames(rnadf) <- rnadf$Gene_ID
rnadf$Gene_ID <- NULL

# old cleaning code -- delete later
if(F){
  # parse sample names
  rna_meta <- data.frame("full" = colnames(rnadf),stringsAsFactors = F)
  # split into columns
  rna_meta$sample <- sub(pattern = "\\..*$",replacement="",x=rna_meta$full,perl=T)
  rna_meta$dep <- grepl("Dep",rna_meta$sample)
  rna_meta$sample <- sub(pattern="Dep",replacement = "",x = rna_meta$sample)
  rna_meta$pe <- grepl("pe$",rna_meta$full,perl = T)
  rna_meta$date <- sub(pattern = "^.*\\.(\\d{8})[\\.pe]*$",replacement="\\1",x=rna_meta$full,perl=T)
  
  # add zeros in front of single-digit numbers in $sample
  rna_meta$sample <- sub("([A-Z])([1-9])$",replacement="\\10\\2",x = rna_meta$sample,perl=TRUE)
  
  # then merge with df$`PDX RNA-Seq Name`
  dfr <- merge(df,rna_meta,by.x="PDX RNA-Seq Name",by.y="sample",all=F)
}

# add zeros in front of single-digit numbers in $sample
names(rnadf) <- sub("([A-Z])([1-9])$",replacement="\\10\\2",x = names(rnadf),perl=TRUE)

# filter df for those that have RNA seq data here.
dfr <- df[df$`PDX RNA-Seq Name` %in% names(rnadf),]

# --- detect incorrect duplicates --- #

dups <- dfr[duplicated(dfr$`PDX RNA-Seq Name`),]$`PDX RNA-Seq Name`

# debugging line of code
# pair[,c("PDX RNA-Seq Name","PDX Name","full","date")]
if(T){
  # selects between duplicates of type 1 above
  for(dup in dups) {
    pair <- dfr[dfr$`PDX RNA-Seq Name` == dup,]
    if(identical(
      stringr::str_sub(pair[1,"PDX Name"],1,10),
      stringr::str_sub(pair[2,"PDX Name"],1,10))) next # skips type (2) dups
    # order to keep paired-end, then recent date.
    warning(paste("RNA-seq sample",dup,"is associated with unrelated PDXs"))
    full_to_drop <- pair[-1,]$`PDX Name`
    dfr <- dfr[!(dfr$`PDX Name` %in% full_to_drop),]
  }
}
if (any(duplicated(dfr$`PDX Name`))) warning("there are duplicates in the loaded-in RNA-seq")

# TODO: determine what happens with those duplicated samples in heatmap.
  # option to select rows from database is broken; TODO fix.
    # probably has to do with renaming.
  # order of clustered samples isn't as pretty as before; TODO confirm not mixed up.

# subset rnadf for only those columns in dfr
rnadf_sub <- rnadf[,colnames(rnadf) %in% dfr$`PDX RNA-Seq Name`]
rnamat_sub <- as.matrix(rnadf_sub)
print("didcomehere0")

# --- change rnamat_sub names to df$`PDX Name`+ PE/SE + asterisk if inexact sample --- #

dfr$PDX_RNA_abrv <- sub(pattern = "(\\w{4}-\\d{5}-\\w\\d).*$",replacement = "\\1",dfr$`PDX Name`,perl=TRUE)
# Add asterisk if type (2) above and "mCLP","Luc" or "-R\\d"
  # TODO add comment to app explaining asterisk.
dups <- dfr[duplicated(dfr$`PDX RNA-Seq Name`),]$`PDX RNA-Seq Name`

to_asterisk <- (dfr$`PDX RNA-Seq Name` %in% dups)
# dfr[to_asterisk,c("PDX Name","PDX_RNA_abrv","PDX RNA-Seq Name","full")] # just to view which
dfr$PDX_RNA_abrv <- paste0(dfr$PDX_RNA_abrv,ifelse(to_asterisk,"*",""))

for(dup in dups) {
  pair <- dfr[dfr$`PDX RNA-Seq Name` == dup,]
  w <- (grepl("mCLP",pair$`PDX Name`) | grepl("Luc",pair$`PDX Name`) | grepl("-R\\d",pair$`PDX Name`))
  to_replace <- pair[w,]$PDX_RNA_abrv
  replacement <- pair[!w,]$PDX_RNA_abrv
  dfr[dfr$PDX_RNA_abrv == to_replace,]$PDX_RNA_abrv <- replacement
}

# convert expression matrix sample names to new type
colnames(rnamat_sub) <- dfr[match(colnames(rnamat_sub),table = dfr$`PDX RNA-Seq Name`),]$PDX_RNA_abrv


# --- read in lists of genes --- #

# code for converting from excel
  # genesets_df <- read.xlsx("data/2015-10-30_Multiplex_Gene_Panels.xlsx")
  # write.csv(x = genesets_df,file = "data/2015-10-30_Multiplex_Gene_Panels.csv",row.names = F)

genesets_df <-read.csv("data/2015-10-30_Multiplex_Gene_Panels.csv")
genesets_df$RHP_v2.0_exons <- NULL
genesets_df[,5] <- NULL
genesets_list <- as.list(genesets_df)
rm(genesets_df)
genesets_list <- lapply(genesets_list, function(x) x[!is.na(x)])

# create color pallette
my_palette <- colorRampPalette(c("blue", "yellow"))(n = 299)

print("didcomehere1")

rm(rnadf,rnadf_sub)
