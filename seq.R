
# to be run after clean_liquid.R

# -- read in seq tracking -- #

# read in metadata
seq.filename <- dir(data_outside_app_dir,pattern = glob2rx("SEQUENCING_checklist*xlsx"))
if(length(seq.filename) != 1) stop("too few or too many SEQUENCING_checklist sheets in dropbox")
seq_meta <- read_excel(file.path(data_outside_app_dir,seq.filename),sheet="Header_Data")
seq_meta <- as.data.frame(seq_meta)

stopifnot(all(names(table(seq_meta$read_excel_type)) %in% c("character","factor","logical","numeric","date")))
seq_meta$read_excel_type[seq_meta$read_excel_type %in% c("character","factor")] <- "text"
seq_meta$read_excel_type[seq_meta$read_excel_type %in% c("logical","numeric")] <- "numeric"

# read in data, returning difference with seq_meta if error.
# try(expr={ # TODO: implement try-else-print-debugging
seq <- read_excel(file.path(data_outside_app_dir,seq.filename),sheet="sequencing",
  col_types =rep("text",nrow(seq_meta))) # seq_meta$read_excel_type)
# })
seq <- as.data.frame(seq) # added because the default class of read_excel output is ‘tbl_df’, ‘tbl’ and 'data.frame' which is incompatible with FUN of convert.magic() 8/2016

# remove flagged rows
seq <- seq[seq$Qualitative_Flag==0,]
if(anyNA(seq$Qualitative_Flag)) warning("Sequencing_checklist qualitative flag contains NAs.")

# convert column names from SEQUENCING name to desired PRoXe name
# order 'seq_meta' by 'seq_meta$Interal_Column_Header' matching names(df)
seq_meta <- seq_meta[match(names(seq),seq_meta$Internal_Column_Header),]
if(!all(names(seq) == seq_meta$Internal_Column_Header)) stop("seq names ordering incorrect")
# names(seq) <- seq_meta$PRoXe_Column_Header # leaving this out for now for easier coding

# check which sra sample names overlap/don't with this list
# a <- names(table(seq$pdx_name)) # 254
# b <- names(table(df$`RNA-Seq Data File Name SRA`)) #170

# setdiff(a,b) # do not exist in prima
# setdiff(b,a)
# intersect(a,b)

# -- read in SRA submission -- #

# SRA submission
sra.filename <- dir(data_outside_app_dir,pattern = glob2rx("*SRA_metadata*xlsx"))
if(length(sra.filename) != 1) stop("too few or too many SRA_metadata sheets in dropbox")
sra <- read_excel(file.path(data_outside_app_dir,sra.filename),sheet=2,
  col_types ="text")

# c <- names(table(sra$library_ID))
# setdiff(c,a) # in SRA submission, but not in SEQ-tracking
# 
# table(df$`PDX Name` == df$`RNA-Seq Data File Name SRA`)
# library(stringr)
# table(str_sub(df$`PDX Name`,1,10) == str_sub(df$`RNA-Seq Data File Name SRA`,1,10))
# 
# setdiff(c,b)

# -- add SRA submission url as hyperlink to df -- #
urlbase = "https://www.ncbi.nlm.nih.gov/sra/"
sra <- sra[!is.na(sra$library_ID),]
# remove suffixes for merge
sra$lib_pdx_id = substring(sra$library_ID,1,10)
# merge
# dfsra <- merge(x=df,y=sra,by.x = "RNA-Seq Data File Name SRA",by.y="library_ID",all.x=TRUE,all.y=FALSE)
dfsra <- merge(x=df,y=sra,by.x = "namenum",by.y="lib_pdx_id",all.x=TRUE,all.y=FALSE)
for(i in 1:nrow(df)){
  ssnmlong <- df$`RNA-Seq Data File Name SRA`[i]
  ssnm <- substring(ssnmlong,1,10)
  if(is.na(ssnm)| ssnm=="NA") next
  # lookup SRA id in dfsra
  sra_accn <- dfsra[which(substring(dfsra$`RNA-Seq Data File Name SRA`,1,10)==ssnm),]$biosample_accession
  if(length(sra_accn)<1) next
  if(length(sra_accn)>1) sra_accn <- sra_accn[1] # ok because all seen so far are duplicates.
  if (sra_accn=="NA" | is.na(sra_accn)) next
  # print hyperlink with SRA library ID
  library_ID <- sra[sra$biosample_accession==sra_accn,]$library_ID
  df$`RNA-Seq Data File Name SRA`[i] <- as.character(
    a(target="blank",href=paste0(urlbase,sra_accn),library_ID)
  )
}
# if should have SRA data but doesn't, label 'in process'
tmp8 <- sapply(df$`RNA-Seq Data File Name SRA`,nchar)
for(i in 1:nrow(df)) {
  tmp8i <- tmp8[i]
  if(tmp8i<20 & !is.na(tmp8i)){
    df$`RNA-Seq Data File Name SRA`[i] <- "In process"
  }
}

rm(ssnmlong,ssnm,sra_accn,library_ID,tmp8,tmp8i)

rm(dfsra,urlbase)
