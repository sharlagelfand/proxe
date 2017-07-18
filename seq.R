
# to be run after clean_liquid.R

# -- read in seq tracking -- #

# read in metadata
seq.filename <- dir("../data_outside_app/",pattern = glob2rx("SEQUENCING_checklist*xlsx"))
if(length(seq.filename) != 1) stop("too few or too many SEQUENCING_checklist sheets in dropbox")
seq_meta <- read_excel(paste0("../data_outside_app/",seq.filename),sheet="Header_Data")
seq_meta <- as.data.frame(seq_meta)

stopifnot(all(names(table(seq_meta$read_excel_type)) %in% c("character","factor","logical","numeric","date")))
seq_meta$read_excel_type[seq_meta$read_excel_type %in% c("character","factor")] <- "text"
seq_meta$read_excel_type[seq_meta$read_excel_type %in% c("logical","numeric")] <- "numeric"

# read in data, returning difference with seq_meta if error.
# try(expr={ # TODO: implement try-else-print-debugging
seq <- read_excel(paste0("../data_outside_app/",seq.filename),sheet="sequencing",
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
# b <- names(table(df$`SRA Sample Name`)) #170

# setdiff(a,b) # do not exist in prima
# setdiff(b,a)
# intersect(a,b)

# -- read in SRA submission -- #

# SRA submission
sra.filename <- dir("../data_outside_app/",pattern = glob2rx("*SRA_metadata*xlsx"))
if(length(sra.filename) != 1) stop("too few or too many SEQUENCING_checklist sheets in dropbox")
sra <- read_excel(paste0("../data_outside_app/",sra.filename),sheet=2,
  col_types ="text")

# c <- names(table(sra$library_ID))
# setdiff(c,a) # in SRA submission, but not in SEQ-tracking
# 
# table(df$`PDX Name` == df$`SRA Sample Name`)
# library(stringr)
# table(str_sub(df$`PDX Name`,1,10) == str_sub(df$`SRA Sample Name`,1,10))
# 
# setdiff(c,b)

# -- add SRA submission url as hyperlink to df -- #
urlbase = "https://www.ncbi.nlm.nih.gov/sra/"
sra <- sra[!is.na(sra$library_ID),]
dfsra <- merge(x=df,y=sra,by.x = "SRA Sample Name",by.y="library_ID",all.x=TRUE,all.y=FALSE)
for(i in 1:nrow(df)){
  ssnm <- df$`SRA Sample Name`[i]
  if(is.na(ssnm)| ssnm=="NA") next
  # lookup SRA id in dfsra
  sra_accn <- dfsra[which(dfsra$`SRA Sample Name`==ssnm),]$biosample_accession
  if(length(sra_accn)!=1 | sra_accn=="NA" | is.na(sra_accn)) next
  df$`SRA Sample Name`[i] <- as.character(a(target="blank",href=paste0(urlbase,sra_accn),ssnm))
}
  
rm(dfsra,urlbase)
