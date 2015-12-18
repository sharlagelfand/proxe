

# M is the alteration matrix with T => Altered, F => Not-altered
# Rows are genes, columns are samples
# M <- matrix(rep(FALSE, 100*3), ncol=100, nrow=3);
# rownames(M) <- c("Gene1", "Gene2", "Gene2");
# colnames(M) <- paste("Sample", 1:100, sep="");
# M[1, 1:25] <- TRUE;
# M[2, 26:50] <- TRUE;
# M[3, 51:75] <- TRUE;

# We don't care about the order, so samples can be ordered in any way before plotting
# M <- M[, sample(1:100, 100)];
# End of custom alteration matrix

# Create the plot
# oncoPrint(M);

# You can also disable the sorting
# oncoPrint(M, sort=FALSE);

# note prima.filename comes from clean_data.R
mut_df <- read.xlsx2(paste0("data//",prima.filename),sheetName = "Mutation_Matrix",header = T,colClasses = "character")
mut_df <- mut_df[mut_df$Include_in_Primary_Analysis==1,]

M2 <- mut_df
# rm(mut_df)

# Remove all columns but those desired, rename
M2cols <- colnames(M2)
M2rows <- M2$Line.Name.for.Figure
M2colsbin <- grep(pattern = "binary",M2cols)
M2colstype <- grep(pattern = "^type$",M2cols,perl=T)
M2colsRNAseq <- grep(pattern= "PDX.RNA.Seq_Name",M2cols,fixed = T)
M2colsLineName <- grep(pattern="Line.Name.for.Figure",M2cols,fixed=T)
M2colskeep <- c(M2colstype,M2colsRNAseq,M2colsLineName,M2colsbin)
M2 <- M2[,M2cols[M2colskeep]]
colnames(M2) <- gsub(".binary","",colnames(M2))
M2_genes <- colnames(M2)[4:ncol(M2)]


# -- take create mutation columns from mut_df or M2 and add to main 'df' --- #
  # goal columns:
  # PDX Molecular Alterations Positive - make this just e.g. "DNMT3A" from 'binary'
  # PDX Molecular Alterations Type - concat this from 'type'
  # PDX Molecular Details - concat this from 'details' 

# Wrangle title name from 'binary' column, creating new column for df
  # perhaps insert title name into all '1's of binary column
  # perhaps do this also for 'type' and 'detail'

m3 <- mut_df
m3cols <- colnames(m3)
m3cols.bin <- grep("^.*\\.binary$",m3cols,perl=T)
m3cols.type <- grep("^.*\\.type$",m3cols,perl=T)
m3cols.details <- grep("^.*\\.details$",m3cols,perl=T)
m3genes <- gsub(".binary","",m3cols[m3cols.bin])
# convert all factors to char
i <- sapply(m3, is.factor)
m3[i] <- lapply(m3[i], as.character)

# remove all non-'1's in binary and adjacent type, details columns
  # know position of non-empty binary cell
  # split binary by pipes into vector
  # split adjacent two by pipes into vector
  # test binary for 1s, keep those in adjacent two corresponding, depositing in same columns
    # maybe add creation of new column set replacing 1s with genes to this step.
bin_seq <- seq_along(m3cols.bin)

# for testing loop below
# which(m3cols=="TET2.binary") #278
# which(m3[,278]=="1|2")#90
# gene_name="TET2"; j=278; i=90
# gene_name= "TP53"; j=285; i=89 # m3[i,j] == "1|1" # m3[i,j:(j+2)]
# gene_name="ARID1A"; j=31  ; i=11

for (n in bin_seq) {
  gene_name = m3genes[n]
  j = m3cols.bin[n]
  for (i in 1:nrow(m3)) {
    if ( grepl("1",m3[i,j]) ) {
      x <- unlist(strsplit( m3[i,j],"\\|"))
      y <- unlist(strsplit( m3[i,j+1],"\\|"))
      z <- unlist(strsplit( m3[i,j+2],"\\|"))
      g <- rep(gene_name,length(x))
      # if type and description are missing, note and warn.
      diffyx <- length(x) - length(y)
      if(diffyx > 0) {
        warning(paste("missing type at:",i,j,gene_name))
        y <- c(y,rep("(type pending)",diffyx))
      }
      diffzx <- length(x) - length(z)
      if(diffzx > 0) {
        warning(paste("missing details at:",i,j,gene_name))
        z <- c(z,rep("(details pending)",diffyx))
      }
      # subset y,z,g by x==1
      y = y[x==1]
      z = z[x==1]
      g = g[x==1]
      z = paste(g,y,z) # combines 'type' into 'details'
      # replace original data.
      m3[i,j] = paste(g,collapse=" | ")
      m3[i,j+1] = paste(y,collapse=" | ")
      m3[i,j+2] = paste(z,collapse=" | ")
    } else m3[i,j] = m3[i,j+1] = m3[i,j+2] = ""
  }
}

# combine columns right
m3$temp1 <- do.call(paste, c(m3[,m3cols.bin], sep=" | "))
m3$temp2 <- do.call(paste, c(m3[,m3cols.type], sep=" | "))
m3$temp3 <-do.call(paste, c(m3[,m3cols.details], sep=" | "))

# save as other columns: TODO: move and use
# m3$PDX_Molecular_Alterations_Positive <- m3$temp1
# m3$PDX_Molecular_Alterations_Type <- m3$temp2
# m3$PDX_Molecular_Details <- m3$temp3

m3t <- m3[c("temp1","temp3")] # removed temp2 to remove 'type'
# remove all pipes at beginning, end, and adjacent to other pipes
  # adjacent
i=1; temp <- NA
while (!identical(temp,m3t)){
  print(i); i=i+1
  temp <- m3t
  # m3t <- apply(m3t,2,function(i) gsub("\\| \\|","\\|",i))
  m3t <- apply(m3t,2,function(i) gsub("||","|",i,fixed=T))
  m3t <- apply(m3t,2,function(i) gsub("| |","|",i,fixed=T))
  m3t <- apply(m3t,2,function(i) gsub("|  |","|",i,fixed=T))
}
rm(temp)
  # beginning and end
m3t <- apply(m3t,2,function(i) gsub("^ \\| ","",i,perl = TRUE))
m3t <- apply(m3t,2,function(i) gsub(" \\| $","",i,perl = TRUE))

# rename columns and combine with index column
m3t <- cbind(m3$PDX.RNA.Seq_Name,m3t)
colnames(m3t) <- c("PDX-RNA-Seq Name","PDX Molecular Alterations Positive",
                  "PDX Molecular Details")
m3t <- as.data.frame(m3t,stringsAsFactors = F)


# Merge with 'df', making sure indexing doesn't change and inner/outer is correct.
  # match on df$`PDX-RNA-Seq Name`,m3$PDX.RNA.Seq_Name and keep all in df.
df <- merge(df,m3t,by = "PDX-RNA-Seq Name",all.x=T)
# move PDX-RNA-Seq Name to just invisible (5, the current condVis_ind)
  # function from http://stackoverflow.com/questions/18339370/reordering-columns-in-a-large-dataframe
moveMe <- function(data, tomove, where = "last", ba = NULL) {
  temp <- setdiff(names(data), tomove)
  x <- switch(
    where,
    first = data[c(tomove, temp)],
    last = data[c(temp, tomove)],
    before = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)-1))]
    },
    after = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)))]
    })
  x
}

df <- moveMe(df, c( "PDX-RNA-Seq Name"), "after", "Treatment Phase at Time of Sample")
# move three new columns on right side (91:93) to just after Source Molecular Details (currently 32)
df <- moveMe(df, c("PDX Molecular Alterations Positive",
                   "PDX Molecular Details"),"after","PDX HemoSeq")
obInvisRet_ind <- obInvisRet_ind + 3

#TODO, perhaps: clean up memory/storage/loadtime leaks from unused variables above.
rm(mut_df,m3,m3t)

set.seed(13)
df <- df[sample(1:nrow(df)),]
