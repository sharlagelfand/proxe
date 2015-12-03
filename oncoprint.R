

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