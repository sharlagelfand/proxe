### --- this script is for new functions in the PRoXe app --- ###

# source of oncoprint functions, which I revised minorly: https://gist.github.com/armish/564a65ab874a770e2c26

# This oncoprint function sorts the matrix for better visualization of mutual exclusivity across genes
memoSort <- function(M) {
  geneOrder <- sort(rowSums(M), decreasing=TRUE, index.return=TRUE)$ix;
  scoreCol <- function(x) {
    score <- 0;
    for(i in 1:length(x)) {
      if(x[i]) {
        score <- score + 2^(length(x)-i);
      }
    }
    return(score);
  }
  scores <- apply(M[geneOrder, ], 2, scoreCol);
  sampleOrder <- sort(scores, decreasing=TRUE, index.return=TRUE)$ix;
  return(M[geneOrder, sampleOrder]);
}

# Oncoprint plotting function
oncoPrint <- function(M, sort=TRUE) {
  if(sort) {
    alts <- memoSort(M);		
  } else {
    alts <- M;
  }
  
  ngenes <- nrow(alts);
  nsamples <- ncol(alts);
  coverage <- sum(colSums(alts) > 0); # SPK: edited from mistake: rowSums
  
  ### OncoPrint
  numOfOncos <- ngenes*nsamples;
  oncoCords <- matrix( rep(0, numOfOncos * 5), nrow=numOfOncos );
  colnames(oncoCords) <- c("xleft", "ybottom", "xright", "ytop", "altered");
  
  xpadding <- .01;
  ypadding <- .01;
  cnt <- 1;
  for(i in 1:ngenes) {
    for(j in 1:nsamples) {
      xleft <- j-1 + xpadding;
      ybottom <- ((ngenes-i+1) -1) + ypadding;
      xright <- j - xpadding;
      ytop <- (ngenes-i+1) -ypadding;
      altered <- alts[i, j];
      
      oncoCords[cnt, ] <- c(xleft, ybottom, xright, ytop, altered);
      cnt <- cnt+1;
    }
  }
  
  colors <- rep("lightgray", cnt);
  colors[ which(oncoCords[, "altered"] == 1) ] <- "black";
  plot(c(0, nsamples), c(0, ngenes), type="n", main=sprintf("Gene set altered in %.2f%%: %d of %d cases", coverage/nsamples*100, coverage, nsamples), xlab="", ylab="", yaxt="n",xaxt="n");# note SPK added xaxt
  rect(oncoCords[, "xleft"], oncoCords[, "ybottom"],oncoCords[, "xright"], oncoCords[, "ytop"], col=colors, border="white");
  axis(2, at=(ngenes:1)-.5, labels=rownames(alts), las=2);
  axis(1, at=(nsamples:1)-.5, labels=colnames(alts), las=2); # SPK new line in function
}