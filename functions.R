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
  plot(c(0, nsamples), c(0, ngenes), type="n", 
    main=sprintf("Gene set altered in %.2f%%: %d of %d cases", coverage/nsamples*100, coverage, nsamples),
    xlab="", ylab="", yaxt="n",xaxt="n");# note SPK added xaxt
  rect(oncoCords[, "xleft"], oncoCords[, "ybottom"],oncoCords[, "xright"], oncoCords[, "ytop"], col=colors, border="white");
  axis(2, at=(ngenes:1)-.5, labels=rownames(alts), las=2);
  axis(1, at=(1:nsamples)-.5, labels=colnames(alts), las=2); # SPK new line in function
}

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


# source: http://stackoverflow.com/questions/7680959/convert-type-of-multiple-columns-of-a-dataframe-at-once
convert.magic <- function(obj,types){
  for (i in 1:length(obj)){
    FUN <- switch(types[i],character = as.character, 
      numeric = as.numeric, 
      factor = as.factor,
      logical = as.logical,
      integer = as.integer,
      date = as.POSIXct)
    if (class(obj[,i]) != types[i]){
      obj[,i] <- FUN(obj[,i])
    }
  }
  obj
}

# dropdownMenu button
dropdownButton <- function(
  label = "", # note label should be simple, perhaps A-z. Not sure what characters are allowed in CSS IDs.
  status = c("default", "primary", "success", "info", "warning", "danger"),
  ...,
  width = NULL) {
  
  status <- match.arg(status)
  css_id <- paste0("dropdownButton-",label)
  # dropdown button content
  html_ul <- list(
    id = css_id,
    class = "dropdown-menu",
    style = if (!is.null(width)) 
      paste0("width: ", validateCssUnit(width), ";"),
    lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;")
  )
  # dropdown button apparence
  html_button <- list(
    class = paste0("btn btn-", status," dropdown-toggle"),
    type = "button", 
    `data-toggle` = "dropdown"
  )
  html_button <- c(html_button, list(label))
  html_button <- c(html_button, list(tags$span(class = "caret")))
  # final result
  tags$div(
    class = "dropdown",
    do.call(tags$button, html_button),
    do.call(tags$ul, html_ul),
    tags$script(
      paste0(
        "$('#",css_id,"').click(function(e) {
          e.stopPropagation();
        });"
      )
    )
  )
}
