# for pre-compiling data and running/uploading app
rm(list=ls())
setwd("/Users/scott/Dropbox/work/other/PRoXe/PRoXe_app")
source("clean_data.R")
source("rna_seq.R")
source("oncoprint.R")
# source("clean_solid.R")

# check on largest-memory items
z <- sapply(ls(), function(x)
  object.size(get(x)))
as.matrix(rev(sort(z))[1:10])

save.image(file = "pre-compiled.RData",compress = FALSE)

# -- comment or uncomment sections below as desired -- #

## 1 - deploy
# deployApp(appDir = "/Users/scott/Dropbox/work/other/PRoXe/PRoXe_app",
#           appName = "PRoXe", account = "proxe")
# capture.output(shinyapps::showLogs(appName="PRoXe_alpha",account="proxe",entries=5000),file="~/logs5000.txt")

## 2 - run normally
runApp()
# 
## 3 - run for debugging
# runApp(display.mode = "showcase") # for debugging