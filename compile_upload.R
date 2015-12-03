# for pre-compiling data and running/uploading app

setwd("/Users/scott/Dropbox/work/other/PRoXe/PRoXe_app")
source("clean_data.R")
source("rna_seq.R")
source("oncoprint.R")
save.image(file = "pre-compiled.RData")

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