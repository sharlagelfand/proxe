# for pre-compiling data and running/uploading app
rm(list=ls())
setwd("/Users/scott/Dropbox/PRoXe/PRoXe_app")
source("clean_liquid.R")
source("rna_seq.R")
source("oncoprint.R")
source("clean_solid.R")
source("finalize.R")

# check on largest-memory items
z <- sapply(ls(), function(x)
  object.size(get(x)))
as.matrix(rev(sort(z))[1:10])

save.image(file = "pre-compiled.RData",compress = FALSE)

# print size of PRoXe_app folder
cat("Size of app:\n")
system("du -h -d 0 ../PRoXe_app")

# -- comment or uncomment sections below as desired -- #

## 1 - deploy
# deployApp(appDir = "/Users/scott/Dropbox/PRoXe/PRoXe_app",
#           appName = "PRoXe", account = "proxe")
# capture.output(shinyapps::showLogs(appName="PRoXe_alpha",account="proxe",entries=5000),file="~/logs5000.txt")

# interesting way to visualize dependencies: enable, then press cmd+fn+F3 while app is running.
# options(shiny.reactlog = TRUE)

## 2 - run normally
rm(list=ls()) # this cleans environment to mimic shinyapps.io
runApp()
# 
## 3 - run for debugging
# runApp(display.mode = "showcase") # for debugging