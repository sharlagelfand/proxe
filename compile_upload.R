## this script is for pre-compiling data and running/uploading app

# clean slate
rm(list=ls())

# set working directory based on user -- hardcoded for Mark and Scott
host <- system("hostname",intern=TRUE)
essential_dir = c("data","global.R","server.R","ui.R","www")
if(basename(getwd()) == "PRoXe_app" & all(essential_dir %in% dir())){
  print("Seem to be in PRoXe_app directory. Continuing...")
} else if (grepl("skallgren",host)){
    wd_full = "/Users/scott/Dropbox/PRoXe/PRoXe_app"
    print(paste("User is Scott. Setting working directory to",wd_full))
    setwd(wd_full)
} else if (grepl("Mark",host)){
    stop("Insert Mark's PRoXe_app path into compile_upload.R")
    wd_full = "INSERT HERE"
    print(paste("User is Mark. Setting working directory to",wd_full))
    setwd(wd_full)
} else {
    stop("User not recognized; manually navigate to PRoXe_app directory via setwd()")
}

# run read-in scripts
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