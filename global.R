# for loading of objects to be used in both ui.R and server.R
  # a la http://shiny.rstudio.com/articles/scoping.html

loadedGlobal <- load("pre-compiled.RData")
print(loadedGlobal)

# developer code for manually stepping through all loaded objects
if(F){
  for(i in 1:length(loadedGlobal)){
    print(loadedGlobal[i])
    print(str(get(loadedGlobal[i])))
    readline(prompt="Press [enter] to continue")
  }
}
