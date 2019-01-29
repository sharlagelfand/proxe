server_liquid_tumors_methods <- function(input, output, server) {
  output$PDX_methods <- renderUI({
    # filename <- dir(path = "www/methods",pattern="_PDX_Methods_for_proxe.pdf",full.names=T)
    # TODO: later functionalize this
    filename <- dir(path = "www/methods", pattern = "_PDX_Methods_for_proxe.pdf", full.names = T)
    if (length(filename) < 1) warning("Where is the methods pdf?")
    if (length(filename) > 1) {
      warning("> 1 methods PDFs in www/methods folder. Taking last saved.")
      tmp <- sapply(filename, function(i) file.info(i)$mtime)
      newest_file <- sort(tmp, decreasing = T)[1]
      filename <- names(newest_file)
    }
    filename <- gsub("www/", "", filename)
    tags$iframe(
      src = filename,
      width = "100%",
      height = "800px"
    )
  })
  output$Renal_methods <- renderUI({
    filename <- "methods/Renal_capsule_methods.pdf"
    tags$iframe(
      src = filename,
      width = "100%",
      height = "800px"
    )
  })
  output$Lucifer_methods <- renderUI({
    filename <- "methods/2017-3-13_luciferization_PDX_models.pdf"
    tags$iframe(
      src = filename,
      width = "100%",
      height = "800px"
    )
  })
}
