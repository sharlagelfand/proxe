server_more_ilab_manual <- function(input, output, session) {
  output$iLab_manual <- renderUI({
    filename <- "iLab_Customer_Manual_for_LLX.pdf"
    tags$iframe(
      src = filename,
      width = "80%",
      height = "800px"
    )
  })
}
