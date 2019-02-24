ui_solid_tumors_pdx_glossary <- function() {
  tabPanel(
    "Glossary",
    h1("Solid Tumor Glossary"),
    column(
      width = 12,
      DT::dataTableOutput(outputId = "solid_glossary")
    )
  )
}
