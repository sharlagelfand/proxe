ui_liquid_tumors_glossary <- function() {
  tabPanel(
    "Glossary",
    h1("Liquid Tumor Glossary"),
    column(
      width = 12,
      DT::dataTableOutput(outputId = "glossary")
    )
  )
}
