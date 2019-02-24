ui_liquid_tumors_line_report <- function() {
  tabPanel(
    "Line Report",
    h1("Liquid Tumor Line Report"),
    basicPage(
      # h4("Select a line in the Database Explorer to see a report here"),
      radioButtons(
        "line_report_input_type", "Method for selecting line to show here:",
        c("Choose from drop-down menu" = "dropdown", "Click a row in Database Explorer" = "click"),
        selected = "dropdown"
      ),
      conditionalPanel(
        condition = "input.line_report_input_type == 'dropdown'",
        selectInput("line_report_name", "Type or select a line name below to see a full report",
          c(df[, "PDX Name"], NULL),
          selected = "DFTL-85005-V4"
        ) # TODO: softcode this so it doesn't break upon further passage.
      ),
      DT::dataTableOutput(outputId = "line_report"),
      br(),
      h3("Specific Inventory:"),
      p("Last updated in PRoXe", inv_upDate, "."),
      DT::dataTableOutput(outputId = "line_report_inventory"),
      h3("Flow Cytometry:"),
      uiOutput("line_report_FC"),
      h3("Immunohistochemistry:"),
      uiOutput("line_report_IHC"),
      h3("Pathology Report:"),
      uiOutput("line_report_Path")
    )
  )
}
