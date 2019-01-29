ui_liquid_tumors_contingency_table <- function() {
  tabPanel(
    "Contingency Table",
    h1("Liquid Tumor Contingency Table"),
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          "ctable_numcats",
          "Number of contingency table categories",
          1:2,
          selected = 2
        ),
        # selectInput("ctable_numcats","Number of contingency table categories",
        # 1:2,selected=2),
        selectInput("tablevar1", "First category", sort(names(df)[factor_cols_vis]),
          selected = "WHO Category"
        ),
        conditionalPanel(
          condition = "input.ctable_numcats > 1",
          selectInput("tablevar2", "Second category", sort(names(df)[factor_cols_vis]),
            selected = "Latest Passage Banked"
          )
        )
      ),
      mainPanel(
        h4(
          "Contingency table. Updates based on filtering in Database Explorer."
        ),
        tableOutput("table_various")
      )
    )
  )
}
