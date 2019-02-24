ui_liquid_tumors_pdx_mutations <- function() {
  tabPanel(
    "PDX Mutations",
    h1("Liquid Tumor PDX Mutations"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        sliderInput("onco2_plotheight", label = "Plot height (px)", min = 600, max = 1600, step = 200, value = 800),
        selectizeInput(
          inputId = "onco2_whocat", label = "WHO categories to show",
          choices = unique(df$`WHO Category`), multiple = TRUE, selected = "AML"
        ),
        actionButton("onco2_go", "Compute plot"),
        helpText("Note: This plot can take >10 seconds to compute."),
        checkboxInput("onco2_showWHOclass", "Filter on WHO classification", value = FALSE),
        conditionalPanel(
          condition = "input.onco2_showWHOclass == true",
          checkboxGroupInput(
            inputId = "onco2_whoclass", label = "WHO classification(s)",
            choices = unique(arrange(df, `WHO Category`, `WHO Classification`)$`WHO Classification`),
            selected = unique(df$`WHO Classification`)
          )
        ),
        tags$h5(tags$b("HemoSeq 2.0 coordinates")),
        a("HemoSeq 2.0 baits", href = "methods/150127_Hemoseq_2.0_Baits.interval_list", download = "150127_Hemoseq_2.0_Baits"), br(),
        a("HemoSeq 2.0 targets", href = "methods/150127_Hemoseq_2.0_Targets.interval_list", download = "150127_Hemoseq_2.0_Targets"), br()
      ),
      mainPanel(
        # plotOutput("plot_oncoprint2",height = onco2_plotheight,width="100%")
        uiOutput("onco2.ui")
      )
    )
  )
}
