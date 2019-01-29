ui_liquid_tumors_database_explorer <- function() {
  tabPanel(
    "Database Explorer",
    includeCSS("styles.css"),
    tags$head(
      # probably overkill code for favicon, as per http://www.favicon-generator.org/
      # unimportant TODO: determine whether href to root ("/fav..." rather than "fav...") even works.
      # relevant to this, `a("text",href="methods/file.txt",download="file")` works, while
      # `a("text",href="/methods/file.txt",download="file")` doesn't work. This occurred in Methods page below.
      # I thought of this because some of the tags$link below have root and some (perhaps the critical ones) don't.
      # Could easy test on tiny_test_app
      tags$link(rel = "apple-touch-icon", sizes = "57x57", href = "/favicon_files/apple-icon-57x57.png"),
      tags$link(rel = "apple-touch-icon", sizes = "60x60", href = "/favicon_files/apple-icon-60x60.png"),
      tags$link(rel = "apple-touch-icon", sizes = "72x72", href = "/favicon_files/apple-icon-72x72.png"),
      tags$link(rel = "apple-touch-icon", sizes = "76x76", href = "/favicon_files/apple-icon-76x76.png"),
      tags$link(rel = "apple-touch-icon", sizes = "114x114", href = "/favicon_files/apple-icon-114x114.png"),
      tags$link(rel = "apple-touch-icon", sizes = "120x120", href = "/favicon_files/apple-icon-120x120.png"),
      tags$link(rel = "apple-touch-icon", sizes = "144x144", href = "/favicon_files/apple-icon-144x144.png"),
      tags$link(rel = "apple-touch-icon", sizes = "152x152", href = "/favicon_files/apple-icon-152x152.png"),
      tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "/favicon_files/apple-icon-180x180.png"),
      tags$link(rel = "icon", type = "image/png", sizes = "192x192", href = "/android-icon-192x192.png"),
      tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon_files/favicon-32x32.png"),
      tags$link(rel = "icon", type = "image/png", sizes = "96x96", href = "favicon_files/favicon-96x96.png"),
      tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "favicon_files/favicon-16x16.png"),
      tags$link(rel = "manifest", href = "/favicon_files/manifest.json"),
      tags$meta(name = "msapplication-TileColor", content = "#ffffff"),
      tags$meta(name = "msapplication-TileImage", content = "/favicon_files/ms-icon-144x144.png"),
      tags$meta(name = "theme-color", content = "#ffffff"),

      # Google Analytics
      includeScript("google_analytics.js")

      # playing with jQuery code -- another way to roll up sidebar.
      # ,includeScript("testing.js")
      # this code has not yet and probably won't be added to the git repo
    ),

    # customHeaderPanel("Logo"),
    h1("Liquid Tumor Database Explorer"),
    br(),
    tags$div(
      style = "margin:15px;",
      # top row of app
      fluidRow(
        column(6,
          # dropdown buttons - left side
          id = "dt-col-select",
          tags$div(
            style = "display: flex;",
            tags$h4("First, select columns to show:", style = "margin-right: 15px; font-weight: bold;"),
            mydropdownButton("administrative", meta3, condVis_ind, button_group = "liquid"),
            mydropdownButton("patient", meta3, condVis_ind, button_group = "liquid"),
            mydropdownButton("tumor", meta3, condVis_ind, button_group = "liquid"),
            mydropdownButton("pdx", meta3, condVis_ind, button_group = "liquid")
          )
        ),
        column(6,
          # links, buttons on right side
          id = "email-request",
          p(
            a("Email us", href = "mailto:proxe.feedback@gmail.com?Subject=PRoXe%20feedback", target = "_top"),
            " with questions.",
            actionButton("Request_link", "Request lines", icon("arrow-circle-o-right"),
              class = "btn btn-primary"
            ), # style="background-color: #1486ba; color: #fff; border-color: #2e6da4"
            align = "right"
          )
        )
      ),
      # 1. data table
      fluidRow(
        DT::dataTableOutput(outputId = "table")
      ),
      # 2. Create download button below table
      fluidRow(h4(" ")), # just an empty space.
      fluidRow(
        p(class = "text-center", downloadButton("download_filtered", "Download Filtered Data"))
      ),
      # 2. take filtered data table and apply various interactive graphical analysis.
      fluidRow(
        sidebarPanel(
          width = 4,
          selectInput(
            "plotType", "Plot Type",
            c(
              Histogram = "hist", Scatter = "scatter", Bar = "bar", "1D Scatter-Box" = "scatbox",
              "2D Contingency Table" = "ctable_plot"
            ),
            selected = "scatbox"
          ),

          # Option 1: show histogram.
          conditionalPanel(
            condition = "input.plotType == 'hist'",
            selectInput("hist_var", "Variable to plot",
              sort(names(df)[numeric_cols_vis]),
              selected = "Age"
            ),
            selectInput(
              "hist_log", "Scaling",
              c(linear = FALSE, log10 = TRUE)
            ),
            selectInput(
              "breaks", "Breaks",
              c(
                "[Custom]" = "custom", "Sturges",
                "Scott", "Freedman-Diaconis"
              )
            ),
            # Only show this sub-panel if Custom is selected
            conditionalPanel(
              condition = "input.breaks == 'custom'",
              sliderInput("breakCount", "Break Count", min = 1, max = 100, value = 25)
            )
          ),

          # Option 2: show scatterplot.
          conditionalPanel(
            condition = "input.plotType == 'scatter'",
            selectInput("scat_var_x", "X variable to plot", sort(names(df)[numeric_cols_vis]),
              selected = "Age"
            ),
            selectInput("scat_var_y", "Y variable to plot", sort(names(df)[numeric_cols_vis]),
              selected = "Presenting WBC"
            )
          ),

          # Option 3: show barplot.
          conditionalPanel(
            condition = "input.plotType == 'bar'",
            selectInput("bar_var", "Category to plot", sort(names(df)[factor_cols_vis]),
              selected = "WHO Category"
            )
          ),

          # Option 4: show 1D-scatter+boxplot.
          conditionalPanel(
            condition = "input.plotType == 'scatbox'",
            selectInput("scatbox_cat", "Category to plot", sort(names(df)[factor_cols_vis]),
              selected = "WHO Category"
            ),
            selectInput("scatbox_num", "Numeric to plot", sort(names(df)[numeric_cols_vis]),
              selected = "Days to Engraft P0"
            )
          ),
          #                # Option 5: contingency table of categories
          #                conditionalPanel(
          #                  condition = "input.plotType == 'ctable'",
          #                  selectInput("tablevarA","First category",sort(names(df)[factor_cols_vis]),
          #                              selected = "WHO Category"),
          #                  selectInput("tablevarB","Second category",sort(names(df)[factor_cols_vis]),
          #                              selected = "Latest Passage Banked")
          #                 ),
          # Option 6: mosaic plot of contingency table.
          conditionalPanel(
            condition = "input.plotType == 'ctable_plot'",
            selectInput("ctable_plot_var1", "First category", sort(names(df)[factor_cols_vis]),
              selected = "WHO Category"
            ),
            selectInput("ctable_plot_var2", "Second category", sort(names(df)[factor_cols_vis]),
              selected = "Latest Passage Banked"
            )
          )
        ),
        column(
          width = 8,
          ({
            #                   if (input$plotType == 'ctable') {
            #                      tableOutput("table_various")
            #                   } else
            plotOutput("plot_various", height = "600px")
          })
        )
      )
    )
  )
}
