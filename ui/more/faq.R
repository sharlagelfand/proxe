ui_more_faq <- function() {
  tabPanel(
    "FAQ",
    h1("Frequently Asked Questions"),
    # TODO: ideally, eventually make a TOC to each item at the top, linking via anchors or shiny equivalent.
    column(
      width = 10,
      uiOutput("FAQ")
    )
  )
}
