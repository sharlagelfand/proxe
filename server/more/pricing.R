server_more_pricing <- function(input, output, server) {
  output$pricing <- DT::renderDataTable({
    pricing <- data.frame(
      "Service Name" = c("Per vial", "Handling rate (per shipment)", "Consulting (hourly)", "Shipping (domestic)", "Shipping (international)"),
      "DFCI Rate" = c("$700", "$94", "$125", "varies", "n/a"),
      "Academic Rate" = c("$900", "$126", "$169", "by shipment zone", "by shipment address"),
      "Corporate Rate" = "<a href=\"mailto:proxe.feedback@gmail.com?Subject=PRoXe%20corporate%20rates\" target=\"_top\">contact us</a>",
      row.names = NULL
    )
    # changed from $385 and $519 per DFCI and Academic vial 8/31/2017
    # change colnames to remove automatic periods instead of spaces.
    colnames(pricing) <- c("Service Name", "DFCI Rate", "Academic Rate", "Corporate Rate")
    pricing
  },
  escape = FALSE,
  rownames = FALSE,
  selection = "none",
  options = list(
    dom = "t",
    ordering = FALSE
  )
  )
}
