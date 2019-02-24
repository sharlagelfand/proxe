ui_liquid_tumors_methods <- function() {
  tabPanel(
    "Methods",
    h1("Liquid Tumor Methods"),
    # TODO: ideally, eventually make a TOC to each item at the top, linking via anchors or shiny equivalent.
    column(
      width = 10,
      # note these (ABCD) are Mark's ideal ordering:
      # helpText("A) Xenografting Techniques: with subheadings Tail Vein Injection, Subrenal Capsule Implantation, +/- Subcutaneous Implantation."),
      # helpText("B) Xenografting Cell Doses: with subheadings For In Vivo Expansion, For In Vivo Treatment Studies"),
      # helpText("C) Monitoring Xenografted Animals: with subheadings for Cell Banking, for Initiation of Treatment"),
      # helpText("D) Banking Xenografted Cells: with subheadings Animal Euthanasia, Enriching Malignant Cells from Peripheral Blood, Enriching Malignant Cells from Bone Marrow, Enriching Malignant Cells from Spleen, and Enriching Malignant Cells from Lymph Nodes or Solid Tumors"),
      tags$h2("Table of contents"),
      tags$h4(a("I. General PDX methods", href = "#general")),
      tags$h4(a("II. Renal method", href = "#renal")),
      tags$h4(a("III. Luciferization method", href = "#luciferization")),
      a(name = "general"), # anchor
      tags$h2("General PDX methods"),
      uiOutput("PDX_methods"),
      a(name = "renal"), # anchor
      tags$h2("Renal capsule implantation method"),
      uiOutput("Renal_methods"),
      a(name = "luciferization"),
      tags$h2("Luciferization method"),
      uiOutput("Lucifer_methods")
    ),
    column(
      width = 2,
      tags$h2("Other files"),
      tags$h4("HemoSeq 2.0 coordinates"),
      a("HemoSeq 2.0 baits", href = "methods/150127_Hemoseq_2.0_Baits.interval_list", download = "150127_Hemoseq_2.0_Baits"), br(),
      a("HemoSeq 2.0 targets", href = "methods/150127_Hemoseq_2.0_Targets.interval_list", download = "150127_Hemoseq_2.0_Targets"), br()
    )
  )
}
