ui_liquid_tumors_pdx_gene_fusion_predictions <- function() {
  tabPanel(
    "PDX Gene Fusion Predictions",
    h1("Liquid Tumor PDX Gene Fusion Predictions"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        
        sliderInput(
          inputId = "pdx_gene_fusion_predictions_plot_height",
          label = "Plot height (px)",
          min = 600,
          max = 1600,
          step = 200,
          value = 1000
        ),
        
        sliderInput(
          inputId = "pdx_gene_fusion_predictions_plot_width",
          label = "Plot width (px)",
          min = 600,
          max = 1600,
          step = 200,
          value = 1000
        ),
        
        radioButtons(
          inputId = "pdx_gene_fusion_predictions_select_fusions",
          label = "Select fusions",
          choices = c("All",
                      "By fusion" = "fusion",
                      "By reads" = "reads"
          ),
          selected = "All"
        ),
        
        conditionalPanel(
          condition = "pdx_gene_fusion_predictions_select_fusions == 'fusion'",
          selectizeInput(
            inputId = "pdx_gene_fusion_predictions_fusins",
            label = "Fusion:",
            choices = sort(unique(gene_fusion_predictions[["fusion_name"]])),
            selected = NULL,
            multiple = TRUE
          )
        ),
        
        # conditionalPanel(
        #   condition = "input.pdx_gene_fusion_predictions_all_transcripts == 'reads'",
        #   checkboxGroupInput(
        #     inputId = "pdx_gene_fusion_predictions_virus",
        #     label = "Virus:",
        #     choices = c(
        #       "HTLV1",
        #       "EBV",
        #       "Hep C",
        #       "HHV5",
        #       "HPV",
        #       "XMRV"
        #     ),
        #     selected = NULL
        #   )
        # ),
        
        radioButtons(
          inputId = "pdx_gene_fusion_predictions_selection_method",
          label = "Type of sample input",
          choices = c(
            "All samples" = "all",
            "Select line names" = "line_name",
            "Select WHO Category" = "who_category",
            "Select WHO Classification" = "who_classification",
            "Click rows in Database Explorer" = "database_explorer"
          ),
          selected = "all"
        ),
        
        conditionalPanel(
          condition = "input.pdx_gene_fusion_predictions_selection_method == 'line_name'",
          selectInput(
            inputId = "pdx_gene_fusion_predictions_line_name",
            label = "Type or select line names",
            choices = unique(gene_fusion_predictions[["pdx_name"]]),
            selected = NULL,
            multiple = TRUE
          )
        ),
        
        conditionalPanel(
          condition = "input.pdx_gene_fusion_predictions_selection_method == 'who_category'",
          selectizeInput(
            inputId = "pdx_gene_fusion_predictions_who_category",
            label = "WHO Category",
            choices = unique(df[["WHO Category"]]),
            multiple = TRUE,
            selected = "ALL"
          )
        ),
        
        conditionalPanel(
          condition = "input.pdx_gene_fusion_predictions_selection_method == 'who_classification'",
          selectizeInput(
            inputId = "pdx_gene_fusion_predictions_who_classification",
            label = "WHO Classification",
            choices = unique(df[["WHO Classification"]]),
            multiple = TRUE,
            selected = c("T-ALL", "B-ALL NOS", "B-ALL with t(12;21) TEL-AML1 (ETV6-RUNX1)",
                         "B-ALL with t(9;22) BCR-ABL", "B-ALL with t(v;11q23) MLL rearranged",
                         "B-ALL with t(1;19) E2A-PBX1 (TCF3-PBX1)", 
                         "B-ALL with hyperdiploidy", "B-ALL with hypodiploidy", 
                         "B/myeloid acute leukemia")
          )
        ),
        
        downloadLink("pdx_gene_fusion_predictions_download", "Download raw data")
      ),
      mainPanel(
        uiOutput("ui_plot_pdx_gene_fusion_predictions_detection")
      )
    )
  )
}
