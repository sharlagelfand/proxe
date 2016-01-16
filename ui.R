# ui.R #

library(shiny)
library(DT)
library(ggplot2)
library(readxl)
library(xlsx)
library(gplots)
library(RColorBrewer)
library(plyr)


# setwd, load, and clean data
# commenting out because not necessary in both server and UI
# delete later if still unnecessary
loaded <- load("pre-compiled.RData")

# source("clean_data.R")
# source("rna_seq.R")
# source("oncoprint.R") #TODO: debug here
source("functions.R")

print("didcomehere3")
# # make lists of which variables to show as options
numeric_cols_vis <- which(sapply(df[,1:obInvisRet_ind], is.numeric))
factor_cols_vis <- which(sapply(df[,1:obInvisRet_ind], is.factor))

# find indices of transitions from obligate visible to conditionally visible to obligate invisible columns
  # TODO: discuss with Mark that I think we should have a fourth section, thus:
    # 1. obligate visible from the start
    # 2. option to click to make visible
    # 3. option to type in box to make visible (genes)
    # 4. never visible (should we even have these in the dataframe we read in?)

# Define the overall UI
shinyUI(
#   fluidPage(
#     titlePanel("PRoXe: Public Repository of Xenografts"),
  navbarPage("PRoXe: Public Repository of Xenografts",
    tabPanel("Database Explorer",
    # Left sidebar for selecting which columns to show
      sidebarLayout(
        sidebarPanel(
          # adjusting sidebar width and text manually with CSS
          tags$head(
            # tags$style(type='text/css', ".well { max-width: 310px; }"),
            # tags$style(type='text/css', ".span3 { max-width: 310px; }"),
            tags$style(type='text/css', ".radio, .checkbox { margin-bottom: 2px; }"),
            tags$style(type='text/css', ".radio label, .checkbox label {
              width: 100%;
              overflow: hidden;
              text-overflow: ellipsis;
              white-space: nowrap;
            }"),
            tags$style(type='text/css', "
              @media (min-width: 768px)
              .col-sm-3 {
                width: 25%;
                max-width: 29em;
              }")#,
            # for adding ellipsis to ColVis
  #           tags$style(type='text/css', "
  #             ul.ColVis_collection li span {
  #               overflow: hidden;
  #               text-overflow: ellipsis;
  #               width: 90%;
  #             }")
            
              
          ),
          h4(strong("First, select columns to show:")),
          actionButton("selectall", label="Select/Deselect all"),
          checkboxGroupInput("show_vars",
                             NULL,
                             names(df[1:(obInvisRet_ind-1)]),
                             selected=names(df)[1:(condVis_ind-1)]
                             )
          ,width=3 # used to be width 4. 3 works better for full screenwidth. Would prefer fixed to longest name length. TODO.
          ),
        
        # Right panel for showing table with subsettable columns, alphabetized.
        mainPanel(
          # 0. Email us (temporary)
          fluidRow(
            HTML("<p align=\"right\">
                  <a href=\"mailto:proxe.feedback@gmail.com?Subject=PRoXe%20feedback\" target=\"_top\">Email us</a> with feedback or to inquire about lines.
                </p>")
          ),
          # 1. data table. -- maybe add a histogram or something below if desired.
          fluidRow(
            DT::dataTableOutput(outputId="table")
          ),
          # 2. Create download button below table
          fluidRow(h4(" ")), # just an empty space.
          fluidRow(
            p(class = 'text-center', downloadButton('download_filtered', 'Download Filtered Data'))
          ),
          # 2. TODO: take filtered data table and apply some kind of graphical analysis.
          fluidRow(
            sidebarPanel(
               selectInput(
                 "plotType", "Plot Type",
                 c(Histogram = "hist", Scatter = "scatter", Bar = "bar", "1D Scatter-Box" = "scatbox",
                    "2D Contingency Table" = "ctable_plot"),
                 selected = "hist"
                 ),
               
               # Option 1: show histogram.
               conditionalPanel(
                 condition = "input.plotType == 'hist'",
                 selectInput("hist_var","Variable to plot",
                             sort(names(df)[numeric_cols_vis]),selected="Age"),
                 selectInput(
                   "hist_log","Scaling",
                   c(linear=FALSE,log10=TRUE)
                   ),
                 selectInput(
                   "breaks", "Breaks",
                   c("[Custom]" = "custom", "Sturges",
                     "Scott","Freedman-Diaconis")
                   ),
                 # Only show this sub-panel if Custom is selected
                 conditionalPanel(
                   condition = "input.breaks == 'custom'",
                   sliderInput("breakCount", "Break Count", min=1, max=100, value=25)
                   
                  )
                 
                ),
               
               # Option 2: show scatterplot.
               conditionalPanel(
                 condition = "input.plotType == 'scatter'",
                 selectInput("scat_var_x","X variable to plot",sort(names(df)[numeric_cols_vis]),
                             selected="Age"),
                 selectInput("scat_var_y","Y variable to plot",sort(names(df)[numeric_cols_vis]),
                             selected="Presenting WBC")
                 ),
               
               # Option 3: show barplot.
               conditionalPanel(
                 condition = "input.plotType == 'bar'",
                 selectInput("bar_var","Category to plot",sort(names(df)[factor_cols_vis]),
                             selected="WHO Category")
                 ),
               
               # Option 4: show 1D-scatter+boxplot.
               conditionalPanel(
                 condition = "input.plotType == 'scatbox'",
                 selectInput("scatbox_cat","Category to plot",sort(names(df)[factor_cols_vis]),
                             selected="WHO Category"),
                 selectInput("scatbox_num","Numeric to plot",sort(names(df)[numeric_cols_vis]),
                             selected="Days to Engraft P0")
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
                 selectInput("ctable_plot_var1","First category",sort(names(df)[factor_cols_vis]),
                             selected = "WHO Category"),
                 selectInput("ctable_plot_var2","Second category",sort(names(df)[factor_cols_vis]),
                             selected = "Latest Passage Banked")
                )
             ),
            column(width=8,
                ({ 
#                   if (input$plotType == 'ctable') {
#                      tableOutput("table_various")
#                   } else 
                    plotOutput("plot_various") 
                })
             )
            
          )
        )
        ,fluid=TRUE
      )
    ),
    tabPanel("PDX Gene Expression",
      sidebarLayout(
        sidebarPanel(width=2,
          selectInput(
            "expType", "Graph type",
            c("Heatmap (2+ genes and samples)" = "heat", "Barplot (1 gene or sample)" = "bar"),
            selected="heat"
          ),
          # Graph Type 1: heatmap
          conditionalPanel(
            condition = "input.expType == 'heat'",
            selectInput(
              "geneInput", "Type of gene input",
              c("Individual genes" = "indiv", "Panels" = "panels"),
              selected="indiv"
            ),
            # option 1: Individual genes
            conditionalPanel(
              condition = "input.geneInput == 'indiv'",
              selectizeInput(inputId="rna_genes",label="Select genes",
                          choices=NULL,multiple=TRUE)
            ),
            
            # option 2: Gene lists
            conditionalPanel(
              condition = "input.geneInput == 'panels'",
              selectInput("rna_panel","Select gene panel list",names(genesets_list),
                        selected="HemoSeq_v2")
            ),
            selectInput(
              "sampleInput","Type of sample input",
              c("All samples" = "all","Click rows in Database Explorer" = "click"),
              selected="all"
            )
          ),
          # Graph Type 2: barplot
          conditionalPanel(
            condition = "input.expType == 'bar'",
            selectInput(
              "across_bar", "Data type",
              c("One gene, many samples" = "samples","One sample, many genes" = "gene_set"),
              selected="samples"
            ),
            # option 1: Samples
            conditionalPanel(
              condition = "input.across_bar == 'samples'",
              selectizeInput(inputId="bar_gene",label="Enter/select gene name",
                             choices=NULL,multiple=FALSE)
            ),
            
            # option 2: Gene set
            conditionalPanel(
              condition = "input.across_bar == 'gene_set'",
              selectInput("bar_rna_panel","Select gene panel list",names(genesets_list),
                          selected="HemoSeq_v2"),
              selectizeInput(inputId="bar_rna_sample",label = "Enter/select sample name",
                             choices = colnames(rnamat_sub),multiple=FALSE,
                             selected = "AML12.20140429.pe")
            )
          )
        ),
        mainPanel(
          plotOutput("plot_rna",height = 1500,width=1300)
        )
      )
    ),
    tabPanel("PDX Mutations",
      sidebarLayout(
        sidebarPanel(width=2,
          selectInput(
            "oncop_gene_input", "Type of gene input",
            c("Gene sets" = "gene_sets","Individual genes" = "indiv"),
            selected="gene_sets"
          ),
          # option 1: Gene lists
          conditionalPanel(
            condition = "input.oncop_gene_input == 'gene_sets'",
            selectInput(
              "oncop_gene_set", "Gene set optimized for:",
              c("all types" = "all", "AML" = "AML", "B-ALL" = "BA"),
              selected="all"
            )
          ),
          # option 2: Individual genes         ### TODO: make sure this hits server.R ###
          conditionalPanel(
            condition = "input.oncop_gene_input == 'indiv'",
            selectizeInput(inputId="oncop_genes",label="Select genes",
                           choices=NULL,multiple=TRUE)
          ),
          selectInput(
            "oncop_sample_input","Type of sample input",
            c("Cancer subtype" = "subtype","Click rows in Database Explorer" = "click"), 
            #TODO: implement 'click' in server.R #TODO: maybe delete -- might be done.
            selected="subtype"
          ),
          conditionalPanel(
            condition = "input.oncop_sample_input == 'subtype'",
            selectInput(
              "oncop_sample_type", "Type of disease samples to show",
              c("all types" = "all", "AML" = "AML", "B-ALL" = "BA",
              "T-ALL" = "TA"),
              selected="all"
            )
          )
        ),
        mainPanel(
          plotOutput("plot_oncoprint",height = 800,width=1300)
        )
      )
    ),
    tabPanel("Contingency Table",
      sidebarLayout(
        sidebarPanel(
          selectInput("ctable_numcats","Number of contingency table categories",
                      1:2,selected=2),
          selectInput("tablevar1","First category",sort(names(df)[factor_cols_vis]),
                      selected = "WHO Category"),
          conditionalPanel(
            condition = "input.ctable_numcats > 1",
            selectInput("tablevar2","Second category",sort(names(df)[factor_cols_vis]),
                        selected = "Latest Passage Banked")
          )
        ),
        mainPanel(
          h4("Contingency table. Updates based on filtering in Database Explorer."),
          tableOutput("table_various")
        )
      )
    ),  # works
    tabPanel("Line Report",
      fluidPage(
        fluidRow(
          # h4("Select a line in the Database Explorer to see a report here"),
          DT::dataTableOutput(outputId="line_report")
          # TODO: insert selected report format
          # TODO: decide on report format and PDF rendering option.
        )
      )
    ),
    tabPanel("Glossary",
      fluidPage(
        fluidRow(
          column(width = 12,
            DT::dataTableOutput(outputId="glossary")
          )
        )
      )
    )
  )  
)