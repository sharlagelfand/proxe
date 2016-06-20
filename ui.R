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
  navbarPage(#"Public Repository of Xenografts",
    id="mainNavBar",
    span(img(src="PRoXe-Blue-Small.png",width="90px",
      style="float:left; padding-right:5px; display:inline; position:relative; top:-10px"),
    "Public Repository of Xenografts"),
    windowTitle="PRoXe: Public Repository of Xenografts",
#     navbarMenu("Database Explorer",
#     tabPanel("Hematological",
    tabPanel("Database Explorer",
      checkboxInput("hide_sidebar","Hide sidebar",FALSE),
      # customHeaderPanel("Logo"),
      # Left sidebar for selecting which columns to show
      sidebarLayout(
        conditionalPanel(condition = "input.hide_sidebar == false",
        sidebarPanel(
          # adjusting sidebar width and text manually with CSS
          tags$head(
            # tags$style(type='text/css', ".col-sm-8 { margin-left: 10px;}"),
            # tags$style(type='text/css', ".col-sm-3 { margin-right: -20px;}"),
            # tags$link(rel="shortcut icon", href="favicon.ico"),
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
              }"),
            tags$style(type='text/css', "
              .container-fluid {
                padding-top:4em
              }"),
            tags$style(type='text/css', "
              #hidebox {
                display:inline;
              }")
#,
            # for adding ellipsis to ColVis
  #           tags$style(type='text/css', "
  #             ul.ColVis_collection li span {
  #               overflow: hidden;
  #               text-overflow: ellipsis;
  #               width: 90%;
  #             }")
            
              
          ),
          h4(strong("First, select columns to show:")),
          actionButton("selectall", label="(Un)select all"),
          checkboxGroupInput("show_vars",
                             NULL,
                             names(df[1:(obInvisRet_ind-1)]),
                             selected=names(df)[1:(condVis_ind-1)]
                             )
          ,width=3 # used to be width 4. 3 works better for full screenwidth. Would prefer fixed to longest name length. TODO.
          )
        ),
        # Right panel for showing table with subsettable columns, alphabetized.
        mainPanel(
          # 0. Email us (temporary)
          fluidRow(
            # p(checkboxInput("hide_sidebar","Hide sidebar",FALSE),id="hidebox"),
            p(
              a("Email us",href="mailto:proxe.feedback@gmail.com?Subject=PRoXe%20feedback",target="_top"),
              " with questions.",
              actionButton("Request_link","Request lines"),
              align="right"
            )
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
#       tabPanel("Solid",
#         h1("Solid"),
#         DT::dataTableOutput(outputId="solid_table")
#       )
#       ),
      navbarMenu("PDX Molecular", #new
      tabPanel("PDX Gene Expression",
        h1("PDX Gene Expression"),
        sidebarLayout(
          sidebarPanel(width=3,
            radioButtons(
              "expType", "Graph type",
              c("Heatmap (2+ genes and samples)" = "heat", "Barplot (1 gene or sample)" = "bar"),
              selected="heat"
            ),
            # Graph Type 1: heatmap
            conditionalPanel(
              condition = "input.expType == 'heat'",
              radioButtons(
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
              radioButtons(
                "sampleInput","Type of sample input",
                c("All samples" = "all","Click rows in Database Explorer" = "click"),
                selected="all"
              ),
              helpText("Note: not all rows selected in Database Explorer will have associated RNA-seq data.")
            ),
            # Graph Type 2: barplot
            conditionalPanel(
              condition = "input.expType == 'bar'",
              radioButtons(
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
        h1("PDX Mutations"),
        sidebarLayout(
          sidebarPanel(width=3,
            radioButtons(
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
            radioButtons(
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
      )
    ),
    tabPanel("Contingency Table",
      sidebarLayout(
        sidebarPanel(
          radioButtons("ctable_numcats","Number of contingency table categories",
                       1:2,selected=2),
          # selectInput("ctable_numcats","Number of contingency table categories",
                      # 1:2,selected=2),
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
      h1("Line Report"),
      basicPage(
        # h4("Select a line in the Database Explorer to see a report here"),
        radioButtons(
          "line_report_input_type","Method for selecting line to show here:",
          c("Choose from drop-down menu" = "dropdown","Click a row in Database Explorer" = "click"),
          selected="dropdown"
        ),
        conditionalPanel(
          condition = "input.line_report_input_type == 'dropdown'",
          selectInput("line_report_name","Type or select a line name below to see a full report",
            c(df[,"PDX Name"],NULL),selected = "DFTL-85005-R1")
        ),
        DT::dataTableOutput(outputId="line_report"),
        h3("Flow Cytometry:"),
        uiOutput("line_report_FC"),
        h3("Immunohistochemistry:"),
        uiOutput("line_report_IHC"),
        h3("Pathology Report:"),
        uiOutput("line_report_Path")
      )
    ),
    navbarMenu("More",
      tabPanel("Glossary",
      h1("Glossary"),
          column(width = 12,
            DT::dataTableOutput(outputId="glossary")
          )
      ),
      tabPanel("Methods",
        h1("Methods"),
        column(width = 10,
          # note these (ABCD) are Mark's ideal ordering:
          # helpText("A) Xenografting Techniques: with subheadings Tail Vein Injection, Subrenal Capsule Implantation, +/- Subcutaneous Implantation."),
          # helpText("B) Xenografting Cell Doses: with subheadings For In Vivo Expansion, For In Vivo Treatment Studies"),
          # helpText("C) Monitoring Xenografted Animals: with subheadings for Cell Banking, for Initiation of Treatment"),
          # helpText("D) Banking Xenografted Cells: with subheadings Animal Euthanasia, Enriching Malignant Cells from Peripheral Blood, Enriching Malignant Cells from Bone Marrow, Enriching Malignant Cells from Spleen, and Enriching Malignant Cells from Lymph Nodes or Solid Tumors"),
          tags$h2("PDX methods"),
          uiOutput("PDX_methods"),
          tags$h2("Renal capsule implantation method"),
          uiOutput("Renal_methods")
        )
      ),
      tabPanel("Line Request/Pricing",
        h1("Line Request / Pricing"),
        column(width = 8,
          h2("Request process:"),
          img(src='line_request_user2.png', align = "left", width = "100%"),
          br(),br(),br(),br(),br(),br(),br(),
          h2("Pricing:"),
          dataTableOutput("pricing"),
          br(),
          p(strong("Shipping:"),"For domestic shipments, shipping fees are calculated by shipment zone (zone 2-16, based on FedEx Standard Overnight).
            For international shipments, shipping fees are calculated using FedEx Rate Tools prior to submitting a billing estimate (based on FedEx International First rates.)"),
          p(strong("Vials:"),"We can offer one vial per line. If more are required, please",
            a("contact us",href="mailto:proxe.feedback@gmail.com?Subject=PRoXe%20extra%20vial%20request"),
            "to discuss."),
          p(strong("Consulting:"),"We are happy to discuss scientific details to help you order the appropriate line.  
            For simple inquires, this is free, but for projects that will take more than 5 minutes, 
            after agreement with you we will charge the consulting rate above."),
          br(),
          p(tags$b("Please read the following before requesting lines. "),
            "General information relevant to PDX acquisition and use is available on PRoXe.  Each line is annotated with available demographic, pathologic and genomic information, as well as details like the time from injection to engraftment.  Protocols for a variety of PDX approaches, including expansion and subrenal capsule implantation are available in the Methods tab.  We provide consulting to assist Investigators, but this is intended to focus on the selection and use of ",tags$i("individual")," PDXs. Note that billing is processed through the DFCI iLab Solutions platform, and requests are fulfilled by the DFCI Leukemia / Lymphoma Xenograft (LLX) core facility."),
          br(),
          h2(a("Click here to request lines.",
            href="https://docs.google.com/forms/d/1RiQU4ABOWssH6vzy24jhdn6qhIjDcSprr6jiC1pLpQQ/viewform",target="_blank")),
          br(),br(),br()
          
        )
      ),
      tabPanel("iLab Billing Platform",
        h1(a("iLab Solutions",
          href="https://dfci.ilabsolutions.com/service_center/show_external/7625?name=leukemia-lymphoma-xenograft-core-llx",
          target="_blank"),"Billing Platform"),
        h2("Manual"),
        uiOutput("iLab_manual")
        
      ),
      tabPanel("About",
        h1("About PRoXe"),
        column(width = 8,
          p("Open repositories of cell lines, plasmids, and transgenic mouse models have been essential for advancing discovery and preclinical translation in cancer.  PRoXe is an open-source website designed to disseminate information relevant to patient-derived xenografts (PDXs), and particularly PDXs generated from patients with leukemia or lymphoma.   The site and repository were developed by the ",
            tags$a("Weinstock laboratory",href="http://weinstock.dfci.harvard.edu/",target="_blank"),
            " through philanthropic funds and published in April 2016 (",
            tags$a("Townsend et al. Cancer Cell",href="http://authors.elsevier.com/a/1SsLj5TA51E0Td",target="_blank"),
            # tags$a("Townsend et al. Cancer Cell",href="http://www.cell.com/cancer-cell/fulltext/S1535-6108(16)30090-3",target="_blank"),
            ").  A subset of PDXs hosted on PRoXe are available for distribution through the Leukemia and Lymphoma Xenograft (LLX) core laboratory we established at ",
            tags$a("Dana-Farber Cancer Institute (DFCI)",href="http://www.dana-farber.org/",target="_blank"),
            "."),
          p(tags$b("Limitations in line availability."),
            " Unfortunately, not all PDXs with information hosted on PRoXe can be distributed by the LLX Core to academic and/or industry investigators.  In some cases, limitations in consent from the patient who provided the sample preclude distribution.  In most cases, limited distribution is due to restrictions placed by outside institutions (through their Technology Transfer offices) on the ability of DFCI to distribute PDXs derived from their patients to academic and/or industry investigators, to utilize those specimens for research outside of the ",
            tags$a("Weinstock laboratory",href="http://weinstock.dfci.harvard.edu/",target="_blank"),
            ", or to provide them for industry-sponsored research.  We are actively encouraging outside institutions to adopt the open-source model and we ask that you convey the importance of open access to your institution.  For some lines that are currently restricted, we can provide contact information for investigators at centers outside of DFCI who may be able to facilitate access through direct MTAs."),
          p(tags$b("Line distribution."),
            "The LLX Core is operated as a cost-neutral core according to NCI rules, with charges modified over time to ensure that the core maintains financial sustainability (i.e., breaks even) but does not generate profit.  As a result, prices may change over time inversely proportional to volume.  Based on the current costs for cell lines and mouse models, we believe that our current pricing for PDXs will allow academic laboratories to acquire large numbers of PDXs within their disease(s) of interest to support investigation across disease heterogeneity."),
          p("After gaining access, investigators are free to review line- and disease-level data and can find links to download raw genomics data.  Investigators can then request individual lines in multiple formats by completing a specimen request form.  We ask for a very brief description of the studies planned for these lines solely to track usage and this information is included in the MTA.  We are not selecting investigators, projects, areas of interest or other criteria for distribution.  Available vials are provided on a first come, first served basis."),
          p("PDX lines are currently available in the following formats:"),
          tags$ol(
            tags$li(tags$i("Single"),"vial of viably frozen cells"),
            tags$li(tags$i("Multiple"),"vials of viably frozen cells - ",
              tags$b("Under most circumstances, we are currently unable to immediately provide multiple frozen vials of individual lines due to a lack of adequate frozen stocks."),
              "Thus, providing multiple vials may require expansion within 1 or more mice, which we can perform to meet individual requests.  Expansion in a single mouse can generate between 5-500 million cells depending on the line."
              ),
            tags$li("Fresh cells obtained from one more engrafted mice – this will require expansion within 1 or more mice.  Expansion in a single mouse can generate between 5-500 million cells depending on the line.")
          ),
          p("Once the request is received and approved, we will generate a Material Transfer Agreement (MTA) request from the ",
            tags$a("DFCI Office of Innovation",href="http://www.dana-farber.org/Research/Technology-Transfer.aspx",target="_blank"),
            " that is forwarded to the Institutional Officer designated in your request.  Because of the very large number of requests, the MTA is non-negotiable (like MTAs with Addgene or ATCC) and has specific restrictions:"),
          tags$ol(
            tags$li("Academic research laboratories - like cell lines acquired from non-profit resources (e.g. ATCC or RIKEN), the individual laboratory that acquires the vial(s) and/or fresh cells has rights to expand, modify and bank.  They DO NOT have rights to transfer these products or any derivatives generated from it to any other laboratory, academic institution or industrial partner.  The line and/or fresh cells may be used for industry-sponsored research performed in direct collaboration with the purchasing laboratory the line within the purchasing laboratory or a designated core laboratory at that academic institution.  In an effort to maximize the use of these lines, DFCI does not retain intellectual property on inventions made with these PDXs or their derivatives, but does not allow for distribution or transfer."),
            tags$li("Industry – vials and fresh cells can be shipped to industry investigators for one time use only, either in vitro or in vivo.  There are no rights to modify the lines, to bank cells after in vitro or in vivo passage, to expand the lines in any way or to transfer the lines to either academic or industry investigators.  Industry investigators wishing to discuss more extensive in vivo studies or in-license of lines should ",
              tags$a("contact PRoXe",href="mailto:proxe.feedback@gmail.com?Subject=PRoXe%20corporate%20rates"),
              "."),
            tags$li("Other potential users (e.g. academic core laboratories, educators) – we do not currently have template agreements to share with other potential users but would like to pursue this on an individual basis.  Please ",
              tags$a("contact PRoXe",href="mailto:proxe.feedback@gmail.com?Subject=PRoXe%20academic%20sharing"),
              " to discuss any ideas or possible sharing models.")
          ),
          p("Once the MTA is approved by both institutions and billing information is confirmed (see Billing through iLab below), plans for shipping will be finalized by email between the requesting Investigator and LLX Core personnel.  Tracking information will be provided and billing will not occur until shipping.  We will arrange for shipping through FedEx or DHL."),
          p(tags$b("Billing through iLab."),
            "Upon your first PRoXe request you will receive an introductory “Welcome to Leukemia/Lymphoma Xenograft Core” email, which provides basic information about registering for our Core in iLab. The LLX Core’s purpose is to expand and distribute our PDX models. Internal DFCI customers may already have an iLab account, in which case you don’t need to register. Internal and external investigators without an iLab account will need to register, which only takes a few minutes. After registration you will receive a cost projection from the Core covering fees for PDX vials, shipping costs, and consulting costs (if applicable). At that time you will need to both confirm that you accept the projected cost and provide payment information. For internal customers please provide a DFCI-approved project cost center #. For external customers a PO # is preferred; if you must use a credit card please contact ",
            tags$a("Amanda Christie",href="mailto:amandal_christie@dfci.harvard.edu?Subject=PRoXe%20credit%20card%20use"),
            " and she will forward this request to purchasing to process. Charges can be added or removed from a billing event up until final billing is processed. The initial communication is simply to collect payment information and agree upon an initial cost. You will not be charged until the Material Transfer Agreement is fully executed (if applicable) and the vials are shipped."),
          p(tags$b("Consulting."),
            "We offer consulting in 30-minute increments to guide the selection and use of PDXs.  A consulting appointment is required to address any question(s) that require more than 5 minutes to address by e-mail or that clearly require a telephone interaction.  Although we would like to provide this service free-of-charge, doing so would compromise our sustainability.  Please do not be offended if we require a formal, billed appointment.  Similarly, if the consultation extends beyond the initial allotted appointment, please do not be offended that this will require additional charges. If you are interested in a consulting appointment, please ",
            tags$a("contact us",href="mailto:proxe.feedback@gmail.com?Subject=PRoXe%20consulting"),
            " so that we can better understand your specific needs and ensure that the correct expertise is available.  Before a consultation can occur, billing information must be available (see "
            ,tags$i("Billing through iLab"),
            ")."
          ),
          p(tags$b("App web development."),"PRoXe is written mainly in the R programming language. As an open-source project, 
            its code and latest updates can be viewed",
            a("in this Bitbucket repository.",href="https://bitbucket.org/scottkall/proxe",target="_blank")),
          HTML("<p>If you have ideas for ways to improve PRoXe, please 
                <a href=\"mailto:proxe.feedback@gmail.com?Subject=PRoXe%20feedback\" target=\"_top\">tell us</a> 
            .</p>"),
          br(),
          p("David Weinstock, M.D.",br(),
            "PRoXe Faculty Sponsor",br(),
            "Associate Professor of Medicine, Harvard Medical School",br(),
            "450 Brookline Avenue",br(),
            "Dana Building, Room 510B",br(),
            "Boston, MA 02215",br(),
            # a("davidm_weinstock@dfci.harvard.edu",href="mailto:davidm_weinstock@dfci.harvard.edu?Subject=PRoXe"),br(),
            a("http://weinstock.dfci.harvard.edu",href="http://weinstock.dfci.harvard.edu",target="_blank")
          ),
          br(),
          p("Amanda Christie",br(),
            "Director, Leukemia/Lymphoma Xenograft Core Facility",br(),
            "Dana-Farber Cancer Institute",br(),
            "Dana Building, Room 512",br(),
            "450 Brookline Avenue",br(),
            "Boston, MA 02215"),
          br(),
          p("Prafulla Gokhale, Ph.D.",br(),
            "Head of Experimental Therapeutics Core",br(),
            "Belfer Center for Applied Cancer Science",br(),
            "27 Drydock Avenue, DD472",br(),
            "Boston, MA 02210",br(),
            # a("prafulla_gokhale@dfci.harvard.edu",href="mailto:prafulla_gokhale@dfci.harvard.edu?Subject=PRoXe"),
            a("http://belfercenter.dfci.harvard.edu",href="http://belfercenter.dfci.harvard.edu",target="_blank")
          ),br(),br()
        )
      )
    )
  ,position="fixed-top"
  )
)

