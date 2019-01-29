# ui.R #

# find indices of transitions from obligate visible to conditionally visible to obligate invisible columns
  # TODO: discuss with Mark that I think we should have a fourth section, thus:
    # 1. obligate visible from the start
    # 2. option to click to make visible
    # 3. option to type in box to make visible (genes)
    # 4. never visible (should we even have these in the dataframe we read in?)

# Define the overall UI
shinyUI(
  navbarPage(#title="Public Repository of Xenografts",
    title=span(img(src="PRoXe-Blue-Small.png",width="90px",
      style="float:left; padding-right:5px; display:inline; position:relative; top:-10px"),
      "Public Repository of Xenografts"),
    id="mainNavBar",
    windowTitle="PRoXe: Public Repository of Xenografts",
    
    navbarMenu("Liquid Tumors", # UIs are defined in ui/liquid_tumors/
               
               ui_liquid_tumors_database_explorer(),
               ui_liquid_tumors_pdx_gene_expression(),
               ui_liquid_tumors_pdx_mutations(),
               ui_liquid_tumors_contingency_table(),
               ui_liquid_tumors_line_report(),
               ui_liquid_tumors_glossary(),
               ui_liquid_tumors_methods()
               
    ),
    
    navbarMenu("Solid Tumors",
      # tabPanel("Solid Tumors (beta)",
      tabPanel("Database Explorer",
        h1("Solid Tumor Database Explorer"),
        br(),
        tags$div(
          style="margin:15px;",
          # top row of app
          fluidRow(
            column(6,
              # dropdown buttons - left side
              id="dt-col-select-solid",
              tags$div(
                style="display: flex;",
                tags$h4("First, select columns to show:",style="margin-right: 15px; font-weight: bold;"),
                mydropdownButton("administrative",solid_meta2,condVis_ind_solid,button_group="solid",solid),
                mydropdownButton("tumor",solid_meta2,condVis_ind_solid,button_group="solid",solid),
                mydropdownButton("pdx",solid_meta2,condVis_ind_solid,button_group="solid",solid)
              )
            ),
            column(6,
              # links, buttons on right side
              id="email-request",
              p(
                a("Email us",href="mailto:proxe.feedback@gmail.com?Subject=PRoXe%20feedback",target="_top"),
                " with questions.",
                actionButton("Request_link_solid","Request lines",icon("arrow-circle-o-right"),
                  class="btn btn-primary"), # style="background-color: #1486ba; color: #fff; border-color: #2e6da4"
                align="right"
              )
            )
          ),
          # 1. Display data table
          fluidRow(
            DT::dataTableOutput(outputId="solid_table")
          ),
          # 2. Create download button below table
          fluidRow(h4(" ")), # just an empty space.
          fluidRow(
            p(class = 'text-center', downloadButton('solid_download_filtered', 'Download Filtered Data'))
          ),
          # 3. Allow user to choose from a number of visualizations and parameters.
          fluidRow(
            sidebarPanel(
              selectInput(
                "solid_plotType", "Plot Type",
                c(Histogram = "hist", Scatter = "scatter", Bar = "bar", "1D Scatter-Box" = "scatbox",
                  "2D Contingency Table" = "ctable_plot"),
                selected = "scatbox"
              ),
              
              # Option 1: show histogram.
              conditionalPanel(
                condition = "input.solid_plotType == 'hist'",
                selectInput("solid_hist_var","Variable to plot",
                  sort(names(solid)[solid_numeric_cols_vis]),selected=NULL), # TODO: change to default value when numeric columns are added to solid.
                selectInput(
                  "solid_hist_log","Scaling",
                  c(linear=FALSE,log10=TRUE)
                ),
                selectInput(
                  "solid_breaks", "Breaks",
                  c("[Custom]" = "custom", "Sturges",
                    "Scott","Freedman-Diaconis")
                ),
                # Only show this sub-panel if Custom is selected
                conditionalPanel(
                  condition = "input.solid_breaks == 'custom'",
                  sliderInput("solid_breakCount", "Break Count", min=1, max=100, value=25)
                  
                )
                
              ),
              
              # Option 2: show scatterplot.
              conditionalPanel(
                condition = "input.solid_plotType == 'scatter'",
                selectInput("solid_scat_var_x","X variable to plot",sort(names(solid)[solid_numeric_cols_vis]),
                  selected=NULL),
                selectInput("solid_scat_var_y","Y variable to plot",sort(names(solid)[solid_numeric_cols_vis]),
                  selected=NULL)
              ),
              
              # Option 3: show barplot.
              conditionalPanel(
                condition = "input.solid_plotType == 'bar'",
                selectInput("solid_bar_var","Category to plot",sort(names(solid)[solid_factor_cols_vis]),
                  selected="COSMIC Primary Site")
              ),
              
              # Option 4: show 1D-scatter+boxplot.
              conditionalPanel(
                condition = "input.solid_plotType == 'scatbox'",
                selectInput("solid_scatbox_cat","Category to plot",sort(names(solid)[solid_factor_cols_vis]),
                  selected="COSMIC Primary Site"),
                selectInput("solid_scatbox_num","Numeric to plot",sort(names(solid)[solid_numeric_cols_vis]),
                  selected="PDX_Mutations_Count"),
                selectInput("solid_scatbox_log","Numeric axis scaling",c("linear","log"),
                  selected="log")
              ),
              #                # Option 5: contingency table of categories
              #                conditionalPanel(
              #                  condition = "input.plotType == 'ctable'",
              #                  selectInput("tablevarA","First category",sort(names(solid)[solid_numeric_cols_vis]),
              #                              selected = "WHO Category"),
              #                  selectInput("tablevarB","Second category",sort(names(solid)[solid_numeric_cols_vis]),
              #                              selected = "Latest Passage Banked")
              #                 ),
              # Option 6: mosaic plot of contingency table.
              conditionalPanel(
                condition = "input.solid_plotType == 'ctable_plot'",
                selectInput("solid_ctable_plot_var1","First category",sort(names(solid)[solid_factor_cols_vis]),
                  selected = "COSMIC Type"),
                selectInput("solid_ctable_plot_var2","Second category",sort(names(solid)[solid_factor_cols_vis]),
                  selected = "COSMIC Primary Site")
              )
            ),
            column(width=8,
              ({ 
                #                   if (input$plotType == 'ctable') {
                #                      tableOutput("table_various")
                #                   } else 
                plotOutput("solid_plot_various") 
              })
            ) # end plot column
          ) # end fluidRow for custom plotting
        )
      ),
      tabPanel("PDX Gene Expression",
        h1("Solid Tumor PDX Gene Expression"),
        sidebarLayout(
          sidebarPanel(width=3,
            # log(RPKM) or Z-score
            radioButtons(
              "z_log_solid", "Type of data to plot",
              c("Z-score" = "z","Expression (log2)" = "log","Expression (linear)" = "lin"),
              selected="z"
            ),
            # graph type
            radioButtons(
              "expType_solid", "Graph type",
              c("Heatmap (2+ genes and samples)" = "heat", "Barplot (1 gene or sample)" = "bar"),
              selected="heat"
            ),
            # Graph Type 1: heatmap
            conditionalPanel(
              condition = "input.expType_solid == 'heat'",
              radioButtons(
                "geneInput_solid", "Type of gene input",
                c("Individual genes" = "indiv", "Panels" = "panels"),
                selected="indiv"
              ),
              # option 1: Individual genes
              conditionalPanel(
                condition = "input.geneInput_solid == 'indiv'",
                selectizeInput(inputId="rna_genes_solid",label="Select genes",
                            choices=NULL,multiple=TRUE)
              ),
              
              # option 2: Gene lists
              conditionalPanel(
                condition = "input.geneInput_solid == 'panels'",
                selectInput("rna_panel_solid","Select gene panel list","OncoPanel",
                          selected="OncoPanel")
              ),
              radioButtons(
                "sampleInput_solid","Type of sample input",
                c("All samples" = "all","Click rows in Database Explorer" = "click"),
                selected="all"
              ),
              # filter for sample categories
              conditionalPanel(
                condition = "input.sampleInput_solid == 'all'",
                selectizeInput(inputId="COSMIC_Type_solid",
                  label = "Select COSMIC Type to show:",
                  choices = levels(solid$`COSMIC Type`),
                  multiple=TRUE,
                  selected = c("carcinoma","malignant_melanoma","sarcoma")
                ),
                selectizeInput(inputId="COSMIC_Subtype_solid",
                  label = "Select COSMIC Subtype to show:",
                  choices = levels(solid$`COSMIC Subtype`),
                  multiple=TRUE,
                  selected = c("ductal_carcinoma","adenocarcinoma","NS","squamous_cell_carcinoma")
                )
              )
            ),
            # Graph Type 2: barplot
            conditionalPanel(
              condition = "input.expType_solid == 'bar'",
              radioButtons(
                "across_bar_solid", "Data type",
                c("One gene, many samples" = "samples","One sample, many genes" = "gene_set"),
                selected="samples"
              ),
              # option 1: Samples
              conditionalPanel(
                condition = "input.across_bar_solid == 'samples'",
                selectizeInput(inputId="bar_gene_solid",label="Enter/select gene name",
                               choices=NULL,multiple=FALSE)
              ),
              
              # option 2: Gene set
              conditionalPanel(
                condition = "input.across_bar_solid == 'gene_set'",
                selectInput("bar_rna_panel_solid","Select gene panel list","OncoPanel",
                            selected="OncoPanel"),
                selectizeInput(inputId="bar_rna_sample_solid",label = "Enter/select sample name",
                               choices = colnames(gao_rna),multiple=FALSE,
                               selected = "NIBR-1004")
              )
            )
          ),
          mainPanel(
            plotOutput("plot_rna_solid",height = 800,width=1300)
          )
        )
      ),
      tabPanel("Glossary",
        h1("Solid Tumor Glossary"),
          column(width = 12,
            DT::dataTableOutput(outputId="solid_glossary")
          )
      )
    ), # end navbarMenu - Solid Tumors
    
    navbarMenu("More",
      tabPanel("Line Request/Pricing",
        h1("Line Request / Pricing"),
        column(width = 8,
          h2("Request process:"),
          img(src='line_request_user2.png', align = "left", width = "100%"),
          br(),br(),br(),br(),br(),br(),br(),
          h2("Pricing:"),
          dataTableOutput("pricing"),
          br(),
          p("Note: Prices increased on 9/1/17 to cover costs. All orders placed before this date will be honored at the original price."),
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
            "General information relevant to PDX acquisition and use is available on PRoXe.  Each line is annotated with available demographic, pathologic and genomic information, as well as details like the time from injection to engraftment.  Protocols for a variety of PDX approaches, including expansion and subrenal capsule implantation are available in the ",actionLink("Methods_link","Methods")," tab.  We provide consulting to assist Investigators, but this is intended to focus on the selection and use of ",tags$i("individual")," PDXs. Note that billing is processed through the DFCI iLab Solutions platform, and requests are fulfilled by the DFCI Leukemia / Lymphoma Xenograft (LLX) core facility."),
          p(strong("Note:"),"Inventory last updated in PRoXe",inv_upDate,"."),
          br(),
          a(
            HTML(
              "<button type='button' class='btn btn-primary' style='border-radius: 4px;
                 padding-left: 20px; padding-right: 20px; font-size: x-large;'>
                Click here to request lines
              </button>"
            ),
            href="https://docs.google.com/forms/d/1RiQU4ABOWssH6vzy24jhdn6qhIjDcSprr6jiC1pLpQQ/viewform",target="_blank"
          ),
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
            tags$a("Townsend et al. Cancer Cell 2016",href="http://www.cell.com/cancer-cell/fulltext/S1535-6108(16)30268-9",target="_blank"),
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
          p(tags$b("JAX distribution."),"A subset of AML models are now distributed through the Jackson Laboratory (JAX), where you can purchase vials of splenocytes (academic customers) or engrafted NSG-SGM3 mice (industry customers). These models are clearly marked under the ‘Distribution Permissions’ column in Database Explorer. For more information please go to this page where you can request a quote and see additional model information. We can no longer distribute these 7 AML models directly and customers must contact JAX for further information."),
          p("The JAX model names from our collection are as follows:"),
          HTML("<table class=\"table\"><tr>
                  <th>JAX Tumor #</th>
                  <th>DFCI Tumor #</th>
                </tr>
                <tr>
                  <th>J000106566</th>
                  <th>DFAM-32000-V1</th>
                </tr>
                <tr>
                  <th>J000106569</th>
                  <th>DFAM-61345-V1</th>
                </tr>
                <tr>
                  <th>J000106132</th>
                  <th>DFAM-22359-V1</th>
                </tr>
                <tr>
                  <th>J000106134</th>
                  <th>DFAM-61786-V2</th>
                </tr>
                <tr>
                  <th>J000106124</th>
                  <th>DFAM-15354-V2</th>
                </tr>
                <tr>
                  <th>J000106565</th>
                  <th>DFAM-16835-V1</th>
                </tr>
                <tr>
                  <th>J000106143</th>
                  <th>DFAM-84910-V1</th>
                </tr>
                </table>"),
          p(tags$b("Billing through iLab."),
            "Upon your first PRoXe request you will receive an introductory “Welcome to Leukemia/Lymphoma Xenograft Core” email, which provides basic information about registering for our Core in iLab. The LLX Core’s purpose is to expand and distribute our PDX models. Internal DFCI customers may already have an iLab account, in which case you don’t need to register. Internal and external investigators without an iLab account will need to register, which only takes a few minutes. After registration you will receive a cost projection from the Core covering fees for PDX vials, shipping costs, and consulting costs (if applicable). At that time you will need to both confirm that you accept the projected cost and provide payment information. For internal customers please provide a DFCI-approved project cost center #. For external customers a PO # is preferred; if you must use a credit card please contact ",
            tags$a("Kristen Jones",href="mailto:kristenl_jones@dfci.harvard.edu?Subject=PRoXe%20credit%20card%20use"),
            " and she will forward this request to purchasing to process. Charges can be added or removed from a billing event up until final billing is processed. The initial communication is simply to collect payment information and agree upon an initial cost. You will not be charged until the Material Transfer Agreement is fully executed (if applicable) and the vials are shipped."),
          p(tags$b("Consulting."),
            "We offer consulting in 30-minute increments to guide the selection and use of PDXs.  A consulting appointment is required to address any question(s) that require more than 5 minutes to address by e-mail or that clearly require a telephone interaction.  Although we would like to provide this service free-of-charge, doing so would compromise our sustainability.  Please do not be offended if we require a formal, billed appointment.  Similarly, if the consultation extends beyond the initial allotted appointment, please do not be offended that this will require additional charges. If you are interested in a consulting appointment, please ",
            tags$a("contact us",href="mailto:proxe.feedback@gmail.com?Subject=PRoXe%20consulting"),
            " so that we can better understand your specific needs and ensure that the correct expertise is available.  Before a consultation can occur, billing information must be available (see "
            ,tags$i("Billing through iLab"),
            ")."
          ),
          p(tags$b("App web development."),"PRoXe is written mainly in the R programming language by Scott Kallgren (Twitter: @scottkall). As an open-source project, 
            its code and latest updates can be viewed",
            a("in Scott's GitHub repository.",href="https://github.com/scottkall/proxe",target="_blank")),
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
          p("Kristen Jones",br(),
            "Director, Leukemia/Lymphoma Xenograft Core Facility",br(),
            "Dana-Farber Cancer Institute",br(),
            "Dana Building, Room 512",br(),
            "450 Brookline Avenue",br(),
            "Boston, MA 02215",br(),
            a("kristenl_jones@dfci.harvard.edu",href="mailto:kristenl_jones@dfci.harvard.edu?Subject=PRoXe",target="_top")),
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
      ),
      tabPanel("FAQ",
        h1("Frequently Asked Questions"),
        # TODO: ideally, eventually make a TOC to each item at the top, linking via anchors or shiny equivalent.
        column(width = 10,
          uiOutput("FAQ")
        )
      )
      
    )
    # navbarPage options
    ,position="fixed-top",collapsible=TRUE
  )
)

