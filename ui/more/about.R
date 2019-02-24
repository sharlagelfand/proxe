ui_more_about <- function() {
  tabPanel(
    "About",
    h1("About PRoXe"),
    column(
      width = 8,
      p(
        "Open repositories of cell lines, plasmids, and transgenic mouse models have been essential for advancing discovery and preclinical translation in cancer.  PRoXe is an open-source website designed to disseminate information relevant to patient-derived xenografts (PDXs), and particularly PDXs generated from patients with leukemia or lymphoma.   The site and repository were developed by the ",
        tags$a("Weinstock laboratory", href = "http://weinstock.dfci.harvard.edu/", target = "_blank"),
        " through philanthropic funds and published in April 2016 (",
        tags$a("Townsend et al. Cancer Cell 2016", href = "http://www.cell.com/cancer-cell/fulltext/S1535-6108(16)30268-9", target = "_blank"),
        # tags$a("Townsend et al. Cancer Cell",href="http://www.cell.com/cancer-cell/fulltext/S1535-6108(16)30090-3",target="_blank"),
        ").  A subset of PDXs hosted on PRoXe are available for distribution through the Leukemia and Lymphoma Xenograft (LLX) core laboratory we established at ",
        tags$a("Dana-Farber Cancer Institute (DFCI)", href = "http://www.dana-farber.org/", target = "_blank"),
        "."
      ),
      p(
        tags$b("Limitations in line availability."),
        " Unfortunately, not all PDXs with information hosted on PRoXe can be distributed by the LLX Core to academic and/or industry investigators.  In some cases, limitations in consent from the patient who provided the sample preclude distribution.  In most cases, limited distribution is due to restrictions placed by outside institutions (through their Technology Transfer offices) on the ability of DFCI to distribute PDXs derived from their patients to academic and/or industry investigators, to utilize those specimens for research outside of the ",
        tags$a("Weinstock laboratory", href = "http://weinstock.dfci.harvard.edu/", target = "_blank"),
        ", or to provide them for industry-sponsored research.  We are actively encouraging outside institutions to adopt the open-source model and we ask that you convey the importance of open access to your institution.  For some lines that are currently restricted, we can provide contact information for investigators at centers outside of DFCI who may be able to facilitate access through direct MTAs."
      ),
      p(
        tags$b("Line distribution."),
        "The LLX Core is operated as a cost-neutral core according to NCI rules, with charges modified over time to ensure that the core maintains financial sustainability (i.e., breaks even) but does not generate profit.  As a result, prices may change over time inversely proportional to volume.  Based on the current costs for cell lines and mouse models, we believe that our current pricing for PDXs will allow academic laboratories to acquire large numbers of PDXs within their disease(s) of interest to support investigation across disease heterogeneity."
      ),
      p("After gaining access, investigators are free to review line- and disease-level data and can find links to download raw genomics data.  Investigators can then request individual lines in multiple formats by completing a specimen request form.  We ask for a very brief description of the studies planned for these lines solely to track usage and this information is included in the MTA.  We are not selecting investigators, projects, areas of interest or other criteria for distribution.  Available vials are provided on a first come, first served basis."),
      p("PDX lines are currently available in the following formats:"),
      tags$ol(
        tags$li(tags$i("Single"), "vial of viably frozen cells"),
        tags$li(
          tags$i("Multiple"), "vials of viably frozen cells - ",
          tags$b("Under most circumstances, we are currently unable to immediately provide multiple frozen vials of individual lines due to a lack of adequate frozen stocks."),
          "Thus, providing multiple vials may require expansion within 1 or more mice, which we can perform to meet individual requests.  Expansion in a single mouse can generate between 5-500 million cells depending on the line."
        ),
        tags$li("Fresh cells obtained from one more engrafted mice – this will require expansion within 1 or more mice.  Expansion in a single mouse can generate between 5-500 million cells depending on the line.")
      ),
      p(
        "Once the request is received and approved, we will generate a Material Transfer Agreement (MTA) request from the ",
        tags$a("DFCI Office of Innovation", href = "http://www.dana-farber.org/Research/Technology-Transfer.aspx", target = "_blank"),
        " that is forwarded to the Institutional Officer designated in your request.  Because of the very large number of requests, the MTA is non-negotiable (like MTAs with Addgene or ATCC) and has specific restrictions:"
      ),
      tags$ol(
        tags$li("Academic research laboratories - like cell lines acquired from non-profit resources (e.g. ATCC or RIKEN), the individual laboratory that acquires the vial(s) and/or fresh cells has rights to expand, modify and bank.  They DO NOT have rights to transfer these products or any derivatives generated from it to any other laboratory, academic institution or industrial partner.  The line and/or fresh cells may be used for industry-sponsored research performed in direct collaboration with the purchasing laboratory the line within the purchasing laboratory or a designated core laboratory at that academic institution.  In an effort to maximize the use of these lines, DFCI does not retain intellectual property on inventions made with these PDXs or their derivatives, but does not allow for distribution or transfer."),
        tags$li(
          "Industry – vials and fresh cells can be shipped to industry investigators for one time use only, either in vitro or in vivo.  There are no rights to modify the lines, to bank cells after in vitro or in vivo passage, to expand the lines in any way or to transfer the lines to either academic or industry investigators.  Industry investigators wishing to discuss more extensive in vivo studies or in-license of lines should ",
          tags$a("contact PRoXe", href = "mailto:proxe.feedback@gmail.com?Subject=PRoXe%20corporate%20rates"),
          "."
        ),
        tags$li(
          "Other potential users (e.g. academic core laboratories, educators) – we do not currently have template agreements to share with other potential users but would like to pursue this on an individual basis.  Please ",
          tags$a("contact PRoXe", href = "mailto:proxe.feedback@gmail.com?Subject=PRoXe%20academic%20sharing"),
          " to discuss any ideas or possible sharing models."
        )
      ),
      p("Once the MTA is approved by both institutions and billing information is confirmed (see Billing through iLab below), plans for shipping will be finalized by email between the requesting Investigator and LLX Core personnel.  Tracking information will be provided and billing will not occur until shipping.  We will arrange for shipping through FedEx or DHL."),
      p(tags$b("JAX distribution."), "A subset of AML models are now distributed through the Jackson Laboratory (JAX), where you can purchase vials of splenocytes (academic customers) or engrafted NSG-SGM3 mice (industry customers). These models are clearly marked under the ‘Distribution Permissions’ column in Database Explorer. For more information please go to this page where you can request a quote and see additional model information. We can no longer distribute these 7 AML models directly and customers must contact JAX for further information."),
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
      p(
        tags$b("Billing through iLab."),
        "Upon your first PRoXe request you will receive an introductory “Welcome to Leukemia/Lymphoma Xenograft Core” email, which provides basic information about registering for our Core in iLab. The LLX Core’s purpose is to expand and distribute our PDX models. Internal DFCI customers may already have an iLab account, in which case you don’t need to register. Internal and external investigators without an iLab account will need to register, which only takes a few minutes. After registration you will receive a cost projection from the Core covering fees for PDX vials, shipping costs, and consulting costs (if applicable). At that time you will need to both confirm that you accept the projected cost and provide payment information. For internal customers please provide a DFCI-approved project cost center #. For external customers a PO # is preferred; if you must use a credit card please contact ",
        tags$a("Kristen Jones", href = "mailto:kristenl_jones@dfci.harvard.edu?Subject=PRoXe%20credit%20card%20use"),
        " and she will forward this request to purchasing to process. Charges can be added or removed from a billing event up until final billing is processed. The initial communication is simply to collect payment information and agree upon an initial cost. You will not be charged until the Material Transfer Agreement is fully executed (if applicable) and the vials are shipped."
      ),
      p(
        tags$b("Consulting."),
        "We offer consulting in 30-minute increments to guide the selection and use of PDXs.  A consulting appointment is required to address any question(s) that require more than 5 minutes to address by e-mail or that clearly require a telephone interaction.  Although we would like to provide this service free-of-charge, doing so would compromise our sustainability.  Please do not be offended if we require a formal, billed appointment.  Similarly, if the consultation extends beyond the initial allotted appointment, please do not be offended that this will require additional charges. If you are interested in a consulting appointment, please ",
        tags$a("contact us", href = "mailto:proxe.feedback@gmail.com?Subject=PRoXe%20consulting"),
        " so that we can better understand your specific needs and ensure that the correct expertise is available.  Before a consultation can occur, billing information must be available (see ", tags$i("Billing through iLab"),
        ")."
      ),
      p(
        tags$b("App web development."), "PRoXe is written mainly in the R programming language by Scott Kallgren (Twitter: @scottkall). As an open-source project, 
            its code and latest updates can be viewed",
        a("in Scott's GitHub repository.", href = "https://github.com/scottkall/proxe", target = "_blank")
      ),
      HTML("<p>If you have ideas for ways to improve PRoXe, please 
                <a href=\"mailto:proxe.feedback@gmail.com?Subject=PRoXe%20feedback\" target=\"_top\">tell us</a> 
            .</p>"),
      br(),
      p(
        "David Weinstock, M.D.", br(),
        "PRoXe Faculty Sponsor", br(),
        "Associate Professor of Medicine, Harvard Medical School", br(),
        "450 Brookline Avenue", br(),
        "Dana Building, Room 510B", br(),
        "Boston, MA 02215", br(),
        # a("davidm_weinstock@dfci.harvard.edu",href="mailto:davidm_weinstock@dfci.harvard.edu?Subject=PRoXe"),br(),
        a("http://weinstock.dfci.harvard.edu", href = "http://weinstock.dfci.harvard.edu", target = "_blank")
      ),
      br(),
      p(
        "Kristen Jones", br(),
        "Director, Leukemia/Lymphoma Xenograft Core Facility", br(),
        "Dana-Farber Cancer Institute", br(),
        "Dana Building, Room 512", br(),
        "450 Brookline Avenue", br(),
        "Boston, MA 02215", br(),
        a("kristenl_jones@dfci.harvard.edu", href = "mailto:kristenl_jones@dfci.harvard.edu?Subject=PRoXe", target = "_top")
      ),
      br(),
      p(
        "Prafulla Gokhale, Ph.D.", br(),
        "Head of Experimental Therapeutics Core", br(),
        "Belfer Center for Applied Cancer Science", br(),
        "27 Drydock Avenue, DD472", br(),
        "Boston, MA 02210", br(),
        # a("prafulla_gokhale@dfci.harvard.edu",href="mailto:prafulla_gokhale@dfci.harvard.edu?Subject=PRoXe"),
        a("http://belfercenter.dfci.harvard.edu", href = "http://belfercenter.dfci.harvard.edu", target = "_blank")
      ), br(), br()
    )
  )
}
