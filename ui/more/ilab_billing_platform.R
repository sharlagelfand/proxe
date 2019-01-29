ui_more_ilab_billing_platform <- function(){
  tabPanel("iLab Billing Platform",
           h1(a("iLab Solutions",
                href="https://dfci.ilabsolutions.com/service_center/show_external/7625?name=leukemia-lymphoma-xenograft-core-llx",
                target="_blank"),"Billing Platform"),
           h2("Manual"),
           uiOutput("iLab_manual") 
  )
}