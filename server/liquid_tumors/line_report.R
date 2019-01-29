server_liquid_tumors_line_report <- function(input, output, session) {
  # line report
  output$line_report <- DT::renderDataTable({
    # df_idx <- 4
    # print(df_idx)
    if (input$line_report_input_type == "click") {
      df_idx <- input$table_rows_selected
      if (length(df_idx) == 0) {
        return(data.frame(
          Instructions = "Select a line in the Database Explorer to see a report here.",
          row.names = ""
        ))
      } else if (length(df_idx) > 1) {
        return(data.frame(
          Problem = "Please select only one row from Database Explorer",
          row.names = ""
        ))
      }
      #       else if (length(df_idx) == 1) {
      #         continue
      #       }
    } else if (input$line_report_input_type == "dropdown") {
      df_idx <- which(df[, "PDX Name"] == input$line_report_name)
    }
    temp_df <- as.data.frame(t(df[df_idx, 1:(obInvisRet_ind - 1)]))
    colnames(temp_df) <- "sample"
    temp_df$PRoXe_Column_Header <- rownames(temp_df)
    rownames(temp_df) <- NULL
    temp_df <- temp_df[, 2:1]

    # 1. reorder by column groupings
    m3 <- meta_gloss
    m3$PRoXe_Column_Header <- gsub("_", " ", m3$PRoXe_Column_Header)
    td2 <- merge(temp_df, m3, by = "PRoXe_Column_Header", all.x = T, sort = F)
    td2$Column_Groupings <- factor(td2$Column_Groupings, levels = c("patient", "tumor", "pdx", "administrative"))
    td3 <- td2[with(td2, order(Column_Groupings, Row_Order)), ]
    # TODO below: remove columns that are NA as a temporary stopgap. Later get Mark to edit Glossary doc.
    if (anyNA(td3$Line_Report)) {
      warning("These rows are not yet annotated for Line Report; removing:")
      warning(td3[is.na(td3$Line_Report), ]$PRoXe_Column_Header)
      td3 <- td3[!is.na(td3$Line_Report), ]
    }

    # 2. Remove/edit rows as marked in 'Line Report' and 'Line Report Notes' columns.
    # 0=do not include
    # 1=include
    # 2=do not include this column per se, but similar data from an external spreadsheet will be shown
    # TODO: determine what to do regarding this (see Mark), then include below.
    # 3=include in report conditionally on instructions in Line Report Notes column.

    # remove 0s
    td4 <- td3[td3$Line_Report != 0, ]

    # TODO: discuss further with Mark re: 2s
    # the only columns that is 2 currently is 'Limited Distribution' (now Available for Distribution)
    # Mark says can read this in from a spreadsheet he's shared wiht me
    # but has not yet fully determined which spreadsheet that is
    # Maybe the BODFI tracking sheet or something like it.

    # conditionally reformat 3s
    # include FAB Classification only when WHO Cat == AML
    who_cat <- td4[td4$PRoXe_Column_Header == "WHO Category", "sample"]
    if (!is.na(who_cat)) {
      if (who_cat != "AML") {
        td4 <- td4[!(td4$PRoXe_Column_Header == "FAB Classification"), ]
        td4 <- td4[!(td4$PRoXe_Column_Header == "Cytogenetic Risk Category"), ]
      }
    }

    # combine into single row in order CD45,33,34,19,2,3,
    # in format CD45(superscript + or -)CD33(etc. no spaces.
    # If PDX_Other_Immunophenotypes, add ", and" and append to end.
    # then remove combined rows.
    to_combine <- c(
      "PDX CD45", "PDX CD34", "PDX CD33", "PDX CD19", "PDX CD20",
      "PDX CD2", "PDX CD3", "PDX Other Immunophenotype"
    ) # note in correct order
    # TODO: softcode (grep?) this based on something like "PDX CD*", accomodating for 'other'
    CDs <- to_combine[-length(to_combine)]
    temp <- as.character(td4[td4$PRoXe_Column_Header %in% to_combine, ]$sample)
    temp <- gsub("Positive", "<sup>+</sup> ", temp)
    temp <- gsub("Negative", "<sup>-</sup> ", temp)
    temp <- gsub("Dim", "<sup>Dim</sup> ", temp)
    temp <- gsub("Partial", "<sup>Partial</sup> ", temp)
    names(temp) <- gsub("PDX ", "", to_combine)
    temp <- temp[!is.na(temp)]
    if (!is.na(temp["Other Immunophenotype"])) {
      oi <- temp["Other Immunophenotype"]
      temp <- temp[names(temp) != "Other Immunophenotype"]
    }
    temp2 <- paste0(names(temp), temp, collapse = "")
    if (exists("oi")) temp2 <- paste0(temp2, ", and ", oi)
    # remove unwanted rows and add new row
    td5 <- td4[!(td4$PRoXe_Column_Header %in% CDs), ]
    td5[td5$PRoXe_Column_Header == "PDX Other Immunophenotype", 1] <- "PDX Immunophenotype"
    td5$sample <- as.character(td5$sample)
    td5[td5$PRoXe_Column_Header == "PDX Immunophenotype", "sample"] <- temp2

    # combine into single row: HLA Alleles TODO TODO currnelty not working
    # TODO: determine format with Mark
    # then remove combined rows.
    to_combine <- c("HLA-A Alleles", "HLA-B Alleles", "HLA-C Alleles")
    temp <- as.character(td5[td5$PRoXe_Column_Header %in% to_combine, ]$sample)
    temp <- temp[!is.na(temp)]
    temp2 <- paste0(names(temp), temp, collapse = " ")
    # remove unwanted rows and add new row
    td5[td5$PRoXe_Column_Header == "HLA-A Alleles", 1] <- "HLA Alleles"
    td6 <- td5[!(td5$PRoXe_Column_Header %in% to_combine), ]
    td6$sample <- as.character(td6$sample)
    td6[td6$PRoXe_Column_Header == "HLA Alleles", "sample"] <- temp2

    td8 <- td6[, c("PRoXe_Column_Header", "sample", "Column_Groupings")]
    colnames(td8) <- c(
      "Data Type",
      as.character(td8[td8$PRoXe_Column_Header == "PDX Name", "sample"]),
      "Column_Groupings"
    )
    td8 <- td8[, 1:2]
    return(td8)
  },
  escape = FALSE,
  filter = "top",
  server = FALSE, # note this means the entire dataframe is sent to user. Should be fine.
  rownames = FALSE,
  options = list(
    searchHighlight = TRUE,
    paging = FALSE
  )
  )

  output$line_report_inventory <- DT::renderDataTable({
    # copy-pasted input code from above #TODO factor out.
    if (input$line_report_input_type == "click") {
      df_idx <- input$table_rows_selected
      if (length(df_idx) == 0) {
        return(data.frame(
          Instructions = "Select a line in the Database Explorer to see a report here.",
          row.names = ""
        ))
      } else if (length(df_idx) > 1) {
        return(data.frame(
          Problem = "Please select only one row from Database Explorer",
          row.names = ""
        ))
      }
    } else if (input$line_report_input_type == "dropdown") {
      df_idx <- which(df[, "PDX Name"] == input$line_report_name)
    }

    loi <- df[df_idx, "PDX Name"] # line of interest
    idoi <- stringr::str_sub(loi, 1, 10) # id of interest
    tmp_df <- inv_lr[inv_lr$PDX_id == idoi, ]

    tmp_df$PDX_id <- NULL
    tmp_df
  },
  escape = FALSE,
  server = FALSE, # note this means the entire dataframe is sent to user. Should be fine.
  rownames = FALSE,
  options = list(
    dom = "t",
    searchHighlight = TRUE,
    paging = FALSE
  )
  )

  output$line_report_FC <- renderUI({
    if (input$line_report_input_type == "click") {
      df_idx <- input$table_rows_selected
    } else if (input$line_report_input_type == "dropdown") {
      df_idx <- which(df[, "PDX Name"] == input$line_report_name)
    }
    fc_path <- df[df_idx, "Flow Cytometry PDF"]
    if (length(df_idx) < 1) {
      tags$p(" ")
    } else if (is.na(fc_path)) {
      tags$p("There is no Flow Cytometry data for the selected line.")
    } else if (length(df_idx) > 1) {
      tags$p(" ")
    } else {
      outfile <- str_extract(fc_path, "Flow_Cytometry/.*.pdf")
      tags$iframe(
        src = outfile,
        width = "100%",
        height = "800px"
      )
    }
  })
  output$line_report_IHC <- renderUI({
    if (input$line_report_input_type == "click") {
      df_idx <- input$table_rows_selected
    } else if (input$line_report_input_type == "dropdown") {
      df_idx <- which(df[, "PDX Name"] == input$line_report_name)
    }
    fc_path <- df[df_idx, "IHC PDF"]
    if (length(df_idx) < 1) {
      tags$p(" ")
    } else if (is.na(fc_path)) {
      tags$p("There is no IHC data for the selected line.")
    } else if (length(df_idx) > 1) {
      tags$p(" ")
    } else {
      outfile <- str_extract(fc_path, "IHC/.*.pdf")
      tags$iframe(
        src = outfile,
        width = "100%",
        height = "800px"
      )
    }
  })
  output$line_report_Path <- renderUI({
    if (input$line_report_input_type == "click") {
      df_idx <- input$table_rows_selected
    } else if (input$line_report_input_type == "dropdown") {
      df_idx <- which(df[, "PDX Name"] == input$line_report_name)
    }
    fc_path <- df[df_idx, "Path Report PDF"]
    if (length(df_idx) < 1) {
      tags$p(" ")
    } else if (is.na(fc_path)) {
      tags$p("There is no Path Report for the selected line.")
    } else if (length(df_idx) > 1) {
      tags$p(" ")
    } else {
      outfile <- str_extract(fc_path, "Pathology_Reports/.*.pdf")
      tags$iframe(
        src = outfile,
        width = "100%",
        height = "800px"
      )
    }
  })

  # download the line report TODO: change filename to reflect line.
  # TODO: consider pre-processing all these if slow.

  ## TODO, perhaps: finish at some point: concatenate/format all information to make an easily downloadable Line Report per line:
  #   output$download_line_report = downloadHandler("PRoXe_line_report.pdf", content = function(filename) {
  #     df_idx <- input$table_rows_selected
  #     library(knitr)
  #     library(rmarkdown)
  #     if (length(df_idx) == 1) {
  #       temp_df <- t(df[df_idx,1:(obInvisRet_ind-1)])
  #       colnames(temp_df) <- "sample"
  #       knitr::knit2pdf()
  #       # rmarkdown::
  #     }
  #   })
}
