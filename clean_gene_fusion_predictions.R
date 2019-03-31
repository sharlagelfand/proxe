data_outside_app_dir <- file.path("~", "tcb", "proxe", "Dropbox (Partners HealthCare)", "PRoXe", "data_outside_app")

library(readr)
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library(forcats)

gene_fusion_predictions <- read_csv(paste0(data_outside_app_dir, "/STAR_Fusion_Report.csv")) %>%
  select(Sample, FusionName, TotalReads)

sequencing_checklist <- read_excel(paste0(data_outside_app_dir, "/SEQUENCING_checklist_091718.xlsx")) %>%
  select(pdx_name, CFCE_ID, Date_mRNA_Extracted) %>%
  distinct()

# Convert Sample (CFCE_ID) to pdx_name. Some notes:

# The samples in virusseq starting with AML are coded as AML# (for single digit numbers), but should be coded as AML0# for lookup.
# Exclude sample starting with MC
# For now, censor BA124 because there is not a 1:1 relationship of Sample <-> proxe_id
# Censor TA13 and AML04 since there is another sample of this PDX which we will prefer to use
# Use the *root* for matching i.e., [A-Z][1-9]. if a Sample has e.g. _[A-Z] after the root, remove it.
# If there is more than one pdx_name for a given CFCE_ID, use the latest based on Date_mRNA_Extracted

gene_fusion_predictions <- gene_fusion_predictions %>% 
  filter(!str_detect(Sample, "MC[0-9]")) %>% # Remove MC## samples
  filter(Sample != "BA124") %>% # Censor this sample until it is not listed twice in the sequencing file
  mutate(Sample = case_when(startsWith(Sample, "AML") & nchar(Sample) == 4 ~ # AML Fix
                                str_replace(Sample, "AML", "AML0"),
                              str_detect(Sample, "_") ~ str_replace_all(Sample, "\\_.*", ""), # Remove everything including and after _, to just use root
                              TRUE ~ Sample)) %>%
  filter(!(Sample %in% c("TA13", "AML04"))) # Censor additional samples

gene_fusion_predictions_with_pdx_name <- gene_fusion_predictions %>%
  left_join(sequencing_checklist, 
            by = c("Sample" = "CFCE_ID"))  %>%
  group_by(Sample, FusionName) %>% # If more than one pdx_name for SampleID use latest Date_mRNA_Extracted
  arrange(desc(Date_mRNA_Extracted)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(-Date_mRNA_Extracted)

# Verify that there is a corresponding pdx_name for every Sample

gene_fusion_predictions_with_pdx_name %>%
  filter(is.na(pdx_name)) %>%
  nrow() == 0

# Verify there is only one pdx_name for each Sample

gene_fusion_predictions_with_pdx_name %>%
  distinct(pdx_name, Sample) %>%
  add_count(pdx_name) %>%
  filter(n > 1) %>%
  nrow() == 0

# Verify there is only one Sample for each pdx_name

gene_fusion_predictions_with_pdx_name %>%
  distinct(pdx_name, Sample) %>%
  add_count(Sample) %>%
  filter(n > 1) %>%
  nrow() == 0
