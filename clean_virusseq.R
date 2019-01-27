library(readr)
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)

virusseq <- read_csv(paste0(data_outside_app_dir, "/virusseq_table.csv"))
sequencing_checklist <- read_excel(paste0(data_outside_app_dir, "/SEQUENCING_checklist_091718.xlsx")) %>%
  select(pdx_name, CFCE_ID) %>%
  distinct()

# Convert SampleID (CFCE_ID) to pdx_name. Some notes:

# The samples in virusseq starting with AML are coded as AML# (for single digit numbers), but should be coded as AML0# for lookup.
# Exclude sample starting with MC
# For now, censor BA124 because there is not a 1:1 relationship of SampleID <-> proxe_id
# Use the *root* for matching i.e., [A-Z][1-9]. if a SampleID has e.g. _[A-Z] after the root, remove it.

virusseq <- virusseq %>% 
  filter(!str_detect(SampleID, "MC[0-9]")) %>% # Remove MC## samples
  filter(SampleID != "BA124") %>% # Censor this sample until it is not listed twice in the sequencing file
  mutate(SampleID = case_when(startsWith(SampleID, "AML") & nchar(SampleID) == 4 ~ # AML Fix
                                str_replace(SampleID, "AML", "AML0"),
                              str_detect(SampleID, "_") ~ str_replace_all(SampleID, "\\_.*", ""), # Remove everything including and after _, to just use root
                              TRUE ~ SampleID))

virusseq_with_pdx_name <- virusseq %>%
  left_join(sequencing_checklist, 
            by = c("SampleID" = "CFCE_ID"))

# Verify that there is a corresponding proxe_id for every sampleID

virusseq_with_pdx_name %>%
  filter(is.na(pdx_name)) %>%
  nrow() == 0

# convert to matrix form for heatmap

virusseq_fpkm_matrix <- virusseq_with_pdx_name %>% 
  select(pdx_name, TranscriptID, FPKM) %>% 
  group_by(pdx_name, TranscriptID) %>% # Until we figure out to do with duplication
  arrange(FPKM) %>%
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  spread(pdx_name, FPKM, fill = 0) %>%
  as.data.frame()

row.names(virusseq_fpkm_matrix) <- virusseq_fpkm_matrix[["TranscriptID"]]

virusseq_fpkm_matrix <- virusseq_fpkm_matrix %>%
  select(-TranscriptID) %>%
  as.matrix()

# Remove objects that do not need to be available in app
rm(virusseq, sequencing_checklist, virusseq_with_pdx_name)
