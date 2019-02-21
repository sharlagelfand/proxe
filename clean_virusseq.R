library(readr)
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)

virusseq <- read_csv(paste0(data_outside_app_dir, "/virusseq_table.csv"))
sequencing_checklist <- read_excel(paste0(data_outside_app_dir, "/SEQUENCING_checklist_091718.xlsx")) %>%
  select(pdx_name, CFCE_ID, Date_mRNA_Extracted) %>%
  distinct()

# Convert SampleID (CFCE_ID) to pdx_name. Some notes:

# The samples in virusseq starting with AML are coded as AML# (for single digit numbers), but should be coded as AML0# for lookup.
# Exclude sample starting with MC
# For now, censor BA124 because there is not a 1:1 relationship of SampleID <-> proxe_id
# Censor TA13  and AML04 since there is another sample of this PDX which we will prefer to use
# Use the *root* for matching i.e., [A-Z][1-9]. if a SampleID has e.g. _[A-Z] after the root, remove it.
# If there is more than one pdx_name for a given CFCE_ID, use the latest based on Date_mRNA_Extracted

virusseq <- virusseq %>% 
  filter(!str_detect(SampleID, "MC[0-9]")) %>% # Remove MC## samples
  filter(SampleID != "BA124") %>% # Censor this sample until it is not listed twice in the sequencing file
  mutate(SampleID = case_when(startsWith(SampleID, "AML") & nchar(SampleID) == 4 ~ # AML Fix
                                str_replace(SampleID, "AML", "AML0"),
                              str_detect(SampleID, "_") ~ str_replace_all(SampleID, "\\_.*", ""), # Remove everything including and after _, to just use root
                              TRUE ~ SampleID)) %>%
  filter(!(SampleID %in% c("TA13", "AML04"))) # Censor additional samples

virusseq_with_pdx_name <- virusseq %>%
  left_join(sequencing_checklist, 
            by = c("SampleID" = "CFCE_ID"))  %>%
  group_by(SampleID, TranscriptID) %>% # If more than one pdx_name for SampleID use latest Date_mRNA_Extracted
  arrange(desc(Date_mRNA_Extracted)) %>%
  filter(row_number() == 1) %>%
  ungroup()

# Verify that there is a corresponding pdx_name for every sampleID

virusseq_with_pdx_name %>%
  filter(is.na(pdx_name)) %>%
  nrow() == 0

# Verify there is only one pdx_name for each SampleID

virusseq_with_pdx_name %>%
  distinct(pdx_name, SampleID) %>%
  add_count(pdx_name) %>%
  filter(n > 1) %>%
  nrow() == 0

# Verify there is only one sampleID for each pdx_name

virusseq_with_pdx_name %>%
  distinct(pdx_name, SampleID) %>%
  add_count(SampleID) %>%
  filter(n > 1) %>%
  nrow() == 0

# Complete data set (every SampleID with every pdx_name), for visualizing distributions

virusseq_with_pdx_name <- virusseq_with_pdx_name %>%
  select(pdx_name, TranscriptID, FPKM, Counts) %>%
  complete(pdx_name, TranscriptID, fill = list(FPKM = 0, Counts = 0))

# convert to matrix form for heatmap

# FPKM

virusseq_fpkm_matrix <- virusseq_with_pdx_name %>% 
  select(pdx_name, TranscriptID, FPKM) %>%
  spread(pdx_name, FPKM) %>%
  as.data.frame()

row.names(virusseq_fpkm_matrix) <- virusseq_fpkm_matrix[["TranscriptID"]]

virusseq_fpkm_matrix <- virusseq_fpkm_matrix %>%
  select(-TranscriptID) %>%
  as.matrix()

# Counts

virusseq_counts_matrix <- virusseq_with_pdx_name %>% 
  select(pdx_name, TranscriptID, Counts) %>%
  spread(pdx_name, Counts) %>%
  as.data.frame()

row.names(virusseq_counts_matrix) <- virusseq_counts_matrix[["TranscriptID"]]

virusseq_counts_matrix <- virusseq_counts_matrix %>%
  select(-TranscriptID) %>%
  as.matrix()

# Remove objects that do not need to be available in app
rm(virusseq, sequencing_checklist)

