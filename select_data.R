library(plyr)
library(dplyr)

set.seed(42)
ADNI <- read.csv("/Users/isd_neuroimaging/Library/Mobile Documents/com~apple~CloudDocs//ADNI/Data_Freeze/ADNI_DF_16_07_2024/data_sheets/ADNI_DF_11_2022_16_07_2024.csv")
ADNI_selected <- 
  ADNI %>% 
  select(ID, DX, amyloid_status, centiloid, tau.SUVR.DK.temporal_meta, 
         MMSE, EXAMDATE_diagnosis, EXAMDATE_tau, paste0("tau.SUVR.Schaefer200.ROI.idx.", 1:200)) %>% 
  mutate(DX_AB = paste0(DX, "_", amyloid_status)) %>% 
  mutate(EXAMDATE_diagnosis = as.Date(EXAMDATE_diagnosis),
         EXAMDATE_tau = as.Date(EXAMDATE_tau))
ADNI_selected <- ADNI_selected[complete.cases(ADNI_selected) == T,]

ADNI_selected_longitudinal <- 
  ADNI_selected %>% 
  group_by(ID) %>% 
  count() %>% 
  mutate(n_visits_tau = n) %>% 
  select(ID, n_visits_tau) %>% 
  left_join(ADNI_selected, by = "ID") %>% 
  filter(n_visits_tau>1, DX_AB %in% c("CN_Ab.neg", "CN_Ab.pos", "MCI_Ab.pos"))


ADNI_selected_longitudinal_baseline = 
  ADNI_selected_longitudinal %>% 
  group_by(ID) %>% 
  mutate(EXAMDATE_tau_baseline = min(EXAMDATE_tau)) %>% 
  select(ID, EXAMDATE_tau_baseline) %>% 
  unique() %>% 
  left_join(ADNI_selected_longitudinal, by = "ID") %>% 
  mutate(tau_fup_yrs = as.numeric(EXAMDATE_tau - EXAMDATE_tau_baseline)/365)


sampled_data <- ADNI_selected_longitudinal_baseline %>%
  filter(tau_fup_yrs == 0) %>% 
  select(ID, DX_AB) %>% 
  group_by(DX_AB) %>%
  sample_n(30) %>%
  ungroup()

ADNI_selected_longitudinal_random_sample = ADNI_selected_longitudinal_baseline %>% filter(ID %in% sampled_data$ID) %>% 
  select(ID, DX, amyloid_status, centiloid, DX_AB, tau.SUVR.DK.temporal_meta, 
         MMSE, EXAMDATE_diagnosis, EXAMDATE_tau, EXAMDATE_tau_baseline, 
         tau_fup_yrs, paste0("tau.SUVR.Schaefer200.ROI.idx.", 1:200))

write.csv(ADNI_selected_longitudinal_random_sample, "/Users/isd_neuroimaging/Library/Mobile Documents/com~apple~CloudDocs/ISD/Presentations/Talks/2024_07_AAIC/data_sharing/ADNI_ADWB_selection.csv")
