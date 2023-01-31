# RESPOND-HCWs - Secondary analysis (R code)
#
# Author: Roberto Mediavilla (roberto.mediavilla@uam.es)
# Date: October 2022
#
# README
#
# Code for the secondary analysis of the RESPOND-WP4 trial.
#
# Study protocol available here:
# https://doi.org/10.1177/20552076221129084
#
# The following scripts must be sourced:
#   * src/req_pack.R (load required packages)
#   * src/data_cleaning.R (provides clean datasets)
#   * R/respond_functions.R (load required functions
#   into global environment)

#### Pre-specified: female HCWs ####

fem_ids <-
  ds_long %>%
  filter(soc_01 == "Female") %>%
  select(castor_record_id) %>%
  pull()

ds_long_fem <-
  ds_long %>%
  filter(castor_record_id %in% fem_ids)

plots_ds_long_fem <- rRespond_get_plots(ds_long_fem)

models_ds_long_fem <- rRespond_get_models_fem(ds_long_fem)

#### Pre-specified: frontline HCWs ####

front_ids <-
  ds_long %>%
  filter(soc_18 == "Yes") %>%
  select(castor_record_id) %>%
  pull()

ds_long_front <-
  ds_long %>%
  filter(castor_record_id %in% front_ids)

models_ds_long_front <- rRespond_get_models_baselinecov(ds_long_front)

#### Pre-specified: probable depression and/or anxiety disorders ####

sev_ids <-
  ds_long %>%
  mutate(phqads_cut = case_when(phq9_cut == "No" & gad7_cut == "No" ~ "No",
                                is.na(phq9_cut) ~ NA_character_,
                                is.na(gad7_cut) ~ NA_character_,
                                TRUE ~ "Yes") %>%
           factor(., levels = c("Yes", "No"))) %>%
  filter(phqads_cut == "Yes" & time == "1") %>%
  select(castor_record_id) %>%
  as.vector()

ds_long_sev <-
  ds_long %>%
  filter(castor_record_id %in% sev_ids$castor_record_id)

ds_long_sev %>%
  filter(time == "1") %>%
  group_by(randomization_group) %>%
  count()

plots_ds_long_sev <- rRespond_get_plots(ds_long_sev)

models_ds_long_sev <- rRespond_get_models_baselinecov(ds_long_sev)

#### Exploratory: modified per-protocol analysis ####

ds_long <-
  ds_long %>%
  group_by(castor_record_id) %>%
  mutate(k10_score_t2 = k10_score[time=2]) %>%
  ungroup()

completers_dwm_only <-
  ds_long %>%
  filter(dwm_completer == 1 & k10_score_t2 < 16) %>%
  select(castor_record_id) %>%
  unique()

completers_dwm_pm <-
  ds_long %>%
  filter(dwm_completer == 1
         & k10_score_t2 > 15
         & pm_completer == 1) %>%
  select(castor_record_id) %>%
  unique()

ds_long_pp <-
  ds_long %>%
  filter(castor_record_id %in% completers_dwm_only$castor_record_id
         | castor_record_id %in% completers_dwm_pm$castor_record_id
         | randomization_group == "Control")

plots_ds_long_pp <- rRespond_get_plots(ds_long_pp)

models_ds_long_pp <- rRespond_get_models_baselinecov(ds_long_pp)

#### Exploratory: binary logistic models ####

main_summary_log <- rRespond_get_models_log_baselinecov(ds_long)

nnt_phq9 <-
  genodds(response = ds_long$phq9_cut,
          group = ds_long$randomization_group,
          strata = ds_long$time,
          nnt = TRUE)

nnt_gad7 <-
  genodds(response = ds_long$gad7_cut,
          group = ds_long$randomization_group,
          strata = ds_long$time,
          nnt = TRUE)

nnt_phqads <-
  genodds(response = ds_long$phqads_cut,
          group = ds_long$randomization_group,
          strata = ds_long$time,
          nnt = TRUE)

#### Exploratory: complete-case analysis ####

ids_noncom <-
  ds_long %>%
  filter(is.na(phqads_t) | is.na(pcl5_t)) %>%
  select(castor_record_id)

ds_long_com <-
  ds_long %>%
  filter(!castor_record_id %in% ids_noncom$castor_record_id)

remove(ids_noncom)

plots_ds_long_com <- rRespond_get_plots(ds_long_com)

models_ds_long_com <- rRespond_get_models_baselinecov(ds_long_com)

#### Protocol adherence (intervention providers) ####

# Madrid

ds_quality_mad <-
  readxl::read_xlsx("dat/MAD/HELPERS_calls_registry.xlsx",
                    sheet = 1,
                    skip = 2) %>%
  janitor::clean_names() %>%
  select(-participant_id_4,
         -comments,
         -x13,
         -case_notes)

ds_quality_mad <-
  ds_quality_mad %>%
  mutate(
    across(
      .cols = where(is.character),
      .fns = as_factor
    ),
    recorded_session = fct_recode(recorded_session,
                                  "no" = "No",
                                  "no" = "NO",
                                  "yes" = "Yes"),
    adverse_event = fct_recode(adverse_event,
                               "no" = "No",
                               "yes" = "Yes",
                               "no" = "mo"),
    helper_id_dwmpfa = fct_recode(helper_id_dwmpfa,
                                  NULL = "N/A"),
    helper_id_pm = fct_recode(helper_id_pm,
                              NULL = "N/A"),
    contact_type = fct_recode(contact_type,
                              "PM+ 5" = "pm+ 5",
                              "DWM Welcome" = "DWM - Welcome call",
                              "DWM 1" = "DWM ongoing support call 1",
                              "DWM 2" = "DWM ongoing support call 2",
                              "DWM 3" = "DWM ongoing support call 3",
                              "DWM 4" = "DWM ongoing support call 4",
                              "DWM 5" = "DWM ongoing support call 5"),
    across(
      .cols = starts_with("time_"),
      .fns = ~  str_extract(., "[:graph:]{8}$") %>% hms),
    min_call = time_finish - time_start,
    min_call = as.numeric(min_call)/60
  )

dwm_mad <-
  ds_quality_mad %>%
  filter(str_detect(contact_type, "DWM")) %>%
  mutate(contact_type = fct_recode(contact_type,
                                   NULL = "DWM other")) %>%
  drop_na(contact_type) %>%
  ggplot(aes(x = contact_type,
             y = min_call,
             fill = contact_type)) +
  geom_violin() +
  theme(legend.position = "none") +
  scale_fill_viridis_d() +
  ylim(0,80) +
  geom_hline(yintercept = 15, color = "black", linetype = "dashed") +
  labs(x = "DWM session",
       y = "Duration (in min)")

pm_mad <-
  ds_quality_mad %>%
  filter(str_detect(contact_type, "PM")) %>%
  mutate(contact_type = fct_recode(contact_type,
                                   NULL = "PM+ other")) %>%
  drop_na(contact_type) %>%
  ggplot(aes(x = contact_type,
             y = min_call,
             fill = contact_type)) +
  geom_violin() +
  theme(legend.position = "none") +
  scale_fill_viridis_d() +
  ylim(0,80) +
  geom_hline(yintercept = 60, colour = "black", linetype = "dashed") +
  labs(x = "PM+ session",
       y = "Duration (in min)")

plot_adh_mad <- ggpubr::ggarrange(dwm_mad, pm_mad)

remove(dwm_mad, pm_mad)

dwm_mad_check <-
  readxl::read_xlsx("dat/MAD/dwm_fidelity_trial. EFJ.xlsx") %>%
  clean_names()

dwm_mad_check_tbl <-
  dwm_mad_check %>%
  mutate(call_id = str_to_lower(call_id),
         call_id = str_replace(call_id,
                               " ",
                               ""),
         call_id = str_replace(call_id,
                               "seg",
                               "dwm"),
         call_id = str_replace(call_id,
                               "welcome",
                               "dwm0")) %>%
  group_by(call_id) %>%
  summarise(n = n(),
            median = median(score),
            min = min(score),
            max = max(score)
  )

pm_mad_check <-
  readxl::read_xlsx("dat/MAD/pm_fidelity_trial. EFJ.xlsx") %>%
  clean_names()

pm_mad_check_tbl <-
  pm_mad_check %>%
  mutate(call_id = str_replace(call_id,
                               "_.*",
                               "")) %>%
  group_by(call_id) %>%
  summarise(n = n(),
            median = median(total),
            min = min(total),
            max = max(total))

# Barcelona

ds_quality_bcn_dwm <-
  readxl::read_xlsx("dat/BCN/Recuento duración contactos DWM_V2.xlsx",
                    skip = 1) %>%
  janitor::clean_names()

ds_quality_bcn_dwm <-
  ds_quality_bcn_dwm %>%
  mutate(
    across(
      where(is.character),
      as_factor
    ),
    contact_type = fct_recode(numer_of_call,
                              "DWM Welcome" = "Welcome call",
                              "DWM 1" = "DWM1",
                              "DWM 2" = "DWM2",
                              "DWM 3" = "DWM 3",
                              "DWM 4" = "DWM 4",
                              "DWM 5" = "DWM5"),
    min_call = min,
    castor_record_id = id,
    min_call = if_else(min_call < 1,
                       NaN,
                       min_call)
  ) %>%
  select(castor_record_id,
         contact_type,
         min_call)

ds_quality_bcn_pm <-
  readxl::read_xlsx("dat/BCN/Recuento duración contactos PM.xlsx",
                    skip = 1) %>%
  janitor::clean_names()

ds_quality_bcn_pm <-
  ds_quality_bcn_pm %>%
  rename(castor_record_id = id,
         contact_type = x2,
         min_call = min) %>%
  mutate(
    across(
      c(castor_record_id, contact_type),
      as_factor
    ),
    min_call = as.numeric(min_call),
    contact_type = fct_recode(contact_type,
                              "PM+ 1" = "PM1",
                              "PM+ 2" = "PM2",
                              "PM+ 3" = "PM3",
                              "PM+ 4" = "PM4",
                              "PM+ 5" = "PM5"),
    min_call = if_else(min_call < 1,
                       NaN,
                       min_call)
  )

ds_quality_bcn <-
  bind_rows(ds_quality_bcn_dwm,
            ds_quality_bcn_pm)

remove(ds_quality_bcn_dwm,
       ds_quality_bcn_pm)

dwm_bcn <-
  ds_quality_bcn %>%
  filter(str_detect(contact_type, "DWM")) %>%
  drop_na(contact_type) %>%
  ggplot(aes(x = contact_type,
             y = min_call,
             fill = contact_type)) +
  geom_violin() +
  theme(legend.position = "none") +
  scale_fill_viridis_d() +
  ylim(0,80) +
  geom_hline(yintercept = 15, color = "black", linetype = "dashed") +
  labs(x = "DWM session",
       y = "Duration (in min)")

pm_bcn <- ds_quality_bcn %>%
  filter(str_detect(contact_type, "PM")) %>%
  drop_na(contact_type) %>%
  ggplot(aes(x = contact_type,
             y = min_call,
             fill = contact_type)) +
  geom_violin() +
  theme(legend.position = "none") +
  scale_fill_viridis_d() +
  ylim(0,80) +
  geom_hline(yintercept = 60, colour = "black", linetype = "dashed") +
  labs(x = "PM+ session",
       y = "Duration (in min)")

plot_adh_bcn <- ggpubr::ggarrange(dwm_bcn, pm_bcn)

dwm_bcn_check <-
  readxl::read_xlsx("dat/BCN/BCN_DWM_fidelity_trial.xlsx") %>%
  clean_names()

dwm_bcn_check_tbl <-
  dwm_bcn_check %>%
  mutate(call_id = str_to_lower(call_id),
         call_id = str_replace(call_id,
                               " ",
                               ""),
         call_id = str_replace(call_id,
                               "seg",
                               "dwm"),
         call_id = str_replace(call_id,
                               "welcome",
                               "dwm0")) %>%
  group_by(call_id) %>%
  summarise(n = n(),
            median = median(score),
            min = min(score),
            max = max(score)
  )

pm_bcn_check <-
  readxl::read_xlsx("dat/BCN/BCN_PM_fidelity_trial.xlsx") %>%
  clean_names()

pm_bcn_check_tbl <-
  pm_bcn_check %>%
  mutate(call_id = str_replace(call_id,
                               "_.*",
                               "")) %>%
  group_by(call_id) %>%
  summarise(n = n(),
            median = median(total),
            min = min(total),
            max = max(total))

#### Cronbach's alpha ####

# PHQ-9

ds_long %>%
  filter(time == 1) %>%
  select(phq9_01:phq9_09) %>%
  psych::alpha()

# GAD-7

ds_long %>%
  filter(time == 1) %>%
  select(gad7_1:gad7_7) %>%
  psych::alpha()

# PCL-5

ds_long %>%
  filter(time == 1) %>%
  select(pcl5_1:pcl5_8) %>%
  psych::alpha()

# PHQ-ADS

ds_long %>%
  filter(time == 1) %>%
  select(starts_with(c("phq", "gad"))) %>%
  select(!c(phq9_10,
            phq9_t,
            phq9_cut,
            phq9_cat,
            gad7_t,
            gad7_cut,
            phqads_t,
            phqads_cat,
            phqads_cut)) %>%
  psych::alpha()

