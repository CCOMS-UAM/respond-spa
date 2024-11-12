# R Code for RESPOND Spain - Data cleaning #
# Author: Roberto Mediavilla (roberto.mediavilla@uam.es) #
# Date: September 2022 #

#### Source data ####

# About the data

# There are three sources of data for this project, each collected
# at one of the project sites
#   Screening: collected using Castor EDC
#   Surveys (i.e., outcome assessment): collected using Castor EDC and Qualtrics
#   Adherence to intervention protocol: collected using DWM metadata
#   and facilitator-reported PM+ data

# To avoid conflicts


# library(tidyverse)

# Screening data

ds_study_mad <-
  read_delim("dat/MAD/RESPOND_-_Spain_export_20220919.csv",
             ";",
             escape_double = FALSE,
             locale = locale(date_names = "es"),
             trim_ws = TRUE)

ds_study_mad_k10 <-
  read_delim("dat/MAD/RESPOND_-_Spain_export_20220818(1).csv",
             ";",
             escape_double = FALSE,
             locale = locale(date_names = "es"),
             trim_ws = TRUE)

ds_study_bcn <-
  read_delim("dat/BCN/BCN_Spain_export_20220906_v2.csv",
             ";",
             escape_double = FALSE,
             locale = locale(date_names = "es"),
             trim_ws = TRUE)

ds_study_bcn_k10 <-
  read_delim("dat/BCN/BCN_Spain_export_20220906_v2.csv",
             ";",
             escape_double = FALSE,
             locale = locale(date_names = "es"),
             trim_ws = TRUE)

# Survey data

ds_t1_mad_num <-
  read_delim("dat/MAD/RESPOND_-_Spain_RESPOND_WP4_T1_export_20220818(1).csv",
             ";",
             escape_double = FALSE,
             locale = locale(date_names = "es"),
             trim_ws = TRUE) %>%
  clean_names()

ds_t2_mad_num <-
  read_delim("dat/MAD/RESPOND_-_Spain_RESPOND_WP4_T2_export_20220818(1).csv",
             ";",
             escape_double = FALSE,
             locale = locale(date_names = "es"),
             trim_ws = TRUE) %>%
  clean_names()

ds_t3_mad_num <-
  read_delim("dat/MAD/RESPOND_-_Spain_RESPOND_WP4_T3_export_20220818(1).csv",
             ";",
             escape_double = FALSE,
             locale = locale(date_names = "es"),
             trim_ws = TRUE) %>%
  clean_names()

ds_t4_mad_num <-
  read_delim("dat/MAD/RESPOND_-_Spain_RESPOND_WP4_T4_export_20220818(1).csv",
             ";",
             escape_double = FALSE,
             locale = locale(date_names = "es"),
             trim_ws = TRUE) %>%
  clean_names()

ds_t1_bcn_num <-
  read_delim("dat/BCN/RESPOND WP4 T1_V3_enviada_CSV.csv",
             ";",
             escape_double = FALSE,
             # locale = locale(date_names = "es"),
             trim_ws = TRUE) %>%
  clean_names()

ds_t2_bcn_num <-
  read_delim("dat/BCN/RESPOND WP4 T2_V3_enviada_CSV.csv",
             ";",
             escape_double = FALSE,
             locale = locale(date_names = "es"),
             trim_ws = TRUE) %>%
  clean_names()

ds_t3_bcn_num <-
  read_delim("dat/BCN/RESPOND WP4 T3_V3_enviada_CSV.csv",
             ";",
             escape_double = FALSE,
             locale = locale(date_names = "es"),
             trim_ws = TRUE) %>%
  clean_names()

ds_t4_bcn_num <-
  read_delim("dat/BCN/RESPOND WP4 T4_V3_enviada_CSV.csv",
             ";",
             escape_double = FALSE,
             locale = locale(date_names = "es"),
             trim_ws = TRUE) %>%
  clean_names()

# Adherence data

dwm_adherence_mad <-
  read.delim("dat/MAD/Respond Aid Workers_user_activity_06-09-2022 12 14 46.csv",
             sep = ";") %>%
  clean_names() %>%
  select(!x) %>%
  filter(stringr::str_detect(research_id,
                    "UAM"))

dwm_adherence_bcn <-
  read.delim("dat/BCN/Meta-data app_v2_BCN.csv",
             sep = ";") %>%
  clean_names() %>%
  filter(str_detect(research_id,
                    "SJD"))

pm_adherence_mad <-
  read_delim("dat/MAD/pm_attendance.csv", delim = ";")

pm_adherence_bcn <-
  readxl::read_xlsx("dat/BCN/Num sesiones PM.xlsx")

#### Prepare datasets for merging ####

# Many transformations are needed because different electronic
# data capture softwares were used and because CASTOR EDC
# changed some features while the trial was ongoing

##### Screening data #####

ds_study_mad <-
  ds_study_mad %>%
  clean_names() %>%
  select(participant_id:randomized_on,
         contains("helper"),
         pm_si_no_uam) %>%
  rename(record_id = participant_id,
         record_status = participant_status,
         institute_abbreviation = site_abbreviation) %>%
  rename_with(~ str_replace(.,
                            "t0",
                            "t1")
  ) %>%
  mutate(
    across(
      .cols = where(is_character),
      .fns = as.factor
    )
  ) %>%
  mutate(
    across(
      .cols = where(is_logical),
      .fns = as.factor
    )
  ) %>%
  mutate(randomized_on = date(randomized_on),
         pm_si_no_uam = fct_recode(pm_si_no_uam,
                                   NULL = "##USER_MISSING_96##",
                                   NULL = "##USER_MISSING_95##"))

ds_study_mad_k10 <-
  ds_study_mad_k10 %>%
  clean_names() %>%
  select(record_id,
         institute_abbreviation,
         contains("k10")) %>%
  mutate(record_id = as.factor(record_id))

ds_study_bcn <-
  ds_study_bcn %>%
  clean_names() %>%
  select(record_id:randomized_on,
         contains("helper"),
         pm_si_no_sjd) %>%
  rename_with(~ str_replace(.,
                            "t0",
                            "t1")
  ) %>%
  mutate(
    across(
      .cols = where(is_character),
      .fns = as.factor
    )
  ) %>%
  mutate(
    across(
      .cols = where(is_logical),
      .fns = as.factor
    )
  ) %>%
  mutate(randomized_on = dmy_hm(randomized_on) %>% date()) %>% # BCN only
  mutate(randomization_id = as_factor(randomization_id),
         helper_dwm_sjd = as_factor(helper_dwm_sjd),
         pm_helper_sjd = as_factor(pm_helper_sjd),
         pm_si_no_sjd = as_factor(pm_si_no_sjd),
         pm_si_no_sjd = fct_recode(pm_si_no_sjd,
                                   "Sí" = "1",
                                   "No" = "0")) # BCN only

ds_study_bcn_k10 <-
  ds_study_bcn_k10 %>%
  clean_names() %>%
  select(record_id,
         institute_abbreviation,
         contains("k10")) %>%
  mutate(record_id = as.factor(record_id),
         institute_abbreviation = as.factor(institute_abbreviation))

# I merge the first 4 datasets

ds_study <-
  bind_rows(ds_study_mad,
            ds_study_bcn) %>%
  rename(., castor_record_id = record_id) %>%
  mutate(
    across(
      .cols = contains("helper"),
      .fns = as.character
    )
  ) %>%
  mutate(helper_dwm = if_else(institute_abbreviation == "UAM",
                              helper_dwm_uam,
                              helper_dwm_sjd),
         helper_pm = if_else(institute_abbreviation == "UAM",
                             pm_helper_uam,
                             pm_helper_sjd)) %>%
  mutate(
    across(
      .cols = contains("helper"),
      .fns = as_factor
    )
  ) %>%
  mutate(step_up = if_else(institute_abbreviation == "UAM",
                           pm_si_no_uam,
                           pm_si_no_sjd))

ds_study_k10 <-
  bind_rows(ds_study_bcn_k10,
            ds_study_mad_k10) %>%
  mutate(institute_abbreviation = as_factor(institute_abbreviation)) %>%
  rename_with(~ str_replace(.,
                            "t0",
                            "t1")) %>%
  rename(castor_record_id = record_id) %>%
  mutate(castor_record_id = as.character(castor_record_id))

remove(ds_study_mad,
       ds_study_bcn,
       ds_study_mad_k10,
       ds_study_bcn_k10)

##### Survey data #####

ds_list_combined <-
  list(ds_t1_bcn_num,
       ds_t2_bcn_num,
       ds_t3_bcn_num,
       ds_t4_bcn_num,
       ds_t1_mad_num,
       ds_t2_mad_num,
       ds_t3_mad_num,
       ds_t4_mad_num)

reduce(ds_list_combined[1:4],
       ~ left_join(.x,
                   .y,
                   by = "castor_record_id"),
       "forward") -> ds_wide_bcn

reduce(ds_list_combined[5:8],
       ~ left_join(.x,
                   .y,
                   by = "castor_record_id"),
       "forward") -> ds_wide_mad

left_join(ds_wide_mad,
          ds_study_k10,
          by = "castor_record_id") -> ds_wide_mad

left_join(ds_wide_bcn,
          ds_study_k10,
          by = "castor_record_id") -> ds_wide_bcn

remove(ds_t1_bcn_num,
       ds_t2_bcn_num,
       ds_t3_bcn_num,
       ds_t4_bcn_num,
       ds_t1_mad_num,
       ds_t2_mad_num,
       ds_t3_mad_num,
       ds_t4_mad_num,
       ds_list_combined,
       ds_study_k10)

##### Adherence data #####

dwm_adherence_mad <-
  dwm_adherence_mad %>%
  mutate(research_id = str_replace(research_id,
                                   "^UAM",
                                   "ES-UAM")) %>%
  filter(!research_id == "ES-UAM0001") %>% # incorrect id
  rename(castor_record_id = research_id) %>%
  mutate(castor_record_id = as_factor(castor_record_id),
         dwm_n = select(.,
                        c(finished_grounding:finished_making_room)) %>% rowSums(),
         dwm_completer = if_else(dwm_n > 2,
                                 "1",
                                 "0") %>% as.factor())

dwm_adherence_bcn <-
  dwm_adherence_bcn %>%
  mutate(research_id = str_replace(research_id,
                                   "^ ",
                                   "")) %>%
  rename(castor_record_id = research_id) %>%
  mutate(castor_record_id = as_factor(castor_record_id),
         dwm_n = select(.,
                        c(finished_grounding:finished_making_room)) %>% rowSums(),
         dwm_completer = if_else(dwm_n > 2,
                                 "1",
                                 "0") %>% as.factor()) %>%
  filter(user_id != "337") # duplicated id

pm_adherence_mad <-
  pm_adherence_mad %>%
  mutate(castor_record_id = castor_id,
         pm_n = as.integer(pm_videocalls),
         pm_completer = if_else(pm_n > 3,
                                "1",
                                "0") %>%
           as_factor()) %>%
  select(castor_record_id,
         pm_n,
         pm_completer)

pm_adherence_bcn <-
  pm_adherence_bcn %>%
  mutate(castor_record_id = as_factor(castor_record_id),
         pm_n = as.integer(pm_n),
         pm_completer = if_else(pm_n > 3,
                                "1",
                                "0") %>%
           as_factor())

#### Combine datasets ####

# I make some transformations required for successful merging

ds_wide_bcn <-
  ds_wide_bcn %>%
  mutate(t2_csri_sp_mental_g_n = as.character(t2_csri_sp_mental_g_n))

ds_wide <-
  bind_rows(ds_wide_mad,
            ds_wide_bcn)

colnames(ds_wide) <-
  colnames(ds_wide) %>%
  if_else(str_detect(., ".x.x$"),
          paste("t3", ., sep = "_") %>%
            str_replace(., ".x.x$", ""),
          .) %>%
  if_else(str_detect(., ".y.y$"),
          paste("t4", ., sep = "_") %>%
            str_replace(., ".y.y$", ""),
          .) %>%
  if_else(str_detect(., ".y$"),
          paste("t2", ., sep = "_") %>%
            str_replace(., ".y$", ""),
          .) %>%
  if_else(str_detect(., ".x$"),
          paste("t1", ., sep = "_") %>%
            str_replace(., ".x$", ""),
          .)

ds_wide <-
  ds_wide %>%
  rename_with(~ str_replace(.,
                            "_baseline",
                            "")
  ) %>%
  rename_with(~ str_replace(.,
                            "covid19_0",
                            "covid19_")
  ) %>%
  mutate(
    t2_csri_sp_mental_g_n = as.numeric(t2_csri_sp_mental_g_n),
    t3_m_t1_csri_sp_mental_g_t = as.numeric(t3_m_t1_csri_sp_mental_g_t)) %>%
  select(!starts_with("x"))

ds_long <-
  pivot_longer(ds_wide,
               cols = !c(castor_record_id, institute_abbreviation),
               names_to = c("time", ".value"),
               names_sep = 3) %>%
  mutate(time = str_replace(time,
                            "_",
                            "")) %>%
  mutate(time = str_replace(time,
                            "t",
                            ""))

# Combine datasets

ds_long <-
  ds_study %>%
  select(castor_record_id,
         institute_abbreviation,
         randomization_group,
         randomized_on,
         step_up) %>%
  full_join(., ds_long, by = c("castor_record_id",
                               "institute_abbreviation"))

remove(ds_wide_bcn,
       ds_wide_mad,
       ds_study)

ds_long <-
  ds_long %>%
  left_join(.,
            dwm_adherence_mad[,c(2,40,41)],
            by = "castor_record_id") %>%
  left_join(.,
            dwm_adherence_bcn[,c(2,40,41)],
            by = "castor_record_id")

ds_long <-
  pm_adherence_mad %>%
  right_join(.,
             ds_long,
             by = "castor_record_id") %>%
  relocate(c(pm_n, pm_completer), .after = last_col())


ds_long <-
  pm_adherence_bcn %>%
  right_join(.,
             ds_long,
             by = "castor_record_id")

# I fix some errors

ds_long <-
  ds_long %>%
  mutate(dwm_n = if_else(institute_abbreviation == "UAM",
                         dwm_n.x,
                         dwm_n.y),
         dwm_completer = if_else(institute_abbreviation == "UAM",
                                 dwm_completer.x,
                                 dwm_completer.y)) %>%
  select(!c(dwm_n.x,
            dwm_n.y,
            dwm_completer.x,
            dwm_completer.y))

ds_long <-
  ds_long %>%
  mutate(pm_n = if_else(institute_abbreviation == "UAM",
                        pm_n.y,
                        pm_n.x),
         pm_completer = if_else(institute_abbreviation == "UAM",
                                pm_completer.y,
                                pm_completer.x)) %>%
  select(!c(pm_n.x,
            pm_n.y,
            pm_completer.x,
            pm_completer.y)) %>%
  relocate(c(pm_n, pm_completer), .after = last_col())

remove(dwm_adherence_mad,
       dwm_adherence_bcn,
       pm_adherence_mad,
       pm_adherence_bcn)

#### Prepare daset for analysis ####

##### Create new variables #####

ds_long <-
  ds_long %>%
  mutate(phq9_t = select(., c(phq9_01:phq9_09)) %>% rowSums(),
         phq9_cut = if_else(phq9_t > 9,
                            "Yes",
                            "No") %>% as_factor(.),
         phq9_cat = case_when(phq9_t < 4 ~ "Minimal symptoms",
                              phq9_t >= 4 & phq9_t < 10 ~ "Mild symptoms",
                              phq9_t >= 10 & phq9_t < 14 ~ "Moderate symptoms",
                              phq9_t >= 14 & phq9_t < 20 ~ "Moderately severe symptoms",
                              phq9_t >= 20 ~ "Severe symptoms"),
         phq9_cat = as.ordered(phq9_cat),
         gad7_t = select(., c(gad7_1:gad7_7)) %>% rowSums(),
         gad7_cut = if_else(gad7_t > 9,
                            "Yes",
                            "No") %>% as_factor(.),
         pcl5_t = select(., c(pcl5_1:pcl5_8)) %>% rowSums(),
         k10_cut = if_else(k10_score[time=2] < 16,
                           "No",
                           "Yes")
         )


ds_long <-
  ds_long %>%
  mutate(phqads_t = select(., c(phq9_01,
                                phq9_02,
                                phq9_03,
                                phq9_04,
                                phq9_05,
                                phq9_06,
                                phq9_07,
                                phq9_08,
                                phq9_09,
                                gad7_1,
                                gad7_2,
                                gad7_3,
                                gad7_4,
                                gad7_5,
                                gad7_6,
                                gad7_7))
         %>% rowSums(),
         phqads_cat = case_when(phqads_t < 10 ~
                                  "No symptoms",
                                phqads_t >= 10 & phqads_t < 20 ~
                                  "Mild symptoms",
                                phqads_t >= 20 & phqads_t < 30 ~
                                  "Moderate symptoms",
                                phqads_t >= 30 ~
                                  "Severe symptoms") %>%
           factor(.,
                  levels = c("No symptoms",
                             "Mild symptoms",
                             "Moderate symptoms",
                             "Severe symptoms")),
         phqads_cut = case_when(phq9_cut == "No" & gad7_cut == "No" ~ "No",
                                is.na(phq9_cut) ~ NA_character_,
                                is.na(gad7_cut) ~ NA_character_,
                                TRUE ~ "Yes") %>%
           factor(., levels = c("Yes", "No")))

ds_long <-
  ds_long %>%
  mutate(
    across(
      c("survey_creation_date",
        "survey_sent_date",
        "survey_completed_on"),
      ~ str_extract(., "[:graph:]{10}") %>% dmy),
    soc_02_age = year(survey_completed_on) - t0_soc_02) %>%
  relocate(soc_02_age, .after = t0_soc_02)

##### Transform variables #####

ds_long <-
  ds_long %>%
  rename_with(~ str_replace(.,
                            "^m_",
                            "")
  ) %>%
  rename_with(~ str_replace(.,
                            "^t[:digit:]_",
                            "")
  ) %>%
  mutate(soc_2 = as.integer(soc_2),
         across(
           .cols = where(is.numeric),
           .fns = as.integer)
         )

ds_long$soc_01 <-
  factor(x = ds_long$soc_01,
         levels = c(1, 2, 3, 4, 5, 6),
         labels = c("Female",
                    "Male",
                    "Other",
                    "I prefer not to answer",
                    "Transgender woman",
                    "Transgender man"))

ds_long$soc_1 <- factor(x = ds_long$soc_1,
                        levels = c(0, 1, 4, 5, 6),
                        labels = c("Single",
                                   "In a relationship / married",
                                   "Divorced",
                                   "Widower",
                                   "Other"))

ds_long$soc_2 <- factor(x = ds_long$soc_2,
                        levels = c(0, 1, 2, 3, 4, 5),
                        labels = c("None",
                                   "One",
                                   "Two",
                                   "Three",
                                   "Four",
                                   "Five or more"))

ds_long$soc_12 <- factor(x = ds_long$soc_12,
                         levels = c(0, 1, 2, 3, 4),
                         labels = c("Primary studies (not finished)",
                                    "Primary studies (finished)",
                                    "Secondary",
                                    "Technical-professional",
                                    "University"))

ds_long <-
  ds_long %>%
  mutate(soc_12 = fct_collapse(soc_12,
                               "Primary" = c("Primary studies (not finished)",
                                             "Primary studies (finished)")))

ds_long$soc_16 <- factor(x = ds_long$soc_16,
                         levels = c(1, 2, 3, 4, 5, 6),
                         labels = c("Physician",
                                    "Nurse",
                                    "Nursing technician",
                                    "Orderly",
                                    "Administration",
                                    "Other"))

ds_long <-
  ds_long %>%
  mutate(soc_16 = fct_recode(soc_16,
                             NULL = "Orderly")) # No orderlies in dataset

ds_long$soc_17 <- factor(x = ds_long$soc_17,
                         levels = c(1, 2, 3, 4, 5),
                         labels = c("Hospital facilities",
                                    "Primary care facilities",
                                    "Specialised care facilities",
                                    "Emergencies",
                                    "Other"))

ds_long$soc_18 <- factor(x = ds_long$soc_18,
                         levels = c(0, 1),
                         labels = c("No",
                                    "Yes"))

ds_long$covid19_1 <- factor(x = ds_long$covid19_1,
                            levels = c(0, 1, 2),
                            labels = c("No",
                                       "Probable",
                                       "Yes"))

ds_long$covid19_1_1 <- factor(x = ds_long$covid19_1_1,
                              levels = c(0, 1, 2),
                              labels = c("No or mild symptoms",
                                         "Moderate symptoms",
                                         "Severe symptoms"))

ds_long$covid19_2 <- factor(x = ds_long$covid19_2,
                            levels = c(0, 1, 2, 3),
                            labels = c("Not at all",
                                       "Slightly",
                                       "Considerably",
                                       "Extremely"))

ds_long$covid19_3 <- factor(x = ds_long$covid19_3,
                            levels = c(0, 1, 2, 3),
                            labels = c("Not at all",
                                       "Slightly",
                                       "Considerably",
                                       "Extremely"))

ds_long$covid19_4 <- factor(x = ds_long$covid19_4,
                            levels = c(0, 1),
                            labels = c("No",
                                       "Yes"))

ds_long$sbs_1 <- factor(x = ds_long$sbs_1,
                        levels = c(0, 1, 2),
                        labels = c("No",
                                   "Yes",
                                   "No")) # Level "2" is for BCN

ds_long$csri_sp_mental_g_t <- factor(x = ds_long$csri_sp_mental_g_t,
                                     levels = c(0, 1, 2, 3, 4),
                                     labels = c("Less than 15 minutes",
                                                "15-29 minutes",
                                                "30-44 minutes",
                                                "45-59 minutes",
                                                "More than 60 minutes"))

ds_long$csri_sp_nurse_g_t <- factor(x = ds_long$csri_sp_nurse_g_t,
                                    levels = c(0, 1, 2, 3, 4),
                                    labels = c("Less than 15 minutes",
                                               "15-29 minutes",
                                               "30-44 minutes",
                                               "45-59 minutes",
                                               "More than 60 minutes"))

ds_long$csri_sp_mental_i_t <- factor(x = ds_long$csri_sp_mental_i_t,
                                     levels = c(0, 1, 2, 3, 4),
                                     labels = c("Less than 15 minutes",
                                                "15-29 minutes",
                                                "30-44 minutes",
                                                "45-59 minutes",
                                                "More than 60 minutes"))

ds_long$csri_sp_phys_i_t <- factor(x = ds_long$csri_sp_phys_i_t,
                                   levels = c(0, 1, 2, 3, 4),
                                   labels = c("Less than 15 minutes",
                                              "15-29 minutes",
                                              "30-44 minutes",
                                              "45-59 minutes",
                                              "More than 60 minutes"))

ds_long$csri_sp_nurse_i_t <- factor(x = ds_long$csri_sp_nurse_i_t,
                                    levels = c(0, 1, 2, 3, 4),
                                    labels = c("Less than 15 minutes",
                                               "15-29 minutes",
                                               "30-44 minutes",
                                               "45-59 minutes",
                                               "More than 60 minutes"))

ds_long <- droplevels(ds_long)

##### Add labels #####

ds_long <-
  ds_long %>%
  sjlabelled::var_labels(randomization_group = "Group",
             castor_record_id = "Participant ID",
             survey_completed_on = "Assessment date",
             institute_abbreviation = "Site",
             soc_01 = "Gender",
             soc_1 = "Marital status",
             soc_02 = "Year of birth",
             soc_02_age = "Age",
             soc_2 = "Number of children",
             soc_12 = "Educational level",
             soc_16 = "Type of job",
             soc_17 = "Job facility",
             soc_18 = "Frontline worker (ever)",
             covid19_1 = "COVID-19 infection (ever)",
             covid19_2 = "Worry about getting infected",
             covid19_3 = "Worry about infecting others",
             covid19_4 = "Closed ones died due to COVID-19",
             phqads_t = "Anxiety/Depression (PHQ-ADS score)",
             phq9_t = "Depression (PHQ-9 score)",
             gad7_t = "Anxiety (GAD-7 score)",
             pcl5_t = "Posttraumatic stress (PCL-5 score)",
             k10_score = "Psychological Distress (K-10 score)",
             phq9_cut = "Probable depression (PHQ-9 > 9)",
             gad7_cut = "Probable anxiety (GAD-7 > 9)",
             k10_cut = "Probable distress (K-10 > 15)",
             csri_sp_mental_i_n = "Individual mental health appointments (n)",
             csri_sp_mental_g_n = "Group mental health appointments (n)",
             csri_sp_mental_i_t = "Individual mental health appointments (time)",
             csri_sp_mental_g_t = "Group mental health appointments (time)",
             csri_sp_phys_i_n = "Appointments with physician (n)",
             csri_sp_phys_i_t = "Appointments with physician (duration)",
             csri_sp_nurse_i_n = "Individual appointments with nurse (n)",
             csri_sp_nurse_i_t = "Individual appointments with nurse (time)",
             csri_sp_nurse_g_n = "Group appointments with nurse (n)",
             csri_sp_nurse_g_t = "Group appointments with nurse (time)",
             le_1 = "Serious illnes, accident",
             le_2 = "Death",
             le_3 = "Break up, divorce",
             btq_01 = "War",
             btq_02 = "Accident",
             btq_03 = "Natural disaster",
             btq_04 = "Serious illness",
             btq_05 = "Physical violence (childhood)",
             btq_06 = "Physical violence",
             btq_07 = "Sexual assault",
             btq_08 = "Physical danger",
             btq_09 = "Death",
             btq_10 = "Witness")

