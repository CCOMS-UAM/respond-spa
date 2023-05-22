# RESPOND-HCWs - Main analysis (R code)
#
# Author: Roberto Mediavilla (roberto.mediavilla@uam.es)
# Date: October 2022
#
# README
#
# Code for the main analysis of the RESPOND-WP4 trial.
#
# Study protocol available here:
# https://doi.org/10.1177/20552076221129084 and
# in www/study_protocol.pdf
#
# The following scripts must be sourced:
#   * src/req_pack.R (load required packages)
#   * src/data_cleaning.R (provides clean datasets)
#   * R/respond_functions.R (load required functions
#   into global environment)

#### Missing data ####

ds_miss <-
  ds_long %>%
  select(soc_01,
         soc_12,
         soc_16,
         soc_17,
         soc_18,
         covid19_1,
         soc_02_age,
         institute_abbreviation,
         randomization_group,
         phqads_t,
         castor_record_id,
         k10_score,
         time) %>%
  pivot_wider(.,
              names_from = time,
              values_from = c(soc_01,
                              soc_12,
                              soc_16,
                              soc_17,
                              soc_18,
                              covid19_1,
                              soc_02_age,
                              k10_score,
                              institute_abbreviation,
                              randomization_group,
                              phqads_t),
              names_sep = "_t") %>%
  select(castor_record_id,
         ends_with("t1"),
         phqads_t_t4)

ds_long %>%
  group_by(institute_abbreviation,
           castor_record_id,
           randomization_group,
           time) %>%
  mutate(
    survey_completed_on = date(survey_completed_on)
  ) %>%
  select(institute_abbreviation,
         randomization_group,
         time,
         survey_completed_on) %>%
  pivot_wider(names_from = "time",
              values_from = "survey_completed_on",
              names_prefix = "t") %>%
  mutate(
    t2_days = t2 - t1,
    t3_days = t3 - t1,
    t4_days = t4 - t1
  ) %>%
  group_by(randomization_group,
           institute_abbreviation) %>%
  summarise(
    t2_median = median(t2_days, na.rm = T),
    t3_median = median(t3_days, na.rm = T),
    t4_median = median(t4_days, na.rm = T)
  ) %>%
  pivot_longer(cols = contains("median"),
               names_to = c("time", ".values"),
               names_sep = "_") %>%
  arrange(time) %>%
  kable(caption = "Time elapsed from baseline to follow-up assessments")

#### Adverse events ####

ds_long %>%
  filter(sbs_1 == "Yes") %>%
  select(castor_record_id,
         institute_abbreviation,
         randomization_group,
         time) %>%
  mutate(anon_id = as_factor(castor_record_id) %>%
           fct_anon()) %>%
  relocate(anon_id) %>%
  select(!castor_record_id) %>%
  mutate(time = fct_recode(time,
                           "T1. Baseline" = "1",
                           "T2. Week 7 (post DWM)" = "2",
                           "T4. Week 21 (follow-up)" = "4")) %>%
  kable(caption = "Suicide thoughts reported in follow-up assessments")

# Please note that one participant reported suicide thoughts
# during the baseline assessment. People who reported suicide thoughts
# over the 2-week period prior to screening were excluded, so this person
# had these thoughts three or for weeks before enrollment

#### Trial overview ####

ds_long %>%
  mutate(time = fct_recode(time,
                           "T1. Baseline" = "1",
                           "T2. Week 7 (post DWM)" = "2",
                           "T3. Week 13 (post PM+)" = "3",
                           "T4. Week 21 (follow-up)" = "4")) %>%
  group_by(time,
           randomization_group) %>%
  miss_var_summary %>%
  filter(variable == "phqads_t") %>%
  select(!variable) %>%
  relocate(time) %>%
  arrange(time) %>%
  mutate(pct_miss = round(pct_miss, 1))

#### Descriptive analyses ####

##### Tables ####

table1 <-
  ds_long %>%
  filter(time == 1) %>%
  select(soc_02_age,
         soc_01,
         soc_12,
         soc_16,
         soc_17,
         soc_18,
         covid19_1,
         institute_abbreviation,
         phqads_t,
         phq9_t,
         gad7_t,
         pcl5_t,
         phq9_cut,
         gad7_cut,
         randomization_group) %>%
  mutate(institute_abbreviation = fct_recode(institute_abbreviation,
                                             Madrid = "UAM",
                                             Catalonia = "SJD"),
         covid19_1 = fct_collapse(covid19_1,
                                  "Yes" = c("Probable", "Yes"))) %>%
  tbl_summary(by = randomization_group,
              statistic = all_continuous() ~ "{mean} ({sd})",
              digits = all_continuous() ~ c(1,1),
              missing = "no"
  ) %>%
  add_overall() %>%
  modify_caption(caption = "Table 1. Characteristics of the participants") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Group**") %>%
  modify_footnote(
    all_stat_cols() ~ "Mean (SD) or Frequency (%)"
  )


table2 <-
  ds_long %>%
  select(randomization_group, time,
         phqads_t,
         phq9_t,
         gad7_t,
         pcl5_t,
         phq9_cut,
         gad7_cut) %>%
  mutate(time = fct_recode(time,
                           "Baseline" = "1",
                           "Week 7 (post DWM)" = "2",
                           "Week 13 (post PM+)" = "3",
                           "Week 21 (follow-up)" = "4")) %>%
  tbl_strata(
    strata = time,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(by = randomization_group,
                  statistic = all_continuous() ~ "{mean} ({sd})",
                  digits = all_continuous() ~ c(1,2),
                  missing = "ifany",
                  missing_text = "Missing") %>%
      modify_header(label ~ "**Outcome**") %>%
      modify_caption(caption =
                       "Table 2. Mental health outcomes by randomisation group and time") %>%
      modify_footnote(label ~
      "PHQ-ADS = Patient Health Questionnaire -
      Anxiety and Depression Scale;
      PHQ -9 = Patient Health Questionnaire Scale;
      GAD-7 = Generalised Anxiety Disorder Scale;
      PCL-5 = PTSD Checklist for DSM-5",
                      all_stat_cols() ~ "Mean (SD) or Frequency (%)"))

##### Plots ####

plots_ds_long <- rRespond_get_plots(ds_long) # FIXME: not working properly

#### Models ####

# The following function can be used on any RESPOND dataset
# to run the baseline- and fully adjusted linear mixed models.
# For our primary analysis, I run the function on the full dataset,
# i.e., on all observations from all randomised participants.

models_ds_long <- rRespond_get_models_baselinecov(ds_long)
