# R Code for RESPOND Spain - Resilience analysis #
# Author: Roberto Mediavilla (roberto.mediavilla@uam.es) #
# Date: March 2023 #

#### Readme ####

# To conduct the resilience analysis on the RESPOND-Spain dataset,
# we need to introduce some changes to the stressor data collected
# in Barcelona.

#### Source data ####

# About the data

# This code is intended to work on the ds_long dataset. To get that
# dataset, please source the following files:

source("src/req_pack.R")
source("src/data_cleaning.R")

# Read new data from Barcelona

stress_bcn <-
  read_delim("dat/BCN/Quality control_LIR.csv", delim = ";") %>%
  select(-19) # bug

#### Target data structure vs actual data structure ####

ds_long %>%
  select(castor_record_id,
         time,
         starts_with(c("covid19",
                       "le_",
                       "btq_")),
         -covid19_2,
         -covid19_3) %>%
  str()

str(stress_bcn)

#### Data transformation ####

stress_bcn <-
  stress_bcn %>%
  rename_with(.,
              ~str_replace(., "_NEW", ""),
              ends_with("_NEW")) %>%  # old names
  rename(castor_record_id = Record_Id,
         covid19_1_1 = covid19_01_1,
         btq_10 = btq_010,
         time = wave)

stress_bcn <-
  stress_bcn %>%
  mutate(
    across(
      .cols = contains("le_"),
      .fns = ~recode(.,
                     "No me ha sucedido" = 0,
                     "No ha impactado en absoluto" = 1,
                     "Ha impactado un poco" = 2,
                     "Ha impactado moderadamente" = 3,
                     "Ha impactado moderadamen" = 3, # typo
                     "Ha impactado gravemente" = 4) %>%
        as.integer()
    ),
    across(
      .cols = contains("btq_"),
      .fns = ~recode(., "No" = 0, "Sí"= 1) %>%
        as.integer()
    ),
    time = as.character(time),
    across(
      .cols = starts_with("covid19_"),
      .fns = as.factor
    ))

stress_bcn <-
  stress_bcn %>%
  mutate(covid19_1 = fct_recode(covid19_1,
                                "No" =
                                  "No",
                                 "Probable" =
                                  "Probablemente, pero no tengo ninguna prueba formal
                                (ej., antígenos, PCR)",
                                 "Yes" = "Sí, confirmado por pruebas formales de COVID-19 (ej., antígenos, PCR)"),
         covid19_1_1 = fct_recode(covid19_1_1,
                                  "No or mild symptoms" = "Sin síntomas o con síntomas leves que no requirieron hospitalización",
                                  "Moderate symptoms" = "Síntomas moderados (fiebre más de dos días y dolor de garganta) que no requirieron hospitalización",
                                  "Severe symptoms" = "Síntomas graves que requirieron hospitalización"),
         covid19_4 = fct_recode(covid19_4,
                                "No" = "No",
                                "Yes" = "Sí"))

stress_bcn <-
  stress_bcn %>%
  var_labels(covid19_1 = "COVID-19 infection (ever)",
             covid19_4 = "Closed ones died due to COVID-19",
             castor_record_id = "Participant ID")



#### Data combination ####

ds_long_resil <- ds_long

ds_long_resil <-
  ds_long_resil %>%
  left_join(., stress_bcn, by = c("castor_record_id", "time"))

ds_long_resil %>%
  group_by(institute_abbreviation) %>%
  select(castor_record_id,
         time,
         institute_abbreviation,
         starts_with(c("covid19",
                       "le_",
                       "btq_")),
         -covid19_2,
         -covid19_3) %>%
  skimr::skim() # check

bcn <- split(ds_long_resil, ds_long_resil$institute_abbreviation)$SJD
mad <- split(ds_long_resil, ds_long_resil$institute_abbreviation)$UAM

bcn <-
  bcn %>%
  rename_with(., ~str_replace(., "\\.y", "")) %>%
  select(-ends_with(".x"))

mad <-
  mad %>%
  rename_with(., ~str_replace(., "\\.x", "")) %>%
  select(-ends_with(".y"))

ds_long_test <- rbind(bcn, mad)

remove(mad, bcn)

#### Final tests ####

ds_long %>%
  group_by(institute_abbreviation) %>%
  select(castor_record_id,
         time,
         starts_with(c("covid19",
                       "le_",
                       "btq_")),
         -covid19_2,
         -covid19_3) %>%
  skimr::skim()

ds_long_test %>%
    group_by(institute_abbreviation) %>%
    select(castor_record_id,
         time,
         starts_with(c("covid19",
                       "le_",
                       "btq_")),
         -covid19_2,
         -covid19_3) %>%
  skimr::skim() # looks good

#### New dataset ####

ds_long <- ds_long_test

remove(ds_long_resil,
       ds_long_test,
       stress_bcn)

#### pivot_wider ####

ds_wide <-
  ds_long %>%
  pivot_wider(.,
              id_cols = c(castor_record_id,
                          institute_abbreviation,
                          randomization_group),
              names_from = time,
              names_glue = "{.value}_t{time}",
              values_from = c(soc_01:btq_10))

# saveRDS(ds_wide, "dat/ds_long_resil.Rds")
