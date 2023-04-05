# lse

ds_long %>%
  select(contains(c("csri_","eq5d5l_"))) %>%
  skimr::skim()

ds_wide %>%
  select(contains("eq5d5l_")) %>%
  skimr::skim()

# export

haven::write_dta(ds_long,
                 "output/respond_spa_long.dta")
haven::write_dta(ds_wide,
                 "output/respond_spa_wide.dta")
haven::write_sav(ds_long,
                 "output/respond_spa_long.sav")
haven::write_sav(ds_wide,
                 "output/respond_spa_wide.sav")


# test

haven::read_dta("output/respond_spa_long.dta") %>%
  str()

haven::read_sav("output/respond_spa_long.dta")

skimr::skim(ds_wide)
