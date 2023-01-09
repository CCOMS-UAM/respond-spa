respond_wp4_pack <- list("devtools",
                         "readr",
                      "janitor",
                      "lubridate",
                      "sjlabelled",
                      "stringr",
                      "tidyverse",
                      "readr",
                      "janitor",
                      "lubridate",
                      "naniar",
                      "finalfit",
                      "flextable",
                      "gtsummary",
                      "effectsize",
                      "knitr",
                      "lme4",
                      "clubSandwich",
                      "performance",
                      "emmeans",
                      "forestplot",
                      "ggplotify",
                      "ggpubr",
                      "patchwork",
                      "lmerTest", 
                      "nntcalc",
                      "genodds")

lapply(respond_wp4_pack, require, character.only = TRUE)
remove(respond_wp4_pack)

# install.packages("remotes")
# remotes::install_github("MathiasHarrer/dmetar")
# This will probably need to be installed differently.
# TODO: keep track
