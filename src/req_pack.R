# Packages required to run scripts and functions (.R files)

respond_wp4_pack <-
  list("huxtable",
       "devtools",
       "readr",
       "janitor",
       "lubridate",
       "sjlabelled",
       "stringr",
       "tidyverse",
       "kableExtra",
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
       "genodds")

lapply(respond_wp4_pack, require, character.only = TRUE)

remove(respond_wp4_pack)
