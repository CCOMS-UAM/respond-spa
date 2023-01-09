# RESPOND-HCWs - Required functions (R code)
# 
# Author: Roberto Mediavilla (roberto.mediavilla@uam.es)
# Date: October 2022
# 
# README
# 
# Functions required for the RESPOND-HCWs analyses:
#   * rRespond_get_plots (returns a list with plots
#   based on descriptive data)
#   * rRespond_get_models(returns a list of objects
#   that summarise linear mixed models across all outcomes)
#   * rRespond_get_models (returns a table that summarises
#   logistic mixed models on the PHQ-9 and the GAD-7 binary scores)
#   
# The following scripts must be sourced:
#   * req_pack.R (load required packages)

rRespond_get_plots <- function(df) {
  
  #### Tables ####
  
  tbl_plots <- 
    df %>% 
    mutate(time = fct_recode(time,
                             "Baseline" = "1",
                             "Week 7 (post DWM)" = "2",
                             "Week 13 (post PM+)" = "3",
                             "Week 21 (follow-up)" = "4")) %>% 
    group_by(time,
             randomization_group) %>% 
    summarise(
      across(
        .cols = c(phqads_t, phq9_t, gad7_t, pcl5_t),
        .fns = c(n = ~ sum(!is.na(.)),
                 mean = mean,
                 sd = sd,
                 se = ~ sd(., na.rm = T) / sqrt(sum(!is.na(.)))),
        na.rm = T,
        .names = "{.fn}_{.col}"
      )
    )
  
  tbl_k10_1_t1 <- 
    df %>% 
    filter(time == 1,
           randomization_group == "Intervention") %>% 
    summarise(mean_k10_t1 = mean(k10_score, na.rm = T),
              sd_k10_t1 = sd(k10_score, na.rm = T),
              low_k10_t1 = mean_k10_t1 - sd_k10_t1,
              upp_k10_t1 = mean_k10_t1 + sd_k10_t1) %>% 
    mutate(across(where(is.numeric), round, 1))
  
  
  tbl_k10_1_t2 <- 
    df %>% 
    filter(time == 2,
           randomization_group == "Intervention") %>% 
    summarise(mean_k10_t2 = mean(k10_score, na.rm = T),
              sd_k10_t2 = sd(k10_score, na.rm = T),
              low_k10_t2 = mean_k10_t2 - sd_k10_t2,
              upp_k10_t2 = mean_k10_t2 + sd_k10_t2) %>% 
    mutate(across(where(is.numeric), round, 1))
  
  tbl_k10_2_t1 <- 
    df %>% 
    filter(time == 1,
           randomization_group == "Control") %>% 
    summarise(mean_k10_t1 = mean(k10_score, na.rm = T),
              sd_k10_t1 = sd(k10_score, na.rm = T),
              low_k10_t1 = mean_k10_t1 - sd_k10_t1,
              upp_k10_t1 = mean_k10_t1 + sd_k10_t1) %>% 
    mutate(across(where(is.numeric), round, 1))
  
  
  tbl_k10_2_t2 <- 
    df %>% 
    filter(time == 2,
           randomization_group == "Control") %>% 
    summarise(mean_k10_t2 = mean(k10_score, na.rm = T),
              sd_k10_t2 = sd(k10_score, na.rm = T),
              low_k10_t2 = mean_k10_t2 - sd_k10_t2,
              upp_k10_t2 = mean_k10_t2 + sd_k10_t2) %>% 
    mutate(across(where(is.numeric), round, 1))
  
  df <- 
    df %>% 
    select(castor_record_id,
           time,
           k10_score) %>% 
    pivot_wider(names_from = time,
                values_from = k10_score,
                names_prefix = "t") %>% 
    select(castor_record_id:t2) %>% 
    mutate(k10_change = case_when(t1-t2>0 ~ "better",
                                  t1-t2<0 ~"worse",
                                  NA ~ NA_character_,
                                  t1==t2 ~ "no change") %>% 
             as_factor()
    ) %>% 
    select(castor_record_id,
           k10_change)  %>% 
    right_join(.,
               df,
               by = "castor_record_id") %>% 
    relocate(k10_change,
             .after = k10_score)
  
  #### Plots ####
  
  phqads_plot <- 
    tbl_plots %>% 
    ggplot(aes(x = time,
               y = mean_phqads_t,
               group = randomization_group,
               colour = randomization_group)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = mean_phqads_t - se_phqads_t,
                      ymax = mean_phqads_t + se_phqads_t),
                  width= .2) +
    labs(x = "Time",
         y = "PHQ-ADS score",
         colour = "Group",
         title = "Anxiety/Depression (primary outcome)") +
    scale_color_grey() +
    ylim(0,48)
  
  phq9_plot <- 
    tbl_plots %>% 
    ggplot(aes(x = time,
               y = mean_phq9_t,
               group = randomization_group,
               colour = randomization_group)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = mean_phq9_t - se_phq9_t,
                      ymax = mean_phq9_t + se_phq9_t),
                  width= .2) +
    labs(x = "Time",
         y = "PHQ-9 score",
         colour = "Group",
         title = "Depression") +
    scale_color_grey() +
    ylim(0,27)
  
  gad7_plot <- 
    tbl_plots %>% 
    ggplot(aes(x = time,
               y = mean_gad7_t,
               group = randomization_group,
               colour = randomization_group)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = mean_gad7_t - se_gad7_t,
                      ymax = mean_gad7_t + se_gad7_t),
                  width= .2) +
    labs(x = "Time",
         y = "GAD-7 score",
         colour = "Group",
         title = "Anxiety") +
    scale_color_grey() +
    ylim(0,21)
  
  pcl5_plot <- 
    tbl_plots %>% 
    ggplot(aes(x = time,
               y = mean_pcl5_t,
               group = randomization_group,
               colour = randomization_group)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = mean_pcl5_t - se_pcl5_t,
                      ymax = mean_pcl5_t + se_pcl5_t),
                  width= .2) +
    labs(x = "Time",
         y = "PCL-5 score",
         colour = "Group",
         title = "Posttraumatic stress") +
    scale_color_grey() +
    ylim(0,32) -> pcl5_plot
  
  #### Out ####
  
  out_plots <- 
    ggpubr::ggarrange(phqads_plot, phq9_plot, gad7_plot, pcl5_plot,
                      nrow = 2,           
                      ncol = 2,
                      common.legend = TRUE)
  
  k10_plot <- 
    df %>% 
    mutate(k10_change = fct_recode(k10_change,
                                   NULL = NA_character_),
           k10_change = fct_infreq(k10_change),
           time = fct_recode(time,
                             "Baseline" = "1",
                             "Week 7 (post DWM)" = "2"),
           k10_change = fct_recode(k10_change,
                                   "Better" = "better",
                                   "Worse" = "worse",
                                   "No change" = "no change"))  %>%
    filter(time == c("Baseline",
                     "Week 7 (post DWM)"),
           !is.na(k10_change)) %>% 
    ggplot(aes(x = k10_score,
               y = reorder(castor_record_id, 
                           k10_score))) +
    geom_line(aes(group = castor_record_id,
                  colour = k10_change),
              size = 0.8) +
    scale_color_manual(values= c("Better" = "green",
                                 "Worse" = "red",
                                 "No change" = "grey")) +
    geom_point(aes(shape = time),
               alpha = 0.5) +
    facet_grid(rows = vars(randomization_group)) +
    labs(y = "Participant",
         x = "K-10 score",
         color = "Change in K-10 score",
         shape = "Time",
         title = "Changes in psychological distress (K-10 scores) at T2 (post DWM)") + 
    geom_rect(xmin = 8,
              xmax = 16,
              ymin = -Inf,
              ymax = Inf,
              fill = "grey", 
              alpha = .01) +
    geom_text(x = 12,
              y = 150,
              label = "K-10 < 16",
              vjust = -1,
              nudge_y = 0.5,
              angle = 0,
              colour = "white") +
    theme_classic() +
    theme(legend.position = "bottom",
          axis.text.y = element_blank())
  
  out_list <- list("out_plots" = out_plots,
                   "k10_plot" = k10_plot)
  
  return(out_list)
}
rRespond_get_models <- function(df){

#### Crude models ####
  
fit <- 
  df %>% 
  select(phqads_t,
         phq9_t,
         gad7_t,
         pcl5_t) %>% 
  map(~ lmer(.x ~ randomization_group*time + (1|castor_record_id),
             df))

vcCR <- map(fit,
                ~ vcovCR(., type = "CR2")) # OK

emm <- 
  map2(fit, vcCR, 
       ~ emmeans(.x,
                 "randomization_group",
                 by = "time",
                 vcov. = .y,
                 lmer.df = "satterthwaite")) # OK

emm_pw <- 
  map2(fit, vcCR, 
       ~ emmeans(.x,
                 pairwise ~ randomization_group,
                 by = "time",
                 vcov. = .y,
                 lmer.df = "satterthwaite")) # OK

emm_for_plots <- 
  map(emm_pw,
      ~ unlist(.[1:4]$emmeans)) # lo tengo creo

emm_plots <- 
  map2(emm_pw, emm_for_plots,
       ~ emmip(.y,
               randomization_group ~ time, 
               CIs = TRUE)) # nailed it!

edf_es <-  
  map(emm,
      ~summary(.x)$df %>% min(.))

es <- 
  map2(fit, 
       emm,
       ~ eff_size(.y, sigma = sigma(.x), edf = min(summary(pairs(.y))$df)))

# tidy datasets

emm_tidy <- 
  emm_for_plots %>% 
  map_dfr(.,
          ~ broom.mixed::tidy(.,
                              conf.int = TRUE),
          .id = "outcome") %>% 
  mutate(adj = "crude",
         type = "emm")

emm_pw_tidy <- 
  emm_pw %>% 
  map(., ~ unlist(.[[2]])) %>% 
  map_dfr(., ~ broom::tidy(., conf.int = TRUE),
          .id = "outcome") %>% 
  mutate(adj = "crude",
         type = "pwemm")

es_tidy <- 
  es %>% 
  map_dfr(., ~ broom::tidy(.,
                           conf.int = TRUE),
          .id = "outcome") %>% 
  mutate(adj = "crude",
         type = "cohensd")

# Plot primary outcome

plot_emm_phqads <- 
  emm_tidy %>% 
  filter(outcome == "phqads_t") %>% 
  mutate(randomization_group = factor(randomization_group,
                                      levels = c("Intervention", "Control"))) %>% 
  ggplot(aes(x = time,
             y = estimate,
             group = randomization_group,
             colour = randomization_group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low,
                    ymax = conf.high),
                width = 0.1) +
  labs(x = "Time",
       y = "PHQ-ADS score",
       colour = "Group",
       title = "Crude estimated marginal means of the PHQ-ADS total score (primary outcome)") +
  scale_color_grey() +
  ylim(0,48)

es_fplot <- 
  es_tidy %>% 
  filter(time != 1) %>%
  select(outcome, time, estimate, conf.low, conf.high) %>% 
  mutate(
    across(
      .cols = where(is.numeric),
      .fns = round, 2
    ),
    main = paste0(estimate,
                  " (",
                  conf.low,
                  ", ",
                  conf.high,
                  ")"),
    mean = estimate,
    lower = conf.low,
    upper = conf.high) %>%
  mutate(outcome = fct_recode(outcome,
                              "PHQ-ADS" = "phqads_t",
                              "PHQ-9" = "phq9_t",
                              "GAD-7" = "gad7_t",
                              "PCL-5" = "pcl5_t"),
         time = fct_recode(time,
                           "Week 7 (post DWM)" = "2",
                           "Week 13 (post PM+)" = "3",
                           "Week 21 (follow-up)" = "4"),
         outcome_bis = outcome) %>%
  arrange(desc(time)) %>% 
  forestplot(labeltext = c(time, outcome, main),
             xlab = "Cohen's d", 
             title = "Effect sizes (crude models)",
             xticks = c(-0.5, 0, 0.3, 0.6, 0.9, 1.2, 1.5, 1.8),
             boxsize = 0.2,
             graph.pos = 3,
             align = "l") %>%
  fp_set_style(axes = gpar(lower_limit = -2.0)) %>% 
  fp_add_header("Time",
                "Outcome",
                "Cohen's d (95% CI)")

#### Adjusted models ####

df_adj <- 
  df %>% 
  group_by(castor_record_id) %>% 
  mutate(
    across(
      .cols = starts_with(c("soc_", "covid", "csri")),
      .fns = ~ .[time=1]
    ),
    phqads_t_cov = phqads_t[time=1],
    phq9_t_cov = phq9_t[time=1],
    gad7_t_cov = gad7_t[time=1],
    pcl5_t_cov = pcl5_t[time=1],
    mh_cov = if_else(csri_sp_mental_g_n == "0"
                     & csri_sp_mental_i_n == "0",
                     0,
                     1)
    ) %>% 
  ungroup()

fit_adj_phqads <- 
  df_adj %>% 
  lmer(phqads_t ~ randomization_group*time + (1|castor_record_id)
       + phqads_t_cov
       + institute_abbreviation
       + soc_01 # gender
       + soc_02 # year of birth
       + soc_12 # educational level
       + mh_cov, # consultations with MH services at baseline
       data = .) 

fit_adj_phq9 <- 
  df_adj %>% 
  lmer(phq9_t ~ randomization_group*time + (1|castor_record_id)
       + phq9_t_cov
       + institute_abbreviation
       + soc_01 # gender
       + soc_02 # year of birth
       + soc_12 # educational level
       + mh_cov, # consultations with MH services at baseline
       data = .)

fit_adj_gad7 <- 
  df_adj %>% 
  lmer(gad7_t ~ randomization_group*time + (1|castor_record_id)
       + gad7_t_cov
       + institute_abbreviation
       + soc_01 # gender
       + soc_02 # year of birth
       + soc_12 # educational level
       + mh_cov, # consultations with MH services at baseline
       data = .) 

fit_adj_pcl5 <- 
  df_adj %>% 
  lmer(pcl5_t ~ randomization_group*time + (1|castor_record_id)
       + pcl5_t_cov
       + institute_abbreviation
       + soc_01 # gender
       + soc_02 # year of birth
       + soc_12 # educational level
       + mh_cov, # consultations with MH services at baseline
       data = .) 

fit_adj <- 
  list(phqads_t = fit_adj_phqads,
       phq9_t = fit_adj_phq9,
       gad7_t = fit_adj_gad7,
       pcl5_t = fit_adj_pcl5)

vcCR_adj <- 
  map(fit_adj,
      ~ vcovCR(., type = "CR2")) # OK

emm_adj <- 
  map2(fit_adj, vcCR_adj, 
       ~ emmeans(.x,
                 "randomization_group",
                 by = "time",
                 vcov. = .y,
                 lmer.df = "satterthwaite")) # OK

emm_pw_adj <- 
  map2(fit_adj, vcCR_adj, 
       ~ emmeans(.x,
                 pairwise ~ randomization_group,
                 by = "time",
                 vcov. = .y,
                 lmer.df = "satterthwaite")) # OK

emm_for_plots_adj <- 
  map(emm_pw_adj,
      ~ unlist(.[1:4]$emmeans)) # lo tengo creo

emm_plots_adj <- 
  map2(emm_pw_adj, emm_for_plots_adj,
       ~ emmip(.y,
               randomization_group ~ time, 
               CIs = TRUE)) # nailed it!

edf_es_adj <-  
  map(emm_adj,
      ~summary(.x)$df %>% min(.))

es_adj <- 
  map2(fit_adj, 
       emm_adj,
       ~ eff_size(.y, 
                  sigma = sigma(.x), 
                  edf = min(summary(pairs(.y))$df)))

# tidy datasets

emm_adj_tidy <- 
  emm_for_plots_adj %>% 
  map_dfr(.,
          ~ broom.mixed::tidy(.,
                              conf.int = TRUE),
          .id = "outcome") %>% 
  mutate(adj = "adjusted",
         type = "emm")

emm_pw_adj_tidy <- 
  emm_pw_adj %>% 
  map(., ~ unlist(.[[2]])) %>% 
  map_dfr(., ~ broom::tidy(., conf.int = TRUE),
          .id = "outcome")%>% 
  mutate(adj = "adjusted",
         type = "pwemm")

es_adj_tidy <- 
  es_adj %>% 
  map_dfr(., ~ broom::tidy(.,
                           conf.int = TRUE),
          .id = "outcome") %>% 
  mutate(adj = "adjusted",
         type = "cohensd")

# plot primary outcome

plot_emm_phqads_adj <- 
  emm_adj_tidy %>% 
  filter(outcome == "phqads_t") %>% 
  mutate(randomization_group = factor(randomization_group,
                                      levels = c("Intervention", "Control"))) %>% 
  ggplot(aes(x = time,
             y = estimate,
             group = randomization_group,
             colour = randomization_group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low,
                    ymax = conf.high),
                width = 0.1) +
  labs(x = "Time",
       y = "PHQ-ADS score",
       colour = "Group",
       title = "Adjusted estimated marginal means of the PHQ-ADS total score (primary outcome)") +
  scale_color_grey() +
  ylim(0,48)

# plot effect size

es_adj_fplot <- 
  es_adj_tidy %>% 
  filter(time != 1) %>%
  select(outcome, time, estimate, conf.low, conf.high) %>% 
  mutate(
    across(
      .cols = where(is.numeric),
      .fns = round, 2
    ),
    main = paste0(estimate,
                  " (",
                  conf.low,
                  ", ",
                  conf.high,
                  ")"),
    mean = estimate,
    lower = conf.low,
    upper = conf.high) %>%
  mutate(outcome = fct_recode(outcome,
                              "PHQ-ADS" = "phqads_t",
                              "PHQ-9" = "phq9_t",
                              "GAD-7" = "gad7_t",
                              "PCL-5" = "pcl5_t"),
         time = fct_recode(time,
                           "Week 7 (post DWM)" = "2",
                           "Week 13 (post PM+)" = "3",
                           "Week 21 (follow-up)" = "4"),
         outcome_bis = outcome) %>%
  arrange(desc(time)) %>% 
  forestplot(labeltext = c(time, outcome, main),
             xlab = "Cohen's d",
             title = "Effect sizes (adjusted models)",
             xticks = c(-0.5, 0, 0.3, 0.6, 0.9, 1.2, 1.5, 1.8),
             boxsize = 0.2,
             graph.pos = 3,
             align = "l") %>% 
  fp_add_header("Time",
                "Outcome" %>% fp_align_left(),
                "Cohen's d (95% CI)")

#### Out ####

full_summary <- # output
  emm_tidy %>% 
  bind_rows(., emm_adj_tidy) %>% 
  bind_rows(., emm_pw_tidy) %>% 
  bind_rows(., emm_pw_adj_tidy) %>% 
  bind_rows(., es_tidy) %>% 
  bind_rows(., es_adj_tidy) %>% 
  relocate(adj, 
           type, 
           outcome, 
           time, 
           randomization_group,
           estimate, 
           conf.low, 
           conf.high) %>% 
  select(!c(term, contrast, null.value)) # %>% mutate(model = "itt")


full_summary_tidy <-
  full_summary %>% 
  filter(type != "cohensd") %>% 
  mutate(
    across(
      where(is.numeric),
      round, 1),
    out = paste0(estimate,
                 " (",
                 conf.low,
                 ", ",
                 conf.high,
                 ")")) %>% 
  select(adj:randomization_group,
         out) %>% 
  filter(time != 1) %>%  # Simplest model before pivoting
  pivot_wider(.,
              names_from = c(adj,
                             type,
                             randomization_group),
              values_from = out) %>%
  relocate(crude_pwemm_NA, .after = crude_emm_Intervention) %>% 
  mutate(outcome = factor(outcome,
                          levels = c("phqads_t",
                                     "phq9_t",
                                     "gad7_t",
                                     "pcl5_t")),
         outcome = fct_recode(outcome,
                              "PHQ-ADS" = "phqads_t",
                              "PHQ-9" = "phq9_t",
                              "GAD-7" = "gad7_t",
                              "PCL-5" = "pcl5_t"),
         time = fct_recode(time,
                           "Week 7 (post DWM)" = "2",
                           "Week 13 (post PM+)" = "3",
                           "Week 21 (follow-up)" = "4")
  ) %>% 
  arrange(outcome, desc(time))

main_summary <- #FIXME: fontsize
  full_summary_tidy %>% 
  flextable() %>% 
  add_header_row(.,
                 values = c("", "Crude models", "Adjusted models"),
                 colwidths = c(2, 3, 3)) %>% 
  set_header_labels(.,
                    outcome = "",
                    time = "",
                    crude_emm_Control = "Control",
                    crude_emm_Intervention = "Intervention",
                    crude_pwemm_NA = "Difference",
                    adjusted_emm_Control = "Control",
                    adjusted_emm_Intervention = "Intervention",
                    adjusted_pwemm_NA = "Difference") %>% 
  add_footer_lines(.,
                   value = list("Note.",
                                paste0("All models include participant as a random effect. ",
                                       "All confidence intervals calculated using robust standard errors."),
                                paste0("PHQ-ADS = Patient Health Questionnaire - Anxiety and Depression Scale; ",
                                       "PHQ-9 = 9-item Patient Health Questionnaire; ",
                                       "GAD-7 = 7-item Generalised Anxiety Disorder; ",
                                       "PCL-5 = PTSD checklist for DSM-5; ",
                                       "PM+ = Problem Management Plus; ",
                                       "DWM = Doing What Matters.")
                   )
  ) %>% 
  footnote(.,
           i = 1,
           j = c(6:8),
           value = as_paragraph(
             as_chunk("Adjusted for age, gender, level of education, "),
             as_chunk("use of mental health services prior to enrolment, "),
             as_chunk("outcome score measured at baseline, and site.")),
           ref_symbols = "†",
           part = "header",
           inline = FALSE) %>% 
  merge_v(., j = 1) %>% 
  valign(., j = 1, valign = "top", part = "body") %>%
  align(., part = "header", align = "center") %>%   
  padding(.,
          i = 1,
          j = 2:8,
          padding.top = 30) %>%
  padding(.,
          i = 4,
          j = 2:8,
          padding.top = 30) %>% 
  padding(.,
          i = 7,
          j = 2:8,
          padding.top = 30) %>% 
  padding(.,
          i = 10,
          j = 2:8,
          padding.top = 30) %>% 
  align(.,
         j = 3:8,
         align = "center",
         part = "body") %>% 
  set_caption(., 
              caption = paste("Table 3. Estimated marginal means and ",
                              "95 percent confidence intervals"), 
              align_with_table = FALSE) %>% 
  fontsize(., size = 10, part = "body") %>%
  fontsize(., size = 10, part = "header") %>% 
  fontsize(., size = 8.5, part = "footer") %>% 
  fix_border_issues() %>%
  autofit()

p1 <- grid2grob(print(es_fplot))
p2 <- grid2grob(print(es_adj_fplot))
cohen_plot <- wrap_elements(p1) / wrap_elements(p2) # output

phqads_plot <-
  ggpubr::ggarrange(plot_emm_phqads,
                  plot_emm_phqads_adj,
                  nrow = 2,
                  ncol = 1)

out_list <- list("full_summary" = full_summary,
                 "main_summary" = main_summary,
                 "cohen_plot" = cohen_plot,
                 "phqads_plot" = phqads_plot)

return(out_list)

}
rRespond_get_models_log <- function(df){
  
  #### Crude models ####
  
  fit_log <- 
    df %>% 
    select(phq9_cut,
           gad7_cut,
           phqads_cut) %>% 
    mutate(phq9_cut = factor(phq9_cut,
                             levels = c("Yes", "No")),
           gad7_cut = factor(gad7_cut,
                             levels = c("Yes", "No")),
           phqads_cut = factor(phqads_cut,
                               levels = c("Yes", "No"))) %>% 
    map(~ glmer(formula = . ~ randomization_group*time + (1|castor_record_id),
                family = binomial(link = "logit"),
                data = df))
  
  summary_crude <- 
    map(fit_log,
        ~jtools::summ(.,
                      exp = TRUE,
                      confint = TRUE,
                      robust = FALSE, 
                      digits = 3))
  
  #### Adjusted models ####
  
  df_adj <- 
    df %>% 
    group_by(castor_record_id) %>% 
    mutate(
      across(
        .cols = starts_with(c("soc_", "covid", "csri")),
        .fns = ~ .[time=1]
      ),
      phqads_t_cov = phqads_t[time=1],
      phq9_t_cov = phq9_t[time=1],
      gad7_t_cov = gad7_t[time=1],
      pcl5_t_cov = pcl5_t[time=1],
      mh_cov = if_else(csri_sp_mental_g_n == "0"
                       & csri_sp_mental_i_n == "0",
                       0,
                       1)
    ) %>% 
    ungroup()
  
  fit_log_adj_phq9 <- 
    df_adj %>% 
    glmer(phq9_cut ~ randomization_group*time + (1|castor_record_id)
          + phq9_t_cov
          + institute_abbreviation
          + soc_01 # gender
          + soc_02 # year of birth
          + soc_12 # educational level
          + mh_cov, # consultations with MH services at baseline
          family = binomial(link = "logit"),
          data = .)
  
  fit_log_adj_gad7 <- 
    df_adj %>% 
    glmer(gad7_cut ~ randomization_group*time + (1|castor_record_id)
          + gad7_t_cov
          + institute_abbreviation
          + soc_01 # gender
          + soc_02 # year of birth
          + soc_12 # educational level
          + mh_cov, # consultations with MH services at baseline
          family = binomial(link = "logit"),
          data = .) 
  
  fit_log_adj_phqads <- 
    df_adj %>% 
    glmer(phqads_cut ~ randomization_group*time + (1|castor_record_id)
          + phqads_t_cov
          + institute_abbreviation
          + soc_01 # gender
          + soc_02 # year of birth
          + soc_12 # educational level
          + mh_cov, # consultations with MH services at baseline
          family = binomial(link = "logit"),
          data = .)
  
  fit_log_adj <- 
    list("phq9_cut" = fit_log_adj_phq9,
         "gad7_cut" = fit_log_adj_gad7,
         "phqads_cut" = fit_log_adj_phqads)
  
  summary_adj <- 
    map(fit_log_adj,
        ~jtools::summ(.,
                      exp = TRUE,
                      confint = TRUE,
                      robust = FALSE, 
                      digits = 3))
  
  #### Out ####
  
  main_summary_log <- 
    jtools::export_summs(summary_crude$phqads_cut,
                         summary_adj$phqads_cut,
                         summary_crude$phq9_cut, 
                         summary_adj$phq9_cut,
                         summary_crude$gad7_cut,
                         summary_adj$gad7_cut,
                         model.names = c("PHQ-ADS (crude)",
                                         "PHQ-ADS (adjusted)",
                                         "PHQ-9 (crude)",
                                         "PHQ-9 (adjusted)",
                                         "GAD-7 (crude)",
                                         "GAD-7 (adjusted)"),
                         error_format = "[{conf.low}, {conf.high}]",
                         error_pos = "below",
                         coefs = c("Intercept" = "(Intercept)", 
                                   "Group (Intervention)" = 
                                     "randomization_groupIntervention",
                                   "Time 2 (post DWM)" = 
                                     "time2",
                                   "Time 3 (post PM`+)" = 
                                     "time3",
                                   "Time 4 (2-month follow-up)" = 
                                     "time4",
                                   "Intervention at time 2" = 
                                     "randomization_groupIntervention:time2",
                                   "Intervention at time 3" = 
                                     "randomization_groupIntervention:time3",
                                   "Intervention at time 4" = 
                                     "randomization_groupIntervention:time4")
    )
  
  return(main_summary_log)
  
}

rRespond_get_models_baselinecov <- function(df){
  
  #### Crude models ####

  df <- 
    df %>% 
    group_by(castor_record_id) %>% 
    mutate(
      across(
        .cols = starts_with(c("soc_", "covid", "csri")),
        .fns = ~ .[time=1]
      ),
      phqads_t_cov = phqads_t[time=1],
      phq9_t_cov = phq9_t[time=1],
      gad7_t_cov = gad7_t[time=1],
      pcl5_t_cov = pcl5_t[time=1],
      mh_cov = if_else(csri_sp_mental_g_n == "0"
                       & csri_sp_mental_i_n == "0",
                       0,
                       1)
    ) %>% 
    ungroup()
  
  fit_phqads <- 
    df %>% 
    lmer(phqads_t ~ randomization_group*time + (1|castor_record_id)
         + phqads_t_cov,
         data = .) 
  
  fit_phq9 <- 
    df %>% 
    lmer(phq9_t ~ randomization_group*time + (1|castor_record_id)
         + phq9_t_cov,
         data = .)
  
  fit_gad7 <- 
    df %>% 
    lmer(gad7_t ~ randomization_group*time + (1|castor_record_id)
         + gad7_t_cov,
         data = .) 
  
  fit_pcl5 <- 
    df %>% 
    lmer(pcl5_t ~ randomization_group*time + (1|castor_record_id)
         + pcl5_t_cov,
         data = .) 
  
  fit <- 
    list(phqads_t = fit_phqads,
         phq9_t = fit_phq9,
         gad7_t = fit_gad7,
         pcl5_t = fit_pcl5)
  
  vcCR <- map(fit,
              ~ vcovCR(., type = "CR2")) # OK
  
  emm <- 
    map2(fit, vcCR, 
         ~ emmeans(.x,
                   "randomization_group",
                   by = "time",
                   vcov. = .y,
                   lmer.df = "satterthwaite")) # OK
  
  emm_pw <- 
    map2(fit, vcCR, 
         ~ emmeans(.x,
                   pairwise ~ randomization_group,
                   by = "time",
                   vcov. = .y,
                   lmer.df = "satterthwaite")) # OK
  
  emm_for_plots <- 
    map(emm_pw,
        ~ unlist(.[1:4]$emmeans)) # lo tengo creo
  
  emm_plots <- 
    map2(emm_pw, emm_for_plots,
         ~ emmip(.y,
                 randomization_group ~ time, 
                 CIs = TRUE)) # nailed it!
  
  edf_es <-  
    map(emm,
        ~summary(.x)$df %>% min(.))
  
  es <- 
    map2(fit, 
         emm,
         ~ eff_size(.y, sigma = sigma(.x), edf = min(summary(pairs(.y))$df)))
  
  # tidy datasets
  
  emm_tidy <- 
    emm_for_plots %>% 
    map_dfr(.,
            ~ broom.mixed::tidy(.,
                                conf.int = TRUE),
            .id = "outcome") %>% 
    mutate(adj = "crude",
           type = "emm")
  
  emm_pw_tidy <- 
    emm_pw %>% 
    map(., ~ unlist(.[[2]])) %>% 
    map_dfr(., ~ broom::tidy(., conf.int = TRUE),
            .id = "outcome") %>% 
    mutate(adj = "crude",
           type = "pwemm")
  
  es_tidy <- 
    es %>% 
    map_dfr(., ~ broom::tidy(.,
                             conf.int = TRUE),
            .id = "outcome") %>% 
    mutate(adj = "crude",
           type = "cohensd")
  
  # Plot primary outcome
  
  plot_emm_phqads <- 
    emm_tidy %>% 
    filter(outcome == "phqads_t") %>% 
    mutate(randomization_group = factor(randomization_group,
                                        levels = c("Intervention", "Control"))) %>% 
    ggplot(aes(x = time,
               y = estimate,
               group = randomization_group,
               colour = randomization_group)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = conf.low,
                      ymax = conf.high),
                  width = 0.1) +
    labs(x = "Time",
         y = "PHQ-ADS score",
         colour = "Group",
         title = "Baseline-adjusted estimated marginal means of the PHQ-ADS total score (primary outcome)") +
    scale_color_grey() +
    ylim(0,48)
  
  es_fplot <- 
    es_tidy %>% 
    filter(time != 1) %>%
    select(outcome, time, estimate, conf.low, conf.high) %>% 
    mutate(
      across(
        .cols = where(is.numeric),
        .fns = round, 2
      ),
      main = paste0(estimate,
                    " (",
                    conf.low,
                    ", ",
                    conf.high,
                    ")"),
      mean = estimate,
      lower = conf.low,
      upper = conf.high) %>%
    mutate(outcome = fct_recode(outcome,
                                "PHQ-ADS" = "phqads_t",
                                "PHQ-9" = "phq9_t",
                                "GAD-7" = "gad7_t",
                                "PCL-5" = "pcl5_t"),
           time = fct_recode(time,
                             "Week 7 (post DWM)" = "2",
                             "Week 13 (post PM+)" = "3",
                             "Week 21 (follow-up)" = "4"),
           outcome_bis = outcome) %>%
    arrange(desc(time)) %>% 
    forestplot(labeltext = c(time, outcome, main),
               xlab = "Cohen's d", 
               title = "Effect sizes (baseline-adjusted models)",
               xticks = c(-0.5, 0, 0.3, 0.6, 0.9, 1.2, 1.5, 1.8),
               boxsize = 0.2,
               graph.pos = 3,
               align = "l") %>%
    fp_set_style(axes = gpar(lower_limit = -2.0)) %>% 
    fp_add_header("Time",
                  "Outcome",
                  "Cohen's d (95% CI)")
  
  # New forest plot
  
  es_fplot_phqads <- 
    es_tidy %>% 
    filter(time != 1) %>%
    select(outcome, time, estimate, conf.low, conf.high) %>% 
    mutate(
      across(
        .cols = where(is.numeric),
        .fns = round, 1
      ),
      main = paste0(estimate,
                    " (",
                    conf.low,
                    ", ",
                    conf.high,
                    ")"),
      mean = estimate,
      lower = conf.low,
      upper = conf.high) %>%
    mutate(outcome = fct_recode(outcome,
                                "PHQ-ADS" = "phqads_t",
                                "PHQ-9" = "phq9_t",
                                "GAD-7" = "gad7_t",
                                "PCL-5" = "pcl5_t"),
           time = fct_recode(time,
                             "DWM completion (t2)" = "2",
                             "PM+ completion (t3)" = "3",
                             "2-month follow-up (t4)*" = "4"),
           outcome_bis = outcome) %>%
    arrange(desc(time)) %>% 
    filter(outcome == "PHQ-ADS") %>%
    forestplot(labeltext = c(time, main),
               xlab = "SES", 
               title = "Anxiety and depression symptoms (PHQ-ADS)**",
               xticks = c(-0.5, 0, 0.3, 0.6, 0.9, 1.2, 1.5, 1.8),
               boxsize = 0.1,
               graph.pos = 3,
               align = "l") %>%
    fp_set_style(axes = gpar(lower_limit = -2.0)) %>% 
    fp_add_header("",
                  "SES (95% CI)")
  
  es_fplot_phq <- 
    es_tidy %>% 
    filter(time != 1) %>%
    select(outcome, time, estimate, conf.low, conf.high) %>% 
    mutate(
      across(
        .cols = where(is.numeric),
        .fns = round, 1
      ),
      main = paste0(estimate,
                    " (",
                    conf.low,
                    ", ",
                    conf.high,
                    ")"),
      mean = estimate,
      lower = conf.low,
      upper = conf.high) %>%
    mutate(outcome = fct_recode(outcome,
                                "PHQ-ADS" = "phqads_t",
                                "PHQ-9" = "phq9_t",
                                "GAD-7" = "gad7_t",
                                "PCL-5" = "pcl5_t"),
           time = fct_recode(time,
                             "DWM completion (t2)" = "2",
                             "PM+ completion (t3)" = "3",
                             "2-month follow-up (t4)*" = "4"),
           outcome_bis = outcome) %>%
    arrange(desc(time)) %>% 
    filter(outcome == "PHQ-9") %>%
    forestplot(labeltext = c(time, main),
               xlab = "SES", 
               title = "Depression symptoms (PHQ-9)",
               xticks = c(-0.5, 0, 0.3, 0.6, 0.9, 1.2, 1.5, 1.8),
               boxsize = 0.1,
               graph.pos = 3,
               align = "l") %>%
    fp_set_style(axes = gpar(lower_limit = -2.0)) %>% 
    fp_add_header("",
                  "SES (95% CI)")
  
  es_fplot_gad <- 
    es_tidy %>% 
    filter(time != 1) %>%
    select(outcome, time, estimate, conf.low, conf.high) %>% 
    mutate(
      across(
        .cols = where(is.numeric),
        .fns = round, 1
      ),
      main = paste0(estimate,
                    " (",
                    conf.low,
                    ", ",
                    conf.high,
                    ")"),
      mean = estimate,
      lower = conf.low,
      upper = conf.high) %>%
    mutate(outcome = fct_recode(outcome,
                                "PHQ-ADS" = "phqads_t",
                                "PHQ-9" = "phq9_t",
                                "GAD-7" = "gad7_t",
                                "PCL-5" = "pcl5_t"),
           time = fct_recode(time,
                             "DWM completion (t2)" = "2",
                             "PM+ completion (t3)" = "3",
                             "2-month follow-up (t4)*" = "4"),
           outcome_bis = outcome) %>%
    arrange(desc(time)) %>% 
    filter(outcome == "GAD-7") %>%
    forestplot(labeltext = c(time, main),
               xlab = "SES", 
               title = "Anxiety symptoms (GAD-7)",
               xticks = c(-0.5, 0, 0.3, 0.6, 0.9, 1.2, 1.5, 1.8),
               boxsize = 0.1,
               graph.pos = 3,
               align = "l") %>%
    fp_set_style(axes = gpar(lower_limit = -2.0)) %>% 
    fp_add_header("",
                  "SES (95% CI)")
  
  es_fplot_ptsd <- 
    es_tidy %>% 
    filter(time != 1) %>%
    select(outcome, time, estimate, conf.low, conf.high) %>% 
    mutate(
      across(
        .cols = where(is.numeric),
        .fns = round, 1
      ),
      main = paste0(estimate,
                    " (",
                    conf.low,
                    ", ",
                    conf.high,
                    ")"),
      mean = estimate,
      lower = conf.low,
      upper = conf.high) %>%
    mutate(outcome = fct_recode(outcome,
                                "PHQ-ADS" = "phqads_t",
                                "PHQ-9" = "phq9_t",
                                "GAD-7" = "gad7_t",
                                "PCL-5" = "pcl5_t"),
           time = fct_recode(time,
                             "DWM completion (t2)" = "2",
                             "PM+ completion (t3)" = "3",
                             "2-month follow-up (t4)*" = "4"),
           outcome_bis = outcome) %>%
    arrange(desc(time)) %>% 
    filter(outcome == "PCL-5") %>%
    forestplot(labeltext = c(time, main),
               xlab = "SES", 
               title = "Posttraumatic stress symptoms (PCL-5)",
               xticks = c(-0.5, 0, 0.3, 0.6, 0.9, 1.2, 1.5, 1.8),
               boxsize = 0.1,
               graph.pos = 3,
               align = "l") %>%
    fp_set_style(axes = gpar(lower_limit = -2.0)) %>% 
    fp_add_header("",
                  "SES (95% CI)")
  
  #### Adjusted models ####
  
  df_adj <- 
    df %>% 
    group_by(castor_record_id) %>% 
    mutate(
      across(
        .cols = starts_with(c("soc_", "covid", "csri")),
        .fns = ~ .[time=1]
      ),
      phqads_t_cov = phqads_t[time=1],
      phq9_t_cov = phq9_t[time=1],
      gad7_t_cov = gad7_t[time=1],
      pcl5_t_cov = pcl5_t[time=1],
      mh_cov = if_else(csri_sp_mental_g_n == "0"
                       & csri_sp_mental_i_n == "0",
                       0,
                       1)
    ) %>% 
    ungroup()
  
  fit_adj_phqads <- 
    df_adj %>% 
    lmer(phqads_t ~ randomization_group*time + (1|castor_record_id)
         + phqads_t_cov
         + institute_abbreviation
         + soc_01 # gender
         + soc_02 # year of birth
         + soc_12 # educational level
         + mh_cov, # consultations with MH services at baseline
         data = .) 
  
  fit_adj_phq9 <- 
    df_adj %>% 
    lmer(phq9_t ~ randomization_group*time + (1|castor_record_id)
         + phq9_t_cov
         + institute_abbreviation
         + soc_01 # gender
         + soc_02 # year of birth
         + soc_12 # educational level
         + mh_cov, # consultations with MH services at baseline
         data = .)
  
  fit_adj_gad7 <- 
    df_adj %>% 
    lmer(gad7_t ~ randomization_group*time + (1|castor_record_id)
         + gad7_t_cov
         + institute_abbreviation
         + soc_01 # gender
         + soc_02 # year of birth
         + soc_12 # educational level
         + mh_cov, # consultations with MH services at baseline
         data = .) 
  
  fit_adj_pcl5 <- 
    df_adj %>% 
    lmer(pcl5_t ~ randomization_group*time + (1|castor_record_id)
         + pcl5_t_cov
         + institute_abbreviation
         + soc_01 # gender
         + soc_02 # year of birth
         + soc_12 # educational level
         + mh_cov, # consultations with MH services at baseline
         data = .) 
  
  fit_adj <- 
    list(phqads_t = fit_adj_phqads,
         phq9_t = fit_adj_phq9,
         gad7_t = fit_adj_gad7,
         pcl5_t = fit_adj_pcl5)
  
  vcCR_adj <- 
    map(fit_adj,
        ~ vcovCR(., type = "CR2")) # OK
  
  emm_adj <- 
    map2(fit_adj, vcCR_adj, 
         ~ emmeans(.x,
                   "randomization_group",
                   by = "time",
                   vcov. = .y,
                   lmer.df = "satterthwaite")) # OK
  
  emm_pw_adj <- 
    map2(fit_adj, vcCR_adj, 
         ~ emmeans(.x,
                   pairwise ~ randomization_group,
                   by = "time",
                   vcov. = .y,
                   lmer.df = "satterthwaite")) # OK
  
  emm_for_plots_adj <- 
    map(emm_pw_adj,
        ~ unlist(.[1:4]$emmeans)) # lo tengo creo
  
  emm_plots_adj <- 
    map2(emm_pw_adj, emm_for_plots_adj,
         ~ emmip(.y,
                 randomization_group ~ time, 
                 CIs = TRUE)) # nailed it!
  
  edf_es_adj <-  
    map(emm_adj,
        ~summary(.x)$df %>% min(.))
  
  es_adj <- 
    map2(fit_adj, 
         emm_adj,
         ~ eff_size(.y, 
                    sigma = sigma(.x), 
                    edf = min(summary(pairs(.y))$df)))
  
  # tidy datasets
  
  emm_adj_tidy <- 
    emm_for_plots_adj %>% 
    map_dfr(.,
            ~ broom.mixed::tidy(.,
                                conf.int = TRUE),
            .id = "outcome") %>% 
    mutate(adj = "adjusted",
           type = "emm")
  
  emm_pw_adj_tidy <- 
    emm_pw_adj %>% 
    map(., ~ unlist(.[[2]])) %>% 
    map_dfr(., ~ broom::tidy(., conf.int = TRUE),
            .id = "outcome")%>% 
    mutate(adj = "adjusted",
           type = "pwemm")
  
  es_adj_tidy <- 
    es_adj %>% 
    map_dfr(., ~ broom::tidy(.,
                             conf.int = TRUE),
            .id = "outcome") %>% 
    mutate(adj = "adjusted",
           type = "cohensd")
  
  # plot primary outcome
  
  plot_emm_phqads_adj <- 
    emm_adj_tidy %>% 
    filter(outcome == "phqads_t") %>% 
    mutate(randomization_group = factor(randomization_group,
                                        levels = c("Intervention", "Control"))) %>% 
    ggplot(aes(x = time,
               y = estimate,
               group = randomization_group,
               colour = randomization_group)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = conf.low,
                      ymax = conf.high),
                  width = 0.1) +
    labs(x = "Time",
         y = "PHQ-ADS score",
         colour = "Group",
         title = "Fully adjusted estimated marginal means of the PHQ-ADS total score (primary outcome)") +
    scale_color_grey() +
    ylim(0,48)
  
  # plot effect size
  
  es_adj_fplot <- 
    es_adj_tidy %>% 
    filter(time != 1) %>%
    select(outcome, time, estimate, conf.low, conf.high) %>% 
    mutate(
      across(
        .cols = where(is.numeric),
        .fns = round, 2
      ),
      main = paste0(estimate,
                    " (",
                    conf.low,
                    ", ",
                    conf.high,
                    ")"),
      mean = estimate,
      lower = conf.low,
      upper = conf.high) %>%
    mutate(outcome = fct_recode(outcome,
                                "PHQ-ADS" = "phqads_t",
                                "PHQ-9" = "phq9_t",
                                "GAD-7" = "gad7_t",
                                "PCL-5" = "pcl5_t"),
           time = fct_recode(time,
                             "Week 7 (post DWM)" = "2",
                             "Week 13 (post PM+)" = "3",
                             "Week 21 (follow-up)" = "4"),
           outcome_bis = outcome) %>%
    arrange(desc(time)) %>% 
    forestplot(labeltext = c(time, outcome, main),
               xlab = "Cohen's d",
               title = "Effect sizes (fully adjusted models)",
               xticks = c(-0.5, 0, 0.3, 0.6, 0.9, 1.2, 1.5, 1.8),
               boxsize = 0.2,
               graph.pos = 3,
               align = "l") %>% 
    fp_add_header("Time",
                  "Outcome" %>% fp_align_left(),
                  "Cohen's d (95% CI)")
  
  #### Out ####
  
  full_summary <- # output
    emm_tidy %>% 
    bind_rows(., emm_adj_tidy) %>% 
    bind_rows(., emm_pw_tidy) %>% 
    bind_rows(., emm_pw_adj_tidy) %>% 
    bind_rows(., es_tidy) %>% 
    bind_rows(., es_adj_tidy) %>% 
    relocate(adj, 
             type, 
             outcome, 
             time, 
             randomization_group,
             estimate, 
             conf.low, 
             conf.high) %>% 
    select(!c(term, contrast, null.value)) # %>% mutate(model = "itt")
  
  
  full_summary_tidy <-
    full_summary %>% 
    filter(type != "cohensd") %>% 
    mutate(
      across(
        where(is.numeric),
        round, 1),
      out = paste0(estimate,
                   " (",
                   conf.low,
                   ", ",
                   conf.high,
                   ")")) %>% 
    select(adj:randomization_group,
           out) %>% 
    filter(time != 1) %>%  # Simplest model before pivoting
    pivot_wider(.,
                names_from = c(adj,
                               type,
                               randomization_group),
                values_from = out) %>%
    relocate(crude_pwemm_NA, .after = crude_emm_Intervention) %>% 
    mutate(outcome = factor(outcome,
                            levels = c("phqads_t",
                                       "phq9_t",
                                       "gad7_t",
                                       "pcl5_t")),
           outcome = fct_recode(outcome,
                                "PHQ-ADS" = "phqads_t",
                                "PHQ-9" = "phq9_t",
                                "GAD-7" = "gad7_t",
                                "PCL-5" = "pcl5_t"),
           time = fct_recode(time,
                             "Week 7 (post DWM)" = "2",
                             "Week 13 (post PM+)" = "3",
                             "Week 21 (follow-up)" = "4")
    ) %>% 
    arrange(outcome, desc(time))
  
  n_valid <- 
    df %>% 
    group_by(randomization_group,
             time) %>% 
    select(phqads_t,
           phq9_t,
           gad7_t,
           pcl5_t) %>% 
    mutate(
      across(
        phqads_t:pcl5_t,
        ~if_else(is.na(.), 0, 1)
      )
    ) %>% 
    summarise(
      across(
        phqads_t:pcl5_t,
        ~sum(., na.rm = T)
      )
    )
  
  n_phqads <- 
    n_valid %>% 
    filter(time != 1) %>% 
    arrange(randomization_group,
            desc(time)) %>% 
    pull(phqads_t) %>% 
    as_character()
  
  n_phq9 <- 
    n_valid %>% 
    filter(time != 1) %>% 
    arrange(randomization_group,
            desc(time)) %>% 
    pull(phq9_t) %>% 
    as_character()
  
  n_gad7 <- 
    n_valid %>% 
    filter(time != 1) %>% 
    arrange(randomization_group,
            desc(time)) %>% 
    pull(gad7_t) %>% 
    as_character()
  
  n_pcl5 <- 
    n_valid %>% 
    filter(time != 1) %>% 
    arrange(randomization_group,
            desc(time)) %>% 
    pull(pcl5_t) %>% 
    as_character()
  
  
  
  main_summary <- #FIXME: fontsize
    full_summary_tidy %>% 
    flextable() %>% 
    add_header_row(.,
                   values = c("", 
                              "Baseline-adjusted models", 
                              "Fully adjusted models"),
                   colwidths = c(2, 3, 3)) %>% 
    set_header_labels(.,
                      outcome = "",
                      time = "",
                      crude_emm_Control = "Control",
                      crude_emm_Intervention = "Intervention",
                      crude_pwemm_NA = "Difference",
                      adjusted_emm_Control = "Control",
                      adjusted_emm_Intervention = "Intervention",
                      adjusted_pwemm_NA = "Difference") %>% 
    add_footer_lines(.,
                     value = list("Note.",
                                  paste0("All models include participant as a random effect. ",
                                         "All confidence intervals calculated using robust standard errors."),
                                  paste0("PHQ-ADS = Patient Health Questionnaire - Anxiety and Depression Scale; ",
                                         "PHQ-9 = 9-item Patient Health Questionnaire; ",
                                         "GAD-7 = 7-item Generalised Anxiety Disorder; ",
                                         "PCL-5 = PTSD checklist for DSM-5; ",
                                         "PM+ = Problem Management Plus; ",
                                         "DWM = Doing What Matters.")
                     )
    ) %>% 
    footnote(.,
             i = 1,
             j = c(3:5),
             value = as_paragraph("Adjusted for outcome score measured at baseline"),
             ref_symbols = "¥",
             part = "header",
             inline = FALSE) %>% 
    footnote(.,
             i = 1,
             j = c(6:8),
             value = as_paragraph(
               as_chunk("Adjusted for age, gender, level of education, "),
               as_chunk("use of mental health services prior to enrolment, "),
               as_chunk("outcome score measured at baseline, and site.")),
             ref_symbols = "†",
             part = "header",
             inline = FALSE) %>% 
    merge_v(., j = 1) %>% 
    valign(., j = 1, valign = "top", part = "body") %>%
    align(., part = "header", align = "center") %>%   
    padding(.,
            i = 1,
            j = 2:8,
            padding.top = 30) %>%
    padding(.,
            i = 4,
            j = 2:8,
            padding.top = 30) %>% 
    padding(.,
            i = 7,
            j = 2:8,
            padding.top = 30) %>% 
    padding(.,
            i = 10,
            j = 2:8,
            padding.top = 30) %>% 
    align(.,
          j = 3:8,
          align = "center",
          part = "body") %>% 
    set_caption(., 
                caption = paste("Estimated marginal means and ",
                                "95 percent confidence intervals"), 
                align_with_table = FALSE) %>% 
    footnote(.,
             i = 1,
             j = 1,
             value = as_paragraph(
               as_chunk("Ns valid at weeks 21, 13, and 7 are "),
               as_chunk(paste0(n_phqads[1], ", ", n_phqads[2], ", and ", n_phqads[3])),
               as_chunk(", in the control arm, and "),
               as_chunk(paste0(n_phqads[4], ", ", n_phqads[5], ", and ", n_phqads[6])),
               as_chunk(", in the intervention arm")
             ),
             ref_symbols = "1",
             part = "body",
             inline = FALSE) %>%
    footnote(.,
             i = 4,
             j = 1,
             value = as_paragraph(
               as_chunk("Ns valid at weeks 21, 13, and 7 are "),
               as_chunk(paste0(n_phq9[1], ", ", n_phq9[2], ", and ", n_phq9[3])),
               as_chunk(", in the control arm, and "),
               as_chunk(paste0(n_phq9[4], ", ", n_phq9[5], ", and ", n_phq9[6])),
               as_chunk(", in the intervention arm")
             ),
             ref_symbols = "2",
             part = "body",
             inline = FALSE) %>%
    footnote(.,
             i = 7,
             j = 1,
             value = as_paragraph(
               as_chunk("Ns valid at weeks 21, 13, and 7 are "),
               as_chunk(paste0(n_gad7[1], ", ", n_gad7[2], ", and ", n_gad7[3])),
               as_chunk(", in the control arm, and "),
               as_chunk(paste0(n_gad7[4], ", ", n_gad7[5], ", and ", n_gad7[6])),
               as_chunk(", in the intervention arm")
             ),
             ref_symbols = "3",
             part = "body",
             inline = FALSE) %>%
    footnote(.,
             i = 10,
             j = 1,
             value = as_paragraph(
               as_chunk("Ns valid at weeks 21, 13, and 7 are "),
               as_chunk(paste0(n_pcl5[1], ", ", n_pcl5[2], ", and ", n_pcl5[3])),
               as_chunk(", in the control arm, and "),
               as_chunk(paste0(n_pcl5[4], ", ", n_pcl5[5], ", and ", n_pcl5[6])),
               as_chunk(", in the intervention arm")
             ),
             ref_symbols = "4",
             part = "body",
             inline = FALSE) %>%
    fontsize(., size = 10, part = "body") %>%
    fontsize(., size = 10, part = "header") %>% 
    fontsize(., size = 8.5, part = "footer") %>% 
    fix_border_issues() %>%
    autofit()
  
  p1 <- grid2grob(print(es_fplot))
  p2 <- grid2grob(print(es_adj_fplot))
  cohen_plot <- wrap_elements(p1) / wrap_elements(p2) # output
  
  p3 <- grid2grob(print(es_fplot_phqads))
  p4 <- grid2grob(print(es_fplot_phq))
  p5 <- grid2grob(print(es_fplot_gad))
  p6 <- grid2grob(print(es_fplot_ptsd))
  ses_plot_grid <- 
    (wrap_elements(p3) * wrap_elements(p4)) / 
    (wrap_elements(p5) * wrap_elements(p6))
  
  phqads_plot <-
    ggpubr::ggarrange(plot_emm_phqads,
                      plot_emm_phqads_adj,
                      nrow = 2,
                      ncol = 1)
  
  out_list <- list("full_summary" = full_summary,
                   "main_summary" = main_summary,
                   "cohen_plot" = cohen_plot,
                   "phqads_plot" = phqads_plot,
                   "ses_plot" = ses_plot_grid)
  
  return(out_list)
  
}

rRespond_get_models_log_baselinecov <- function(df){
  
  #### Crude models ####
  
  df <- 
    df %>% 
    group_by(castor_record_id) %>% 
    mutate(
      across(
        .cols = starts_with(c("soc_", "covid", "csri")),
        .fns = ~ .[time=1]
      ),
      phqads_t_cov = phqads_t[time=1],
      phq9_t_cov = phq9_t[time=1],
      gad7_t_cov = gad7_t[time=1],
      pcl5_t_cov = pcl5_t[time=1],
      mh_cov = if_else(csri_sp_mental_g_n == "0"
                       & csri_sp_mental_i_n == "0",
                       0,
                       1)
    ) %>% 
    ungroup()
  
  fit_log_phq9 <- 
    df %>% 
    glmer(phq9_cut ~ randomization_group*time + (1|castor_record_id)
          + phq9_t_cov,
          family = binomial(link = "logit"),
          data = .)
  
  fit_log_gad7 <- 
    df %>% 
    glmer(gad7_cut ~ randomization_group*time + (1|castor_record_id)
          + gad7_t_cov,
          family = binomial(link = "logit"),
          data = .) 
  
  fit_log_phqads <- 
    df %>% 
    glmer(phqads_cut ~ randomization_group*time + (1|castor_record_id)
          + phqads_t_cov,
          family = binomial(link = "logit"),
          data = .)
  
  fit_log <- 
    list("phq9_cut" = fit_log_phq9,
         "gad7_cut" = fit_log_gad7,
         "phqads_cut" = fit_log_phqads)
  
  summary_crude <- 
    map(fit_log,
        ~jtools::summ(.,
                      exp = TRUE,
                      confint = TRUE,
                      robust = FALSE, 
                      digits = 3))
  
  #### Adjusted models ####
  
  df_adj <- 
    df %>% 
    group_by(castor_record_id) %>% 
    mutate(
      across(
        .cols = starts_with(c("soc_", "covid", "csri")),
        .fns = ~ .[time=1]
      ),
      phqads_t_cov = phqads_t[time=1],
      phq9_t_cov = phq9_t[time=1],
      gad7_t_cov = gad7_t[time=1],
      pcl5_t_cov = pcl5_t[time=1],
      mh_cov = if_else(csri_sp_mental_g_n == "0"
                       & csri_sp_mental_i_n == "0",
                       0,
                       1)
    ) %>% 
    ungroup()
  
  fit_log_adj_phq9 <- 
    df_adj %>% 
    glmer(phq9_cut ~ randomization_group*time + (1|castor_record_id)
          + phq9_t_cov
          + institute_abbreviation
          + soc_01 # gender
          + soc_02 # year of birth
          + soc_12 # educational level
          + mh_cov, # consultations with MH services at baseline
          family = binomial(link = "logit"),
          data = .)
  
  fit_log_adj_gad7 <- 
    df_adj %>% 
    glmer(gad7_cut ~ randomization_group*time + (1|castor_record_id)
          + gad7_t_cov
          + institute_abbreviation
          + soc_01 # gender
          + soc_02 # year of birth
          + soc_12 # educational level
          + mh_cov, # consultations with MH services at baseline
          family = binomial(link = "logit"),
          data = .) 
  
  fit_log_adj_phqads <- 
    df_adj %>% 
    glmer(phqads_cut ~ randomization_group*time + (1|castor_record_id)
          + phqads_t_cov
          + institute_abbreviation
          + soc_01 # gender
          + soc_02 # year of birth
          + soc_12 # educational level
          + mh_cov, # consultations with MH services at baseline
          family = binomial(link = "logit"),
          data = .)
  
  fit_log_adj <- 
    list("phq9_cut" = fit_log_adj_phq9,
         "gad7_cut" = fit_log_adj_gad7,
         "phqads_cut" = fit_log_adj_phqads)
  
  summary_adj <- 
    map(fit_log_adj,
        ~jtools::summ(.,
                      exp = TRUE,
                      confint = TRUE,
                      robust = FALSE, 
                      digits = 3))
  
  #### Out ####
  
  main_summary_log <- 
    jtools::export_summs(summary_crude$phqads_cut,
                         summary_adj$phqads_cut,
                         summary_crude$phq9_cut, 
                         summary_adj$phq9_cut,
                         summary_crude$gad7_cut,
                         summary_adj$gad7_cut,
                         model.names = c("PHQ-ADS (baseline)",
                                         "PHQ-ADS (fully)",
                                         "PHQ-9 (baseline)",
                                         "PHQ-9 (fully)",
                                         "GAD-7 (baseline)",
                                         "GAD-7 (fully)"),
                         error_format = "[{conf.low}, {conf.high}]",
                         error_pos = "below",
                         coefs = c("Intercept" = "(Intercept)", 
                                   "Group (Intervention)" = 
                                     "randomization_groupIntervention",
                                   "Time 2 (post DWM)" = 
                                     "time2",
                                   "Time 3 (post PM+)" = 
                                     "time3",
                                   "Time 4 (2-month follow-up)" = 
                                     "time4",
                                   "Intervention at time 2" = 
                                     "randomization_groupIntervention:time2",
                                   "Intervention at time 3" = 
                                     "randomization_groupIntervention:time3",
                                   "Intervention at time 4" = 
                                     "randomization_groupIntervention:time4")
    )
  
  return(main_summary_log)
  
}

rRespond_get_models_fem <- function(df){
  
  #### Crude models ####
  
  df <- 
    df %>% 
    group_by(castor_record_id) %>% 
    mutate(
      across(
        .cols = starts_with(c("soc_", "covid", "csri")),
        .fns = ~ .[time=1]
      ),
      phqads_t_cov = phqads_t[time=1],
      phq9_t_cov = phq9_t[time=1],
      gad7_t_cov = gad7_t[time=1],
      pcl5_t_cov = pcl5_t[time=1],
      mh_cov = if_else(csri_sp_mental_g_n == "0"
                       & csri_sp_mental_i_n == "0",
                       0,
                       1)
    ) %>% 
    ungroup()
  
  fit_phqads <- 
    df %>% 
    lmer(phqads_t ~ randomization_group*time + (1|castor_record_id)
         + phqads_t_cov,
         data = .) 
  
  fit_phq9 <- 
    df %>% 
    lmer(phq9_t ~ randomization_group*time + (1|castor_record_id)
         + phq9_t_cov,
         data = .)
  
  fit_gad7 <- 
    df %>% 
    lmer(gad7_t ~ randomization_group*time + (1|castor_record_id)
         + gad7_t_cov,
         data = .) 
  
  fit_pcl5 <- 
    df %>% 
    lmer(pcl5_t ~ randomization_group*time + (1|castor_record_id)
         + pcl5_t_cov,
         data = .) 
  
  fit <- 
    list(phqads_t = fit_phqads,
         phq9_t = fit_phq9,
         gad7_t = fit_gad7,
         pcl5_t = fit_pcl5)
  
  vcCR <- map(fit,
              ~ vcovCR(., type = "CR2")) # OK
  
  emm <- 
    map2(fit, vcCR, 
         ~ emmeans(.x,
                   "randomization_group",
                   by = "time",
                   vcov. = .y,
                   lmer.df = "satterthwaite")) # OK
  
  emm_pw <- 
    map2(fit, vcCR, 
         ~ emmeans(.x,
                   pairwise ~ randomization_group,
                   by = "time",
                   vcov. = .y,
                   lmer.df = "satterthwaite")) # OK
  
  emm_for_plots <- 
    map(emm_pw,
        ~ unlist(.[1:4]$emmeans)) # lo tengo creo
  
  emm_plots <- 
    map2(emm_pw, emm_for_plots,
         ~ emmip(.y,
                 randomization_group ~ time, 
                 CIs = TRUE)) # nailed it!
  
  edf_es <-  
    map(emm,
        ~summary(.x)$df %>% min(.))
  
  es <- 
    map2(fit, 
         emm,
         ~ eff_size(.y, sigma = sigma(.x), edf = min(summary(pairs(.y))$df)))
  
  # tidy datasets
  
  emm_tidy <- 
    emm_for_plots %>% 
    map_dfr(.,
            ~ broom.mixed::tidy(.,
                                conf.int = TRUE),
            .id = "outcome") %>% 
    mutate(adj = "crude",
           type = "emm")
  
  emm_pw_tidy <- 
    emm_pw %>% 
    map(., ~ unlist(.[[2]])) %>% 
    map_dfr(., ~ broom::tidy(., conf.int = TRUE),
            .id = "outcome") %>% 
    mutate(adj = "crude",
           type = "pwemm")
  
  es_tidy <- 
    es %>% 
    map_dfr(., ~ broom::tidy(.,
                             conf.int = TRUE),
            .id = "outcome") %>% 
    mutate(adj = "crude",
           type = "cohensd")
  
  # Plot primary outcome
  
  plot_emm_phqads <- 
    emm_tidy %>% 
    filter(outcome == "phqads_t") %>% 
    mutate(randomization_group = factor(randomization_group,
                                        levels = c("Intervention", "Control"))) %>% 
    ggplot(aes(x = time,
               y = estimate,
               group = randomization_group,
               colour = randomization_group)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = conf.low,
                      ymax = conf.high),
                  width = 0.1) +
    labs(x = "Time",
         y = "PHQ-ADS score",
         colour = "Group",
         title = "Baseline-adjusted estimated marginal means of the PHQ-ADS total score (primary outcome)") +
    scale_color_grey() +
    ylim(0,48)
  
  es_fplot <- 
    es_tidy %>% 
    filter(time != 1) %>%
    select(outcome, time, estimate, conf.low, conf.high) %>% 
    mutate(
      across(
        .cols = where(is.numeric),
        .fns = round, 2
      ),
      main = paste0(estimate,
                    " (",
                    conf.low,
                    ", ",
                    conf.high,
                    ")"),
      mean = estimate,
      lower = conf.low,
      upper = conf.high) %>%
    mutate(outcome = fct_recode(outcome,
                                "PHQ-ADS" = "phqads_t",
                                "PHQ-9" = "phq9_t",
                                "GAD-7" = "gad7_t",
                                "PCL-5" = "pcl5_t"),
           time = fct_recode(time,
                             "Week 7 (post DWM)" = "2",
                             "Week 13 (post PM+)" = "3",
                             "Week 21 (follow-up)" = "4"),
           outcome_bis = outcome) %>%
    arrange(desc(time)) %>% 
    forestplot(labeltext = c(time, outcome, main),
               xlab = "Cohen's d", 
               title = "Effect sizes (baseline-adjusted models)",
               xticks = c(-0.5, 0, 0.3, 0.6, 0.9, 1.2, 1.5, 1.8),
               boxsize = 0.2,
               graph.pos = 3,
               align = "l") %>%
    fp_set_style(axes = gpar(lower_limit = -2.0)) %>% 
    fp_add_header("Time",
                  "Outcome",
                  "Cohen's d (95% CI)")
  
  # New forest plot
  
  es_fplot_phqads <- 
    es_tidy %>% 
    filter(time != 1) %>%
    select(outcome, time, estimate, conf.low, conf.high) %>% 
    mutate(
      across(
        .cols = where(is.numeric),
        .fns = round, 1
      ),
      main = paste0(estimate,
                    " (",
                    conf.low,
                    ", ",
                    conf.high,
                    ")"),
      mean = estimate,
      lower = conf.low,
      upper = conf.high) %>%
    mutate(outcome = fct_recode(outcome,
                                "PHQ-ADS" = "phqads_t",
                                "PHQ-9" = "phq9_t",
                                "GAD-7" = "gad7_t",
                                "PCL-5" = "pcl5_t"),
           time = fct_recode(time,
                             "T2 (post DWM)" = "2",
                             "T3 (post PM+)" = "3",
                             "T4 (primary endpoint)" = "4"),
           outcome_bis = outcome) %>%
    arrange(desc(time)) %>% 
    filter(outcome == "PHQ-ADS") %>%
    forestplot(labeltext = c(time, main),
               xlab = "SES PHQ-ADS", 
               title = "Anxiety/depression symptoms (primary outcome)",
               xticks = c(-0.5, 0, 0.3, 0.6, 0.9, 1.2, 1.5, 1.8),
               boxsize = 0.1,
               graph.pos = 3,
               align = "l") %>%
    fp_set_style(axes = gpar(lower_limit = -2.0)) %>% 
    fp_add_header("",
                  "SES (95% CI)")
  
  es_fplot_phq <- 
    es_tidy %>% 
    filter(time != 1) %>%
    select(outcome, time, estimate, conf.low, conf.high) %>% 
    mutate(
      across(
        .cols = where(is.numeric),
        .fns = round, 1
      ),
      main = paste0(estimate,
                    " (",
                    conf.low,
                    ", ",
                    conf.high,
                    ")"),
      mean = estimate,
      lower = conf.low,
      upper = conf.high) %>%
    mutate(outcome = fct_recode(outcome,
                                "PHQ-ADS" = "phqads_t",
                                "PHQ-9" = "phq9_t",
                                "GAD-7" = "gad7_t",
                                "PCL-5" = "pcl5_t"),
           time = fct_recode(time,
                             "T2 (post DWM)" = "2",
                             "T3 (post PM+)" = "3",
                             "T4 (primary endpoint)" = "4"),
           outcome_bis = outcome) %>%
    arrange(desc(time)) %>% 
    filter(outcome == "PHQ-9") %>%
    forestplot(labeltext = c(time, main),
               xlab = "SES PHQ-9", 
               title = "Depression symptoms",
               xticks = c(-0.5, 0, 0.3, 0.6, 0.9, 1.2, 1.5, 1.8),
               boxsize = 0.1,
               graph.pos = 3,
               align = "l") %>%
    fp_set_style(axes = gpar(lower_limit = -2.0)) %>% 
    fp_add_header("",
                  "SES (95% CI)")
  
  es_fplot_gad <- 
    es_tidy %>% 
    filter(time != 1) %>%
    select(outcome, time, estimate, conf.low, conf.high) %>% 
    mutate(
      across(
        .cols = where(is.numeric),
        .fns = round, 1
      ),
      main = paste0(estimate,
                    " (",
                    conf.low,
                    ", ",
                    conf.high,
                    ")"),
      mean = estimate,
      lower = conf.low,
      upper = conf.high) %>%
    mutate(outcome = fct_recode(outcome,
                                "PHQ-ADS" = "phqads_t",
                                "PHQ-9" = "phq9_t",
                                "GAD-7" = "gad7_t",
                                "PCL-5" = "pcl5_t"),
           time = fct_recode(time,
                             "T2 (post DWM)" = "2",
                             "T3 (post PM+)" = "3",
                             "T4 (primary endpoint)" = "4"),
           outcome_bis = outcome) %>%
    arrange(desc(time)) %>% 
    filter(outcome == "GAD-7") %>%
    forestplot(labeltext = c(time, main),
               xlab = "SES GAD-7", 
               title = "Anxiety symptoms",
               xticks = c(-0.5, 0, 0.3, 0.6, 0.9, 1.2, 1.5, 1.8),
               boxsize = 0.1,
               graph.pos = 3,
               align = "l") %>%
    fp_set_style(axes = gpar(lower_limit = -2.0)) %>% 
    fp_add_header("",
                  "SES (95% CI)")
  
  es_fplot_ptsd <- 
    es_tidy %>% 
    filter(time != 1) %>%
    select(outcome, time, estimate, conf.low, conf.high) %>% 
    mutate(
      across(
        .cols = where(is.numeric),
        .fns = round, 1
      ),
      main = paste0(estimate,
                    " (",
                    conf.low,
                    ", ",
                    conf.high,
                    ")"),
      mean = estimate,
      lower = conf.low,
      upper = conf.high) %>%
    mutate(outcome = fct_recode(outcome,
                                "PHQ-ADS" = "phqads_t",
                                "PHQ-9" = "phq9_t",
                                "GAD-7" = "gad7_t",
                                "PCL-5" = "pcl5_t"),
           time = fct_recode(time,
                             "T2 (post DWM)" = "2",
                             "T3 (post PM+)" = "3",
                             "T4 (primary endpoint)" = "4"),
           outcome_bis = outcome) %>%
    arrange(desc(time)) %>% 
    filter(outcome == "PCL-5") %>%
    forestplot(labeltext = c(time, main),
               xlab = "SES PCL-5", 
               title = "Posttraumatic stress symptoms",
               xticks = c(-0.5, 0, 0.3, 0.6, 0.9, 1.2, 1.5, 1.8),
               boxsize = 0.1,
               graph.pos = 3,
               align = "l") %>%
    fp_set_style(axes = gpar(lower_limit = -2.0)) %>% 
    fp_add_header("",
                  "SES (95% CI)")
  
  #### Adjusted models ####
  
  df_adj <- 
    df %>% 
    group_by(castor_record_id) %>% 
    mutate(
      across(
        .cols = starts_with(c("soc_", "covid", "csri")),
        .fns = ~ .[time=1]
      ),
      phqads_t_cov = phqads_t[time=1],
      phq9_t_cov = phq9_t[time=1],
      gad7_t_cov = gad7_t[time=1],
      pcl5_t_cov = pcl5_t[time=1],
      mh_cov = if_else(csri_sp_mental_g_n == "0"
                       & csri_sp_mental_i_n == "0",
                       0,
                       1)
    ) %>% 
    ungroup()
  
  fit_adj_phqads <- 
    df_adj %>% 
    lmer(phqads_t ~ randomization_group*time + (1|castor_record_id)
         + phqads_t_cov
         + institute_abbreviation
         + soc_02 # year of birth
         + soc_12 # educational level
         + mh_cov, # consultations with MH services at baseline
         data = .) 
  
  fit_adj_phq9 <- 
    df_adj %>% 
    lmer(phq9_t ~ randomization_group*time + (1|castor_record_id)
         + phq9_t_cov
         + institute_abbreviation
         + soc_02 # year of birth
         + soc_12 # educational level
         + mh_cov, # consultations with MH services at baseline
         data = .)
  
  fit_adj_gad7 <- 
    df_adj %>% 
    lmer(gad7_t ~ randomization_group*time + (1|castor_record_id)
         + gad7_t_cov
         + institute_abbreviation
         + soc_02 # year of birth
         + soc_12 # educational level
         + mh_cov, # consultations with MH services at baseline
         data = .) 
  
  fit_adj_pcl5 <- 
    df_adj %>% 
    lmer(pcl5_t ~ randomization_group*time + (1|castor_record_id)
         + pcl5_t_cov
         + institute_abbreviation
         + soc_02 # year of birth
         + soc_12 # educational level
         + mh_cov, # consultations with MH services at baseline
         data = .) 
  
  fit_adj <- 
    list(phqads_t = fit_adj_phqads,
         phq9_t = fit_adj_phq9,
         gad7_t = fit_adj_gad7,
         pcl5_t = fit_adj_pcl5)
  
  vcCR_adj <- 
    map(fit_adj,
        ~ vcovCR(., type = "CR2")) # OK
  
  emm_adj <- 
    map2(fit_adj, vcCR_adj, 
         ~ emmeans(.x,
                   "randomization_group",
                   by = "time",
                   vcov. = .y,
                   lmer.df = "satterthwaite")) # OK
  
  emm_pw_adj <- 
    map2(fit_adj, vcCR_adj, 
         ~ emmeans(.x,
                   pairwise ~ randomization_group,
                   by = "time",
                   vcov. = .y,
                   lmer.df = "satterthwaite")) # OK
  
  emm_for_plots_adj <- 
    map(emm_pw_adj,
        ~ unlist(.[1:4]$emmeans)) # lo tengo creo
  
  emm_plots_adj <- 
    map2(emm_pw_adj, emm_for_plots_adj,
         ~ emmip(.y,
                 randomization_group ~ time, 
                 CIs = TRUE)) # nailed it!
  
  edf_es_adj <-  
    map(emm_adj,
        ~summary(.x)$df %>% min(.))
  
  es_adj <- 
    map2(fit_adj, 
         emm_adj,
         ~ eff_size(.y, 
                    sigma = sigma(.x), 
                    edf = min(summary(pairs(.y))$df)))
  
  # tidy datasets
  
  emm_adj_tidy <- 
    emm_for_plots_adj %>% 
    map_dfr(.,
            ~ broom.mixed::tidy(.,
                                conf.int = TRUE),
            .id = "outcome") %>% 
    mutate(adj = "adjusted",
           type = "emm")
  
  emm_pw_adj_tidy <- 
    emm_pw_adj %>% 
    map(., ~ unlist(.[[2]])) %>% 
    map_dfr(., ~ broom::tidy(., conf.int = TRUE),
            .id = "outcome")%>% 
    mutate(adj = "adjusted",
           type = "pwemm")
  
  es_adj_tidy <- 
    es_adj %>% 
    map_dfr(., ~ broom::tidy(.,
                             conf.int = TRUE),
            .id = "outcome") %>% 
    mutate(adj = "adjusted",
           type = "cohensd")
  
  # plot primary outcome
  
  plot_emm_phqads_adj <- 
    emm_adj_tidy %>% 
    filter(outcome == "phqads_t") %>% 
    mutate(randomization_group = factor(randomization_group,
                                        levels = c("Intervention", "Control"))) %>% 
    ggplot(aes(x = time,
               y = estimate,
               group = randomization_group,
               colour = randomization_group)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = conf.low,
                      ymax = conf.high),
                  width = 0.1) +
    labs(x = "Time",
         y = "PHQ-ADS score",
         colour = "Group",
         title = "Fully adjusted estimated marginal means of the PHQ-ADS total score (primary outcome)") +
    scale_color_grey() +
    ylim(0,48)
  
  # plot effect size
  
  es_adj_fplot <- 
    es_adj_tidy %>% 
    filter(time != 1) %>%
    select(outcome, time, estimate, conf.low, conf.high) %>% 
    mutate(
      across(
        .cols = where(is.numeric),
        .fns = round, 2
      ),
      main = paste0(estimate,
                    " (",
                    conf.low,
                    ", ",
                    conf.high,
                    ")"),
      mean = estimate,
      lower = conf.low,
      upper = conf.high) %>%
    mutate(outcome = fct_recode(outcome,
                                "PHQ-ADS" = "phqads_t",
                                "PHQ-9" = "phq9_t",
                                "GAD-7" = "gad7_t",
                                "PCL-5" = "pcl5_t"),
           time = fct_recode(time,
                             "Week 7 (post DWM)" = "2",
                             "Week 13 (post PM+)" = "3",
                             "Week 21 (follow-up)" = "4"),
           outcome_bis = outcome) %>%
    arrange(desc(time)) %>% 
    forestplot(labeltext = c(time, outcome, main),
               xlab = "Cohen's d",
               title = "Effect sizes (fully adjusted models)",
               xticks = c(-0.5, 0, 0.3, 0.6, 0.9, 1.2, 1.5, 1.8),
               boxsize = 0.2,
               graph.pos = 3,
               align = "l") %>% 
    fp_add_header("Time",
                  "Outcome" %>% fp_align_left(),
                  "Cohen's d (95% CI)")
  
  #### Out ####
  
  full_summary <- # output
    emm_tidy %>% 
    bind_rows(., emm_adj_tidy) %>% 
    bind_rows(., emm_pw_tidy) %>% 
    bind_rows(., emm_pw_adj_tidy) %>% 
    bind_rows(., es_tidy) %>% 
    bind_rows(., es_adj_tidy) %>% 
    relocate(adj, 
             type, 
             outcome, 
             time, 
             randomization_group,
             estimate, 
             conf.low, 
             conf.high) %>% 
    select(!c(term, contrast, null.value)) # %>% mutate(model = "itt")
  
  
  full_summary_tidy <-
    full_summary %>% 
    filter(type != "cohensd") %>% 
    mutate(
      across(
        where(is.numeric),
        round, 1),
      out = paste0(estimate,
                   " (",
                   conf.low,
                   ", ",
                   conf.high,
                   ")")) %>% 
    select(adj:randomization_group,
           out) %>% 
    filter(time != 1) %>%  # Simplest model before pivoting
    pivot_wider(.,
                names_from = c(adj,
                               type,
                               randomization_group),
                values_from = out) %>%
    relocate(crude_pwemm_NA, .after = crude_emm_Intervention) %>% 
    mutate(outcome = factor(outcome,
                            levels = c("phqads_t",
                                       "phq9_t",
                                       "gad7_t",
                                       "pcl5_t")),
           outcome = fct_recode(outcome,
                                "PHQ-ADS" = "phqads_t",
                                "PHQ-9" = "phq9_t",
                                "GAD-7" = "gad7_t",
                                "PCL-5" = "pcl5_t"),
           time = fct_recode(time,
                             "Week 7 (post DWM)" = "2",
                             "Week 13 (post PM+)" = "3",
                             "Week 21 (follow-up)" = "4")
    ) %>% 
    arrange(outcome, desc(time))
  
  n_valid <- 
    df %>% 
    group_by(randomization_group,
             time) %>% 
    select(phqads_t,
           phq9_t,
           gad7_t,
           pcl5_t) %>% 
    mutate(
      across(
        phqads_t:pcl5_t,
        ~if_else(is.na(.), 0, 1)
      )
    ) %>% 
    summarise(
      across(
        phqads_t:pcl5_t,
        ~sum(., na.rm = T)
      )
    )
  
  n_phqads <- 
    n_valid %>% 
    filter(time != 1) %>% 
    arrange(randomization_group,
            desc(time)) %>% 
    pull(phqads_t) %>% 
    as_character()
  
  n_phq9 <- 
    n_valid %>% 
    filter(time != 1) %>% 
    arrange(randomization_group,
            desc(time)) %>% 
    pull(phq9_t) %>% 
    as_character()
  
  n_gad7 <- 
    n_valid %>% 
    filter(time != 1) %>% 
    arrange(randomization_group,
            desc(time)) %>% 
    pull(gad7_t) %>% 
    as_character()
  
  n_pcl5 <- 
    n_valid %>% 
    filter(time != 1) %>% 
    arrange(randomization_group,
            desc(time)) %>% 
    pull(pcl5_t) %>% 
    as_character()
  
  
  
  main_summary <- #FIXME: fontsize
    full_summary_tidy %>% 
    flextable() %>% 
    add_header_row(.,
                   values = c("", 
                              "Baseline-adjusted models", 
                              "Fully adjusted models"),
                   colwidths = c(2, 3, 3)) %>% 
    set_header_labels(.,
                      outcome = "",
                      time = "",
                      crude_emm_Control = "Control",
                      crude_emm_Intervention = "Intervention",
                      crude_pwemm_NA = "Difference",
                      adjusted_emm_Control = "Control",
                      adjusted_emm_Intervention = "Intervention",
                      adjusted_pwemm_NA = "Difference") %>% 
    add_footer_lines(.,
                     value = list("Note.",
                                  paste0("All models include participant as a random effect. ",
                                         "All confidence intervals calculated using robust standard errors."),
                                  paste0("PHQ-ADS = Patient Health Questionnaire - Anxiety and Depression Scale; ",
                                         "PHQ-9 = 9-item Patient Health Questionnaire; ",
                                         "GAD-7 = 7-item Generalised Anxiety Disorder; ",
                                         "PCL-5 = PTSD checklist for DSM-5; ",
                                         "PM+ = Problem Management Plus; ",
                                         "DWM = Doing What Matters.")
                     )
    ) %>% 
    footnote(.,
             i = 1,
             j = c(3:5),
             value = as_paragraph("Adjusted for outcome score measured at baseline"),
             ref_symbols = "¥",
             part = "header",
             inline = FALSE) %>% 
    footnote(.,
             i = 1,
             j = c(6:8),
             value = as_paragraph(
               as_chunk("Adjusted for age, level of education, "),
               as_chunk("use of mental health services prior to enrolment, "),
               as_chunk("outcome score measured at baseline, and site.")),
             ref_symbols = "†",
             part = "header",
             inline = FALSE) %>% 
    merge_v(., j = 1) %>% 
    valign(., j = 1, valign = "top", part = "body") %>%
    align(., part = "header", align = "center") %>%   
    padding(.,
            i = 1,
            j = 2:8,
            padding.top = 30) %>%
    padding(.,
            i = 4,
            j = 2:8,
            padding.top = 30) %>% 
    padding(.,
            i = 7,
            j = 2:8,
            padding.top = 30) %>% 
    padding(.,
            i = 10,
            j = 2:8,
            padding.top = 30) %>% 
    align(.,
          j = 3:8,
          align = "center",
          part = "body") %>% 
    set_caption(., 
                caption = paste("Estimated marginal means and ",
                                "95 percent confidence intervals"), 
                align_with_table = FALSE) %>% 
    footnote(.,
             i = 1,
             j = 1,
             value = as_paragraph(
               as_chunk("Ns valid at weeks 21, 13, and 7 are "),
               as_chunk(paste0(n_phqads[1], ", ", n_phqads[2], ", and ", n_phqads[3])),
               as_chunk(", in the control arm, and "),
               as_chunk(paste0(n_phqads[4], ", ", n_phqads[5], ", and ", n_phqads[6])),
               as_chunk(", in the intervention arm")
             ),
             ref_symbols = "1",
             part = "body",
             inline = FALSE) %>%
    footnote(.,
             i = 4,
             j = 1,
             value = as_paragraph(
               as_chunk("Ns valid at weeks 21, 13, and 7 are "),
               as_chunk(paste0(n_phq9[1], ", ", n_phq9[2], ", and ", n_phq9[3])),
               as_chunk(", in the control arm, and "),
               as_chunk(paste0(n_phq9[4], ", ", n_phq9[5], ", and ", n_phq9[6])),
               as_chunk(", in the intervention arm")
             ),
             ref_symbols = "2",
             part = "body",
             inline = FALSE) %>%
    footnote(.,
             i = 7,
             j = 1,
             value = as_paragraph(
               as_chunk("Ns valid at weeks 21, 13, and 7 are "),
               as_chunk(paste0(n_gad7[1], ", ", n_gad7[2], ", and ", n_gad7[3])),
               as_chunk(", in the control arm, and "),
               as_chunk(paste0(n_gad7[4], ", ", n_gad7[5], ", and ", n_gad7[6])),
               as_chunk(", in the intervention arm")
             ),
             ref_symbols = "3",
             part = "body",
             inline = FALSE) %>%
    footnote(.,
             i = 10,
             j = 1,
             value = as_paragraph(
               as_chunk("Ns valid at weeks 21, 13, and 7 are "),
               as_chunk(paste0(n_pcl5[1], ", ", n_pcl5[2], ", and ", n_pcl5[3])),
               as_chunk(", in the control arm, and "),
               as_chunk(paste0(n_pcl5[4], ", ", n_pcl5[5], ", and ", n_pcl5[6])),
               as_chunk(", in the intervention arm")
             ),
             ref_symbols = "4",
             part = "body",
             inline = FALSE) %>%
    fontsize(., size = 10, part = "body") %>%
    fontsize(., size = 10, part = "header") %>% 
    fontsize(., size = 8.5, part = "footer") %>% 
    fix_border_issues() %>%
    autofit()
  
  p1 <- grid2grob(print(es_fplot))
  p2 <- grid2grob(print(es_adj_fplot))
  cohen_plot <- wrap_elements(p1) / wrap_elements(p2) # output
  
  p3 <- grid2grob(print(es_fplot_phqads))
  p4 <- grid2grob(print(es_fplot_phq))
  p5 <- grid2grob(print(es_fplot_gad))
  p6 <- grid2grob(print(es_fplot_ptsd))
  ses_plot_grid <- 
    (wrap_elements(p3) * wrap_elements(p4)) / 
    (wrap_elements(p5) * wrap_elements(p6))
  
  phqads_plot <-
    ggpubr::ggarrange(plot_emm_phqads,
                      plot_emm_phqads_adj,
                      nrow = 2,
                      ncol = 1)
  
  out_list <- list("full_summary" = full_summary,
                   "main_summary" = main_summary,
                   "cohen_plot" = cohen_plot,
                   "phqads_plot" = phqads_plot,
                   "ses_plot" = ses_plot_grid)
  
  return(out_list)
  
}


 