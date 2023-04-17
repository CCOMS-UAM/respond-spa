# R Code for RESPOND Spain - Residual standard deviations #
# Author: Roberto Mediavilla (roberto.mediavilla@uam.es) #
# Date: April 2023 #

#### Motivation ####

# A reviewer asked to provide the residual standard deviations
# of the main models because he/she wanted to replicate
# the calculation of the effect sizes

#### Source data ####

source("src/req_pack.R")
source("data_cleaning.R")

#### Current functions to calculate models ####

# I used these functions to run my models

source("R/respond_functions.R")

# According to the study protocol, the main analyses
# were based on baseline-adjusted models

rRespond_get_models_baselinecov

# First, this function fits linear mixed models for the main outcomes

df <- ds_long

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

# Then, it takes a variance-covariance matrix to estimate marginal means with
# robust standard errors, to make pairwase comparisons

vcCR <- map(fit,
            ~ vcovCR(., type = "CR2"))

emm <-
  map2(fit, vcCR,
       ~ emmeans(.x,
                 "randomization_group",
                 by = "time",
                 vcov. = .y,
                 lmer.df = "satterthwaite"))

emm_pw <-
  map2(fit, vcCR,
       ~ emmeans(.x,
                 pairwise ~ randomization_group,
                 by = "time",
                 vcov. = .y,
                 lmer.df = "satterthwaite"))

# Last, it computes standardised effect sizes using the residual standard
# deviations of the models ("sigma(.x)")

emm # estimated marginal means

# $phqads_t
# time = 1:
#   randomization_group emmean    SE  df lower.CL upper.CL
# Control              20.30 0.270 684    19.77     20.8
# Intervention         20.49 0.249 684    20.00     21.0
#
# time = 2:
#   randomization_group emmean    SE  df lower.CL upper.CL
# Control              16.95 0.527 709    15.92     18.0
# Intervention         13.52 0.711 706    12.12     14.9
#
# time = 3:
#   randomization_group emmean    SE  df lower.CL upper.CL
# Control              15.53 0.831 737    13.90     17.2
# Intervention          9.68 0.805 733     8.10     11.3
#
# time = 4:
#   randomization_group emmean    SE  df lower.CL upper.CL
# Control              14.76 0.831 724    13.12     16.4
# Intervention         10.39 0.824 724     8.78     12.0
#
# Degrees-of-freedom method: satterthwaite
# Confidence level used: 0.95
#
# $phq9_t
# time = 1:
#   randomization_group emmean    SE  df lower.CL upper.CL
# Control              10.13 0.157 689     9.82    10.44
# Intervention         10.33 0.148 689    10.03    10.62
#
# time = 2:
#   randomization_group emmean    SE  df lower.CL upper.CL
# Control               8.29 0.305 713     7.70     8.89
# Intervention          6.72 0.406 710     5.93     7.52
#
# time = 3:
#   randomization_group emmean    SE  df lower.CL upper.CL
# Control               7.82 0.452 738     6.93     8.71
# Intervention          4.60 0.427 736     3.76     5.44
#
# time = 4:
#   randomization_group emmean    SE  df lower.CL upper.CL
# Control               7.52 0.453 727     6.63     8.41
# Intervention          5.05 0.460 728     4.15     5.96
#
# Degrees-of-freedom method: satterthwaite
# Confidence level used: 0.95
#
# $gad7_t
# time = 1:
#   randomization_group emmean    SE  df lower.CL upper.CL
# Control              10.16 0.150 701     9.87    10.46
# Intervention         10.17 0.142 701     9.89    10.45
#
# time = 2:
#   randomization_group emmean    SE  df lower.CL upper.CL
# Control               8.66 0.297 721     8.07     9.24
# Intervention          6.79 0.376 719     6.05     7.53
#
# time = 3:
#   randomization_group emmean    SE  df lower.CL upper.CL
# Control               7.70 0.447 745     6.82     8.58
# Intervention          5.08 0.418 742     4.26     5.90
#
# time = 4:
#   randomization_group emmean    SE  df lower.CL upper.CL
# Control               7.23 0.437 734     6.37     8.09
# Intervention          5.34 0.432 734     4.49     6.19
#
# Degrees-of-freedom method: satterthwaite
# Confidence level used: 0.95
#
# $pcl5_t
# time = 1:
#   randomization_group emmean    SE  df lower.CL upper.CL
# Control              12.76 0.159 672    12.45    13.07
# Intervention         12.85 0.164 671    12.53    13.17
#
# time = 2:
#   randomization_group emmean    SE  df lower.CL upper.CL
# Control              10.98 0.544 695     9.91    12.05
# Intervention          9.89 0.532 693     8.84    10.93
#
# time = 3:
#   randomization_group emmean    SE  df lower.CL upper.CL
# Control               9.71 0.599 720     8.54    10.89
# Intervention          7.39 0.568 707     6.27     8.51
#
# time = 4:
#   randomization_group emmean    SE  df lower.CL upper.CL
# Control               9.53 0.699 704     8.15    10.90
# Intervention          7.37 0.614 703     6.17     8.58
#
# Degrees-of-freedom method: satterthwaite
# Confidence level used: 0.95


map(fit,
    sigma) # residual standard deviations

# $phqads_t
# [1] 5.491634
#
# $phq9_t
# [1] 3.063616
#
# $gad7_t
# [1] 2.974452
#
# $pcl5_t
# [1] 4.235282

map2(fit,
     emm,
     ~ eff_size(.y,
                sigma = sigma(.x),
                edf = min(summary(pairs(.y))$df))) # standardised effect sizes

# $phqads_t
# time = 1:
#   contrast               effect.size    SE  df lower.CL upper.CL
# Control - Intervention     -0.0353 0.067 684   -0.167   0.0963
#
# time = 2:
#   contrast               effect.size    SE  df lower.CL upper.CL
# Control - Intervention      0.6256 0.162 706    0.307   0.9437
#
# time = 3:
#   contrast               effect.size    SE  df lower.CL upper.CL
# Control - Intervention      1.0658 0.213 733    0.648   1.4832
#
# time = 4:
#   contrast               effect.size    SE  df lower.CL upper.CL
# Control - Intervention      0.7941 0.214 724    0.373   1.2150
#
# sigma used for effect sizes: 5.492
# Degrees-of-freedom method: inherited from satterthwaite when re-gridding
# Confidence level used: 0.95
#
# $phq9_t
# time = 1:
#   contrast               effect.size     SE  df lower.CL upper.CL
# Control - Intervention     -0.0632 0.0712 689   -0.203   0.0766
#
# time = 2:
#   contrast               effect.size     SE  df lower.CL upper.CL
# Control - Intervention      0.5132 0.1661 710    0.187   0.8393
#
# time = 3:
#   contrast               effect.size     SE  df lower.CL upper.CL
# Control - Intervention      1.0524 0.2049 736    0.650   1.4547
#
# time = 4:
#   contrast               effect.size     SE  df lower.CL upper.CL
# Control - Intervention      0.8047 0.2121 727    0.388   1.2211
#
# sigma used for effect sizes: 3.064
# Degrees-of-freedom method: inherited from satterthwaite when re-gridding
# Confidence level used: 0.95
#
# $gad7_t
# time = 1:
#   contrast               effect.size     SE  df lower.CL upper.CL
# Control - Intervention    -0.00251 0.0695 701   -0.139    0.134
#
# time = 2:
#   contrast               effect.size     SE  df lower.CL upper.CL
# Control - Intervention     0.62802 0.1620 719    0.310    0.946
#
# time = 3:
#   contrast               effect.size     SE  df lower.CL upper.CL
# Control - Intervention     0.88085 0.2068 742    0.475    1.287
#
# time = 4:
#   contrast               effect.size     SE  df lower.CL upper.CL
# Control - Intervention     0.63662 0.2074 734    0.230    1.044
#
# sigma used for effect sizes: 2.974
# Degrees-of-freedom method: inherited from satterthwaite when re-gridding
# Confidence level used: 0.95
#
# $pcl5_t
# time = 1:
#   contrast               effect.size    SE  df lower.CL upper.CL
# Control - Intervention     -0.0211 0.054 671  -0.1272    0.085
#
# time = 2:
#   contrast               effect.size    SE  df lower.CL upper.CL
# Control - Intervention      0.2582 0.180 693  -0.0954    0.612
#
# time = 3:
#   contrast               effect.size    SE  df lower.CL upper.CL
# Control - Intervention      0.5484 0.196 707   0.1644    0.932
#
# time = 4:
#   contrast               effect.size    SE  df lower.CL upper.CL
# Control - Intervention      0.5081 0.220 703   0.0767    0.940
#
# sigma used for effect sizes: 4.235
# Degrees-of-freedom method: inherited from satterthwaite when re-gridding
# Confidence level used: 0.95

