# required variables

ds_long <-
  ds_long %>%
  mutate(btq_total = select(., starts_with("btq_")) %>%
           rowSums(.)) |>
  group_by(castor_record_id) |>
  mutate(btq_total_base = btq_total[time==1]) |>
  ungroup()

ds_long <-
  ds_long %>%
  mutate(le_total = select(., starts_with("le_")) %>%
           rowSums(.)) |>
  group_by(castor_record_id) |>
  mutate(le_base = le_total[time==1]) |>
  ungroup()

# changes in btq


ds_long %>%
  ggplot(aes(x = time,
             y = btq_total,
             colour = randomization_group)) +
  geom_boxplot()


ds_long %>%
  group_by(time, randomization_group) %>%
  summarise(mean = mean(btq_total, na.rm = TRUE),
            sd = sd(btq_total, na.rm = TRUE),
            median = median(btq_total, na.rm = TRUE))

ds_long %>%
  lm(btq_total ~ randomization_group*time, .) %>%
  summary()

# The intervention group has higher BTQ scores at baseline

not_outliers <-
  ds_long %>%
  filter(time == "1") |>
  mutate(IQR = IQR(btq_total, na.rm = TRUE),
         lower_bound = quantile(btq_total, 0.25, na.rm = TRUE) - 1.5 * IQR,
         upper_bound = quantile(btq_total, 0.75, na.rm = TRUE) + 1.5 * IQR) %>%
  filter(btq_total >= lower_bound & btq_total <= upper_bound) |>
  pull(castor_record_id) |>
  unique()

# removes 15 observations

ds_long %>%
  filter(castor_record_id %in% not_outliers) |>
  group_by(time, randomization_group) %>%
  summarise(mean = mean(btq_total, na.rm = TRUE),
            sd = sd(btq_total, na.rm = TRUE),
            median = median(btq_total, na.rm = TRUE))

ds_long_cleaned <-
  ds_long %>%
  filter(castor_record_id %in% not_outliers)

ds_long_cleaned %>%
  ggplot(aes(x = time,
             y = btq_total,
             colour = randomization_group)) +
  geom_boxplot()

ds_long_cleaned |>
  group_by(time, randomization_group) %>%
  summarise(mean = mean(btq_total, na.rm = TRUE),
            sd = sd(btq_total, na.rm = TRUE),
            median = median(btq_total, na.rm = TRUE))

lm(btq_total ~ randomization_group*time, ds_long_cleaned) %>%
  summary()

# moderation

lm(phqads_t ~ randomization_group*time*btq_total_base, ds_long_cleaned) %>%
  summary() # no interactive effects

lm(phqads_t ~ randomization_group*time + btq_total, ds_long_cleaned) %>%
  summary() # effect holds after adjusting for baseline confounding

# life events

ds_long %>%
  ggplot(aes(x = time,
             y = le_total,
             colour = randomization_group)) +
  geom_boxplot()


ds_long %>%
  group_by(time, randomization_group) %>%
  summarise(mean = mean(le_total, na.rm = TRUE),
            sd = sd(le_total, na.rm = TRUE),
            median = median(le_total, na.rm = TRUE))

ds_long %>%
  lm(le_total ~ randomization_group*time, .) %>%
  summary()

# no significant differences at baseline, no interactive effects at follow-up

# moderation

lm(phqads_t ~ randomization_group*time*le_base, ds_long) %>%
  summary() # no interactive effects

lm(phqads_t ~ randomization_group*time + le_base, ds_long) %>%
  summary() # effect holds after adjusting for baseline confounding

# Overview

ds_long |>
  filter(time == 1) |>
  select(randomization_group,
         starts_with(c("le_", "btq_"))) |>
  tbl_summary(by = "randomization_group")

# Idea: sensitivity analysis in subgroups, based on specific items or count
