# ============================================================
# Project: UK Railway System – Structural Stability Analysis
# File: 01_data_preprocessing.R
# Author: 
# Date: 2026-01-28
# Description:
#   Data loading, cleaning and reshaping, and analysis of ORR station-level
#   passenger entries and exits (Table 1415A).
# ============================================================
# ----------------------------
# Libraries
# ----------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(fixest)

#_____________________________
#Data Loading 
#_____________________________
df <- read.csv("Sources/table-1415-time-series-of-passenger-entries-and-exits-and-interchanges-by-station (2) - 1415a_Entries_and_Exits.csv")
str(df)
summary(df)
dim(df) 

#_____________________________
# Numeric cleaning
#_____________________________

num_cols <- 5:ncol(df)
df[num_cols] <- lapply(df[num_cols], function(x) {
  x <- gsub("\\[.*?\\]", "", x)  
  x <- gsub("[,;]", "", x)       
  as.numeric(x)
})

#_____________________________
# Verification and first exploration, for NA´S 
#_____________________________
str(df)
summary(df[, num_cols])
sum(is.na(df))

#_____________________________
# Remove unusable period (ORR guidance)
#_____________________________
df_clean <- df[, !grepl("Apr.2003.to.Mar.2004", names(df))]
sum(is.na(df_clean))
sum(!is.na(df_clean))

#_____________________________
# Wide to long format
#_____________________________

#______________________________________________________
# Wide to long
#______________________________________________________
names(df_clean)
colnames(df_clean) <- gsub("\\.", " ", colnames(df_clean))
colnames(df_clean) <- gsub("\\s*[bB]\\s*$", "", colnames(df_clean))
year_cols <- grep("^Apr\\s\\d{4}\\sto\\sMar\\s\\d{4}$", names(df_clean), value = TRUE)

df_clean <- df_clean |>
  dplyr::filter(!if_any(dplyr::all_of(year_cols), is.na))
unique(df_clean$`Station name`)
year_cols <- grep("^Apr\\s\\d{4}\\sto\\sMar\\s\\d{4}$", names(df_clean), value = TRUE)

df_long <- df_clean %>%
  pivot_longer(
    cols = 5:ncol(df_clean),
    names_to = "period",
    values_to = "value"
  )

str(df_long)
summary(df_long$value)

#____________________________________________________
# Balanced panel (27 years) - Exclude Elizabeth Line: Recent high usage actor 
#____________________________________________________

#_
# Quality check, lock for duplicates 
#_ 
station_coverage <- df_long %>%
  group_by(Sort) %>%
  summarise(
    n_years = sum(!is.na(value)),
    .groups = "drop"
  )
summary(station_coverage$n_years)

df_long %>% 
  count(Sort, period) %>% 
  filter(n > 1) 

#_________________________________
#Filter to only have complete stations of information 27 years
#__________________________________
df_long_27 <- df_long %>%
  inner_join(
    station_coverage %>% filter(n_years == 27), 
    by = "Sort"
  )
n_distinct(df_long_27$Sort)
df_unique_stations <- df_long_27 %>%
  distinct(Sort, Region)
stations_per_region <- df_unique_stations %>%
  count(Region, name = "n_stations")
write.csv(df_long_27, "df_long_27.csv", row.names = FALSE)
stations_per_region <- df_unique_stations %>%
  count(Region, name = "n_stations")

stations_per_region_table <- stations_per_region %>%
  arrange(desc(n_stations))

stations_per_region_table

write.csv(
  stations_per_region_table,
  "Produced_tables/stations_per_region_table.csv",
  row.names = FALSE
)

#___________________________________
# Visualisation of regional distribution
#___________________________________

ggplot(stations_per_region, aes(x = Region, y = n_stations)) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Region",
    y = "Number of unique stations"
  ) +
  theme_minimal()

#____________________________________
# Mean and median of passengers entries and extis 
#____________________________________
mean_usage_year <- df_long_27 %>%
  group_by(period) %>%
  summarise(
    mean_usage = mean(value, na.rm = TRUE),
    median_usage = median(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    year_start = as.numeric(str_extract(period, "\\d{4}"))
  ) %>%
  arrange(year_start)

#_____________________________________
# Visualisation
#_____________________________________

ggplot(mean_usage_year, aes(x = year_start)) +
  geom_line(aes(y = mean_usage), colour = "steelblue", linewidth = 1) +
  geom_line(aes(y = median_usage), colour = "darkred", linewidth = 0.8, linetype = "dashed") +
  labs(
    title = "Figure 3: Average station usage over time per regions (1997 - 2025)",
    x = "Year",
    y = "Passenger entries and exits",
    caption = "Mean and median annual passenger entries and exits per station. Source: ORR Table 1415A."
  ) +
  theme_minimal()

#__________________________________
# Regional level 
#__________________________________

mean_usage_region <- df_long_27 %>%
  group_by(period, Region) %>%
  summarise(
    mean_usage = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    year_start = as.numeric(str_extract(period, "\\d{4}"))
  ) %>%
  arrange(year_start)

#___________________________________
# Vizualization
#___________________________________

mean_usage_region <- mean_usage_region %>%
  mutate(
    line_type = ifelse(Region == "London", "London", "Other regions")
  )
ggplot(mean_usage_region,
       aes(x = year_start,
           y = mean_usage,
           colour = Region,
           group = Region,
           linetype = line_type)) +
  geom_line(linewidth = 1) +
  scale_linetype_manual(
    values = c("London" = "dashed", "Other regions" = "solid")
  ) +
  labs(
    title = " Figure 3: Average stations entries and exits over time by region",
    x = "Year",
    y = "Passenger entries and exits",
    caption = "Mean annual passenger entries and exits per station. Source: ORR Table 1415A."
  ) +
  theme_minimal()
out_fig <- "Produced_figures"
ggsave(
  filename = file.path(out_fig, "figure_3_mean_stations_entries_and_exits.png"),
  width = 8,
  height = 5,
  dpi = 300
)

#____________________________________
# FETURING 
#____________________________________

#___
# 1.1 Initial year 
#___
df_long_27 <- df_long_27 %>%
  mutate(
    year_start = as.numeric(str_extract(period, "\\d{4}"))
  )

#___
# 1.2 Phase (Covid,pre post)
#___

df_long_27 <- df_long_27 %>%
  mutate(
    phase = case_when(
      year_start >= 2019 & year_start <= 2021 ~ "covid",
      year_start >= 2022                   ~ "post_covid",
      TRUE                                 ~ "pre_covid"
    )
  )

#__
# 1.3 Mean from stations and phases 
#__
station_phase_means <- df_long_27 %>%
  filter(!is.na(phase)) %>%
  group_by(Sort, Region,`Station name`, phase) %>%
  summarise(
    mean_usage = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = phase,
    values_from = mean_usage
  )

#___
#1.4 Ratio 
#__

station_features <- station_phase_means %>%
  mutate(
    shock_ratio = covid / pre_covid,
    recovery_ratio = post_covid / pre_covid
  )

# -----------------------------
# 1.5 Regional size (rank relativo)
# -----------------------------
station_features <- station_features %>%
  group_by(Region) %>%
  mutate(
    regional_rank = percent_rank(desc(pre_covid)),
    regional_size = case_when(
      regional_rank <= 0.10 ~ "core",
      regional_rank <= 0.40 ~ "secondary",
      TRUE ~ "peripheral"
    )
  ) %>%
  ungroup()

#________________________________
# Most used stations 
#________________________________

station_rankings <- df_long_27 %>%
  mutate(
    year_start = as.numeric(str_extract(period, "\\d{4}")),
    phase = case_when(
      year_start >= 2019 & year_start <= 2021 ~ "covid",
      year_start >= 2022                      ~ "post_covid",
      TRUE                                   ~ "pre_covid"
    )
  ) %>%
  group_by(Sort, `Station name`, Region) %>%
  summarise(
    sum_historical = sum(value, na.rm = TRUE),
    sum_pre        = sum(value[phase == "pre_covid"], na.rm = TRUE),
    sum_covid      = sum(value[phase == "covid"], na.rm = TRUE),
    sum_post       = sum(value[phase == "post_covid"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Region) %>%
  mutate(
    rank_historical = dense_rank(desc(sum_historical)),
    rank_pre        = dense_rank(desc(sum_pre)),
    rank_covid      = dense_rank(desc(sum_covid)),
    rank_post       = dense_rank(desc(sum_post))
  ) %>%
  ungroup()

#______________________________________________
# 2. Modeling 
#______________________________________________

#___
# Pre-COVID temporal baseline (time-only linear model)
#___

df_model <- df_long_27 %>%
  mutate(
    year = as.numeric(str_extract(period, "\\d{4}")),
    log_usage = log1p(value)
  ) %>%
  filter(!is.na(year))

train <- df_model %>% filter(year <= 2018)
test  <- df_model %>% filter(year > 2018)


lm_pre <- lm(log_usage ~ year, data = train)
summary(lm_pre)
test <- test %>%
  mutate(
    pred = predict(lm_pre, newdata = test),
    error = log_usage - pred
  )
test %>%
  mutate(
    phase = case_when(
      year %in% c(2019, 2020, 2021) ~ "COVID",
      year >= 2022 ~ "POST"
    )
  ) %>%
  group_by(phase) %>%
  summarise(
    mean_error = mean(error),
    sd_error = sd(error),
    mae = mean(abs(error))
  )

#____
#
#____

#_____
#  lm: Hierarchical station-size interaction model (pre-COVID defined)
#_____

station_size <- df_long_27 %>%
  filter(year_start <= 2018) %>%
  group_by(Sort, Region, `Station name`) %>%
  summarise(
    pre_total = sum(value, na.rm = TRUE),
    .groups = "drop"
  )

station_size <- station_size %>%
  mutate(
    size_class = case_when(
      pre_total >= quantile(pre_total, 0.90) ~ "hub",
      pre_total >= quantile(pre_total, 0.40) ~ "intermediate",
      TRUE                                   ~ "peripheral"
    )
  )

df_long_27 <- df_long_27 %>%
  left_join(
    station_size %>% select(Sort, size_class),
    by = "Sort"
  )

df_model <- df_long_27 %>%
  mutate(
    year = as.numeric(str_extract(period, "\\d{4}")),
    log_usage = log1p(value)
  ) %>%
  filter(
    !is.na(year),
    !is.na(log_usage)
  )

lm_hubs <- lm(log_usage ~ year, data = df_model %>% filter(size_class == "hub"))
lm_mid  <- lm(log_usage ~ year, data = df_model %>% filter(size_class == "intermediate"))
lm_per  <- lm(log_usage ~ year, data = df_model %>% filter(size_class == "peripheral"))

lm_full <- lm(log_usage ~ year * size_class, data = df_model)
lm2 <-summary(lm_full)

#_______
# lm: Size-heterogeneous COVID shock model
#_______

df_model_covid <- df_long_27 %>%
  mutate(
    year = as.numeric(str_extract(period, "\\d{4}")),
    log_usage = log1p(value),
    covid = as.integer(year >= 2019 & year <= 2021)
  ) %>%
  filter(!is.na(year))

df_model_covid <- df_model_covid %>%
  group_by(Sort) %>%
  mutate(
    mean_pre = mean(value[year <= 2018], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    size_class = case_when(
      mean_pre >= quantile(mean_pre, 0.90, na.rm = TRUE) ~ "hub",
      mean_pre >= quantile(mean_pre, 0.40, na.rm = TRUE) ~ "intermediate",
      TRUE                                               ~ "peripheral"
    )
  )

lm_base <- lm(log_usage ~ year + covid, data = df_model_covid)
summary(lm_base)

df_model_covid$size_class <- factor(df_model_covid$size_class)
lm_size_covid <- lm(
  log_usage ~ year + covid * size_class,
  data = df_model_covid
)
summary(lm_size_covid)
lm3 <- summary(lm_size_covid)

#___
# lm: Structural station-level stability analysis

#____

df_model_covid <- df_long_27 %>%
  mutate(
    year = as.numeric(str_extract(period, "\\d{4}")),
    log_usage = log1p(value),
    covid = as.integer(year >= 2019 & year <= 2021)
  ) %>%
  filter(!is.na(year))


df_model_covid <- df_model_covid %>%
  group_by(Sort) %>%
  mutate(
    mean_pre = mean(value[year <= 2018], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    size_class = case_when(
      mean_pre >= quantile(mean_pre, 0.90, na.rm = TRUE) ~ "hub",
      mean_pre >= quantile(mean_pre, 0.40, na.rm = TRUE) ~ "intermediate",
      TRUE                                               ~ "peripheral"
    )
  )

df_model_covid$size_class <- factor(df_model_covid$size_class)
df_model_covid$Region     <- factor(df_model_covid$Region)

lm_safe <- lm(
  log_usage ~ year + covid + size_class + Region,
  data = df_model_covid
)

summary(lm_safe)
lm_no_region <- lm(
  log_usage ~ year + covid + size_class,
  data = df_model_covid
)

anova(lm_no_region, lm_safe)

#__
#Pre-COVID structural baseline with post-COVID deviation analysis

#___

df_model_covid <- df_model_covid %>%
  group_by(Sort) %>%
  mutate(
    mean_pre = mean(value[year <= 2018], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    size_class = case_when(
      mean_pre >= quantile(mean_pre, 0.90, na.rm = TRUE) ~ "hub",
      mean_pre >= quantile(mean_pre, 0.40, na.rm = TRUE) ~ "intermediate",
      TRUE                                               ~ "peripheral"
    ),
    size_class = factor(size_class)
  )
train <- df_model_covid %>% filter(year <= 2018)
test  <- df_model_covid %>% filter(year > 2018)

lm_pre <- lm(
  log_usage ~ year + size_class + Region,
  data = train
)

summary(lm_pre)
test <- test %>%
  mutate(
    fitted = predict(lm_pre, newdata = test),
    error  = log_usage - fitted
  )


test %>%
  mutate(
    phase = case_when(
      year %in% c(2019, 2020, 2021) ~ "COVID",
      year >= 2022 ~ "POST"
    )
  ) %>%
  group_by(phase) %>%
  summarise(
    mean_error = mean(error),
    sd_error   = sd(error),
    mae        = mean(abs(error)),
    .groups = "drop"
  )

test %>%
  group_by(size_class) %>%
  summarise(
    mae = mean(abs(error)),
    mean_error = mean(error),
    .groups = "drop"
  )

#___
# lm: # Behavioural stability assessment using a pre-COVID baseline
#___

lm_pre <- lm(
  log_usage ~ year + size_class + Region,
  data = train
)
summary(lm_pre)
test %>%
  group_by(phase) %>%
  summarise(
    mae = mean(abs(error)),
    mean_error = mean(error)
  )

mae_matrix <- test %>%
  group_by(Region, size_class) %>%
  summarise(
    mae = mean(abs(error), na.rm = TRUE),
    .groups = "drop"
  )

mae_matrix
mae_matrix_phase <- test %>%
  mutate(
    phase = case_when(
      year %in% c(2019, 2020, 2021) ~ "COVID",
      year >= 2022 ~ "POST"
    )
  ) %>%
  group_by(phase, Region, size_class) %>%
  summarise(
    mae = mean(abs(error), na.rm = TRUE),
    .groups = "drop"
  )

mae_matrix_phase
mae_wide <- mae_matrix_phase %>%
  tidyr::pivot_wider(
    names_from = size_class,
    values_from = mae
  )

mae_wide
ggplot(mae_matrix_phase,
       aes(x = size_class, y = Region, fill = mae)) +
  geom_tile() +
  facet_wrap(~ phase) +
  scale_fill_viridis_c() +
  labs(
    title = "Mean Absolute Error by region and station size",
    subtitle = "Model trained on pre-2018 data",
    x = "Station size class",
    y = "Region",
    fill = "MAE"
  ) +
  theme_minimal()

ggplot(test, aes(x = year, y = error)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(alpha = 0.15) +
  geom_smooth(se = FALSE)

c <- c(unique(train$Region))
t <- c(unique(test$Region))


anova(lm(log_usage ~ size_class, data = train))

#_____
# Panel fixed-effects model with phase indicators (PRE / COVID / POST)
#_____

df_one <- df_long_27 %>%
  mutate(
    year = as.numeric(str_extract(period, "\\d{4}")),
    log_usage = log1p(value),
    phase = case_when(
      year %in% c(2019, 2020, 2021) ~ "COVID",
      year >= 2022 ~ "POST",
      TRUE ~ "PRE"
    ),
    phase = factor(phase, levels = c("PRE", "COVID", "POST"))
  ) %>%
  filter(!is.na(year), !is.na(log_usage))

# Single credible model:
m_stability <- feols(
  log_usage ~ year + phase | Sort,   
  data = df_one,
  cluster = "Sort"                   
)

summary(m_stability)
