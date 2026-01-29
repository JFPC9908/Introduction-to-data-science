# ============================================================
# Project: UK Railway System – Structural Stability Analysis
# File: 02_data_preprocessing.R
# Author: 
# Date: 2026-01-28
# Description:
#   Data loading, cleaning and reshaping, and analysis of ORR operative regional tables
# ORR (1540 - 1590).
# ============================================================
#_____________________________________________________________
#Librarys
#_____________________________________________________________
library(tidyr)
library(cluster)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(ggplot2)
library(ggdendro)
#_____________________________________________________________
# Load data: Original raw and treted data can be found in the Failsafe scrit of data visulisation 
#_____________________________________________________________
regions <- read.csv("Produced_tables/df_all_regions.csv")
names(regions)
unique(regions$journey_scope)
Time.perdiods <- unique(regions$Time.period)
Sort <- unique(regions$sort)
Region <- unique(regions$Region)
Destitantions <- unique(regions$type.of.journey)

# ---------------------------------------------------------------------------------------------
# Exploratory Data Analysis (EDA)
#
# Inter-regional connections are initially represented in a symmetric and volumetric way:
# flows from Region A to Region B are equal to flows from Region B to Region A in absolute terms.
#
# However, exploratory analysis of the aggregated regional dataset shows that regions do not
# behave symmetrically in relative terms. Identical volumetric flows can represent very
# different structural relationships once regional scale is taken into account.
#
# For example, if London records 2,000 total passenger journeys in a given year and Scotland
# records 750, a bilateral flow of 400 journeys represents only 20% of London’s activity but
# more than 50% of Scotland’s. This indicates a stronger gravitational dependence of Scotland
# on London than vice versa.
#
# Consequently, regional connectivity must be analysed using relative shares rather than
# absolute volumes, motivating a shift from symmetric counts to origin-normalised measures.
# ---------------------------------------------------------------------------------------------
#_____________________________________________________________________________________________

#_________________________________
#  Feature engineering: origin-normalised OD shares
#_________________________________

df_region_od_year <- regions %>%
  mutate(
    period = Time.period,
    year = as.integer(str_extract(Time.period, "\\d{4}")),
    journeys = amount.of.journeys..thousands. * 1000,
    origin = Region,
    destination = case_when(
      journey_scope == "Within region" ~ Region,
      journey_scope == "Between regions" ~ type.of.journey,
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(destination)) %>%
  group_by(period,year, origin, destination, journey_scope) %>%
  summarise(
    journeys = sum(journeys, na.rm = TRUE),
    .groups = "drop"
  )
names(regions)

df_od_share <- df_region_od_year %>%
  group_by(year, origin) %>%
  mutate(
    total_origin = sum(journeys),
    share = journeys / total_origin
  ) %>%
  ungroup()
unique(df_od_share$destination)

#_____________________________________________________________
# RQ1 – Journey composition features
# Within-region vs inter-regional shares
#_____________________________________________________________
# Aggregated composition features (within vs between region)

df_composition <- df_od_share %>%
  group_by(period, year, origin, journey_scope) %>%
  summarise(
    share = sum(share, na.rm = TRUE),
    .groups = "drop"
  )
# System-level composition features (double-counting adjusted)

df_system_composition <- regions %>%
  mutate(
    year = as.integer(str_extract(Time.period, "\\d{4}")),
    journeys = amount.of.journeys..thousands. * 1000,
    weight = if_else(journey_scope == "Between regions", 0.5, 1)
  ) %>%
  group_by(year, journey_scope) %>%
  summarise(
    journeys_weighted = sum(journeys * weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(year) %>%
  mutate(
    share = journeys_weighted / sum(journeys_weighted)
  ) %>%
  ungroup()
df_system_composition %>%
  group_by(year) %>%
  summarise(total = sum(share))

#_____________________________________________________
# Figure 1: System-level journey composition over time
#_____________________________________________________

figuere1<-ggplot(df_system_composition,
       aes(x = year, y = share, fill = journey_scope)) +
  geom_area(alpha = 0.85) +
  geom_vline(xintercept = 2019, linetype = 2, colour = "grey40") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Figure 1. Composition of passenger journeys by journey type (1995–2025)",
    x = "Year",
    y = "Share of total journeys",
    fill = "Journey scope",
    caption = "Between-region journeys weighted by 0.5; dashed line indicates COVID-19 (2019)."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 10),     
    plot.caption = element_text(size = 9)     
  )

ggsave(
  filename = "Produced_figures/regions/figure_1_journey_composition.png",
  width = 8,
  height = 5,
  dpi = 300
)

#_____________________________________________________________
# System-level aggregation of passenger journeys
# Double-counting adjusted totals
#_____________________________________________________________
#________________________________________________
# # System-level aggregation of passenger journeys (double-counting adjusted)
#________________________________________________

df_system_year <- regions %>%
  mutate(
    year = as.integer(str_extract(Time.period, "\\d{4}")),
    journeys = amount.of.journeys..thousands. * 1000
  ) %>%
  group_by(year, journey_scope) %>%
  summarise(
    journeys_sum = sum(journeys, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    journeys_adj = if_else(journey_scope == "Between regions",
                            journeys_sum / 2,
                            journeys_sum)
  ) %>%
  group_by(year) %>%
  mutate(
    total = sum(journeys_adj),
    share = journeys_adj / total
  ) %>%
  ungroup()

df_system_year_abs <- regions %>%
  mutate(
    year = as.integer(str_extract(Time.period, "\\d{4}")),
    journeys = amount.of.journeys..thousands. * 1000
  ) %>%
  group_by(year, journey_scope) %>%
  summarise(
    journeys_sum = sum(journeys, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    journeys_adj = if_else(
      journey_scope == "Between regions",
      journeys_sum / 2,
      journeys_sum
    )
  )

ggplot(df_system_year_abs,
       aes(x = year, y = journeys_adj, colour = journey_scope)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 2019, linetype = 2, colour = "grey40") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "System-level rail journeys by journey type",
    x = "Year",
    y = "Passenger journeys",
    colour = "Journey type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave(
  filename = "Produced_figures/regions/figure_2_system_level_journeys_by_type.png",
  plot = last_plot(),
  width = 8,
  height = 5,
  dpi = 300
)
#_____________________________________________________________
# RQ2 – Structurally dominant destination regions
# Inter-regional journey shares
#_____________________________________________________________

df_between <- regions %>%
  mutate(
    year = as.integer(str_extract(Time.period, "\\d{4}")),
    period = if_else(year < 2019, "Pre-COVID", "Post-COVID"),
    destination = type.of.journey,
    journeys = amount.of.journeys..thousands.
  ) %>%
  filter(journey_scope == "Between regions")
df_dest_share <- df_between %>%
  group_by(period, destination) %>%
  summarise(
    total_journeys = sum(journeys, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(period) %>%
  mutate(
    share = total_journeys / sum(total_journeys)
  ) %>%
  ungroup()
top_destinations <- df_dest_share %>%
  filter(period == "Pre-COVID") %>%
  arrange(desc(share)) %>%
  slice_head(n = 5) %>%
  pull(destination)
df_top_compare <- df_dest_share %>%
  filter(destination %in% top_destinations)

ggplot(df_top_compare,
       aes(x = destination, y = share, fill = period)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Dominant destination regions in inter-regional journeys",
    x = "Destination region",
    y = "Share of inter-regional journeys",
    fill = "Period"
  ) +
  theme_minimal()

df_dest_share %>%
  group_by(period) %>%
  
#_____________________________________________________________
# System-wide passenger journeys over time
# Contextual exploratory visualisation
#_____________________________________________________________
  
df_region_year_weighted <- regions %>%
  mutate(
    year_start = as.integer(str_extract(Time.period, "\\d{4}")),
    journeys = amount.of.journeys..thousands. * 1000,
    weight = if_else(journey_scope == "Between regions", 0.5, 1)
  ) %>%
  group_by(year_start, Region) %>%
  summarise(
    weighted_journeys = sum(journeys * weight, na.rm = TRUE),
    .groups = "drop"
  )


#_________________________________________________________________________________________
# Exploratory visualisation of system-wide passenger journeys over time (absolute values)
#_________________________________________________________________________________________

df_system <- regions %>%
  mutate(
    year_start = as.integer(str_extract(Time.period, "\\d{4}")),
    journeys_thousands = parse_number(as.character(amount.of.journeys..thousands.))
  ) %>%
  group_by(year_start, journey_scope) %>%
  summarise(
    total_journeys = sum(journeys_thousands, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(df_system, aes(x = year_start, y = total_journeys, colour = journey_scope)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 2019, linetype = 2, colour = "grey40") +
  labs(
    title = "System-wide passenger journeys over time",
    x = "Year",
    y = "Journeys (thousands)",
    colour = "Journey scope"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

#_____________________________________________________________
# RQ2 – Structural concentration and regional dominance
# Rankings and comparative volumes
#_____________________________________________________________

df_base <- regions %>%
  mutate(
    year_start = as.integer(str_extract(Time.period, "\\d{4}")),
    journeys = parse_number(as.character(amount.of.journeys..thousands.))
  ) %>%
  filter(!is.na(journeys))

rank_total <- df_base %>%
  group_by(Region) %>%
  summarise(
    total_journeys = sum(journeys, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_journeys))

rank_total

rank_within <- df_base %>%
  filter(journey_scope == "Within region") %>%
  group_by(Region) %>%
  summarise(
    within_journeys = sum(journeys, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(within_journeys))

rank_within

rank_between <- df_base %>%
  filter(journey_scope == "Between regions") %>%
  group_by(Region) %>%
  summarise(
    between_journeys = sum(journeys, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(between_journeys))

rank_between

rank_connected <- df_base %>%
  filter(journey_scope == "Between regions") %>%
  group_by(dest_region = type.of.journey)%>%
    arrange(desc(dest_region))

rank_connected

#_____________________________________________________________
# RQ2 – Structural concentration metric
# Palma ratio (pre vs post COVID)
#_____________________________________________________________
totals_region <- regions %>%
  mutate(
    year_start = as.integer(str_extract(Time.period, "\\d{4}")),
    journeys_thousands = parse_number(as.character(amount.of.journeys..thousands.))
  ) %>%
  group_by(Region) %>%
  summarise(
    total_journeys = sum(journeys_thousands, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_journeys))

totals_region
library(dplyr)
library(knitr)

table_region_rank <- totals_region %>%
  mutate(
    total_journeys = round(total_journeys)
  ) %>%
  rename(
    Region = Region,
    `Total passenger journeys (thousands)` = total_journeys
  )

kable(
  table_region_rank,
  caption = "Table 1. Total passenger journeys by operational region (1995–2025).",
  align = c("l" , "r")
)
write.csv(
  table_region_rank,
  file = "/Users/juanfelipepatinocastano/Desktop/Coursework-Introduction-to-data-science&dataviz/Produced_tables/Regions/table_1_total_passenger_journeys_by_region.csv",
  row.names = FALSE
)
#______________________
# Palma ratio
#______________________

top_k <- 3
bottom_k <- 4
palma_total <- totals_region %>%
  summarise(
    palma_ratio = sum(total_journeys[1:top_k]) /
                  sum(total_journeys[(n() - bottom_k + 1):n()])
  )

palma_total

palma_by_period <- regions %>%
  mutate(
    year_start = as.integer(str_extract(Time.period, "\\d{4}")),
    period = if_else(year_start < 2019, "Pre-COVID", "Post-COVID"),
    journeys_thousands = parse_number(as.character(amount.of.journeys..thousands.))
  ) %>%
  group_by(period, Region) %>%
  summarise(
    total_journeys = sum(journeys_thousands, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(period) %>%
  arrange(desc(total_journeys), .by_group = TRUE) %>%
  summarise(
    palma_ratio =
      sum(total_journeys[1:top_k]) /
      sum(total_journeys[(n() - bottom_k + 1):n()]),
    .groups = "drop"
  )

palma_by_period
#__________________
#Table palma ratio
#__________________
palma_table <- palma_by_period %>%
  select(period, palma_ratio) %>%
  mutate(
    palma_ratio = round(palma_ratio, 2)
  )

palma_table
View(palma_table)

write.csv(
  palma_table,
  file = "/Users/juanfelipepatinocastano/Desktop/Coursework-Introduction-to-data-science&dataviz/Produced_tables/Regions/table_2_palma_ratio_pre_post_covid.csv",
  row.names = FALSE
)
#_____________________________________________________________
# Relational fingerprints and connectivity structure
#_____________________________________________________________

df_connection_shares_all <- regions %>%
  mutate(
    year_start = as.integer(str_extract(Time.period, "\\d{4}")),
    journeys   = parse_number(as.character(amount.of.journeys..thousands.))
  ) %>%
  filter(
    !is.na(journeys),
    !is.na(year_start)
  ) %>%
  group_by(year_start, Region) %>%
  mutate(
    total_region_year = sum(journeys, na.rm = TRUE),
    connection_share  = journeys / total_region_year
  ) %>%
  ungroup()


#__________________
# Fingerprints 
#__________________
df_fingerprint <- regions %>%
  mutate(
    year = as.integer(str_extract(Time.period, "\\d{4}")),
    journeys = parse_number(as.character(amount.of.journeys..thousands.))
  ) %>%
  group_by(year, Region) %>%
  mutate(
    total = sum(journeys),
    share = journeys / total
  ) %>%
  summarise(
    share_within = sum(share[journey_scope == "Within region"]),
    share_between = sum(share[journey_scope == "Between regions"]),
    london_share = sum(share[type.of.journey == "London"], na.rm = TRUE),
    .groups = "drop"
  )

#_____________________________________________________________
# Total passenger journeys in the UK railway system
#_____________________________________________________________

df_system_total <- regions %>%
  mutate(
    year = as.integer(str_extract(Time.period, "\\d{4}")),
    journeys = amount.of.journeys..thousands. * 1000,
    journeys_adj = if_else(
      journey_scope == "Between regions",
      journeys / 2,
      journeys
    )
  ) %>%
  group_by(year) %>%
  summarise(
    total_journeys = sum(journeys_adj, na.rm = TRUE),
    .groups = "drop"
  )
ggplot(df_system_total, aes(x = year, y = total_journeys)) +
  geom_line(linewidth = 1.1, colour = "steelblue") +
  geom_vline(xintercept = 2019, linetype = 2, colour = "grey40") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Figuere 2: Total passenger journeys in the UK railway system",
    x = "Year",
    y = "Passenger journeys",
    caption ="Inter-regional journeys counted once to avoid double-counting; includes post-2022 usage from the Elizabeth line; dashed line indicates COVID-19 (2019)."
  ) +
  theme_minimal()
ggsave(
  filename = "figure_2_total_passenger_journeys.png",
  width = 8,
  height = 5,
  dpi = 300
)

#_____________________________________________________________
# RQ2 – Regional clustering based on temporal inter-regional activity
# Correlation-based hierarchical clustering
#_____________________________________________________________

#-------------------------------------------------------------
# 1. Construct inter-regional edge weights (Between regions only)
#-------------------------------------------------------------

df_edges <- regions %>%
  filter(journey_scope == "Between regions") %>%
  mutate(
    orig_region = Region,
    dest_region = str_remove(type.of.journey, "\\s*thousands\\s*$"),
    journeys    = amount.of.journeys..thousands. * 1000
  ) %>%
  filter(!is.na(journeys), orig_region != dest_region)

#-------------------------------------------------------------
# 2. Aggregate yearly inter-regional activity per origin region
#-------------------------------------------------------------

df_region_series <- df_edges %>%
  mutate(year = as.integer(str_extract(Time.period, "\\d{4}"))) %>%
  group_by(year, orig_region) %>%
  summarise(
    total_inter_journeys = sum(journeys, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(Region = orig_region)

#-------------------------------------------------------------
# 3. Standardise time series (z-scores) to remove scale effects
#-------------------------------------------------------------

df_wide <- df_region_series %>%
  group_by(Region) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(z_activity = as.numeric(scale(total_inter_journeys))) %>%
  ungroup() %>%
  select(year, Region, z_activity) %>%
  pivot_wider(
    names_from  = Region,
    values_from = z_activity
  ) %>%
  arrange(year)

#-------------------------------------------------------------
# 4. Correlation matrix and hierarchical clustering
#-------------------------------------------------------------

cor_matrix <- cor(
  df_wide %>% select(-year),
  use = "complete.obs"
)

hc_regions <- hclust(
  as.dist(1 - cor_matrix),
  method = "average"
)

#-------------------------------------------------------------
# 5. Visualisation: dendrogram
#-------------------------------------------------------------

dendro <- ggdendro::dendro_data(hc_regions)

figure_4 <- ggplot() +
  geom_segment(
    data = dendro$segments,
    aes(x = x, y = y, xend = xend, yend = yend),
    linewidth = 0.6
  ) +
  geom_text(
    data = dendro$labels,
    aes(x = x, y = y, label = label),
    angle = 30,
    hjust = 1,
    size = 3
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.15, 0.15))) +
  scale_y_continuous(expand = expansion(mult = c(0.10, 0.05))) +
  labs(
    title   = "Figure 4. Hierarchical clustering of UK regions based on inter-regional activity",
    y       = "Correlation distance",
    x       = "",
    caption = "Clustering based on temporal correlation of inter-regional passenger journeys (ORR Tables 1540–1590)."
  ) +
  theme_minimal() +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )

ggsave(
  filename = "figure_4_regional_clustering.png",
  plot     = figure_4,
  width    = 8,
  height   = 6,
  dpi      = 300
)


#_______________
# Tables 
#_______________

#directories 
tables_dir <- "Produced_tables/Regions"
regions_tables_dir <- file.path(tables_dir, "Regions")

write.csv(
  table_region_rank,
  file = file.path(regions_tables_dir, "table_1_region_total_journeys_1995_2025.csv"),
  row.names = FALSE
)

write.csv(
  palma_table,
  file = file.path(regions_tables_dir, "table_2_palma_ratio_pre_post_covid.csv"),
  row.names = FALSE
)

write.csv(
  stations_per_region_table,
  file = file.path(regions_tables_dir, "table_3_stations_per_region.csv"),
  row.names = FALSE
)
