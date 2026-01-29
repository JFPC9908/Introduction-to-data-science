#--------------------------------------------------------------------
# 0) Install all required packages and load the necessary libraries
#--------------------------------------------------------------------


library(ggdendro)
library(patchwork)
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(tidyr)
library(sf)
library(scales)

#-------------------------------------------------------------------
#  1) Upload pre procesed data from failsafe.r 
# This data uses all the information recolected from the ORR.
# If a data review is needed please direct to the following tables published for the ORR:
# 
#-------------------------------------------------------------------
## Totals only 
df_region_totals_clean<- read.csv("Data visualisation/df_region_totals_clean")
unique(df_region_totals_clean$Region)
unique(df_region_totals_clean$journey_scope)
##All regions complet Data set 
df_all_regions <- read.csv("Produced_tables/df_all_regions.csv")
unique(df_all_regions$Region)
unique(df_all_regions$type.of.journey)
unique(df_all_regions$journey_scope)
unique(df_all_regions$Time.period)
unique(df_all_regions$sort)

#--------------------------------------------------------------------
# 2) Reorganizang pre prosed data for graph 1 
#--------------------------------------------------------------------

df_bar <- df_region_totals_clean %>%
  group_by(Region) %>%
  summarise(
    region_total = sum(total_journeys, na.rm = TRUE),
    between_journeys = sum(
      total_journeys[journey_scope == "Between regions"],
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    system_share = region_total / sum(region_total, na.rm = TRUE),
    between_ratio = between_journeys / region_total
  )

#-------------------------------------------------------------------
# Visualisation 1 (Bar graph)
#-------------------------------------------------------------------

## Visualisation using ggplot. All the data is clean.
Figure1 <-ggplot(
  df_bar, # This DF contains the raw and un-changed format of x passengers journeys in thousands. For this specific case, and following the idea of working with percentages rather than raw numbers is usfull. 
  aes(
    x = reorder(Region,system_share),
    y = system_share,
    fill = between_ratio)
) +
 geom_bar(stat = "identity")  +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, by = 0.10),        
    minor_breaks = seq(0, 1, by = 0.05),  
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_fill_gradient(
    low = "#5e3c99",
    high = "#e66101",
    labels = scales::percent_format(accuracy = 1),
    name = "Inter-regional journeys\n(share of region)"
  ) +
  labs(
    title = "Regional contribution to total rail journeys and inter-regional orientation",
    subtitle = "Bar height shows each region’s share of total journeys; colour indicates the proportion of journeys made between regions",
    x = "Region",
    y = "Share of total journeys",
    caption = "Figure 1"
  ) +
theme_minimal() +   # <-- en vez de theme_void()
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.1),

    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(linewidth = 1, colour = "black")
  )+
theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
Figure1 <- Figure1 +
  labs(title = "Figure 1") +
  theme(plot.title.position = "plot")
Figure1
ggsave(
  filename = "Produced_figures/regions/figure_5_regional_share_total_journeys.png",
  plot = Figure1,
  width = 8,
  height = 10,
  dpi = 300
)
#-------------------------------------------------------------------
# Visualisation 2 (Tiles heatmap)
#-------------------------------------------------------------------
flows_hist <- df_all_regions %>%
  filter(journey_scope == "Between regions") %>%
  mutate(
    orig = Region,
    dest = str_remove(type.of.journey, "\\s*thousands\\s*$"),
    w = amount.of.journeys..thousands.* 1000
  ) %>%
  filter(!is.na(w), orig != dest) %>%
  group_by(orig, dest) %>%
  summarise(w = sum(w, na.rm = TRUE), .groups = "drop")

flows_share <- flows_hist %>%
  group_by(orig) %>%
  mutate(
    total_w = sum(w, na.rm = TRUE),
    share = if_else(total_w > 0, w / total_w, NA_real_),
    is_max = share == max(share, na.rm = TRUE)
  ) %>%
  ungroup()

figure2 <- ggplot(flows_share, aes(x = dest, y = orig, fill = share)) +
  geom_tile(color = "grey90") +
  # outline the strongest destination(s) per origin
  geom_tile(
    data = subset(flows_share, is_max),
    fill = NA, color = "black", linewidth = 0.8
  ) +
  scale_fill_gradientn(
    colours = c("#ffeda0", "#feb24c", "#f03b20"),
    limits = c(0, 1),
    oob = scales::squish,
    labels = percent_format(accuracy = 1),
    name = "Share"
  ) +
  labs(
    x = "Destination",
    y = "Origin",
    title = "OD heatmap (shares) + strongest destination per origin outlined"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.25),
    panel.grid = element_blank()
  )
figure2 <- figure2 +
  labs(title = "Figure 2") +
  theme(plot.title.position = "plot")
figure2
# After having the visuzlaization a check is needed 
top_dest_por_region <- flows_hist %>%
  group_by(orig) %>%
  mutate(share = w / sum(w, na.rm = TRUE)) %>%
  slice_max(order_by = w, n = 1, with_ties = TRUE) %>%
  ungroup() %>%
  select(orig, top_dest = dest, w, share) %>%
  arrange(desc(w))

top_dest_por_region
#___________________________________________________
#Top dest per region pre and post covid
#___________________________________________________stringr)
df_all_regions <- df_all_regions %>%
  mutate(
    year = as.numeric(str_extract(Time.period, "\\d{4}")),
    period_covid = case_when(
      year <= 2018 ~ "PRE",
      year >= 2022 ~ "POST",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period_covid))

# -----------------------------
# 2.  OD flows
# -----------------------------
flows_hist <- df_all_regions %>%
  filter(journey_scope == "Between regions") %>%
  mutate(
    orig = Region,
    dest = str_remove(type.of.journey, "\\s*thousands\\s*$"),
    w = amount.of.journeys..thousands. * 1000
  ) %>%
  filter(!is.na(w), orig != dest) %>%
  group_by(period_covid, orig, dest) %>%
  summarise(w = sum(w, na.rm = TRUE), .groups = "drop")

# -----------------------------
# 3. Shares  origin / time period 
# -----------------------------
flows_share <- flows_hist %>%
  group_by(period_covid, orig) %>%
  mutate(
    total_w = sum(w, na.rm = TRUE),
    share = w / total_w,
    is_max = share == max(share, na.rm = TRUE)
  ) %>%
  ungroup()

# -----------------------------
# 4. Heatmap PRE vs POST
# -----------------------------
figure5 <- ggplot(flows_share, aes(x = dest, y = orig, fill = share)) +
  geom_tile(color = "grey90") +
  geom_tile(
    data = subset(flows_share, is_max),
    fill = NA, color = "black", linewidth = 0.7
  ) +
  facet_wrap(~ period_covid) +
  scale_fill_gradientn(
    colours = c("#ffeda0", "#feb24c", "#f03b20"),
    limits = c(0, 1),
    oob = scales::squish,
    labels = scales::percent
  ) +
  labs(
    x = "Destination region",
    y = "Origin region",
    fill = "Share",
    title = "Figure 5: Inter-regional OD structure (PRE vs POST COVID)",
    caption = "Source: Office of Rail and Road (ORR), operational regions datasets."
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.25),
    panel.grid = element_blank()
  )

ggsave(
  filename = "figure_5_interregional_od_structure_pre_post_covid.png",
  plot = figure5,
  width = 8,
  height = 6,
  dpi = 300
)

# -----------------------------
# 5. Dominant destination per región
# -----------------------------
top_dest_por_region <- flows_hist %>%
  group_by(period_covid, orig) %>%
  mutate(share = w / sum(w, na.rm = TRUE)) %>%
  slice_max(order_by = w, n = 1, with_ties = TRUE) %>%
  ungroup() %>%
  select(period_covid, orig, top_dest = dest, w, share) %>%
  arrange(period_covid, desc(w))

top_dest_por_region

#______________________________________________________
# Figure 3 Regional map  
#______________________________________________________

library(sf)
library(dplyr)
library(ggplot2)

itl1 <- st_read("Data visualisation/ITL1_JAN_2025_UK_BUC_7687044825815463778.geojson", quiet = TRUE) %>%
  st_make_valid()

itl1 <- itl1 |>
  filter(
    ITL125NM != "Northern Ireland"
  )
names(itl1)
head(itl1)
ggplot(itl1) +
  geom_sf() +
  theme_void()

unique(df_region_totals_clean$Region)
unique(itl1$ITL125NM)

library(dplyr)
library(stringr)

itl1 <- itl1 %>%
  mutate(
    Region = ITL125NM,
    Region = str_replace(Region, "^East \\(England\\)$", "East England"),
    Region = str_replace(Region, " \\(England\\)$", ""),  # quita "(England)" al final
    Region = str_replace(Region, "^Yorkshire and The Humber$", "Yorkshire and the Humber")
  )

df_totales_region_per <- df_region_totals_clean %>%
  group_by(Region) %>%
  summarise(
    total_region = sum(total_journeys, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    system_share = total_region / sum(total_region, na.rm = TRUE)
  )
sum(df_totales_region_per$system_share, na.rm = TRUE)
setdiff(unique(df_totales_region_per$Region), unique(itl1$Region))
setdiff(unique(itl1$Region), unique(df_totales_region_per$Region))


itl1_join <- itl1 %>%
  left_join(df_totales_region_per, by = "Region")

library(ggplot2)


ggplot(itl1_join) +
  geom_sf(aes(fill = system_share*100), colour = "white", linewidth = 0.2) +
  scale_fill_gradient(
    labels = comma,
    name = "Total journeys"
  ) +
  labs(title = "Total rail journeys by region") +
  theme_minimal() +
  theme(panel.grid = element_blank())

library(dplyr)
library(stringr)
region_codes <- tibble::tribble(
  ~Region,                    ~code,
  "London",                   "LO",
  "East England",             "EE",
  "East Midlands",            "EM",
  "North East",               "NE",
  "North West",               "NW",
  "Yorkshire and\nthe Humber", "YH",
  "South East",               "SE",
  "South West",               "SW",
  "West Midlands",            "WM",
  "Wales",                    "WA",
  "Scotland",                 "SC"
)
library(sf)
library(dplyr)

itl1_map <- itl1_join %>%
  left_join(region_codes, by = "Region")

pts <- itl1_map %>%
  st_point_on_surface() %>%   # mejor que centroid
  mutate(
    X = st_coordinates(.)[,1],
    Y = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry()


bbox <- st_bbox(itl1_map)
 
figure3 <-ggplot(itl1_map) +
  geom_sf(aes(fill = system_share), colour = "white", linewidth = 0.4) +
  geom_label(
    data = pts,
    aes(x = X, y = Y, label = code),
    size = 2.6,
    label.size = 0,
    fill = "white",
    alpha = 0.7,
    fontface = "bold"
  ) +
  annotate(
    "label",
x = bbox["xmin"], y = bbox["ymin"], hjust = 0.12, vjust = 0,
    hjust = 0, vjust = 0,
    size = 3,
    fill = "white",
    alpha = 0.2,
    label =
"
Convertion table: 
LO = London
EE = East England
EM = East Midlands
NE = North East
NW = North West
YH = Yorkshire\nand the Humber
SE = South East
SW = South West
WM = West Midlands
WA = Wales
SC = Scotland"
  ) +
  scale_fill_gradient(
    labels = percent_format(accuracy = 1),
    name = "Share of total journeys"
  ) +
  labs(title = "Figure 5: Regional share of total rail journeys",
caption = "Data source: Office of Rail and Road (ORR) Tables 1540–1590.") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )
ggsave(
  filename = "Produced_figures/regions/figure_5_regional_share_total_journeys.png",
  plot = figure3,
  width = 8,
  height = 10,
  dpi = 300
)
#______________________________________________________
# Figure 4 Cluster of regions  
#______________________________________________________


df_edges <- df_all_regions %>%
  filter(journey_scope == "Between regions") %>%
  mutate(
    orig_region = Region,
    dest_region = str_remove(type.of.journey, "\\s*thousands\\s*$"),
    w = amount.of.journeys..thousands. * 1000   
  ) %>%
  filter(!is.na(w), orig_region != dest_region)
df_edges %>% count(orig_region, dest_region) %>% head(10)
df_reg_series <- df_edges %>%
  group_by(Time.period, sort, orig_region) %>%
  summarise(metric = sum(w, na.rm = TRUE), .groups = "drop") %>%
  rename(Region = orig_region)
wide <- df_reg_series %>%
  group_by(Region) %>%
  arrange(sort, .by_group = TRUE) %>%
  mutate(z = as.numeric(scale(metric))) %>%
  ungroup() %>%
  select(sort, Time.period, Region, z) %>%
  pivot_wider(names_from = Region, values_from = z) %>%
  arrange(sort)

mat <- wide %>% select(-sort, -Time.period) %>% as.matrix()

cor_mat <- cor(mat, use = "complete.obs")
hc <- hclust(as.dist(1 - cor_mat), method = "average")

figure4 <- plot(hc, main="Figure 4. Regional clustering of regions (Between journeys, correlation distance)", xlab="", sub="")
clusters <- cutree(hc, k = 3)
install.packages("ggdendro")
dendro <- ggdendro::dendro_data(hc)

figuere4 <- ggplot() +
  geom_segment(
    data = dendro$segments,
    aes(x = x, y = y, xend = xend, yend = yend),
    linewidth = 0.6
  ) +
  geom_text(
    data = dendro$labels,
    aes(x = x, y = y - 0.02, label = label),
    angle = 90,
    hjust = 1,
    size = 3
  ) +
  labs(
    title = "Figure 4. Regional clustering of regions",
    x = "",
    y = "Height"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

Figure4 <- clusters

dendro <- ggdendro::dendro_data(hc)

figuere4 <- ggplot() +
  geom_segment(
    data = dendro$segments,
    aes(x = x, y = y, xend = xend, yend = yend),
    linewidth = 0.6
  ) +
  geom_text(
    data = dendro$labels,
    aes(x = x, y = y, label = label),
    angle = 30,
    vjust = 1,
    size = 3
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0.15, 0.15))  
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.15, 0.05))
  ) +
  labs(
    title = "Figure 4: clustering of regions based on temporal patterns of inter-regional activity",
    x = "",
    y = "Height",
    caption= "Tables 1540 - 1590 ORR Operational regions Passengers journyes flows."
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
dir.create("Produced_figures/regions", recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = "Produced_figures/regions/figure_4_regional_clustering.png",
  plot = figuere4,
  width = 8,
  height = 6,
  dpi = 300
)


#______________________________________
# Join t 
#______________________________________

all_figs <- (Figure1 | figuere2) / (figure3 | figuere4) +
  plot_annotation(
    title = "Figures 1–4",
    caption = "Source: ORR station usage / your processed dataset"
  ) 
 
  theme(
    plot.title.position = "plot"
  )
  plot_layout(guides = "keep")  # junta leyendas si aplica

all_figs


all_figs <- ... +
  plot_annotation(...) +
  plot_layout(guides="keep") &
  theme(...)

all_figs

all_figs <- ((Figure1 | figuere2) /
             (figure3 | figuere4)) +
  plot_layout(guides = "keep", heights = c(1.2, 1.8))  # 🔑 más espacio abajo
all_figs
figuere3 <- figuere3 & theme(
  plot.title.position = "plot",
  plot.margin = margin(18, 6, 6, 6)   # 🔑 más aire arriba dentro
)

figuere4 <- figuere4 + theme(
  plot.title.position = "plot",
  plot.margin = margin(18, 6, 6, 6)
)
figuere3 <- (p_within | p_between) +
  plot_annotation(title = "Figure 3") &
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.title.position = "plot",
    plot.margin = margin(10, 6, 6, 6)
  )
figuere4 <- figuere4 +
  labs(title = "Figure 4") +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.title.position = "plot",
    plot.margin = margin(10, 6, 6, 6)
  )

all_figs <- ((Figuere1 | figuere2) /
             (figuere3 | figuere4)) +
  plot_layout(guides = "keep", heights = c(1, 1.4))  # más espacio abajo

figuere3 <- ((p_within | p_between) +
  plot_annotation(title = "Figure 3")) &
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.title.position = "plot",
    plot.margin = margin(12, 6, 6, 6)
  )

all_figs <- ((Figuere1 | figuere2) /
             (figuere3 | figuere4)) +
  plot_layout(guides = "keep", heights = c(1, 1.6))
figuere3 <- (p_within | p_between) +
  plot_annotation(title = "Figure 3", tag_levels = "a")

all_figs <- ((Figuere1 | figuere2) /
             (figuere3 | figuere4)) +
  plot_layout(guides = "keep", heights = c(1, 2))
ggsave("Figures_1to4.png", all_figs, width = 18, height = 14, dpi = 300)
