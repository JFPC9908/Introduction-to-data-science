library(ggplot2)
library(ggrepel)
library(dplyr)
library(stringr)
library(tidyr)
library(scales)

#_________________________
# Load the data 
##________________________
df_east_midlands <- read.csv("Sources/table-1540-regional-passenger-journeys-east-midlands - 1540_journeys_east_midlands.csv")
df_east_england <- read.csv("Sources/table-1545-regional-passenger-journeys-east-of-england - 1545_journeys_east_of_england.csv")
df_london <- read.csv("Sources/table-1550-regional-passenger-journeys-london (2) - 1550_journeys_london.csv")
df_north_east <- read.csv("Sources/table-1555-regional-passenger-journeys-north-east - 1555_journeys_north_east.csv")
df_north_west <- read.csv("Sources/table-1560-regional-passenger-journeys-north-west - 1560_journeys_north_west.csv")
df_scotland <- read.csv("Sources/table-1565-regional-passenger-journeys-scotland - 1565_regional_journeys_scotland.csv")
df_south_east <- read.csv("Sources/table-1570-regional-passenger-journeys-south-east - 1570_journeys_south_east.csv")
df_south_west <- read.csv("Sources/table-1575-regional-passenger-journeys-south-west - 1575_journeys_south_west.csv")
df_wales <- read.csv("Sources/table-1580-regional-passenger-journeys-wales - 1580_journeys_wales.csv")
df_west_midlands <- read.csv("Sources/table-1585-regional-passenger-journeys-west-midlands - 1585_journeys_west_midlands.csv")
df_yorkshire_humber <- read.csv("Sources/table-1590-regional-passenger-journeys-yorkshire-and-the-humber - 1590_journeys_yorkshire_humber.csv")

#--------------------------
#Aggregate a new "origin" column for each journey, and clean the informatio,  
##_________________________

df_east_midlands <- df_east_midlands %>%
  mutate(
    Region = "East Midlands",
    Time.period = trimws(gsub("\\s*\\[b\\]\\s*$", "", as.character(Time.period)))
  )

df_east_england <- df_east_england %>%
  mutate(
    Region = "East England",
    Time.period = trimws(gsub("\\s*\\[b\\]\\s*$", "", as.character(Time.period)))
  )

df_london <- df_london %>%
  mutate(
    Region = "London",
    Time.period = trimws(gsub("\\s*\\[b\\]\\s*$", "", as.character(Time.period)))
  )

df_north_east <- df_north_east %>%
  mutate(
    Region = "North East",
    Time.period = trimws(gsub("\\s*\\[b\\]\\s*$", "", as.character(Time.period)))
  )

df_north_west <- df_north_west %>%
  mutate(
    Region = "North West",
    Time.period = trimws(gsub("\\s*\\[b\\]\\s*$", "", as.character(Time.period)))
  )

df_scotland <- df_scotland %>%
  mutate(
    Region = "Scotland",
    Time.period = trimws(gsub("\\s*\\[b\\]\\s*$", "", as.character(Time.period)))
  )

df_south_east <- df_south_east %>%
  mutate(
    Region = "South East",
    Time.period = trimws(gsub("\\s*\\[b\\]\\s*$", "", as.character(Time.period)))
  )

df_south_west <- df_south_west %>%
  mutate(
    Region = "South West",
    Time.period = trimws(gsub("\\s*\\[b\\]\\s*$", "", as.character(Time.period)))
  )

df_wales <- df_wales %>%
  mutate(
    Region = "Wales",
    Time.period = trimws(gsub("\\s*\\[b\\]\\s*$", "", as.character(Time.period)))
  )

df_west_midlands <- df_west_midlands %>%
  mutate(
    Region = "West Midlands",
    Time.period = trimws(gsub("\\s*\\[b\\]\\s*$", "", as.character(Time.period)))
  )

df_yorkshire_humber <- df_yorkshire_humber %>%
  mutate(
    Region = "Yorkshire and the Humber",
    Time.period = trimws(gsub("\\s*\\[b\\]\\s*$", "", as.character(Time.period)))
  )

#----------------------------
#Normalize and transform the wided formated data into long format, mantanining: Time.period and region columns
#____________________________ 

#Transforms everithyng into characters 
all_to_character <- function(df) {
  df[] <- lapply(df, as.character)
  df
}
#Test 
df_scotland<- all_to_character(df_scotland)

#Transform all wide data into long format  

to_long <- function(df) {
  df %>%
    pivot_longer(
      cols = -c(Time.period, Region),
      names_to = "variable",
      values_to = "value"
    )
}
#teste 
df_scotland_long <- to_long(df_scotland)

#Normalize and re shape the data:

df_east_midlands      <- all_to_character(df_east_midlands)
df_east_midlands_long <- to_long(df_east_midlands)

df_east_england      <- all_to_character(df_east_england)
df_east_england_long <- to_long(df_east_england)

df_london      <- all_to_character(df_london)
df_london_long <- to_long(df_london)

df_north_east      <- all_to_character(df_north_east)
df_north_east_long <- to_long(df_north_east)

df_north_west      <- all_to_character(df_north_west)
df_north_west_long <- to_long(df_north_west)

df_scotland      <- all_to_character(df_scotland)
df_scotland_long <- to_long(df_scotland)

df_south_east      <- all_to_character(df_south_east)
df_south_east_long <- to_long(df_south_east)

df_south_east      <- all_to_character(df_south_east)
df_south_east_long <- to_long(df_south_east)

df_south_west      <- all_to_character(df_south_west)
df_south_west_long <- to_long(df_south_west)

df_wales      <- all_to_character(df_wales)
df_wales_long <- to_long(df_wales)

df_west_midlands      <- all_to_character(df_west_midlands)
df_west_midlands_long <- to_long(df_west_midlands)

df_yorkshire_humber      <- all_to_character(df_yorkshire_humber)
df_yorkshire_humber_long <- to_long(df_yorkshire_humber)

#______________________________
## Join all long formated datasets into one 
#______________________________

df_all_regions <- bind_rows(
  df_east_midlands_long,
  df_east_england_long,
  df_london_long,
  df_north_east_long,
  df_north_west_long,
  df_scotland_long,
  df_south_east_long,
  df_south_west_long,
  df_wales_long,
  df_west_midlands_long,
  df_yorkshire_humber_long
)

#________________________________
# Quality check and re formating all information if needed 
##_______________________________
names(df_all_regions)
head(df_all_regions$variable)

df_all_regions$variable <- gsub("\\.", " ", df_all_regions$variable)
unique(df_all_regions$variable)
df_all_regions$variable <- sub("^X\\s+", "", df_all_regions$variable)
df_all_regions$variable <- gsub("\\s+", " ", df_all_regions$variable)
df_all_regions$variable <- trimws(df_all_regions$variable)
unique(df_all_regions$variable)
unique(df_all_regions$Time.period)

names(df_all_regions)[names(df_all_regions) == "variable"] <- "type of journey"

head(df_all_regions$value)
df_all_regions$value_num <- as.numeric(gsub(",", "", df_all_regions$value))
names(df_all_regions)[names(df_all_regions) == "value_num"] <- "amount of journeys (thousands)"
Station_on_the_regions <- read.csv("df_stations_per_region.csv")
df_all_regions <- df_all_regions %>%
  mutate(
    Region = gsub("East of England", "East England", Region),
    `type of journey` = gsub("East of England", "East England", `type of journey`)
  )
df_all_regions <- df_all_regions %>%
  mutate(
    year_start = as.integer(sub(".*?(\\d{4}).*", "\\1", Time.period))
  ) %>%
  arrange(year_start, Time.period) %>%
  mutate(
    Time.period = factor(Time.period, levels = unique(Time.period)),
    sort = as.integer(Time.period)
  ) %>%
  select(-year_start)
