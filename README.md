# UK Railway System: Structural Stability and COVID-19 Disruption

## Project overview
This project analyses long-run passenger journey patterns in the United Kingdom railway system to examine whether the COVID-19 pandemic acted as a temporary demand shock or as a structural break. Using Office of Rail and Road (ORR) data at both regional and station level, the study evaluates persistence, concentration, and behavioural stability across the system.

Rather than focusing on short-term forecasting, the analysis treats COVID-19 as a system-wide stress test, assessing whether pre-existing spatial hierarchies and usage patterns were disrupted or reinforced.

---

## Research questions
- **RQ1:** How did the composition of passenger journeys change following COVID-19, and does the system remain within-region oriented?
- **RQ2:** To what extent is railway activity structurally concentrated in a small subset of regions, and does London retain a dominant role?
- **RQ3:** Do railway stations exhibit stable behavioural patterns over time, and do post-2019 observations remain close to historical behaviour?

---

## Data sources
- **Office of Rail and Road (ORR)** regional passenger journey tables (1540–1590)
- **ORR station-level entries and exits** (Table 1415A)

The data cover approximately 1995–2025 at regional level and 1997–2025 at station level. Passenger journeys are analysed using both absolute volumes and origin-normalised shares to avoid scale bias.

---

## Repository structure
```text
├── Regions_cleandf.r          # Regional-level preprocessing and analysis
├── Stations.r                # Station-level preprocessing and modelling
├── Sources/                  # Helper scripts and functions
├── Produced_tables/          # Output tables (generated)
├── Porduced_figures/         # Output figures (generated)
├── df_long_27.csv             # Balanced station panel (generated)

