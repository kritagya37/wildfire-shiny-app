# wildfire-shiny-app
Interactive Shiny dashboard for wildfire analysis in the United States using USFS fire data
# Wildfire Shiny App: US Fire Risk & Trends Explorer

## Overview

This R Shiny application explores wildfire activity across the United States using USDA/USFS fire incident data (Short, 2022). The goal of this project is to provide an interactive way to understand wildfire patterns over time, space, and severity.

Short, Karen C. 2022. Spatial wildfire occurrence data for the United States, 1992-2020 [FPA_FOD_20221014]. 6th Edition. Fort Collins, CO: Forest Service Research Data Archive. https://doi.org/10.2737/RDS-2013-0009.6

Users can explore how wildfire risk varies across states, years, causes, and fire sizes. The dashboard integrates spatial visualization, temporal trends, and comparative analysis to support better understanding of wildfire dynamics.

Key features include:
- Interactive wildfire map with risk visualization
- Temporal trends of fire occurrences over time
- Fire size distribution analysis
- Seasonal and monthly wildfire patterns
- State-level ranking of wildfire activity and severity
- Cause-based breakdown of wildfire ignitions

---

## Data

The dataset used in this project comes from publicly available USDA Forest Service wildfire records (Fire Occurrence Dataset). It includes wildfire incidents recorded across multiple years, with information on:

- Fire year and discovery date
- Location (latitude/longitude, state, county)
- Fire size (acres burned)
- Cause classification (natural vs human-caused)
- Fire reporting agency and unit information

---

## Packages Used

This application was built in R using the following packages:

- shiny  
- bslib  
- dplyr  
- ggplot2  
- leaflet  
- readr  
- scales
- tidyr
  
---


## Features

### 1. Spatial Fire Risk Map
Interactive leaflet map showing wildfire locations colored by risk index or fire size.

### 2. Temporal Trends
Visualization of wildfire frequency and total burned area over time.

### 3. Fire Size Distribution
Distribution of wildfire sizes using log-scale histograms.

### 4. Seasonal Patterns
Monthly and yearly variation in wildfire occurrence and severity.

### 5. State-Level Ranking
Comparison of states based on fire frequency, total burned area, and severity.

---

## Future Improvements

Future versions of this application could include:

- Integration of fuel treatment and management data
- Incorporation of climate variables (temperature, precipitation, drought indices)
- Predictive modeling of wildfire risk
- Enhanced spatial clustering and hotspot detection
- Performance optimization for larger datasets

---



## Project Structure

---

```
wildfire-app_clean/
│
├── app.R
├── data/
│   └── Fires_Short.csv
├── www/
│   └── style.css
└── README.md
```

---

Wildfire-Shiny-App/
