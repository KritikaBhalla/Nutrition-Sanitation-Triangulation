# Nutrition-Sanitation-Triangulation
## Project Overview

This project examines the relationship between sanitation conditions and child nutrition outcomes in India by triangulating data from the National Family Health Survey (NFHS-5) and the NSS-WASH.

The analysis focuses on how sanitation-related indicators are associated with two key child nutrition outcomes:
- Stunting: low height-for-age
- Wasting: low weight-for-height

## Data Sources

- National Family Health Survey (NFHS-5) 
- NSS: Drinking Water, Sanitation, Hygiene, and Housing Conditions (WASH); 76th round


## Methodology

The analysis is conducted at the district level to capture more nuanced spatial variation than would be possible with state-level aggregates.

Sanitation indicators are merged with district-level child nutrition outcomes after harmonizing geographic identifiers across the two surveys. The study examines bivariate relationships between sanitation conditions and nutrition outcomes.

Scatter plots with trend lines are used to visualize the association between:
- Sanitation indicators and stunting
- Sanitation indicators and wasting

The analysis is exploratory in nature and does not include multivariate or multilevel modeling.

## Tools and Packages

The analysis is conducted in R, primarily using:
- tidyverse
- srvyr
- vroom
- other supporting packages for data handling and visualization


## Data Availability

NFHS variable definitions and recoding follow the Demographic and Health Surveys (DHS) Standard Recode Manual.

The NFHS and NSS microdata, as well as official survey documentation, are not included in this repository due to data access and redistribution restrictions. All scripts are written to reproduce the analysis given authorized access to the respective datasets.

