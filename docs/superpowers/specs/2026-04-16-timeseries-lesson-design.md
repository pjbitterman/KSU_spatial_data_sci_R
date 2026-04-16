# Time Series Lesson Design — Week 15

**Date**: 2026-04-16  
**Course**: GEOG 49073/59073/79073, Environmental Data Analysis in R, KSU  
**Instructor**: Dr. Bitterman  
**Duration**: ~50 minutes  
**Output file**: `src/week15_timeseries_inclass.Rmd`

---

## Goal

Introduce time series analysis basics to students who have spatial/raster/vector R experience but no formal time series background. Enough stats to be meaningful, not enough to overwhelm.

## Decisions Made

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Data source | USGS streamflow via `dataRetrieval` | Environmental relevance, free, no API key, local gauge (Cuyahoga at Independence) |
| R ecosystem | `tsibble` + `feasts` + `fable` | Tidyverse-native, students already know tidyverse pipes |
| Stats depth | Moderate — decomposition + TSLM model + forecast | Interpretable without deep theory |
| Aggregation | Daily → monthly | Reduces noise, speeds models, clearer seasonal signal |

## Lesson Structure (~50 min)

| Section | Time | Content |
|---------|------|---------|
| Framing | 5 min | What is autocorrelation, trend/seasonality/remainder |
| Data pull | 8 min | `dataRetrieval`, `renameNWISColumns`, inspect raw data |
| tsibble | 5 min | Convert, inspect interval, understand index concept |
| Visualization | 12 min | autoplot, gg_season, gg_subseries, ACF |
| Decomposition | 8 min | STL, interpret 4 panels, grey bars |
| Model + forecast | 8 min | TSLM(trend + season), tidy(), augment(), forecast() |
| Student exercise | 5 min | Choose: new gauge / ARIMA / trend interpretation |

## Stats Concepts Covered

- Autocorrelation (conceptual + ACF plot)
- Additive decomposition (trend + season + remainder)
- STL decomposition
- Time series linear model (TSLM): trend coefficient, seasonal dummies
- Prediction intervals

## Key Packages

- `dataRetrieval` — USGS gauge data
- `tsibble` — tidy time series structure
- `feasts` — visualization and decomposition
- `fable` — forecasting models

## Reference Resource for Students

Hyndman, R.J. & Athanasopoulos, G. *Forecasting: Principles and Practice* (3rd ed.)  
https://otexts.com/fpp3/ — uses exactly these packages, free online
