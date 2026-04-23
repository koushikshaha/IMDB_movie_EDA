# IMDB_movie_EDA
Exploratory Data Analysis on IMDB Movies Dataset
# 🎬 Movie Dataset — Exploratory Data Analysis

## Overview
This project performs exploratory data analysis (EDA) on a movies dataset
sourced from Google Drive. The analysis covers data cleaning, univariate and
bivariate visualizations, genre/language/country breakdowns, trend analysis
over time, and outlier handling.

## Dataset
- Source: Google Drive (IMDB-style movies dataset)
- Key variables: `imdb_score`, `budget`, `gross`, `duration`, `genres`,
  `country`, `language`, `title_year`, `actor_1_name`

## Libraries Used
- `readr` — data loading
- `dplyr` — data wrangling
- `ggplot2` — static visualizations
- `gridExtra` — multi-plot layouts
- `GGally` — pair plots
- `corrplot` — correlation matrix
- `plotly` — interactive heatmap

## Steps Performed
1. **Data Loading** — loaded CSV directly from Google Drive
2. **Data Cleaning** — removed irrelevant columns, handled missing values
   (mode imputation for `color`, mean imputation for `duration` and reviews)
3. **Feature Engineering** — created `budget_million`, `gross_million`,
   `profit_million`, and `main_genre`
4. **Univariate Analysis** — histograms and density plots for scores,
   duration, budget, gross, profit
5. **Bivariate Analysis** — scatter plots, boxplots by genre
6. **Multivariate Analysis** — color-coded scatter plots, pair plots,
   correlation heatmap
7. **Time Trends** — yearly averages for IMDb score, budget, profit,
   and movie count
8. **Outlier Analysis** — IQR-based outlier removal with before/after
   comparison plots

## How to Run
1. Clone this repository
2. Open `IMDB_movie_EDA.R` in RStudio
3. Run the script (it will auto-install required packages)
4. Data is fetched automatically from Google Drive — no manual download needed


