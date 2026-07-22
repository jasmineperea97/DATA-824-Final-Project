# Alaska Rural Water & Wastewater Infrastructure Explorer

This is a Shiny final project about water and wastewater infrastructure in Alaska rural and Alaska Native Village contexts.

## Project question

How are public drinking water sources, wastewater/NPDES facilities, and current boil-water or do-not-drink notices distributed across Alaska, and what patterns can we see by facility type, population served, permit/compliance status, and tribal/rural context variables?

## Why this topic matters

EPA describes significant human health and water quality problems in Alaska Native Villages and other rural Alaska communities due to lack of sanitation. This app uses public environmental data to explore that issue visually.

## Data sources used

The app downloads public data directly from official ArcGIS REST services:

1. **Alaska DEC SDWIS active public water system source locations**
   - Active public water system source locations for wells, intakes, springs, infiltration galleries, and rain catchment systems.
   - Source: Alaska Department of Environmental Conservation, Drinking Water Program.

2. **EPA ECHO Clean Water Act/NPDES facility locations**
   - Clean Water Act/NPDES regulated wastewater/stormwater/biosolids facility locations.
   - Includes permit/compliance context and tribal-proximity fields where available.
   - Source: EPA Enforcement and Compliance History Online (ECHO).

3. **EPA ECHO NPDES outfalls/permitted features**
   - Discharge/outfall locations for NPDES facilities.
   - Source: EPA ECHO / ICIS-NPDES.

4. **Alaska DEC open Boil Water / Do Not Drink notices**
   - Active notices for public water systems.
   - Source: Alaska Department of Environmental Conservation, Drinking Water Program.

## Folder contents

- `01_setup_packages.R` installs and loads required R packages.
- `02_download_clean_data.R` downloads and cleans the public datasets.
- `app.R` runs the Shiny app.
- `data/` stores cleaned CSV files created by the download script.
- `data/raw/` stores raw downloaded CSV files.

## How to run the project in RStudio

1. Download and unzip this folder.
2. Move the folder to your Desktop or R assignments folder.
3. Open RStudio.
4. Open `01_setup_packages.R` and click **Source**.
5. Open `02_download_clean_data.R` and click **Source**.
6. Open `app.R` and click **Run App**.

You can also run this from the Console:

```r
setwd("~/Desktop/R Assignments/alaska_water_dashboard_final_project")
source("01_setup_packages.R")
source("02_download_clean_data.R")
shiny::runApp()
```

If your folder is named differently or stored somewhere else, change the `setwd()` path.

## Dashboard tabs

1. **Overview**
   - Summary numbers and charts.
   - Shows the size of the data and major patterns.

2. **Interactive Map**
   - Maps drinking-water system sources, wastewater/NPDES facilities, NPDES outfalls, and boil-water notices.
   - Includes filters for system type, population served, permit status, tribal-context flag, and violation flag.

3. **Water Systems**
   - Focuses on Alaska public water system source types and population served.

4. **Wastewater / NPDES**
   - Focuses on Clean Water Act/NPDES facilities and discharge/outfall locations.

5. **County Profile**
   - Creates a county-level heat map and PCA plot using wastewater facility variables.

6. **Data Quality**
   - Shows missingness patterns so the viewer understands limitations.

7. **About**
   - Explains what the app does, where the data come from, and limitations.

## Important limitation

This app does not claim that every point is an Alaska Native Village. Instead, it explores official water and wastewater infrastructure datasets relevant to rural and tribal Alaska contexts. EPA ECHO includes some tribal-spatial-association variables, but public datasets do not always perfectly identify every Alaska Native village or service area.
