# Iowa Economic Insight Dashboard 🌽 📈

A high-performance **R Shiny** application designed to visualize and analyze unemployment benefit trends across Iowa. This project represents a **Standard Tier ($500+)** portfolio piece, demonstrating production-grade reactivity and geospatial integration.

### 🚀 Key Features
* **Dynamic Geospatial Mapping:** Real-time county-level distribution using `Leaflet` and `leafletProxy` for flicker-free updates.
* **Interactive Analytics:** Statewide time-series trends powered by `Plotly`.
* **Executive KPIs:** Instant calculation of "Total Benefits" and "Active Recipients" with optimized `na.rm` safety logic.
* **Data Portability:** Integrated CSV export tool for filtered subsets.
* **Modern UI:** Built with the `bslib` "Superhero" theme for a professional, accessible dark-mode aesthetic.

### 🛠️ Tech Stack
* **Language:** R
* **UI:** `bslib`, `bsicons`
* **Visualization:** `leaflet`, `plotly`, `ggplot2`
* **Data Processing:** `tidyverse` (dplyr, lubridate, readr)

### 📖 How to Run
1. Clone this repo.
2. Open in RStudio and ensure `pacman` is installed.
3. Click **Run App**.
