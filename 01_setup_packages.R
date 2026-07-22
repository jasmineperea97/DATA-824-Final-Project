# 01_setup_packages.R
# Install and load packages needed for the Alaska water/wastewater Shiny app.

required_packages <- c(
  "shiny",
  "bslib",
  "dplyr",
  "tidyr",
  "readr",
  "stringr",
  "ggplot2",
  "leaflet",
  "DT",
  "jsonlite",
  "httr",
  "scales",
  "tibble"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

invisible(lapply(required_packages, library, character.only = TRUE))

message("Packages are installed and loaded.")
