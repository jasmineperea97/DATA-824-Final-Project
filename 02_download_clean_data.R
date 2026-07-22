# 02_download_clean_data.R
# Downloads and cleans public water/wastewater infrastructure data for Alaska.
# Run this script before app.R.

required_packages <- c("dplyr", "tidyr", "readr", "stringr", "jsonlite", "httr", "tibble")

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

invisible(lapply(required_packages, library, character.only = TRUE))

dir.create("data", showWarnings = FALSE)
dir.create(file.path("data", "raw"), showWarnings = FALSE)

clean_names <- function(x) {
  x <- gsub("^attributes\\.", "", x)
  x <- gsub("^attributes_", "", x)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  tolower(x)
}

as_num <- function(x) {
  suppressWarnings(readr::parse_number(as.character(x)))
}

pick_col <- function(df, cols, default = NA) {
  n <- nrow(df)
  found <- cols[cols %in% names(df)]
  if (length(found) == 0) {
    return(rep(default, n))
  }
  df[[found[1]]]
}

truthy_flag <- function(x) {
  x2 <- toupper(trimws(as.character(x)))
  !is.na(x2) & x2 %in% c("Y", "YES", "TRUE", "T", "1")
}

nonblank <- function(x) {
  x2 <- trimws(as.character(x))
  !is.na(x2) & x2 != "" & toupper(x2) != "NA" & toupper(x2) != "NULL"
}

extract_arcgis_features <- function(dat) {
  if (is.null(dat$features) || length(dat$features) == 0) {
    return(tibble::tibble())
  }

  df <- tibble::as_tibble(dat$features)
  names(df) <- clean_names(names(df))

  # ArcGIS geometry often appears as geometry_x and geometry_y after flattening.
  if ("geometry_x" %in% names(df) && !"longitude" %in% names(df)) {
    df$longitude <- as_num(df$geometry_x)
  }
  if ("geometry_y" %in% names(df) && !"latitude" %in% names(df)) {
    df$latitude <- as_num(df$geometry_y)
  }

  df
}

query_arcgis_layer <- function(layer_url,
                               where = "1=1",
                               out_fields = "*",
                               return_geometry = TRUE,
                               page_size = 2000,
                               max_pages = 50) {
  all_records <- list()
  offset <- 0

  for (page in seq_len(max_pages)) {
    message("Querying: ", layer_url, " | page ", page, " | offset ", offset)

    response <- httr::GET(
      paste0(layer_url, "/query"),
      query = list(
        f = "json",
        where = where,
        outFields = out_fields,
        returnGeometry = tolower(as.character(return_geometry)),
        outSR = 4326,
        resultRecordCount = page_size,
        resultOffset = offset
      )
    )

    if (httr::http_error(response)) {
      stop("HTTP error while querying ArcGIS layer: ", httr::status_code(response))
    }

    txt <- httr::content(response, as = "text", encoding = "UTF-8")
    dat <- jsonlite::fromJSON(txt, flatten = TRUE)

    if (!is.null(dat$error)) {
      msg <- if (!is.null(dat$error$message)) dat$error$message else "Unknown ArcGIS error"
      stop("ArcGIS query error: ", msg)
    }

    records <- extract_arcgis_features(dat)
    if (nrow(records) == 0) {
      break
    }

    all_records[[length(all_records) + 1]] <- records

    exceeded <- isTRUE(dat$exceededTransferLimit)
    if (nrow(records) < page_size || !exceeded) {
      break
    }

    offset <- offset + page_size
  }

  dplyr::bind_rows(all_records)
}

safe_query <- function(layer_url, where, fallback_where = NULL, ...) {
  tryCatch(
    query_arcgis_layer(layer_url = layer_url, where = where, ...),
    error = function(e) {
      if (!is.null(fallback_where)) {
        message("Primary query failed: ", conditionMessage(e))
        message("Trying fallback query: ", fallback_where)
        query_arcgis_layer(layer_url = layer_url, where = fallback_where, ...)
      } else {
        stop(e)
      }
    }
  )
}

# Source endpoints -------------------------------------------------------------

adec_pws_url <- "https://dec.alaska.gov/arcgis/rest/services/EH/SDWIS_Facilities/FeatureServer/0"
adec_bwn_url <- "https://dec.alaska.gov/arcgis/rest/services/EH/BWN_DND/FeatureServer/0"
echo_cwa_url <- "https://services.arcgis.com/cJ9YHowT8TU7DUyn/ArcGIS/rest/services/ECHO_CWA_Facilities/FeatureServer/0"
echo_outfalls_url <- "https://services.arcgis.com/cJ9YHowT8TU7DUyn/ArcGIS/rest/services/ECHO_NPDES_Facilities_Outfalls/FeatureServer/0"

# Download data ----------------------------------------------------------------

pws_raw <- query_arcgis_layer(adec_pws_url, where = "1=1")
cwa_raw <- safe_query(echo_cwa_url, where = "cwp_state = 'AK'", fallback_where = "CWP_STATE = 'AK'")
outfalls_raw <- safe_query(echo_outfalls_url, where = "STATE_CODE = 'AK'", fallback_where = "state_code = 'AK'")

# Boil water notices can be empty at times, so we let this one fail gracefully.
bwn_raw <- tryCatch(
  query_arcgis_layer(adec_bwn_url, where = "1=1"),
  error = function(e) {
    message("Boil water notice download failed or returned no data: ", conditionMessage(e))
    tibble::tibble()
  }
)

readr::write_csv(pws_raw, file.path("data", "raw", "adec_pws_sources_raw.csv"))
readr::write_csv(cwa_raw, file.path("data", "raw", "echo_cwa_facilities_raw.csv"))
readr::write_csv(outfalls_raw, file.path("data", "raw", "echo_npdes_outfalls_raw.csv"))
readr::write_csv(bwn_raw, file.path("data", "raw", "adec_boil_water_notices_raw.csv"))

# Clean public water system sources -------------------------------------------

source_type_lookup <- c(
  "WL" = "Well",
  "IN" = "Intake",
  "SP" = "Spring",
  "IG" = "Infiltration gallery",
  "RC" = "Rain catchment"
)

fed_type_lookup <- c(
  "C" = "Community water system",
  "NC" = "Non-community water system",
  "NTNC" = "Non-transient non-community",
  "TNC" = "Transient non-community"
)

pws_clean <- pws_raw %>%
  mutate(row_id = dplyr::row_number()) %>%
  transmute(
    row_id,
    source_dataset = "ADEC SDWIS active PWS source locations",
    pws_id = as.character(pick_col(., c("pwsid", "pws_id", "tinwsys_is_number"))),
    system_name = as.character(pick_col(., c("tinwsys_name", "system_name", "pws_name", "name"))),
    facility_id = as.character(pick_col(., c("tinwsf_id", "facility_id", "tinwsf_is_number"))),
    source_type_code = as.character(pick_col(., c("tinwsf_type_code", "source_type_code", "facility_type_code"))),
    source_type = dplyr::recode(source_type_code, !!!source_type_lookup, .default = source_type_code),
    federal_type_code = as.character(pick_col(., c("d_pws_fed_type_cd", "pws_fed_type_code", "federal_type"))),
    federal_type = dplyr::recode(federal_type_code, !!!fed_type_lookup, .default = federal_type_code),
    activity_status = as.character(pick_col(., c("d_pws_activity_cd", "activity_status", "activity"))),
    population_served = as_num(pick_col(., c("d_population_count", "population_count", "population_served", "population"))),
    comment = as.character(pick_col(., c("comment", "comments", "location_comment"))),
    longitude = as_num(pick_col(., c("longitude", "longitude83", "lon", "x", "geometry_x"))),
    latitude = as_num(pick_col(., c("latitude", "latitude83", "lat", "y", "geometry_y")))
  ) %>%
  mutate(
    system_name = if_else(is.na(system_name) | system_name == "", "Unknown system", system_name),
    source_type = if_else(is.na(source_type) | source_type == "", "Unknown source type", source_type),
    federal_type = if_else(is.na(federal_type) | federal_type == "", "Unknown classification", federal_type)
  )

# Clean ECHO CWA facilities ----------------------------------------------------

cwa_clean <- cwa_raw %>%
  mutate(row_id = dplyr::row_number()) %>%
  transmute(
    row_id,
    source_dataset = "EPA ECHO CWA/NPDES facility locations",
    npdes_id = as.character(pick_col(., c("source_id", "external_permit_nmbr", "npdes_id"))),
    registry_id = as.character(pick_col(., c("registry_id", "frs_id"))),
    facility_name = as.character(pick_col(., c("cwp_name", "facility_name", "fac_name", "name"))),
    city = as.character(pick_col(., c("cwp_city", "city", "fac_city"))),
    state = as.character(pick_col(., c("cwp_state", "state", "state_code"))),
    county = as.character(pick_col(., c("cwp_county", "fac_county_name", "county", "facility_std_county_name"))),
    permit_status = as.character(pick_col(., c("cwp_permit_status_desc", "permit_status_desc", "permit_status"))),
    permit_type = as.character(pick_col(., c("cwp_permit_type_desc", "permit_type_desc", "permit_type"))),
    facility_type = as.character(pick_col(., c("cwp_facility_type_indicator", "facility_type_desc", "facility_type"))),
    total_design_flow = as_num(pick_col(., c("cwp_total_design_flow_nmbr", "total_design_flow_nmbr", "design_flow"))),
    actual_average_flow = as_num(pick_col(., c("cwp_actual_average_flow_nmbr", "actual_average_flow_nmbr", "actual_flow"))),
    current_snc_status = as.character(pick_col(., c("cwp_current_snc_status", "current_snc_status"))),
    current_violation = as.character(pick_col(., c("cwp_current_viol", "current_violation", "cwa_current_status"))),
    inspections = as_num(pick_col(., c("cwp_inspection_count", "inspection_count", "inspections"))),
    pct_low_income_3mi = as_num(pick_col(., c("percent_below_lowincome_3mile", "pct_low_income_3mi"))),
    pct_people_of_color = as_num(pick_col(., c("percent_people_of_color", "pct_people_of_color"))),
    population_density = as_num(pick_col(., c("acs_population_density", "population_density"))),
    tribal_spatial_flag = as.character(pick_col(., c("fac_indian_spatial_flg", "cwp_indian_cntry_flg", "fac_indian_cntry_flg"))),
    derived_tribes = as.character(pick_col(., c("fac_derived_tribes", "derived_tribes"))),
    latitude = as_num(pick_col(., c("latitude", "latitude83", "lat", "geometry_y"))),
    longitude = as_num(pick_col(., c("longitude", "longitude83", "lon", "geometry_x")))
  ) %>%
  mutate(
    facility_name = if_else(is.na(facility_name) | facility_name == "", "Unknown facility", facility_name),
    county = if_else(is.na(county) | county == "", "Unknown county", county),
    permit_status = if_else(is.na(permit_status) | permit_status == "", "Unknown status", permit_status),
    permit_type = if_else(is.na(permit_type) | permit_type == "", "Unknown permit type", permit_type),
    has_tribal_context = truthy_flag(tribal_spatial_flag) | nonblank(derived_tribes),
    has_current_violation = truthy_flag(current_violation) |
      (nonblank(current_snc_status) & !toupper(current_snc_status) %in% c("NO VIOLATION", "NO CURRENT VIOLATION", "NA", "NULL"))
  )

# Clean ECHO NPDES outfalls ----------------------------------------------------

outfalls_clean <- outfalls_raw %>%
  mutate(row_id = dplyr::row_number()) %>%
  transmute(
    row_id,
    source_dataset = "EPA ECHO NPDES outfalls/permitted features",
    npdes_id = as.character(pick_col(., c("external_permit_nmbr", "source_id", "npdes_id"))),
    facility_name = as.character(pick_col(., c("facility_name", "cwp_name", "name"))),
    city = as.character(pick_col(., c("city", "cwp_city"))),
    state = as.character(pick_col(., c("state_code", "cwp_state", "state"))),
    county = as.character(pick_col(., c("fac_county_name", "cwp_county", "county"))),
    permit_status = as.character(pick_col(., c("permit_status_desc", "cwp_permit_status_desc", "permit_status"))),
    permit_type = as.character(pick_col(., c("permit_type_desc", "cwp_permit_type_desc", "permit_type"))),
    facility_type = as.character(pick_col(., c("facility_type_desc", "facility_type"))),
    total_design_flow = as_num(pick_col(., c("total_design_flow_nmbr", "cwp_total_design_flow_nmbr", "design_flow"))),
    current_snc_status = as.character(pick_col(., c("cwp_current_snc_status", "current_snc_status"))),
    current_violation = as.character(pick_col(., c("cwp_current_viol", "current_violation"))),
    permit_feature_number = as.character(pick_col(., c("perm_feature_nmbr", "permit_feature_number"))),
    state_water_body_name = as.character(pick_col(., c("state_water_body_name", "water_body_name"))),
    tribal_spatial_flag = as.character(pick_col(., c("fac_indian_spatial_flg", "fac_indian_cntry_flg"))),
    derived_tribes = as.character(pick_col(., c("fac_derived_tribes", "derived_tribes"))),
    latitude = as_num(pick_col(., c("latitude83", "latitude", "lat", "geometry_y"))),
    longitude = as_num(pick_col(., c("longitude83", "longitude", "lon", "geometry_x")))
  ) %>%
  mutate(
    facility_name = if_else(is.na(facility_name) | facility_name == "", "Unknown facility", facility_name),
    county = if_else(is.na(county) | county == "", "Unknown county", county),
    permit_status = if_else(is.na(permit_status) | permit_status == "", "Unknown status", permit_status),
    has_tribal_context = truthy_flag(tribal_spatial_flag) | nonblank(derived_tribes),
    has_current_violation = truthy_flag(current_violation) |
      (nonblank(current_snc_status) & !toupper(current_snc_status) %in% c("NO VIOLATION", "NO CURRENT VIOLATION", "NA", "NULL"))
  )

# Clean boil-water and do-not-drink notices -----------------------------------

if (nrow(bwn_raw) == 0) {
  bwn_clean <- tibble::tibble(
    row_id = integer(),
    source_dataset = character(),
    pws_id = character(),
    system_name = character(),
    notice_type = character(),
    notice_status = character(),
    latitude = numeric(),
    longitude = numeric()
  )
} else {
  bwn_clean <- bwn_raw %>%
    mutate(row_id = dplyr::row_number()) %>%
    transmute(
      row_id,
      source_dataset = "ADEC open boil-water/do-not-drink notices",
      pws_id = as.character(pick_col(., c("pwsid", "pws_id", "tinwsys_is_number"))),
      system_name = as.character(pick_col(., c("tinwsys_name", "system_name", "pws_name", "name"))),
      notice_type = as.character(pick_col(., c("notice_type", "order_type", "type", "status"))),
      notice_status = as.character(pick_col(., c("notice_status", "status", "active"))),
      latitude = as_num(pick_col(., c("latitude", "latitude83", "lat", "geometry_y"))),
      longitude = as_num(pick_col(., c("longitude", "longitude83", "lon", "geometry_x")))
    ) %>%
    mutate(
      system_name = if_else(is.na(system_name) | system_name == "", "Unknown system", system_name),
      notice_type = if_else(is.na(notice_type) | notice_type == "", "Open notice", notice_type)
    )
}

# Write clean data -------------------------------------------------------------

readr::write_csv(pws_clean, file.path("data", "pws_sources_clean.csv"))
readr::write_csv(cwa_clean, file.path("data", "cwa_facilities_clean.csv"))
readr::write_csv(outfalls_clean, file.path("data", "npdes_outfalls_clean.csv"))
readr::write_csv(bwn_clean, file.path("data", "boil_water_notices_clean.csv"))

# A small project summary table for the app and presentation
project_summary <- tibble::tibble(
  dataset = c("Public water system source locations", "CWA/NPDES facilities", "NPDES outfalls/permitted features", "Open boil-water/do-not-drink notices"),
  rows = c(nrow(pws_clean), nrow(cwa_clean), nrow(outfalls_clean), nrow(bwn_clean)),
  records_with_coordinates = c(
    sum(is.finite(pws_clean$latitude) & is.finite(pws_clean$longitude)),
    sum(is.finite(cwa_clean$latitude) & is.finite(cwa_clean$longitude)),
    sum(is.finite(outfalls_clean$latitude) & is.finite(outfalls_clean$longitude)),
    sum(is.finite(bwn_clean$latitude) & is.finite(bwn_clean$longitude))
  )
)

readr::write_csv(project_summary, file.path("data", "project_summary.csv"))

message("Done. Clean data files were saved in the data folder.")
print(project_summary)
