# v7
read_one_fn <- function(f) {
  dat <- readr::read_csv(f, show_col_types = FALSE)
  if (!"PintailRegion" %in% names(dat)) {
    stop("Required column PintailRegion missing in file: ", f)
  }
  scenario <- sub("_wtp_out\\.csv$", "", basename(f))
  
  parts <- list()
  
  if ("pintail_birders_k" %in% names(dat)) {
    parts$pintail <- dat |>
      dplyr::group_by(PintailRegion) |>
      dplyr::summarise(
        pintail_birders_k = dplyr::first(pintail_birders_k),
        .groups = "drop"
      )
  }
  
  if ("trips_A" %in% names(dat)) {
    parts$trips <- dat |>
      dplyr::group_by(PintailRegion) |>
      dplyr::summarise(
        trips_A = stats::median(trips_A, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  if (length(parts) == 0) stop("Neither pintail_birders_k nor trips_A found in file: ", f)
  
  dat2 <- Reduce(function(x, y) dplyr::full_join(x, y, by = "PintailRegion"), parts)
  
  long <- tidyr::pivot_longer(
    dat2,
    cols = -PintailRegion,
    names_to = "metric",
    values_to = "value"
  )
  
  dplyr::mutate(long, scenarioName = scenario, .before = 1)
}

combine_scenarios_fn <- function(path = ".", pattern = "_wtp_out\\.csv$") {
  files <- list.files(path = path, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) stop("No files found matching pattern: ", pattern)
  
  long <- purrr::map_dfr(files, read_one_fn)
  
  region_levels <- c(
    "Alaska breeding",
    "Southern breeding",
    "Northern breeding",
    "Western wintering",
    "Central wintering"
  )
  long <- dplyr::mutate(
    long,
    PintailRegion = factor(.data$PintailRegion, levels = region_levels, ordered = TRUE),
    metric = factor(.data$metric, levels = c("pintail_birders_k", "trips_A"), ordered = TRUE)
  )
  
  scens <- sort(unique(long$scenarioName))
  
  wide <- tidyr::pivot_wider(
    long,
    id_cols = c(metric, PintailRegion),
    names_from = scenarioName,
    values_from = value,
    values_fill = NA_real_,
    values_fn = list(value = ~ dplyr::first(.x))
  )
  
  wide <- dplyr::arrange(wide, metric, PintailRegion)
  wide <- wide[, c("metric", "PintailRegion", scens[scens %in% names(wide)]), drop = FALSE]
  wide
}

# concise usage
combined_pintail_birders_k_df <- combine_scenarios_fn()
write.csv(combined_pintail_birders_k_df,"combined_pintail_birders_k_df.csv")