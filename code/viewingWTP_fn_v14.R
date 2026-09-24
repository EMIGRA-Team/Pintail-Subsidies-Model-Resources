# v19 — viewingWTP_fn add trips_A to output
viewingWTP_fn <- function(
    birders_US2011_tbl,
    birdersUS2006_tbl,
    birding_CN_tbl,
    censusCanada_tbl,
    pintail_tbl,
    nd_pop_2006,
    nd_pop_2011,
    pintail_rel_abund,  # numeric scalar or named vector keyed by PintailRegion
    WTP_tbl              # must include: state (USPS), trp_num, max_WTP_BM, trips_dbl_birds
) {
  # 1) Pintail birders by region (k)
  pb <- estimate_pintail_birders_fn(
    birders_US2011_tbl = birders_US2011_tbl,
    birdersUS2006_tbl  = birdersUS2006_tbl,
    birding_CN_tbl     = birding_CN_tbl,
    censusCanada_tbl   = censusCanada_tbl,
    pintail_tbl        = pintail_tbl,
    nd_pop_2006        = nd_pop_2006,
    nd_pop_2011        = nd_pop_2011
  )$region_level %>%
    dplyr::select(PintailRegion, pintail_birders_k)
  
  # 2) Map states to PintailRegion (ND override embedded in helper)
  state_region_map <- .make_state_region_map(birders_US2011_tbl)
  
  # 3) Prepare WTP records with state names and join to region map
  wtp_prepped <- WTP_tbl %>%
    dplyr::mutate(
      State = .abbr_to_name(state)
    ) %>%
    dplyr::left_join(state_region_map, by = "State")
  
  # 4) Abundance input (scalar or named vector), attach per region
  if (length(pintail_rel_abund) == 1 && is.numeric(pintail_rel_abund)) {
    abund_tbl <- pb %>% dplyr::mutate(Abundance = pintail_rel_abund)
  } else if (is.numeric(pintail_rel_abund) && !is.null(names(pintail_rel_abund))) {
    abund_tbl <- tibble::tibble(PintailRegion = names(pintail_rel_abund),
                                Abundance = as.numeric(pintail_rel_abund)) %>%
      dplyr::right_join(pb, by = "PintailRegion")
  } else {
    stop("pintail_rel_abund must be a numeric scalar or a named numeric vector with names matching PintailRegion.")
  }
  
  # 5) Compute per-respondent per-capita WTP at the specified abundance
  #    Zero-intercept linear model: trips(A) = trp_num * A
  wtp_at_A <- wtp_prepped %>%
    dplyr::left_join(abund_tbl %>% dplyr::select(PintailRegion, Abundance), by = "PintailRegion") %>%
    dplyr::mutate(
      trips_A = trp_num * Abundance,
      percap_WTP_resp = max_WTP_BM * trips_A
    )
  
  # 6) Compute regional per-capita WTP (median across respondents)
  region_wtp <- wtp_at_A %>%
    dplyr::filter(!is.na(PintailRegion)) %>%
    dplyr::group_by(PintailRegion) %>%
    dplyr::summarise(
      percap_WTP = stats::median(percap_WTP_resp, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 6b) Compute regional trips_A (median across respondents)
  region_trips <- wtp_at_A %>%
    dplyr::filter(!is.na(PintailRegion)) %>%
    dplyr::group_by(PintailRegion) %>%
    dplyr::summarise(
      trips_A = stats::median(trips_A, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 7) Impute missing regional medians at A (if any)
  get_state_medians_at_A <- function(states) {
    wtp_at_A %>%
      dplyr::filter(State %in% states) %>%
      dplyr::pull(percap_WTP_resp) %>%
      stats::median(na.rm = TRUE)
  }
  get_state_tripsA_at_A <- function(states) {
    wtp_at_A %>%
      dplyr::filter(State %in% states) %>%
      dplyr::pull(trips_A) %>%
      stats::median(na.rm = TRUE)
  }
  
  nd_sd_medA   <- get_state_medians_at_A(c("North Dakota", "South Dakota"))
  ca_or_medA   <- get_state_medians_at_A(c("California", "Oregon"))
  nd_sd_tripsA <- get_state_tripsA_at_A(c("North Dakota", "South Dakota"))
  ca_or_tripsA <- get_state_tripsA_at_A(c("California", "Oregon"))
  
  # Ensure rows exist for all regions; fill using imputation rules
  region_wtp_complete <- tibble::tibble(PintailRegion = unique(pb$PintailRegion)) %>%
    dplyr::left_join(region_wtp, by = "PintailRegion") %>%
    dplyr::mutate(
      percap_WTP = dplyr::case_when(
        PintailRegion == "Northern breeding" & (is.na(percap_WTP) | is.infinite(percap_WTP)) ~ nd_sd_medA,
        PintailRegion == "Alaska breeding"   & (is.na(percap_WTP) | is.infinite(percap_WTP)) ~ ca_or_medA,
        TRUE ~ percap_WTP
      )
    )
  
  region_trips_complete <- tibble::tibble(PintailRegion = unique(pb$PintailRegion)) %>%
    dplyr::left_join(region_trips, by = "PintailRegion") %>%
    dplyr::mutate(
      trips_A = dplyr::case_when(
        PintailRegion == "Northern breeding" & (is.na(trips_A) | is.infinite(trips_A)) ~ nd_sd_tripsA,
        PintailRegion == "Alaska breeding"   & (is.na(trips_A) | is.infinite(trips_A)) ~ ca_or_tripsA,
        TRUE ~ trips_A
      )
    )
  
  # 8) Join counts and abundance; compute total WTP (millions)
  out <- abund_tbl %>%
    dplyr::select(PintailRegion, pintail_birders_k, Abundance) %>%
    dplyr::left_join(region_wtp_complete, by = "PintailRegion") %>%
    dplyr::left_join(region_trips_complete, by = "PintailRegion") %>%
    dplyr::mutate(
      total_WTP_M = (pintail_birders_k * 1000 * percap_WTP) / 1e6
    ) %>%
    dplyr::rename(pintail_rel_abund = Abundance) %>%
    dplyr::select(PintailRegion, pintail_birders_k, pintail_rel_abund, trips_A, percap_WTP, total_WTP_M)
  
  # 9) Warn if any regions still missing percap_WTP (should be none after imputation)
  missing_wtp <- out %>% dplyr::filter(is.na(percap_WTP))
  if (nrow(missing_wtp) > 0) {
    warning(sprintf("Still missing per-capita WTP for: %s",
                    paste(unique(missing_wtp$PintailRegion), collapse = ", ")))
  }
  
  out
}