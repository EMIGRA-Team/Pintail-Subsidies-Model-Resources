# v14 — subtotal imputation from base rows; one row per region; apply proportion to totals
estimate_pintail_birders_fn <- function(
    birders_US2011_tbl,
    birdersUS2006_tbl,
    birding_CN_tbl,
    censusCanada_tbl,
    pintail_tbl,          # expects PintailRegion and Prop_pintails_birding (rename Group -> PintailRegion upstream)
    nd_pop_2006,
    nd_pop_2011,
    canada_province_col = NULL,   # e.g., "Province", "Geographic_name", "NAME"
    canada_prop_col     = NULL,   # e.g., "Birding_prop", "prop", "proportion"
    canada_drop_unmapped = TRUE   # if TRUE, warn and drop unmapped provinces; if FALSE, stop
) {
  normalize_province <- function(x) {
    x0 <- trimws(as.character(x))
    x0 <- gsub("\\+", "", x0)  # strip '+', e.g., "NU+"
    x0 <- gsub("^Newfoundland\\s*&\\s*Labrador$", "Newfoundland and Labrador", x0, ignore.case = TRUE)
    x0 <- gsub("^Prince\\s*Edward\\s*Island$", "Prince Edward Island", x0, ignore.case = TRUE)
    x0 <- gsub("^Northwest\\s*Territories$", "Northwest Territories", x0, ignore.case = TRUE)
    xu <- toupper(x0)
    abbr_map <- c(
      "AB"="Alberta","BC"="British Columbia","MB"="Manitoba","NB"="New Brunswick",
      "NL"="Newfoundland and Labrador","NS"="Nova Scotia","NT"="Northwest Territories",
      "NWT"="Northwest Territories","NU"="Nunavut","ON"="Ontario","PE"="Prince Edward Island",
      "PEI"="Prince Edward Island","QC"="Quebec","PQ"="Quebec","SK"="Saskatchewan",
      "YT"="Yukon","YK"="Yukon"
    )
    out <- x0
    idx <- match(xu, names(abbr_map))
    out[!is.na(idx)] <- abbr_map[idx[!is.na(idx)]]
    full_names <- c(
      "Alberta","British Columbia","Manitoba","New Brunswick","Newfoundland and Labrador",
      "Nova Scotia","Northwest Territories","Nunavut","Ontario","Prince Edward Island",
      "Quebec","Saskatchewan","Yukon"
    )
    for (nm in full_names) out[tolower(out) == tolower(nm)] <- nm
    out[toupper(out) %in% c("CANADA","ALL CANADA","TOTAL CANADA")] <- NA_character_
    out
  }
  
  # 1) U.S. — ND override; treat 'Total' as total birders (k)
  us_tbl <- birders_US2011_tbl %>%
    dplyr::mutate(PintailRegion = dplyr::case_when(
      State == "North Dakota" ~ "Southern breeding",
      TRUE ~ PintailRegion
    ))
  
  state_region_map <- us_tbl %>%
    dplyr::filter(!is.na(PintailRegion)) %>%
    dplyr::select(State, PintailRegion) %>%
    dplyr::distinct()
  
  us_region_total_k <- us_tbl %>%
    dplyr::filter(!is.na(PintailRegion)) %>%
    dplyr::group_by(PintailRegion) %>%
    dplyr::summarise(total_birders_k = sum(Total, na.rm = TRUE), .groups = "drop")
  
  # 2) Canada — detect columns; normalize names; compute total birders; aggregate to region
  cn_cols <- names(birding_CN_tbl)
  cn_cols_lower <- tolower(cn_cols)
  
  if (is.null(canada_province_col)) {
    prov_aliases <- c("province","geographic_name","provinceterritory","province_territory",
                      "province.territory","name","jurisdiction")
    hit <- which(cn_cols_lower %in% prov_aliases)
    if (length(hit) == 0) stop(sprintf(
      "Provide canada_province_col. Available: %s", paste(cn_cols, collapse = ", ")
    ))
    canada_province_col <- cn_cols[hit[1]]
    message(sprintf("Auto-detected Canadian province column: '%s'", canada_province_col))
  } else if (!canada_province_col %in% cn_cols) {
    stop(sprintf("canada_province_col '%s' not found. Available: %s",
                 canada_province_col, paste(cn_cols, collapse = ", ")))
  }
  
  if (is.null(canada_prop_col)) {
    prop_aliases <- c("prop","proportion","share","pct","percent",
                      "pct_birders","prop_birders","birding_prop")
    hit <- which(cn_cols_lower %in% prop_aliases)
    if (length(hit) == 0) hit <- grep("prop", cn_cols_lower, fixed = TRUE)
    if (length(hit) == 0) stop(sprintf(
      "Provide canada_prop_col. Available: %s", paste(cn_cols, collapse = ", ")
    ))
    canada_prop_col <- cn_cols[hit[1]]
    message(sprintf("Auto-detected Canadian proportion column: '%s'", canada_prop_col))
  } else if (!canada_prop_col %in% cn_cols) {
    stop(sprintf("canada_prop_col '%s' not found. Available: %s",
                 canada_prop_col, paste(cn_cols, collapse = ", ")))
  }
  
  cn_map <- censusCanada_tbl %>%
    dplyr::transmute(
      Province        = normalize_province(Geographic_name),
      Population_2011 = as.numeric(Population_2011),
      PintailRegion   = Pintail_region
    ) %>%
    dplyr::filter(!is.na(Province)) %>%
    dplyr::distinct(Province, .keep_all = TRUE)
  
  cn_tbl_std <- birding_CN_tbl %>%
    dplyr::rename(Province_raw = dplyr::all_of(canada_province_col)) %>%
    dplyr::mutate(
      Province = normalize_province(Province_raw),
      prop_raw = !!rlang::sym(canada_prop_col)
    ) %>%
    dplyr::filter(!is.na(Province))
  
  if (!is.numeric(cn_tbl_std$prop_raw)) {
    cn_tbl_std$prop_raw <- suppressWarnings(readr::parse_number(cn_tbl_std$prop_raw))
  }
  if (!is.numeric(cn_tbl_std$prop_raw)) {
    stop(sprintf("Column '%s' could not be coerced to numeric proportions.", canada_prop_col))
  }
  if (any(cn_tbl_std$prop_raw > 1, na.rm = TRUE)) {
    cn_tbl_std <- cn_tbl_std %>% dplyr::mutate(prop = prop_raw / 100)
  } else {
    cn_tbl_std <- cn_tbl_std %>% dplyr::mutate(prop = prop_raw)
  }
  
  cn_prop_by_prov <- cn_tbl_std %>%
    dplyr::group_by(Province) %>%
    dplyr::summarise(prop = mean(prop, na.rm = TRUE), .groups = "drop")
  
  cn_joined <- cn_prop_by_prov %>%
    dplyr::left_join(cn_map %>% dplyr::select(Province, Population_2011, PintailRegion),
                     by = "Province")
  
  # Handle unmapped provinces
  unmapped <- cn_joined %>% dplyr::filter(is.na(PintailRegion)) %>% dplyr::pull(Province)
  if (length(unmapped) > 0) {
    msg <- sprintf(
      "Dropping Canadian provinces/territories without PintailRegion mapping: %s",
      paste(unique(unmapped), collapse = ", ")
    )
    if (isTRUE(canada_drop_unmapped)) {
      warning(msg)
      cn_joined <- cn_joined %>% dplyr::filter(!is.na(PintailRegion))
    } else {
      stop(gsub("^Dropping ", "", msg))
    }
  }
  
  missing_pop <- cn_joined %>% dplyr::filter(is.na(Population_2011)) %>% dplyr::pull(Province)
  if (length(missing_pop) > 0) {
    stop(sprintf(
      "Missing Population_2011 in censusCanada_tbl for: %s",
      paste(unique(missing_pop), collapse = ", ")
    ))
  }
  
  # Compute provincial total birders and convert to thousands
  cn_joined <- cn_joined %>%
    dplyr::mutate(
      birders_count = prop * Population_2011,
      total_k = birders_count / 1000
    )
  
  cn_region_total_k <- cn_joined %>%
    dplyr::group_by(PintailRegion) %>%
    dplyr::summarise(total_birders_k = sum(total_k, na.rm = TRUE), .groups = "drop")
  
  # 3) Combine U.S. and Canada → total birders by region (k)
  region_total_birders_k <- dplyr::bind_rows(us_region_total_k, cn_region_total_k) %>%
    dplyr::group_by(PintailRegion) %>%
    dplyr::summarise(total_birders_k = sum(total_birders_k, na.rm = TRUE), .groups = "drop")
  
  # 4) pintail_tbl subtotal imputation and collapse to one row per region
  #    - Expect PintailRegion and Prop_pintails_birding present
  #    - Optional flags IsSubtotal/IsTotal may exist
  pt_cols <- names(pintail_tbl)
  needed <- c("PintailRegion", "Prop_pintails_birding")
  if (!all(needed %in% pt_cols)) {
    stop(sprintf("pintail_tbl must contain columns: %s", paste(needed, collapse = ", ")))
  }
  
  pintail_props_raw <- pintail_tbl %>%
    dplyr::select(dplyr::any_of(c("PintailRegion", "Prop_pintails_birding", "IsSubtotal", "IsTotal"))) %>%
    dplyr::mutate(
      IsSubtotal = ifelse("IsSubtotal" %in% names(.), IsSubtotal, NA),
      IsTotal    = ifelse("IsTotal"    %in% names(.), IsTotal, NA)
    )
  
  # Coerce/scale proportion to 0–1
  if (!is.numeric(pintail_props_raw$Prop_pintails_birding)) {
    pintail_props_raw$Prop_pintails_birding <- suppressWarnings(
      readr::parse_number(pintail_props_raw$Prop_pintails_birding)
    )
  }
  if (any(pintail_props_raw$Prop_pintails_birding > 1, na.rm = TRUE)) {
    pintail_props_raw <- pintail_props_raw %>%
      dplyr::mutate(Prop_pintails_birding = Prop_pintails_birding / 100)
  }
  
  # Identify base vs subtotal rows
  base_rows <- pintail_props_raw %>%
    dplyr::filter(is.na(IsTotal) | !IsTotal) %>%
    dplyr::filter(is.na(IsSubtotal) | !IsSubtotal)
  
  subtotal_rows <- pintail_props_raw %>%
    dplyr::filter(is.na(IsTotal) | !IsTotal) %>%
    dplyr::filter(!is.na(IsSubtotal) & IsSubtotal)
  
  # Compute regional mean from base rows
  regional_means <- base_rows %>%
    dplyr::group_by(PintailRegion) %>%
    dplyr::summarise(
      mean_prop = mean(Prop_pintails_birding, na.rm = TRUE),
      n_base_nonmiss = sum(!is.na(Prop_pintails_birding)),
      .groups = "drop"
    )
  
  # Impute subtotal NA values with regional mean from base rows
  subtotal_imputed <- subtotal_rows %>%
    dplyr::left_join(regional_means, by = "PintailRegion") %>%
    dplyr::mutate(
      Prop_pintails_birding = dplyr::coalesce(Prop_pintails_birding, mean_prop)
    ) %>%
    dplyr::select(PintailRegion, Prop_pintails_birding, IsSubtotal, IsTotal, n_base_nonmiss)
  
  # Warn on imputation events
  imputed_regions <- subtotal_imputed %>%
    dplyr::filter(!is.na(n_base_nonmiss) & n_base_nonmiss > 0) %>%
    dplyr::filter(!is.na(Prop_pintails_birding)) %>%
    dplyr::pull(PintailRegion) %>% unique()
  if (length(imputed_regions) > 0) {
    warning(sprintf(
      "Imputed subtotal Prop_pintails_birding from base rows for regions: %s",
      paste(imputed_regions, collapse = ", ")
    ))
  }
  
  # Regions where imputation not possible (no non-missing base rows)
  not_imputed_regions <- subtotal_imputed %>%
    dplyr::filter(is.na(Prop_pintails_birding)) %>%
    dplyr::pull(PintailRegion) %>% unique()
  if (length(not_imputed_regions) > 0) {
    warning(sprintf(
      "Could not impute subtotal Prop_pintails_birding (no non-missing base rows) for regions: %s",
      paste(not_imputed_regions, collapse = ", ")
    ))
  }
  
  # Recombine: prefer subtotal (possibly imputed) when available; otherwise use base rows
  # Start with best-available subtotal per region
  subt_best <- subtotal_imputed %>%
    dplyr::group_by(PintailRegion) %>%
    dplyr::summarise(Prop_pintails_birding = dplyr::first(Prop_pintails_birding), .groups = "drop")
  
  # For regions with no subtotal, fall back to base mean
  base_best <- base_rows %>%
    dplyr::group_by(PintailRegion) %>%
    dplyr::summarise(Prop_pintails_birding = mean(Prop_pintails_birding, na.rm = TRUE), .groups = "drop")
  
  pintail_props <- dplyr::full_join(subt_best, base_best, by = "PintailRegion", suffix = c("_subt", "_base")) %>%
    dplyr::mutate(
      Prop_pintails_birding = dplyr::coalesce(Prop_pintails_birding_subt, Prop_pintails_birding_base)
    ) %>%
    dplyr::select(PintailRegion, Prop_pintails_birding)
  
  # Final fallback: coalesce to 0 if still missing
  missing_after <- pintail_props %>% dplyr::filter(is.na(Prop_pintails_birding)) %>% dplyr::pull(PintailRegion)
  if (length(missing_after) > 0) {
    warning(sprintf(
      "Prop_pintails_birding missing after subtotal/base fallback for regions: %s. Setting to 0.",
      paste(missing_after, collapse = ", ")
    ))
  }
  pintail_props <- pintail_props %>%
    dplyr::mutate(Prop_pintails_birding = dplyr::coalesce(Prop_pintails_birding, 0)) %>%
    dplyr::distinct()
  
  # 5) Apply proportions to total birders per region
  region_level <- region_total_birders_k %>%
    dplyr::left_join(pintail_props, by = "PintailRegion") %>%
    dplyr::mutate(
      Prop_pintails_birding = dplyr::coalesce(Prop_pintails_birding, 0),
      pintail_birders_k = total_birders_k * Prop_pintails_birding
    ) %>%
    dplyr::select(PintailRegion, pintail_birders_k) %>%
    dplyr::distinct()
  
  # Ensure canonical regions present
  expected_regions <- c("Southern breeding","Northern breeding","Alaska breeding",
                        "Western wintering","Central wintering")
  region_level <- tibble::tibble(PintailRegion = expected_regions) %>%
    dplyr::left_join(region_level, by = "PintailRegion") %>%
    dplyr::mutate(pintail_birders_k = dplyr::coalesce(pintail_birders_k, 0))
  
  # Return
  list(
    region_level = region_level,
    state_region_map = state_region_map,
    canada_detail = cn_joined %>%
      dplyr::select(Province, PintailRegion, Population_2011, prop, birders_count, total_k)
  )
}