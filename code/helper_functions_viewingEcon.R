# Helpers v16 — consolidated utilities used by viewingWTP_fn and estimate_pintail_birders_fn

# 1) Convert USPS state abbreviations to full state names (vectorized)
.abbr_to_name <- function(x) {
  x0 <- trimws(as.character(x))
  xu <- toupper(x0)
  
  # Mapping for 50 states + DC
  map <- c(
    AL="Alabama", AK="Alaska", AZ="Arizona", AR="Arkansas", CA="California",
    CO="Colorado", CT="Connecticut", DE="Delaware", FL="Florida", GA="Georgia",
    HI="Hawaii", ID="Idaho", IL="Illinois", IN="Indiana", IA="Iowa",
    KS="Kansas", KY="Kentucky", LA="Louisiana", ME="Maine", MD="Maryland",
    MA="Massachusetts", MI="Michigan", MN="Minnesota", MS="Mississippi", MO="Missouri",
    MT="Montana", NE="Nebraska", NV="Nevada", NH="New Hampshire", NJ="New Jersey",
    NM="New Mexico", NY="New York", NC="North Carolina", ND="North Dakota", OH="Ohio",
    OK="Oklahoma", OR="Oregon", PA="Pennsylvania", RI="Rhode Island", SC="South Carolina",
    SD="South Dakota", TN="Tennessee", TX="Texas", UT="Utah", VT="Vermont",
    VA="Virginia", WA="Washington", WV="West Virginia", WI="Wisconsin", WY="Wyoming",
    DC="District of Columbia"
  )
  
  out <- x0
  idx <- match(xu, names(map))
  out[!is.na(idx)] <- unname(map[idx[!is.na(idx)]])
  
  out
}

# 2) Build State -> PintailRegion map (applies ND override)
.make_state_region_map <- function(birders_US2011_tbl) {
  # Detect columns (case-insensitive aliases)
  us_cols <- names(birders_US2011_tbl)
  us_cols_lower <- tolower(us_cols)
  
  # State column
  hit_state <- which(us_cols_lower %in% c("state"))
  if (length(hit_state) == 0) {
    stop(sprintf(
      "Could not detect a State column in birders_US2011_tbl. Available: %s",
      paste(us_cols, collapse = ", ")
    ))
  }
  state_col <- us_cols[hit_state[1]]
  
  # Region column (PintailRegion)
  hit_region <- which(us_cols_lower %in% c("pintailregion","pintail_region","region"))
  if (length(hit_region) == 0) {
    stop(sprintf(
      "Could not detect a PintailRegion/Region column in birders_US2011_tbl. Available: %s",
      paste(us_cols, collapse = ", ")
    ))
  }
  region_col <- us_cols[hit_region[1]]
  
  # Standardize and apply ND override
  map_tbl <- birders_US2011_tbl %>%
    dplyr::rename(
      State = !!rlang::sym(state_col),
      PintailRegion = !!rlang::sym(region_col)
    ) %>%
    dplyr::mutate(
      PintailRegion = dplyr::case_when(
        State == "North Dakota" ~ "Southern breeding",
        TRUE ~ PintailRegion
      )
    ) %>%
    dplyr::filter(!is.na(State), !is.na(PintailRegion)) %>%
    dplyr::select(State, PintailRegion) %>%
    dplyr::distinct()
  
  map_tbl
}

# 3) Normalize Canadian province/territory names to full names used in censusCanada_tbl
.normalize_province <- function(x) {
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
  out[!is.na(idx)] <- unname(abbr_map[idx[!is.na(idx)]])
  
  full_names <- c(
    "Alberta","British Columbia","Manitoba","New Brunswick","Newfoundland and Labrador",
    "Nova Scotia","Northwest Territories","Nunavut","Ontario","Prince Edward Island",
    "Quebec","Saskatchewan","Yukon"
  )
  for (nm in full_names) out[tolower(out) == tolower(nm)] <- nm
  
  # Remove aggregate rows if present
  out[toupper(out) %in% c("CANADA","ALL CANADA","TOTAL CANADA")] <- NA_character_
  
  out
}