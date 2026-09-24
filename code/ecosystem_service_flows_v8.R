# ecosystem_service_flows*.r prepared by B. Mattsson and J. Windt
  # calculations of subsidies and ES flows based on computational formulas provided by Darius J. Semmens

# front matter ----
## If not Rstudio project, then set directory ----
#setwd("C:/Users/jendrik/Seafile/Projekte/DISES/analysis/R/ecosystem_service_flows")


## Optional: load environment----
#load("C:/Users/jendrik/Seafile/Projekte/DISES/analysis/R/ecosystem_service_flows/ecosystem_service_flows.RData")
#save.image("C:/Users/jendrik/Seafile/Projekte/DISES/analysis/R/ecosystem_service_flows/ecosystem_service_flows.RData")


#library(XLConnect)

# set up current Java connection so that XLConnect loads properly ####
adoptium_dir <- "C:/Program Files/Eclipse Adoptium"
if (dir.exists(adoptium_dir)) {
  # List folders and pick the first JDK folder found
  jdk_folders <- list.dirs(adoptium_dir, recursive = FALSE)
  if (length(jdk_folders) > 0) {
    Sys.setenv(JAVA_HOME = jdk_folders[1])
  }
}

## Load packages----
x = c ("tidyverse", "XLConnect") # , "openxlsx"
# install.packages(x)
invisible(lapply(x, library, character.only=TRUE))
rm(x)

# install and load previous version of XLConnect to avoid compatibility issues with Java
#require(devtools) # install.packages("devtools")
#pak::pak("XLConnect", version = "1.0.1", repos = "http://cran.us.r-project.org")

# Function to source the latest-matching file for each of multiple patterns, with optional printing ####
## front matter ####
# - patterns: character vector of patterns (glob by default, or regex if use_regex = TRUE)
# - print_info: logical; if TRUE, print "filename | first line" for each sourced file
# - dir: directory to search (non-recursive by default)
# - use_regex: interpret patterns as regex when TRUE; otherwise as glob
# - recursive: search subdirectories when TRUE
# - ignore.case: case-insensitive matching when TRUE
# - ...: passed to base::source (e.g., local = TRUE)
## define source_latest_fn ####
source_latest_fn <- function(patterns, print_info = FALSE,
                             dir = ".", use_regex = FALSE,
                             recursive = FALSE, ignore.case = FALSE,
                             verbose = TRUE, stop_on_missing = TRUE, ...) {
  stopifnot(is.character(patterns), length(patterns) >= 1)
  
  # Extract trailing version from basename (before extension), e.g., _1, _1.2.3, -v2.10, V3
  get_version <- function(p) {
    stem <- tools::file_path_sans_ext(basename(p))
    m <- regexpr("(?:[._-]?[vV])?(\\d+(?:\\.\\d+)*)$", stem, perl = TRUE)
    if (m[1] == -1) return(NA_character_)
    full <- regmatches(stem, m)
    sub("^(?:[._-]?[vV])?(\\d+(?:\\.\\d+)*)$", "\\1", full, perl = TRUE)
  }
  
  # Helper to list matches for a single pattern with current settings
  list_matches <- function(pattern) {
    rx <- if (use_regex) pattern else utils::glob2rx(pattern)
    list.files(dir, pattern = rx, full.names = TRUE,
               recursive = recursive, ignore.case = ignore.case)
  }
  
  # Precheck: collect which patterns have at least one match
  matches_list <- lapply(patterns, list_matches)
  has_match <- vapply(matches_list, function(x) length(x) > 0L, logical(1))
  
  if (!all(has_match)) {
    missing <- paste(patterns[!has_match], collapse = ", ")
    if (isTRUE(stop_on_missing)) {
      stop(sprintf("No files match the following pattern(s): %s", missing), call. = FALSE)
    } else {
      warning(sprintf("No files match the following pattern(s): %s", missing), call. = FALSE)
      # Filter out missing patterns for subsequent steps
      patterns <- patterns[has_match]
      matches_list <- matches_list[has_match]
      if (length(patterns) == 0L) return(invisible(character()))
    }
  }
  
  # Choose one "latest" file per pattern
  choose_one_from <- function(files) {
    vers_str <- vapply(files, get_version, character(1))
    has_ver  <- !is.na(vers_str)
    
    if (any(has_ver)) {
      vers <- base::numeric_version(vers_str[has_ver])
      max_idx <- which(vers == max(vers))
      cand <- files[has_ver][max_idx]
      if (length(cand) > 1L) {
        mt <- file.info(cand)$mtime
        cand <- cand[which.max(mt)]
      }
      cand
    } else {
      mt <- file.info(files)$mtime
      files[which.max(mt)]
    }
  }
  
  chosen <- mapply(function(pat, files) {
    ch <- choose_one_from(files)
    if (verbose) message("Sourcing: ", ch, "  [pattern: ", pat, "]")
    source(ch, ...)
    ch
  }, patterns, matches_list, SIMPLIFY = TRUE, USE.NAMES = FALSE)
  
  names(chosen) <- patterns
  
  if (isTRUE(print_info)) {
    for (p in chosen) {
      first_line <- tryCatch(readLines(p, n = 1, warn = FALSE), error = function(e) "")
      cat(sprintf("%s | %s\n", basename(p), first_line))
    }
  }
  
  invisible(chosen)
}

# define net_ES_flows_fn #### region_names <- names(Yi)
net_ES_flows_fn <- function(Yi, region_names = names(Yi)) {
  # front matter ####
  # - For receivers with Yi = 0 or if no payers exist, flows are zeros.
  # - Row sums  receivers’ Yi; column sums match absolute payer outflows (-Yi).
  
  # calculations ####
  stopifnot(is.numeric(Yi), all(is.finite(Yi) | is.na(Yi))) ####
  if (is.null(region_names)) region_names <- paste0("R", seq_along(Yi)) ####
  names(Yi) <- region_names ####
  
  pay <- which(Yi > 0 & !is.na(Yi))    # payers
  rec <- which(Yi < 0 & !is.na(Yi))    # receivers
  
  R <- length(region_names) ####
  ES_flow_mat <- matrix(0, nrow = R, ncol = R,
                        dimnames = list(payer = region_names, receiver = region_names)) ####
  
  if (length(rec) == 0 || length(pay) == 0) return(ES_flow_mat) ####
  
  denom <- sum(-Yi[rec]) ####
  if (denom <= 0 || !is.finite(denom)) stop("Invalid denominator: sum of payer outflows must be positive.") ####
  receiver_share <- (-Yi[rec]) / denom  # nonnegative shares summing to 1 ####
  
  # Allocate each receiver’s inflow across payers proportionally ####
  ES_flow_mat[pay, rec] <- outer(Yi[pay], receiver_share, `*`) ####
  #print(ES_flow_mat)
  return(ES_flow_mat)
} # end net_ES_flows_fn

# source required functions and optionally print "filename | first line" ----
source_latest_fn(
  patterns = c("estimate_pintail_birders_fn*.R", "viewingWTP_fn*","helper_functions_viewingEcon*", 
               "population_model_functions*"), 
  print_info = TRUE
)
  # ,"PintailSimulation*"

# Define scenarios & regions for calculating Ds ####

scenarios <- c("1_Conservative", "2_Hunting", "3_Grassland", "4_Integrated") # 
regions <- c("Alaska breeding","Southern breeding","Northern breeding",
             "Western wintering","Central wintering")


# Loop through scenarios & make subsidy/flow calculations ####
 # calculate ES flows between regions and subsidies including Di and Vi by region
  regionResults_list <- vector("list", length(scenarios)) # Container for scenario * region-specific results
  ES_flows_array <- array(NA_real_, dim = c(length(regions) , length(regions) , length(scenarios)),
                  dimnames = list(payer       = regions,
                                  receiver    = regions,
                                  scenario = scenarios)) #### Container for ES flows by scenario
  # scenarioName <- "1_Conservative"; i <- 1
#for(i in seq_along(scenarios)){  
for(scenarioName in scenarios){ 
  #scenarioName <- scenarios[i]  # current scenario ####
  # Get Di & Vi by respectively sourcing latest versions of PintailSimulation*.r and ViewingEconomics*.r ####
    # optionally print_info "filename | first line" and print_objects
  source_latest_fn(
    patterns = c("PintailSimulation*", "ViewingEconomics*"), 
    print_info = FALSE
  )
  
  # Calculate ES flows between & subsidies for each region ----
  
  # total annual viewing value provided by pintails in region i under given scenarioName (millions 2016 USD) 
  # wtp_out generated by ViewingEconomics*.r
  Vi <- wtp_out$total_WTP_M 
  names(Vi) <- wtp_out$PintailRegion 
  Vi <- Vi[regions]
  
  Di <- c(DS) # DS is from PintailSimulation*.r; str(DS)
  names(Di) <- c("Alaska breeding", "Southern breeding", "Northern breeding", "Western wintering", "Central wintering")
  Di <- Di[regions]
  
  MOi <- (sum(Vi)-Vi)*Di
  #outgoing migration support provided by location i to other locations
  
  MIi <- Vi*(1-Di)
  #incoming migration support received at location i from other locations
  
  MLi <- Vi*Di
  #locally received migration support from location i to location i
  
  Yi <- sum(Vi)*Di-Vi # rep(sum(Vi),length(Vi))
  Yi <- rep(sum(Vi),length(Vi))*Di-Vi # 
  
  #spatial subsidy (net benefit flow to/from each location), or Yi= MOi-MIi
  
  YLi <- ifelse(Yi<0, Vi+Yi, Vi)
  #net local flow (net benefit flow from ecosystems to people within each region)
  
  ES_df <- data.frame(Di,Vi)
  
  # Store region-specific results for this scenario ####
  scn_num <- match(scenarioName,scenarios)
  regionResults_list[[scn_num]] <- data.frame(
    scenarioName  = rep(scenarioName, length(regions)),
    region    = regions,
    Di        = as.numeric(DS),
    Vi        = as.numeric(Vi),
    Yi        = as.numeric(Yi),
    YLi       = as.numeric(YLi),
    stringsAsFactors = FALSE
  ) # end filling regionResults_list
  # Calculate and store net ES flows for this scenario (eqn page 64 of Bagstad et al. 2019) ####
  #ES_flows_array[,,scn_num] <- outer(Di, Vi, `*`) ; region_names <- names(Yi)
  
  ## Store ES flows for current scenario #### x <- net_ES_flows_fn(Yi)
  ES_flows_array[,,scn_num] <- net_ES_flows_fn(Yi)

} # end for(scenarioName in scenarios)

# Combine & visualize results ####
regionResults_long <- do.call(rbind, regionResults_list)
  write.csv(regionResults_long,"regionResults_long.csv")
tol <- 1e-12
net_flows_long_df <- do.call(rbind, lapply(seq_len(length(scenarios)), function(k) {
  scn <- scenarios[k]                                ####
  df  <- as.data.frame(as.table(ES_flows_array[, , k]), stringsAsFactors = FALSE) ####
  names(df) <- c("payer", "receiver",  "flow")        ####
  df$scenario <- scn                                  ####
  df <- df[!is.na(df$flow) & abs(df$flow) > tol, , drop = FALSE]  # remove zeros/near-zeros
  df
})) #
write.csv(net_flows_long_df,"net_flows_long_df.csv")
## Plot: dot plot of subsidy by scenarioName with unique symbol per region ####
regionResults_long$scenarioName <- factor(regionResults_long$scenarioName, levels = scenarios)

p_subsidy <- ggplot2::ggplot(regionResults_long, ggplot2::aes(x = scenarioName, y = Yi, shape = region)) +
  ggplot2::geom_point(size = 3) +
  ggplot2::scale_shape_manual(values = c(1, 2, 0, 3, 4)) +  # 16, 17, 15, 3, 4
  ggplot2::labs(x = "Scenario", y = "Region-specific subsidy (millions 2016 USD)",
                shape = "Region") +
  ggplot2::theme_minimal(base_size = 12)

print(p_subsidy)
# Save plot
ggplot2::ggsave("subsidy_dotplot.png", p_subsidy, width = 7, height = 4, dpi = 300)
ggplot2::ggsave("subsidy_dotplot.pdf", p_subsidy, width = 7, height = 4)

## Build and export region results table ####
# Desired scenario columns and order
desired_scenarios <- scenarios_clean <-   sub("^[0-9]+_", "", scenarios)

# Helper: quartile skewness; Bowley's measure of skewness; Bowley, A. L. (1901). Elements of Statistics, P.S. King & Son, London.
quartile_skew <- function(x) {
  q <- quantile(x, probs = c(.25, .5, .75), na.rm = TRUE, names = FALSE)
  if (q[3] == q[1]) return(NA_real_)
  (q[3] + q[1] - 2*q[2]) / (q[3] - q[1])
}

# Base prep: clean scenario labels and ordering
df <- regionResults_long %>%
  mutate(
    Scenario = sub("^[0-9]+_", "", scenarioName),   # strip numeric prefix like "1_"
    Scenario = factor(Scenario, levels = desired_scenarios)
  )

# Region ordering
if (exists("regions", inherits = TRUE)) {
  region_levels <- regions
} else {
  present <- regions[regions %in% unique(df$region)]
  others  <- setdiff(unique(df$region), present)
  region_levels <- c(present, others)
}
df <- df %>% mutate(region = factor(region, levels = region_levels))

# Ensure all desired scenario columns exist and are ordered
ensure_scenario_cols <- function(wide_df) {
  missing <- setdiff(desired_scenarios, names(wide_df))
  for (m in missing) wide_df[[m]] <- NA_real_
  wide_df <- wide_df[, c(setdiff(names(wide_df), desired_scenarios), desired_scenarios)]
  wide_df
}

# Build block for Di and Yi (regions + Range + Quartile skewness)
build_block_with_range_skew <- function(data, value_col, variable_label) {
  wide <- data %>%
    transmute(
      Region = region,
      Scenario = Scenario,
      value = suppressWarnings(as.numeric(.data[[value_col]]))
    ) %>%
    tidyr::pivot_wider(names_from = Scenario, values_from = value, values_fill = NA_real_) %>%
    arrange(Region) %>%
    as.data.frame() %>%
    ensure_scenario_cols()
  
  # Compute summaries across region rows
  rng_vals <- sapply(wide[desired_scenarios], function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  qsk_vals <- sapply(wide[desired_scenarios], quartile_skew)
  
  range_row <- cbind(data.frame(Variable_type = variable_label, Region = "Range"), as.list(rng_vals))
  skew_row  <- cbind(data.frame(Variable_type = variable_label, Region = "Quartile skewness"), as.list(qsk_vals))
  
  core <- wide %>%
    mutate(Variable_type = variable_label) %>%
    select(Variable_type, Region, all_of(desired_scenarios))
  
  out <- dplyr::bind_rows(core, range_row, skew_row)
  # enforce numeric for scenario columns
  for (nm in desired_scenarios) out[[nm]] <- suppressWarnings(as.numeric(out[[nm]]))
  out
}

# Build block for Vi (regions + Total only)
build_block_with_total <- function(data, value_col, variable_label) {
  wide <- data %>%
    transmute(
      Region = region,
      Scenario = Scenario,
      value = suppressWarnings(as.numeric(.data[[value_col]]))
    ) %>%
    tidyr::pivot_wider(names_from = Scenario, values_from = value, values_fill = NA_real_) %>%
    arrange(Region) %>%
    as.data.frame() %>%
    ensure_scenario_cols()
  
  totals <- sapply(wide[desired_scenarios], function(x) sum(x, na.rm = TRUE))
  total_row <- cbind(data.frame(Variable_type = variable_label, Region = "Total"), as.list(totals))
  
  core <- wide %>%
    mutate(Variable_type = variable_label) %>%
    select(Variable_type, Region, all_of(desired_scenarios))
  
  out <- dplyr::bind_rows(core, total_row)
  for (nm in desired_scenarios) out[[nm]] <- suppressWarnings(as.numeric(out[[nm]]))
  out
}

# Build the three requested blocks with exact labels
block_Di <- build_block_with_range_skew(df, "Di", "Proportional demographic dependence (Di)")
block_Vi <- build_block_with_total(df, "Vi", "Economic value generated within region (Vi)")
block_Yi <- build_block_with_range_skew(df, "Yi", "Spatial subsidy provided (-) or received (+) by region (Yi)")
block_YLi <- build_block_with_range_skew(df, "YLi", "Local flow within region (YLi)")

# Combine
final_df <- dplyr::bind_rows(block_Di, block_Vi, block_Yi,block_YLi) %>%
  select(Variable_type, Region, all_of(desired_scenarios))
write.csv(final_df, "region_results.csv")

## not needed?  See flows_long_df above.  Code below will create and export gross flows ####
flow_matrix <- outer(Di, Vi, `*`)
dimnames(flow_matrix) <- list(from = paste("fr",regions,sep="_"), to = paste("to",regions,sep="_"))

write.csv(flow_matrix, "flow_matrix.csv")

files_cr_raw <- list.files(pattern = "^bagstad_raw_.*\\.csv$")
table_bagstad <- do.call(rbind, lapply(files_cr_raw, read.csv))
write.csv(table_bagstad, "Formula_CR_&_POP.csv", row.names = FALSE)