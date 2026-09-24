# v9
combine_scenario_csvs_fn <- function(root = ".") {
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Please install 'data.table'")
  library(data.table)
  
  files <- list.files(root, pattern = utils::glob2rx("season_vars_df_*.csv"),
                      recursive = TRUE, full.names = TRUE)
  if (!length(files)) return(data.frame())
  
  parse_scenario <- function(path, base) {
    segs <- strsplit(normalizePath(path, winslash = "/"), "/", fixed = TRUE)[[1]]
    hit <- grep("^[0-9]+_[^/]+$", segs, value = TRUE)
    if (length(hit)) return(tail(hit, 1))
    m <- regexec("^season_vars_df_([0-9]+)_([^_]+)_\\.csv$", base)
    parts <- regmatches(base, m)[[1]]
    if (length(parts)) return(parts[3])
    "unknown"
  }
  
  dts <- lapply(files, function(f) {
    dt <- fread(f)
    base <- basename(f)
    scen <- parse_scenario(f, base)
    ncolname <- if ("name" %in% names(dt)) "name" else names(dt)[1]
    vcolname <- if ("value" %in% names(dt)) "value" else setdiff(names(dt), ncolname)[1]
    x <- sub("_$", "", dt[[ncolname]])
    m <- regexec("^(.+)_season_([1-3])$", x)
    ss <- regmatches(x, m)
    region <- vapply(ss, function(z) if (length(z)) z[2] else NA_character_, character(1))
    season <- as.integer(vapply(ss, function(z) if (length(z)) z[3] else NA_character_, character(1)))
    data.table(region = region, season = season, scenario = scen, value = dt[[vcolname]])
  })
  
  long <- rbindlist(dts, use.names = TRUE, fill = TRUE)[!is.na(region) & !is.na(season)]
  
  # AK, PR, NU -> season 1; others -> season 2
  keep_season <- ifelse(long$region %in% c("AK","PR","NU"), 1L, 2L)
  long <- long[season == keep_season, .(region, scenario, value)]
  
  wide <- dcast(long, region ~ scenario, value.var = "value", fun.aggregate = sum)
  
  ord <- c("AK","PR","NU","CA","GC")
  wide[, region := factor(region, levels = ord)]
  wide <- wide[order(region)]
  wide[, region := as.character(region)]
  
  as.data.frame(wide)
}
#end of code
abundance_by_scn_season_region_df <- combine_scenario_csvs_fn()
print(abundance_by_scn_season_region_df)
write.csv(abundance_by_scn_season_region_df, 'abundance_by_scn_season_region_df.csv')