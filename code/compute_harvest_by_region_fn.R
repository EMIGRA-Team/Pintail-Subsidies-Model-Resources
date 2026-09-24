# v4
# packages
library(readr)
library(dplyr)
library(tidyr)

compute_harvest_by_region_fn <- function(path = "Formula_CR_&_POP.csv") {
  region_order <- c("Alaska breeding", "Southern breeding", "Northern breeding",
                    "Western wintering", "Central wintering")
  df <- readr::read_csv(path, show_col_types = FALSE)
  long <- df |>
    dplyr::filter(Season == 2) |>
    dplyr::group_by(Scenario, Region) |>
    dplyr::summarise(Total_harvest = sum(Harvested_Individuals_Region, na.rm = TRUE), .groups = "drop")
  wide <- tidyr::pivot_wider(long, names_from = Scenario, values_from = Total_harvest, values_fill = 0)
  scenario_cols <- setdiff(names(wide), "Region")
  wide <- wide[rowSums(as.data.frame(wide[scenario_cols])) > 0, , drop = FALSE]
  wide <- wide |>
    dplyr::mutate(Region = factor(Region, levels = region_order)) |>
    dplyr::arrange(Region)
  return(wide)
}
#end of code
harvest_by_region_df <- compute_harvest_by_region_fn()
print(harvest_by_region_df)
write.csv(harvest_by_region_df, 'harvest_by_region_df.csv')