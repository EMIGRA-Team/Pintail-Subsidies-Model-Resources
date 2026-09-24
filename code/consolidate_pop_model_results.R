# v2
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

compute_dt_i_fn <- function(df) {
  df_p <- df %>%
    group_by(Scenario) %>%
    mutate(p_ti = POP_Population / sum(POP_Population, na.rm = TRUE)) %>%
    ungroup()
  
  contrib <- df_p %>%
    mutate(weighted_contrib = CR_Formula_Pure * p_ti) %>%
    group_by(Scenario, Season, Region, Sex_Name) %>%
    summarise(weighted_contrib = sum(weighted_contrib, na.rm = TRUE), .groups = "drop")
  
  numer <- contrib %>%
    group_by(Scenario, Season, Region) %>%
    summarise(numerator = sum(weighted_contrib, na.rm = TRUE), .groups = "drop")
  
  denom <- numer %>%
    group_by(Scenario, Season) %>%
    summarise(denominator = sum(numerator, na.rm = TRUE), .groups = "drop")
  
  dt <- numer %>%
    left_join(denom, by = c("Scenario", "Season")) %>%
    mutate(d_t_i = if_else(denominator > 0, numerator / denominator, NA_real_)) %>%
    arrange(Scenario, Season, Region)
  
  dt
}

compute_Di_fn <- function(df) {
  dt <- compute_dt_i_fn(df)
  
  Di <- dt %>%
    group_by(Scenario, Region) %>%
    summarise(D_i = mean(d_t_i, na.rm = TRUE), .groups = "drop")
  
  Ci_tbl <- df %>%
    filter(!is.na(CR_Formula_Pure), !is.na(POP_Population),
           CR_Formula_Pure != 0, POP_Population != 0) %>%
    group_by(Scenario, Region) %>%
    summarise(
      Ci_value = if (sum(POP_Population) > 0) {
        weighted.mean(CR_Formula_Pure, POP_Population)
      } else {
        NA_real_
      },
      .groups = "drop"
    )
  
  pi_tbl <- df %>%
    group_by(Scenario) %>%
    mutate(Scenario_POP_total = sum(POP_Population, na.rm = TRUE)) %>%
    group_by(Scenario, Region) %>%
    summarise(region_pop = sum(POP_Population, na.rm = TRUE),
              Scenario_POP_total = first(Scenario_POP_total),
              .groups = "drop") %>%
    mutate(pi_pool = if_else(Scenario_POP_total > 0, region_pop / Scenario_POP_total, NA_real_)) %>%
    select(Scenario, Region, pi_pool)
  
  out <- Di %>%
    left_join(Ci_tbl, by = c("Scenario", "Region")) %>%
    left_join(pi_tbl, by = c("Scenario", "Region")) %>%
    arrange(Scenario, Region)
  
  list(
    dt_i = dt,
    Di_summary = out
  )
}

# Example usage:
# Replace `data_tbl` with the name of your data frame containing the table.
 res <- compute_Di_fn(table_bagstad)
 dt_table <- res$dt_i
 Di_table <- res$Di_summary
# Validate Di sums to 1 within each Scenario:
 Di_table %>% group_by(Scenario) %>% summarise(sum_Di = sum(D_i, na.rm = TRUE))
write.csv(Di_table, 'pop_model_results.csv')