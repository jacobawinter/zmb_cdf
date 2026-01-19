
library(tidyverse)
library(sf)

sf <- read_sf("data/cdf_projects_loans.geojson")
df <- read_csv("data/cdf_projects_loans.csv")

d2 <- sf |>  left_join(df, by="constit_name_id") |> 
  mutate(across(c(cdf_release, cdf_disbursed, cdf_expenditure,
                  loans_issued, loans_recovered, projects_release, projects_expenditure,
                  loans_issued, loans_due, loans_recovered
                  ), ~ .x/1E6),) |> 
  rename(cdf_burn_rate = overall_burn_rate) |> 
  dplyr::select(c(ecz, year, cdf_release, cdf_disbursed, cdf_expenditure, cdf_burn_rate,
         projects_release, projects_expenditure, project_burn_rate,
         loans_issued, loans_due, loans_recovered, repay_rate)) |> 
  st_drop_geometry() |>
  write_csv("data/cdf_data_clean.csv")
  
  
  
  