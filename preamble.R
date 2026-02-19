library(readxl)
library(tidyverse)
library(worlddatr)
library(janitor)
library(ggtext)
library(viridisLite)
library(cowplot)
library(scales)

centroids = read_csv("data/centroids.csv", show_col_types = FALSE,
                     na = "empty")

who_regions = read_csv("data/who-regions.csv", show_col_types = FALSE, guess_max = Inf) %>%
  clean_names()

gas_sr =  read_excel("data/GAS_SR_Raw_50_90_NOS_formatted_20251106.xlsx") %>%
  select(-redcap_repeat_instrument, -redcap_event_name) %>%
  mutate(mic90nos = mic90_1,
         year_grp = if_else(isolate_yr <= 2000, 1, 2))

gas_sr[which(is.na(gas_sr$mic90nos)), "mic90nos"] =
  gas_sr[which(is.na(gas_sr$mic90nos)), "micnos_1"]

# gas_sr[which(gas_sr$country == "Taiwan"), "country"] = "Taiwan (Province of China)"
# gas_sr[which(gas_sr$country == "United Kingdom"), "country"] = "United Kingdom of Great Britain and Northern Ireland"
# gas_sr[which(gas_sr$country == "UK"), "country"] = "United Kingdom of Great Britain and Northern Ireland"
# gas_sr[which(gas_sr$country == "Slovak Republic"), "country"] = "Slovakia"
# gas_sr[which(gas_sr$country == "USA"), "country"] = "United States of America"
# gas_sr[which(gas_sr$country == "South Korea"), "country"] = "S. Korea"
gas_sr[which(gas_sr$country == "(unknown)"), "country"] = "Unknown"
gas_sr[which(gas_sr$country == "(not reported)"), "country"] = "Unknown"

gas_sr[which(gas_sr$country == "USA"), "country"] = "United States of America (the)"
# gas_sr[which(gas_sr$country == "'North America'"), "country"] = "Multiple"
# gas_sr <- gas_sr[-which(gas_sr$country == "breakdown not reported"), ]
# gas_sr[which(gas_sr$country == "Columbia"), "country"] = "Colombia"
# gas_sr[which(gas_sr$country == "Czech Rep"), "country"] = "Czechia"
# gas_sr[which(gas_sr$country == "Ethopia"), "country"] = "Ethiopia"
# gas_sr[which(gas_sr$country == "Europe"), "country"] = "Multiple"
# gas_sr[which(gas_sr$country == "Europe and adjacent"), "country"] = "Multiple"
# gas_sr[which(str_to_title(gas_sr$country) == "Multiple"), "country"] = "Multiple"
# gas_sr[which(str_to_title(gas_sr$country) == "Not Reported"), "country"] = "Unknown"
# gas_sr[which(gas_sr$country == "Russia"), "country"] = "Russian Federation (the)"
gas_sr[which(gas_sr$country == "South Korea"), "country"] = "Korea (the Republic of)"
gas_sr[which(gas_sr$country == "Taiwan"), "country"] = "Taiwan (Province of China)"
# gas_sr[which(gas_sr$country == "Tanzania"), "country"] = "Tanzania, the United Republic of"
gas_sr[which(gas_sr$country == "UK"), "country"] = "United Kingdom of Great Britain and Northern Ireland (the)"
gas_sr[which(gas_sr$country == "Worldwide"), "country"] = "Multiple"
gas_sr[which(gas_sr$country == "worldwide"), "country"] = "Multiple"
# gas_sr[which(gas_sr$country == "Worldwide 25 countries"), "country"] = "Multiple"
gas_sr[which(gas_sr$country == "United Kingdom"), "country"] = "United Kingdom of Great Britain and Northern Ireland (the)"
gas_sr[which(gas_sr$country == "Slovak Republic"), "country"] = "Slovakia"
# gas_sr[which(gas_sr$country == "unknown"), "country"] = "Unknown"
gas_sr[which(gas_sr$country == "Turkey"), "country"] = "Turkiye"
# gas_sr[which(gas_sr$country == "Iran"), "country"] = "Iran (Islamic Republic of)"
gas_sr[which(gas_sr$country == "Netherlands"), "country"] = "Netherlands (Kingdom of the)"

gas_sr = gas_sr %>%
  left_join(world_income, by = c("country")) %>%
  mutate(country = str_to_title(country),
         year_grp = as.factor(year_grp)) %>%
  group_by(alpha_3_code) %>%
  mutate(median_year = median(isolate_yr, na.rm = TRUE),
         med_grp = if_else(isolate_yr <= median_year, 1, 2),
         midpoint_year = (max(isolate_yr, na.rm = TRUE) - min(isolate_yr, na.rm = TRUE))/2 + min(isolate_yr, na.rm = TRUE),
         midpoint_grp = if_else(isolate_yr <= midpoint_year, 1, 2),
         mean_year = floor(mean(isolate_yr, na.rm = TRUE)),
         mean_grp = as.factor(if_else(isolate_yr <= mean_year, 1, 2))) %>%
  ungroup()

ready <- read_csv("data/ALL_geosetting_22082025.csv", show_col_types = FALSE) %>%
  filter(redcap_event_name == "data_extraction_arm_2") %>%
  dplyr::select(ddwho1,ddcountry1,ddno1, redcap_repeat_instrument, record_id) %>%
  rename("who" = "ddwho1",
         "country" = "ddcountry1",
         "count" = "ddno1")

raw <- read_csv("data/ALL_geosetting_22082025.csv", show_col_types = FALSE) %>%
  filter(redcap_event_name == "mic_data_extractio_arm_1") %>%
  rename_with(~ gsub("mic50_", "", .x, fixed = TRUE)) %>%
  pivot_longer(cols = who_1:count_7,
               names_to = c(".value", "record"),
               names_sep = "_") %>%
  dplyr::select(who, country, count, redcap_repeat_instrument, record_id) %>%
  drop_na(count)

geospat_pivot = rbind(ready, raw)
rm(raw)
rm(ready)

dd_sr <- geospat_pivot %>%
  # filter(redcap_repeat_instrument == "sr_data_extraction") %>%
  separate_rows(country, sep = ",") %>%
  separate_rows(country, sep = "/") %>%
  mutate(
    country = str_trim(country),
    country = str_replace_all(country, "\\(", ""),
    country = str_replace_all(country, "\\)", ""))

dd_sr[which(dd_sr$country == "USA"), "country"] = "United States of America (the)"
dd_sr[which(dd_sr$country == "'North America'"), "country"] = "Multiple"
dd_sr <- dd_sr[-which(dd_sr$country == "breakdown not reported"), ]
dd_sr[which(dd_sr$country == "Columbia"), "country"] = "Colombia"
dd_sr[which(dd_sr$country == "Czech Rep"), "country"] = "Czechia"
dd_sr[which(dd_sr$country == "Ethopia"), "country"] = "Ethiopia"
dd_sr[which(dd_sr$country == "Europe"), "country"] = "Multiple"
dd_sr[which(dd_sr$country == "Europe and adjacent"), "country"] = "Multiple"
dd_sr[which(str_to_title(dd_sr$country) == "Multiple"), "country"] = "Multiple"
dd_sr[which(str_to_title(dd_sr$country) == "Not Reported"), "country"] = "Unknown"
dd_sr[which(dd_sr$country == "Russia"), "country"] = "Russian Federation (the)"
dd_sr[which(dd_sr$country == "South Korea"), "country"] = "Korea (the Republic of)"
dd_sr[which(dd_sr$country == "Taiwan"), "country"] = "Taiwan (Province of China)"
dd_sr[which(dd_sr$country == "Tanzania"), "country"] = "Tanzania, the United Republic of"
dd_sr[which(dd_sr$country == "UK"), "country"] = "United Kingdom of Great Britain and Northern Ireland (the)"
dd_sr[which(dd_sr$country == "Worldwide"), "country"] = "Multiple"
dd_sr[which(dd_sr$country == "worldwide"), "country"] = "Multiple"
dd_sr[which(dd_sr$country == "Worldwide 25 countries"), "country"] = "Multiple"
dd_sr[which(dd_sr$country == "United Kingdom"), "country"] = "United Kingdom of Great Britain and Northern Ireland (the)"
dd_sr[which(dd_sr$country == "Slovak Republic"), "country"] = "Slovakia"
dd_sr[which(dd_sr$country == "unknown"), "country"] = "Unknown"
dd_sr[which(dd_sr$country == "Turkey"), "country"] = "Turkiye"
dd_sr[which(dd_sr$country == "Iran"), "country"] = "Iran (Islamic Republic of)"
dd_sr[which(dd_sr$country == "Netherlands"), "country"] = "Netherlands (Kingdom of the)"

dd_sr = dd_sr %>%
  left_join(world_income, by = c("country" = "country"))

