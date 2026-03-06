library(readxl)
library(tidyverse)
library(worlddatr)
library(janitor)
library(ggtext)
library(viridisLite)
library(cowplot)
library(scales)

#============================================================================================================================

who_regions = read_csv("data/who-regions.csv", show_col_types = FALSE, guess_max = Inf) %>%
  clean_names() %>%
  select(-year)

centroids = read_csv("data/centroids.csv", show_col_types = FALSE,
                     na = "empty")

#=============================================================================================================================

gas_data = read_csv("data/GAS_SR_Raw_50_90_NOS_20260305.csv", show_col_types = FALSE) %>%
  select(-redcap_repeat_instrument, -redcap_event_name) %>%
  mutate(mic90nos = if_else(is.na(micnos_1),
                            mic90_1, micnos_1))

gas_data[which(gas_data$country == "(unknown)"), "country"] = "Unknown"
gas_data[which(gas_data$country == "(not reported)"), "country"] = "Unknown"
gas_data[which(gas_data$country == "USA"), "country"] = "United States of America (the)"
gas_data[which(gas_data$country == "South Korea"), "country"] = "Korea (the Republic of)"
gas_data[which(gas_data$country == "Taiwan"), "country"] = "Taiwan (Province of China)"
gas_data[which(gas_data$country == "UK"), "country"] = "United Kingdom of Great Britain and Northern Ireland (the)"
gas_data[which(gas_data$country == "Worldwide"), "country"] = "Multiple"
gas_data[which(gas_data$country == "worldwide"), "country"] = "Multiple"
gas_data[which(gas_data$country == "United Kingdom"), "country"] = "United Kingdom of Great Britain and Northern Ireland (the)"
gas_data[which(gas_data$country == "Slovak Republic"), "country"] = "Slovakia"
gas_data[which(gas_data$country == "Turkey"), "country"] = "Turkiye"
gas_data[which(gas_data$country == "Netherlands"), "country"] = "Netherlands (Kingdom of the)"

gas_data = gas_data %>%
  left_join(world_income %>% select(alpha_3_code, country),
            by = c("country")) %>%
  mutate(country = str_to_title(country)) %>%
  relocate(alpha_3_code, .after = country)

gas_mic_90 = gas_data %>%
  filter(!is.na(mic90_1))

gas_mic_nos_agg = gas_data %>%
  filter(!is.na(micnos_1)) %>%
  uncount(count) %>%
  group_by(record_id, country, alpha_3_code, who, isolate_yr) %>%
  summarise(micnos_q90 = quantile(micnos_1, probs = 0.9),
            count = n()) %>%
  ungroup()

gas_mic_agg = bind_rows(gas_mic_90, gas_mic_nos_agg) %>%
  mutate(mic_90_agg = if_else(is.na(micnos_q90),
                              mic90_1, micnos_q90))

rm(gas_mic_90)
rm(gas_mic_nos_agg)

write.csv(gas_mic_agg, "data/gas_mic_aggregated.csv", row.names = FALSE)

#=============================================================================================================================

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

rm(geospat_pivot)

dd_sr = dd_sr %>%
  left_join(world_income, by = c("country" = "country"))

#==========================================================================================================================
