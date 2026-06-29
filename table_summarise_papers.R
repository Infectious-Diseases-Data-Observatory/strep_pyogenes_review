library(tidyverse)
library(cowplot)
library(readxl)
library(viridisLite)

# To populate WHO regions
who_df <- matrix(c(c("1", "Africa"),
                   c("2","Americas"),
                   c("3", "Eastern Mediterranean"),
                   c("4","Europe"),
                   c("5", "Southeast Asia"),
                   c("6", "Western Pacific")),
                 ncol = 2, byrow = TRUE) %>%
  data.frame() %>%
  setNames(c("nume", "name"))

dat_raw <- read.csv("data/GAS_SR_Raw_50_90_NOS_20260305.csv") %>%
  mutate(who = as.factor(who)) %>%
  left_join(who_df, by = join_by(who == nume)) %>%
  mutate(who_name = ifelse(is.na(name), "Not assigned", name),
         who_name = factor(who_name,
                           levels = c(who_df$name, "Not assigned")))
# dat_nos_only <- dat_raw %>% filter(!is.na(micnos_1))

# derived set from RP
dat_agg <- read.csv("data/gas_mic_aggregated.csv") %>%
  mutate(who = as.factor(who)) %>%
  left_join(who_df, by = join_by(who == nume)) %>%
  mutate(who_name = ifelse(is.na(name), "Not assigned", name),
         who_name = factor(who_name,
                           levels = c(who_df$name, "Not assigned")))

# sorry Rhys I've used the shortened names
dat_raw %>%
  summarise(.by = c(author, country, record_id), isolates = sum(count),
            min_year = min(isolate_yr), max_year = max(isolate_yr)) %>%
  # probably want year published, journal, PubMedID
  mutate(`Sampled years` = ifelse(min_year == max_year,
                        min_year,
                        paste0(min_year, "-", max_year))) %>%
  select(-c(record_id, min_year, max_year)) %>%
  arrange(author, country)









