source("mic_data_load.R")

country_data = gas_data %>%
  group_by(alpha_3_code) %>%
  mutate(median_year = floor(median(isolate_yr, na.rm = TRUE)),
         median_grp = as.factor(if_else(isolate_yr <= median_year, 1, 2)),
         mean_year = floor(mean(isolate_yr, na.rm = TRUE)),
         mean_grp = as.factor(if_else(isolate_yr <= mean_year, 1, 2))) %>%
  ungroup() %>%
  uncount(count) %>%
  # mutate(log_mic_90nos = log2(mic90nos)) %>%
  group_by(alpha_3_code, mean_grp) %>%                               # mean_grp, mean year
  summarise(
            # mean_mic = mean(log_mic_90nos, na.rm = TRUE),
            # sd_mic = sd(log_mic_90nos, na.rm = TRUE),
            n = n(),
            n_studies = length(unique(record_id)),
            # mean_lo = mean_mic - qt(0.975, n - 1)*(sd_mic/sqrt(n)),
            # mean_hi = mean_mic + qt(0.975, n - 1)*(sd_mic/sqrt(n)),
            mean_mic_ori = mean(mic90nos, na.rm = TRUE),
            sd_mic_ori = sd(mic90nos, na.rm = TRUE),
            mean_lo_ori = mean_mic_ori - qt(0.975, n - 1)*(sd_mic_ori/sqrt(n)),
            mean_hi_ori = mean_mic_ori + qt(0.975, n - 1)*(sd_mic_ori/sqrt(n))) %>%
  mutate(
        # change_mean = mean_mic - lag(mean_mic),
        #  change_se = sqrt(((lag(sd_mic)^2)/lag(n)) + ((sd_mic^2)/n)),
        #  ci_change_lo = change_mean - qt(0.975, n + lag(n) - 2)*change_se,
        #  ci_change_hi = change_mean + qt(0.975, n + lag(n) - 2)*change_se,
         change_mean_ori = mean_mic_ori - lag(mean_mic_ori),
         change_se_ori = sqrt(((lag(sd_mic_ori)^2)/lag(n)) + ((sd_mic_ori^2)/n)),
         ci_change_lo_ori = change_mean_ori - qt(0.975, n + lag(n) - 2)*change_se_ori,
         ci_change_hi_ori = change_mean_ori + qt(0.975, n + lag(n) - 2)*change_se_ori) %>%
  ungroup() %>%
  mutate(alpha_3_code = as.factor(alpha_3_code)) %>%
  left_join(gas_data %>%
              group_by(alpha_3_code) %>%
              mutate(median_year = floor(median(isolate_yr, na.rm = TRUE)),
                     median_grp = as.factor(if_else(isolate_yr <= median_year, 1, 2)),
                     mean_year = floor(mean(isolate_yr, na.rm = TRUE)),
                     mean_grp = as.factor(if_else(isolate_yr <= mean_year, 1, 2))) %>%
              ungroup() %>%
              select(alpha_3_code, mean_year) %>%
              group_by(alpha_3_code) %>%
              slice(1) %>%
              ungroup(),
            by = "alpha_3_code")

global_data = gas_data %>%
  mutate(median_year = floor(median(isolate_yr, na.rm = TRUE)),
         median_grp = as.factor(if_else(isolate_yr <= median_year, 1, 2)),
         mean_year = floor(mean(isolate_yr, na.rm = TRUE)),
         mean_grp = as.factor(if_else(isolate_yr <= mean_year, 1, 2))) %>%
  uncount(count) %>%
  # mutate(log_mic_90nos = log2(mic90nos)) %>%
  group_by(mean_grp) %>%                                                        # mean_grp
  summarise(
            # mean_mic = mean(log_mic_90nos, na.rm = TRUE),
            # sd_mic = sd(log_mic_90nos, na.rm = TRUE),
            n = n(),
            n_studies = length(unique(record_id)),
            # mean_lo = mean_mic - qt(0.975, n - 1)*(sd_mic/sqrt(n)),
            # mean_hi = mean_mic + qt(0.975, n - 1)*(sd_mic/sqrt(n)),
            mean_mic_ori = mean(mic90nos, na.rm = TRUE),
            sd_mic_ori = sd(mic90nos, na.rm = TRUE),
            mean_lo_ori = mean_mic_ori - qt(0.975, n - 1)*(sd_mic_ori/sqrt(n)),
            mean_hi_ori = mean_mic_ori + qt(0.975, n - 1)*(sd_mic_ori/sqrt(n))) %>%
  mutate(
        # change_mean = mean_mic - lag(mean_mic),
        #  change_se = sqrt(((lag(sd_mic)^2)/lag(n)) + ((sd_mic^2)/n)),
        #  ci_change_lo = change_mean - qt(0.975, n + lag(n) - 2)*change_se,
        #  ci_change_hi = change_mean + qt(0.975, n + lag(n) - 2)*change_se,
         change_mean_ori = mean_mic_ori - lag(mean_mic_ori),
         change_se_ori = sqrt(((lag(sd_mic_ori)^2)/lag(n)) + ((sd_mic_ori^2)/n)),
         ci_change_lo_ori = change_mean_ori - qt(0.975, n + lag(n) - 2)*change_se_ori,
         ci_change_hi_ori = change_mean_ori + qt(0.975, n + lag(n) - 2)*change_se_ori) %>%
  ungroup() %>%
  mutate(alpha_3_code = as.factor("GLOBAL"))

forest_data = bind_rows(country_data, global_data)%>%
  mutate(alpha_3_code = if_else(is.na(alpha_3_code),
                                "UNCLASSIFIED", alpha_3_code)) %>%
  left_join(who_regions, by = c("alpha_3_code" = "code")) %>%
  mutate(alpha_3_code = factor(
    alpha_3_code, levels = c("SEN", "ZAF", "ARG", "BRA", "CAN", "MEX", "USA",
                             "EGY", "MAR", "AUT", "BEL", "CHE", "DEU", "DNK",
                             "ESP", "FIN", "FRA", "GBR", "GRC", "HRV", "HUN",
                             "ISL", "ISR", "ITA", "NLD", "NOR", "POL", "PRT",
                             "SRB", "SVK", "SVN", "SWE", "TUR", "IDN", "IND", "AUS",
                             "CHN", "JPN", "KOR", "SGP", "HKG", "TWN",
                             "UNCLASSIFIED", "GLOBAL")),
    entity = factor(entity, levels = c("Senegal", "South Africa",
                                       "Argentina", "Brazil", "Canada", "Mexico", "United States",
                                       "Egypt", "Morocco",
                                       "Austria", "Belgium", "Denmark",
                                       "Finland", "France", "Germany", "Greece", "Croatia", "Hungary",
                                       "Iceland", "Israel", "Italy", "Netherlands", "Norway", "Poland", "Portugal",
                                       "Serbia", "Slovakia", "Slovenia","Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom",
                                       "Indonesia", "India", "Australia",
                                       "China", "Japan", "South Korea", "Singapore",
                                       "Hong Kong", "Taiwan", "UNCLASSIFIED", "GLOBAL")),
    who_region = if_else(is.na(who_region), "Unclassifed", who_region))

forest_data[which(forest_data$alpha_3_code == "HKG"), "entity"] = "Hong Kong"
forest_data[which(forest_data$alpha_3_code == "UNCLASSIFIED"), "entity"] = "UNCLASSIFIED"
forest_data[which(forest_data$alpha_3_code == "GLOBAL"), "entity"] = "GLOBAL"
forest_data[which(forest_data$alpha_3_code == "TWN"), "entity"] = "Taiwan"

paired_countries = (forest_data %>%
                      count(alpha_3_code) %>%
                      filter(n == 2))$alpha_3_code

forest_data_pairs = forest_data %>%
  filter(alpha_3_code %in% paired_countries)

write_csv(forest_data_pairs, "data/output/forest_data_granular_logged.csv")

plot_points = ggplot(forest_data_pairs %>% filter(alpha_3_code != "GLOBAL"),
       aes(x = log2(mean_mic_ori),
           y = fct_rev(entity),
           group = mean_grp,                                                    # mean_grp
           colour = mean_grp)) +                                                # mean_grp
  geom_line(aes(group = entity), colour = "gray", linewidth = 1.1, alpha = 0.65,
            arrow = arrow(length = unit(0.45, "cm"), type = "closed"),
            data = forest_data_pairs %>%
              filter(alpha_3_code != "GLOBAL",
                     change_mean_ori > 0 | is.na(change_mean_ori))) +
  geom_line(aes(group = entity), colour = "gray", linewidth = 1.1, alpha = 0.65,
            arrow = arrow(length = unit(0.45, "cm"), type = "closed", ends = "first"),
            data = forest_data_pairs %>%
              filter(alpha_3_code != "GLOBAL",
                     change_mean_ori < 0 | is.na(change_mean_ori))) +
  # geom_line(aes(group = entity), colour = "blue", linewidth = 1.85, alpha = 0.25,
  #           data = forest_data_pairs %>%
  #             filter(change_mean<0 | mean_grp==1, alpha_3_code != "GLOBAL"))+   # mean_grp
  # geom_line(aes(group = entity), colour = "red", linewidth = 1.85, alpha = 0.25,
  #           data = forest_data_pairs %>%
  #             filter(change_mean>0 | mean_grp==1, alpha_3_code != "GLOBAL"))+   # mean_grp
  geom_vline(data = global_data %>% filter(mean_grp == 1),                      # mean_grp
             mapping = aes(xintercept = log2(mean_mic_ori)),
             colour = "#6EB7B4", linewidth = 1) +
  geom_vline(data = global_data %>% filter(mean_grp == 2),                      # mean_grp
             mapping = aes(xintercept =  log2(mean_mic_ori)),
             colour = "#88669C", linewidth = 1) +
  theme_minimal() +
  geom_point(data = forest_data_pairs %>% filter(alpha_3_code != "GLOBAL"),
             mapping = aes(size = n), alpha = 0.65, show.legend = TRUE) +
  scale_size_continuous(range = c(3,10),
                        breaks = c(1, 10, 100, 1000, 4000)) +
  geom_errorbar(mapping = aes(xmin = log2(mean_lo_ori), xmax = log2(mean_hi_ori)), width = 0.7,
                show.legend = FALSE, colour = "#174F4D", linewidth = 0.8,
                data = forest_data_pairs %>% filter(mean_grp == 1,              # mean_grp
                                             alpha_3_code != "GLOBAL"))+
  geom_errorbar(mapping = aes(xmin = log2(mean_lo_ori), xmax = log2(mean_hi_ori)), width = 0.7,
                show.legend = FALSE, colour = "#422D4E", linewidth = 0.8,
                data = forest_data_pairs %>% filter(mean_grp == 2,              # mean_grp
                                             alpha_3_code != "GLOBAL"))+
  scale_color_viridis_d(begin = 0.5, end = 0.05,
                        labels = c("Before and on mean year",
                                   "After mean year")) +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_markdown(lineheight = 1.1),
        legend.position = "top",
        axis.text.x = element_text(size = 10),
        axis.text.y = element_blank(),
        panel.grid.minor.x = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 10),
                              order = 1)) +
  scale_x_continuous(breaks = (c(0.004, 0.008, 0.016, 0.032, 0.064, 0.128) %>% log2()),
                     limits = c(-9.15,-2.75),
                     labels = c(0.004, 0.008, 0.016, 0.032, 0.064, 0.128)) +
  scale_y_discrete(expand = c(0,2))+
  geom_text(mapping = aes(label = str_c(n, " (", n_studies, ")"), x = -9),
            data = forest_data_pairs %>%
              filter(mean_grp == 1, alpha_3_code != "GLOBAL"),                  # mean_grp
            hjust = 1, show.legend = FALSE) +
  geom_text(mapping = aes(label = str_c(n, " (", n_studies, ")"), x = -8.25),
            data = forest_data_pairs %>%
              filter(mean_grp == 2, alpha_3_code != "GLOBAL"),                  # mean_grp
            hjust = 1, show.legend = FALSE) +
  geom_text(mapping = aes(label = "Number of isolates (studies)", x = -8.8, y = 31),
            show.legend = FALSE, colour = "#5D5D5D", size = 3.5) +
  annotate("text", label = "Global before and \non mean year (1999)",                  # mean year
           x = -6, y = 31.4, colour = "#6EB7B4", size = 3) +
  annotate("text", label = "Global after \nmean year (1999)",                          # mean year
           x = -5.05, y = 31.4, colour = "#88669C", size = 3) +
  labs(x = "Mean MIC (log2 scale)",
       y = "",
       size = "Number of Samples",
       colour = "")

plot_regions = ggplot(forest_data_pairs %>% filter(alpha_3_code != "GLOBAL"),
       aes(y = fct_rev(entity)))+
  theme_classic() +
  scale_y_discrete(position = "right", expand = c(0,2))+
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_markdown(lineheight = 1.1),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12))+
  geom_text(aes(label = who_region, x = 1), colour = "white")+
  geom_text(aes(label = who_region, x = 1),
            data = forest_data_pairs %>%
              group_by(who_region) %>%
              slice(1) %>%
              ungroup() %>%
              filter(alpha_3_code != "GLOBAL"),
            hjust = 1, colour = "#5D5D5D", fontface  = "bold")+
  labs(x = "", y = "") +
  coord_cartesian(xlim = c(0,1))

plot_diffs = ggplot(forest_data_pairs %>% filter(alpha_3_code != "GLOBAL"),
       aes(y = fct_rev(entity)))+
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_markdown(lineheight = 1.1),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank())+
  scale_y_discrete(expand = c(0,2))+
  annotate("text", label = "Difference in \nmean MIC", x = 1, y = 31.25, colour = "#5D5D5D") +
  annotate("text", label = "CI of \ndifference", x = 5, y = 31.25, colour = "#5D5D5D") +
  annotate("text", label = "Mean\nyear", x = 8.2, y = 31.25, colour = "#5D5D5D") +  # mean year
  geom_text(aes(label = round(change_mean_ori,3), x = 1), colour = "#5D5D5D")+
  geom_text(aes(label = str_c("[", round(ci_change_lo_ori, 3), ", ", round(ci_change_hi_ori, 3), "]"), x = 6.5),
            hjust = 1, colour = "#5D5D5D")+
  geom_text(aes(label = mean_year, x = 8.75),                                      # mean year
            hjust = 1, colour = "#5D5D5D")+
  labs(x = "", y = "") +
  coord_cartesian(xlim = c(-1,9))

layout <- c(
  patchwork::area(t = 1, l = 1, b = 30, r = 2), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
  patchwork::area(t = 0, l = 3, b = 30, r = 16), # middle plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
  patchwork::area(t = 0, l = 16, b = 30, r = 20) # right most plot starts at top of page, begins where middle plot ends (l=9, and middle plot is r=9), goes to bottom of page (b=30), and extends two units wide (r=11)
)

plot_regions + plot_points + plot_diffs + patchwork::plot_layout(design = layout)

ggsave("forest_plot_granular_logged.tif", path = "figs/", width = 15, height = 9)

#===============================================================================
country_mic_agg_log = gas_mic_agg %>%
  group_by(alpha_3_code) %>%
  mutate(median_year = floor(median(isolate_yr, na.rm = TRUE)),
         median_grp = as.factor(if_else(isolate_yr <= median_year, 1, 2)),
         mean_year = floor(mean(isolate_yr, na.rm = TRUE)),
         mean_grp = as.factor(if_else(isolate_yr <= mean_year, 1, 2))) %>%
  ungroup() %>%
  uncount(count) %>%
  # mutate(log_mic_90_agg = log2(mic_90_agg)) %>%
  group_by(alpha_3_code, mean_grp) %>%                               # mean_grp, mean year
  summarise(
            # mean_mic = mean(log_mic_90_agg, na.rm = TRUE),
            # sd_mic = sd(log_mic_90_agg, na.rm = TRUE),
            n = n(),
            n_studies = length(unique(record_id)),
            # mean_lo = mean_mic - qt(0.975, n - 1)*(sd_mic/sqrt(n)),
            # mean_hi = mean_mic + qt(0.975, n - 1)*(sd_mic/sqrt(n)),
            mean_mic_ori = mean(mic_90_agg, na.rm = TRUE),
            sd_mic_ori = sd(mic_90_agg, na.rm = TRUE),
            mean_lo_ori = mean_mic_ori - qt(0.975, n - 1)*(sd_mic_ori/sqrt(n)),
            mean_hi_ori = mean_mic_ori + qt(0.975, n - 1)*(sd_mic_ori/sqrt(n))) %>%
  mutate(
        # change_mean = mean_mic - lag(mean_mic),
        #  change_se = sqrt(((lag(sd_mic)^2)/lag(n)) + ((sd_mic^2)/n)),
        #  ci_change_lo = change_mean - qt(0.975, n + lag(n) - 2)*change_se,
        #  ci_change_hi = change_mean + qt(0.975, n + lag(n) - 2)*change_se,
         change_mean_ori = mean_mic_ori - lag(mean_mic_ori),
         change_se_ori = sqrt(((lag(sd_mic_ori)^2)/lag(n)) + ((sd_mic_ori^2)/n)),
         ci_change_lo_ori = change_mean_ori - qt(0.975, n + lag(n) - 2)*change_se_ori,
         ci_change_hi_ori = change_mean_ori + qt(0.975, n + lag(n) - 2)*change_se_ori) %>%
  ungroup() %>%
  mutate(alpha_3_code = as.factor(alpha_3_code)) %>%
  left_join(gas_mic_agg %>%
              group_by(alpha_3_code) %>%
              mutate(median_year = floor(median(isolate_yr, na.rm = TRUE)),
                     median_grp = as.factor(if_else(isolate_yr <= median_year, 1, 2)),
                     mean_year = floor(mean(isolate_yr, na.rm = TRUE)),
                     mean_grp = as.factor(if_else(isolate_yr <= mean_year, 1, 2))) %>%
              ungroup() %>%
              select(alpha_3_code, mean_year) %>%
              group_by(alpha_3_code) %>%
              slice(1) %>%
              ungroup(),
            by = "alpha_3_code")

global_mic_agg_log = gas_mic_agg %>%
  mutate(median_year = floor(median(isolate_yr, na.rm = TRUE)),
         median_grp = as.factor(if_else(isolate_yr <= median_year, 1, 2)),
         mean_year = floor(mean(isolate_yr, na.rm = TRUE)),
         mean_grp = as.factor(if_else(isolate_yr <= mean_year, 1, 2))) %>%
  uncount(count) %>%
  mutate(log_mic_90_agg = log2(mic_90_agg)) %>%
  group_by(mean_grp) %>%                                                        # mean_grp
  summarise(
            # mean_mic = mean(log_mic_90_agg, na.rm = TRUE),
            # sd_mic = sd(log_mic_90_agg, na.rm = TRUE),
            n = n(),
            n_studies = length(unique(record_id)),
            # mean_lo = mean_mic - qt(0.975, n - 1)*(sd_mic/sqrt(n)),
            # mean_hi = mean_mic + qt(0.975, n - 1)*(sd_mic/sqrt(n)),
            mean_mic_ori = mean(mic_90_agg, na.rm = TRUE),
            sd_mic_ori = sd(mic_90_agg, na.rm = TRUE),
            mean_lo_ori = mean_mic_ori - qt(0.975, n - 1)*(sd_mic_ori/sqrt(n)),
            mean_hi_ori = mean_mic_ori + qt(0.975, n - 1)*(sd_mic_ori/sqrt(n))) %>%
  mutate(
        # change_mean = mean_mic - lag(mean_mic),
        #  change_se = sqrt(((lag(sd_mic)^2)/lag(n)) + ((sd_mic^2)/n)),
        #  ci_change_lo = change_mean - qt(0.975, n + lag(n) - 2)*change_se,
        #  ci_change_hi = change_mean + qt(0.975, n + lag(n) - 2)*change_se,
         change_mean_ori = mean_mic_ori - lag(mean_mic_ori),
         change_se_ori = sqrt(((lag(sd_mic_ori)^2)/lag(n)) + ((sd_mic_ori^2)/n)),
         ci_change_lo_ori = change_mean_ori - qt(0.975, n + lag(n) - 2)*change_se_ori,
         ci_change_hi_ori = change_mean_ori + qt(0.975, n + lag(n) - 2)*change_se_ori) %>%
  ungroup() %>%
  mutate(alpha_3_code = as.factor("GLOBAL"))

forest_mic_agg_log = bind_rows(country_mic_agg_log, global_mic_agg_log)%>%
  mutate(alpha_3_code = if_else(is.na(alpha_3_code),
                                "UNCLASSIFIED", alpha_3_code)) %>%
  left_join(who_regions, by = c("alpha_3_code" = "code")) %>%
  mutate(alpha_3_code = factor(
    alpha_3_code, levels = c("SEN", "ZAF", "ARG", "BRA", "CAN", "MEX", "USA",
                             "EGY", "MAR", "AUT", "BEL", "CHE", "DEU", "DNK",
                             "ESP", "FIN", "FRA", "GBR", "GRC", "HRV", "HUN",
                             "ISL", "ISR", "ITA", "NLD", "NOR", "POL", "PRT",
                             "SRB", "SVK", "SVN", "SWE", "TUR", "IDN", "IND", "AUS",
                             "CHN", "JPN", "KOR", "SGP", "HKG", "TWN",
                             "UNCLASSIFIED", "GLOBAL")),
    entity = factor(entity, levels = c("Senegal", "South Africa",
                                       "Argentina", "Brazil", "Canada", "Mexico", "United States",
                                       "Egypt", "Morocco",
                                       "Austria", "Belgium", "Denmark",
                                       "Finland", "France", "Germany", "Greece", "Croatia", "Hungary",
                                       "Iceland", "Israel", "Italy", "Netherlands", "Norway", "Poland", "Portugal",
                                       "Serbia", "Slovakia", "Slovenia","Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom",
                                       "Indonesia", "India", "Australia",
                                       "China", "Japan", "South Korea", "Singapore",
                                       "Hong Kong", "Taiwan", "UNCLASSIFIED", "GLOBAL")),
    who_region = if_else(is.na(who_region), "Unclassifed", who_region))

forest_mic_agg_log[which(forest_mic_agg_log$alpha_3_code == "HKG"), "entity"] = "Hong Kong"
forest_mic_agg_log[which(forest_mic_agg_log$alpha_3_code == "UNCLASSIFIED"), "entity"] = "UNCLASSIFIED"
forest_mic_agg_log[which(forest_mic_agg_log$alpha_3_code == "GLOBAL"), "entity"] = "GLOBAL"
forest_mic_agg_log[which(forest_mic_agg_log$alpha_3_code == "TWN"), "entity"] = "Taiwan"

paired_countries_mic_agg_log = (forest_mic_agg_log %>%
                                  count(alpha_3_code) %>%
                                  filter(n == 2))$alpha_3_code

forest_mic_agg_pairs_log = forest_mic_agg_log %>%
  filter(alpha_3_code %in% paired_countries_mic_agg_log)

write_csv(forest_mic_agg_pairs_log, "data/output/forest_data_aggregated_logged.csv")

plot_points_agg_log = ggplot(forest_mic_agg_pairs_log %>% filter(alpha_3_code != "GLOBAL"),
                             aes(x = log2(mean_mic_ori),
                                 y = fct_rev(entity),
                                 group = mean_grp,                                                    # mean_grp
                                 colour = mean_grp)) +                                                # mean_grp
  geom_line(aes(group = entity), colour = "gray", linewidth = 1.1, alpha = 0.65,
            arrow = arrow(length = unit(0.45, "cm"), type = "closed"),
            data = forest_mic_agg_pairs_log %>%
              filter(alpha_3_code != "GLOBAL",
                     change_mean_ori > 0 | is.na(change_mean_ori))) +
  geom_line(aes(group = entity), colour = "gray", linewidth = 1.1, alpha = 0.65,
            arrow = arrow(length = unit(0.45, "cm"), type = "closed", ends = "first"),
            data = forest_mic_agg_pairs_log %>%
              filter(alpha_3_code != "GLOBAL",
                     change_mean_ori < 0 | is.na(change_mean_ori))) +
  # geom_line(aes(group = entity), colour = "blue", linewidth = 1.85, alpha = 0.25,
  #           data = forest_mic_agg_pairs_log %>%
  #             filter(change_mean < 0 | mean_grp == 1, alpha_3_code != "GLOBAL"))+   # mean_grp
  # geom_line(aes(group = entity), colour = "red", linewidth = 1.85, alpha = 0.25,
  #           data = forest_mic_agg_pairs_log %>%
  #             filter(change_mean > 0 | mean_grp == 1, alpha_3_code != "GLOBAL"))+   # mean_grp
  geom_vline(data = global_mic_agg_log %>% filter(mean_grp == 1),                      # mean_grp
             mapping = aes(xintercept =  log2(mean_mic_ori)),
             colour = "#6EB7B4", linewidth = 1) +
  geom_vline(data = global_mic_agg_log %>% filter(mean_grp == 2),                      # mean_grp
             mapping = aes(xintercept =  log2(mean_mic_ori)),
             colour = "#88669C", linewidth = 1) +
  theme_minimal() +
  geom_point(data = forest_mic_agg_pairs_log %>% filter(alpha_3_code != "GLOBAL"),
             mapping = aes(size = n), alpha = 0.65, show.legend = TRUE) +
  scale_size_continuous(range = c(3,10),
                        breaks = c(1, 10, 100, 1000, 4000)) +
  geom_errorbar(mapping = aes(xmin = log2(mean_lo_ori), xmax = log2(mean_hi_ori)), width = 0.7,
                show.legend = FALSE, colour = "#174F4D", linewidth = 0.8,
                data = forest_mic_agg_pairs_log %>% filter(mean_grp == 1,              # mean_grp
                                                           alpha_3_code != "GLOBAL"))+
  geom_errorbar(mapping = aes(xmin = log2(mean_lo_ori), xmax = log2(mean_hi_ori)), width = 0.7,
                show.legend = FALSE, colour = "#422D4E", linewidth = 0.8,
                data = forest_mic_agg_pairs_log %>% filter(mean_grp == 2,              # mean_grp
                                                           alpha_3_code != "GLOBAL"))+
  scale_color_viridis_d(begin = 0.5, end = 0.05,
                        labels = c("Before and on mean year",
                                   "After mean year")) +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_markdown(lineheight = 1.1),
        legend.position = "top",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10),
        panel.grid.minor.x = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 10),
                              order = 1)) +
  scale_x_continuous(breaks = (c(0.004, 0.008, 0.016, 0.032, 0.064, 0.128) %>% log2()),
                     limits = c(-9.15,-2.75),
                     labels = c(0.004, 0.008, 0.016, 0.032, 0.064, 0.128)) +
  scale_y_discrete(expand = c(0,2))+
  geom_text(mapping = aes(label = str_c(n, " (", n_studies, ")"), x = -9),
            data = forest_mic_agg_pairs_log %>%
              filter(mean_grp == 1, alpha_3_code != "GLOBAL"),                  # mean_grp
            hjust = 1, show.legend = FALSE) +
  geom_text(mapping = aes(label = str_c(n, " (", n_studies, ")"), x = -8.25),
            data = forest_mic_agg_pairs_log %>%
              filter(mean_grp == 2, alpha_3_code != "GLOBAL"),                  # mean_grp
            hjust = 1, show.legend = FALSE) +
  geom_text(mapping = aes(label = "Number of isolates (studies)", x = -8.8, y= 31),
            show.legend = FALSE, colour = "#5D5D5D", size = 3.5) +
  annotate("text", label = "Global before and \non mean year (1999)",                  # mean year
           x = -6, y = 31.4, colour = "#6EB7B4", size = 3) +
  annotate("text", label = "Global after \nmean year (1999)",                          # mean year
           x = -5.05, y = 31.4, colour = "#88669C", size = 3) +
  labs(x = "Mean MIC (log2 scale)",
       y = "",
       size = "Number of Samples",
       colour = "")

plot_regions_agg_log = ggplot(forest_mic_agg_pairs_log %>% filter(alpha_3_code != "GLOBAL"),
                              aes(y = fct_rev(entity)))+
  theme_classic() +
  scale_y_discrete(position = "right", expand = c(0,2))+
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_markdown(lineheight = 1.1),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12))+
  geom_text(aes(label = who_region, x = 1), colour = "white")+
  geom_text(aes(label = who_region, x = 1),
            data = forest_mic_agg_pairs_log %>%
              group_by(who_region) %>%
              slice(1) %>%
              ungroup() %>%
              filter(alpha_3_code != "GLOBAL"),
            hjust = 1, colour = "#5D5D5D", fontface  = "bold")+
  labs(x = "", y = "") +
  coord_cartesian(xlim = c(0,1))

plot_diffs_agg_log = ggplot(forest_mic_agg_pairs_log %>% filter(alpha_3_code != "GLOBAL"),
                            aes(y = fct_rev(entity)))+
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_markdown(lineheight = 1.1),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank())+
  scale_y_discrete(expand = c(0,2))+
  annotate("text", label = "Difference in \nmean MIC", x = 1, y = 31.25, colour = "#5D5D5D") +
  annotate("text", label = "CI of \ndifference", x = 5, y = 31.25, colour = "#5D5D5D") +
  annotate("text", label = "Mean\nyear", x = 8.2, y = 31.25, colour = "#5D5D5D") +  # mean year
  geom_text(aes(label = round(change_mean_ori,3), x = 1), colour = "#5D5D5D")+
  geom_text(aes(label = str_c("[", round(ci_change_lo_ori, 3), ", ", round(ci_change_hi_ori, 3), "]"), x = 6.5),
            hjust = 1, colour = "#5D5D5D")+
  geom_text(aes(label = mean_year, x = 8.75),                                      # mean year
            hjust = 1, colour = "#5D5D5D")+
  labs(x = "", y = "") +
  coord_cartesian(xlim = c(-1,9))

layout <- c(
  patchwork::area(t = 1, l = 1, b = 30, r = 2), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
  patchwork::area(t = 0, l = 3, b = 30, r = 16), # middle plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
  patchwork::area(t = 0, l = 16, b = 30, r = 20) # right most plot starts at top of page, begins where middle plot ends (l=9, and middle plot is r=9), goes to bottom of page (b=30), and extends two units wide (r=11)
)

plot_regions_agg_log + plot_points_agg_log + plot_diffs_agg_log + patchwork::plot_layout(design = layout)

ggsave("forest_plot_aggregated_logged.tif", path = "figs/", width = 15, height = 9)

