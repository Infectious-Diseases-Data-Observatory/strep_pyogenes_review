source("clean_gas_data.R")
source("preamble.R")

who_regions = read_csv("data/who-regions.csv", show_col_types = FALSE, guess_max = Inf) %>%
  clean_names()

gas_90 = read_csv("data/GAS_SR_Raw_50_90_NOS_20260228.csv", show_col_types = FALSE) %>%
  filter(!is.na(mic90_1)) %>%
  clean_gas_data()

gas_nos_nintied = read_csv("data/GAS_SR_Raw_50_90_NOS_20260228.csv", show_col_types = FALSE) %>%
  filter(!is.na(micnos_1)) %>%
  uncount(count) %>%
  group_by(record_id, country, who, isolate_yr) %>%
  summarise(micnos_q90 = quantile(micnos_1, probs = 0.9),
            count = n()) %>%
  ungroup()%>%
  clean_gas_data()

gas_nos_raw = read_csv("data/GAS_SR_Raw_50_90_NOS_20260228.csv", show_col_types = FALSE) %>%
  filter(!is.na(micnos_1))%>%
  clean_gas_data() %>%
  left_join(world_income, by = c("country")) %>%
  mutate(country = str_to_title(country)) %>%
  group_by(alpha_3_code) %>%
  mutate(median_year = median(isolate_yr, na.rm = TRUE),
         med_grp = if_else(isolate_yr <= median_year, 1, 2),
         midpoint_year = (max(isolate_yr, na.rm = TRUE) - min(isolate_yr, na.rm = TRUE))/2 + min(isolate_yr, na.rm = TRUE),
         midpoint_grp = if_else(isolate_yr <= midpoint_year, 1, 2),
         mean_year = floor(mean(isolate_yr, na.rm = TRUE)),
         mean_grp = as.factor(if_else(isolate_yr <= mean_year, 1, 2))) %>%
  ungroup()

write.csv(gas_90, "data/gas_90.csv", row.names = FALSE)
write.csv(gas_nos_nintied, "data/gas_nos_nintied.csv", row.names = FALSE)
write.csv(gas_nos_raw, "data/gas_nos_raw.csv", row.names = FALSE)

gas_90_combined = bind_rows(gas_90, gas_nos_nintied)%>%
  left_join(world_income, by = c("country")) %>%
  mutate(country = str_to_title(country)) %>%
  group_by(alpha_3_code) %>%
  mutate(median_year = median(isolate_yr, na.rm = TRUE),
         med_grp = if_else(isolate_yr <= median_year, 1, 2),
         midpoint_year = (max(isolate_yr, na.rm = TRUE) - min(isolate_yr, na.rm = TRUE))/2 + min(isolate_yr, na.rm = TRUE),
         midpoint_grp = if_else(isolate_yr <= midpoint_year, 1, 2),
         mean_year = floor(mean(isolate_yr, na.rm = TRUE)),
         mean_grp = as.factor(if_else(isolate_yr <= mean_year, 1, 2))) %>%
  ungroup()

gas_90_combined[which(is.na(gas_90_combined$mic90_1)),"mic90_1"] =
  gas_90_combined[which(is.na(gas_90_combined$mic90_1)),"micnos_q90"]

write.csv(gas_90_combined, "data/gas_90s combined.csv", row.names = FALSE)
#===============================================================================
# nos raw only
{
  gas_country = gas_nos_raw %>%
    uncount(count) %>%
    group_by(alpha_3_code, mean_grp) %>%
    summarise(mean_mic = mean(micnos_1, na.rm = TRUE),
              sd_mic = sd(micnos_1, na.rm = TRUE),
              n = n(),
              n_studies = length(unique(record_id)),
              mean_hi = mean_mic - qt(0.975, n - 1)*(sd_mic/sqrt(n)),
              mean_lo = mean_mic + qt(0.975, n - 1)*(sd_mic/sqrt(n))) %>%
    mutate(change = mean_mic - lag(mean_mic),
           se_change = sqrt(((lag(sd_mic)^2)/ lag(n)) + ((sd_mic^2)/ n)),
           ci_lo_change = change - qt(0.975, n + lag(n) - 1)*se_change,
           ci_hi_change = change + qt(0.975, n + lag(n) - 1)*se_change,
           percent_change = round((mean_mic - lag(mean_mic))/lag(mean_mic) * 100, 0)) %>%
    ungroup() %>%
    group_by(alpha_3_code) %>%
    mutate(grpid = cur_group_id(),
           facet_grp = if_else(grpid <= 22, "1", "2"),
           mean_grp = as.factor(mean_grp),
           country_n = sum(n),
           prop = n/country_n,
           alpha_3_code = as_factor(alpha_3_code)) %>%
    ungroup()

  gas_global = gas_nos_raw %>%
    uncount(count) %>%
    mutate(global_mean = mean(isolate_yr, na.rm = TRUE),
           mean_grp = as.factor(if_else(isolate_yr <= global_mean, 1, 2))) %>%
    group_by(mean_grp) %>%
    summarise(mean_mic = mean(micnos_1, na.rm = TRUE),
              sd_mic = sd(micnos_1, na.rm = TRUE),
              n = n(),
              n_studies = length(unique(record_id)),
              mean_hi = mean_mic - qt(0.975, n - 1)*(sd_mic/sqrt(n)),
              mean_lo = mean_mic + qt(0.975, n - 1)*(sd_mic/sqrt(n))) %>%
    mutate(change = mean_mic - lag(mean_mic),
           se_change = sqrt(((lag(sd_mic)^2)/ lag(n)) + ((sd_mic^2)/ n)),
           # se_change = () + (lag(sd_mic)/ sqrt(lag(n))),
           ci_lo_change = change - qt(0.975, n + lag(n) -1)*se_change,
           ci_hi_change = change + qt(0.975, n + lag(n) -1)*se_change,
           percent_change = round((mean_mic - lag(mean_mic))/lag(mean_mic) * 100,0),
           global_n = sum(n),
           prop = n/global_n) %>%
    ungroup() %>%
    mutate(grpid = max(gas_country$grpid) + 1,
           facet_grp = as.character(2),
           alpha_3_code = as_factor("GLOBAL"))

  gas_mean = bind_rows(gas_country, gas_global) %>%
    mutate(alpha_3_code = if_else(is.na(alpha_3_code), "UNCLASSIFIED", alpha_3_code)) %>%
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

  gas_mean[which(gas_mean$alpha_3_code == "HKG"), "entity"] = "Hong Kong"
  gas_mean[which(gas_mean$alpha_3_code == "UNCLASSIFIED"), "entity"] = "UNCLASSIFIED"
  gas_mean[which(gas_mean$alpha_3_code == "GLOBAL"), "entity"] = "GLOBAL"
  gas_mean[which(gas_mean$alpha_3_code == "TWN"), "entity"] = "Taiwan"

  paired_countries = (gas_mean %>%
                        count(alpha_3_code) %>%
                        filter(n == 2))$alpha_3_code

  gas_paired = gas_mean %>%
    filter(alpha_3_code %in% paired_countries)

  write_csv(gas_paired, "data/output/forest_data_nos_raw.csv")

  plot_bubble = ggplot(gas_paired %>% filter(alpha_3_code != "GLOBAL"),
                       aes(x = mean_mic,
                           y = fct_rev(entity),
                           group = mean_grp,
                           colour = mean_grp)) +
    geom_line(aes(group = entity), colour = "gray", linewidth = 1.1, alpha = 0.65) +
    geom_vline(data = gas_global %>% filter(mean_grp == 1),
               mapping = aes(xintercept = mean_mic),
               colour = "#6EB7B4", linewidth = 1 #linetype = "dotdash",
    ) +
    geom_vline(data = gas_global %>% filter(mean_grp == 2),
               mapping = aes(xintercept = mean_mic),
               colour = "#88669C", linewidth = 1# linetype = "dotted",
    ) +
    theme_minimal() +
    # geom_hline(yintercept = 1.5, linetype = "dashed", colour = "gray", linewidth = 1.2) +
    geom_point(data = gas_paired%>% filter(alpha_3_code != "GLOBAL"), mapping = aes(size = n),
               alpha = 0.65, show.legend = TRUE) +        ## size = 5,
    scale_size_continuous(range = c(4,10)) +   ##, transform = "log10"
    # geom_point(data = gas_paired %>% filter(med_grp == 2),
    #            size = 5.8, alpha = 1, color = "white") +
    # geom_point(data = gas_paired %>% filter(med_grp == 2),
    #            size = 5, alpha = 0.75, show.legend = FALSE) +
    geom_errorbar(mapping = aes(xmin = mean_lo, xmax = mean_hi), width = 0.7,
                  show.legend = FALSE, colour = "#174F4D", linewidth = 0.8,
                  data = gas_paired %>% filter(mean_grp == 1,
                                               alpha_3_code != "GLOBAL"))+
    geom_errorbar(mapping = aes(xmin = mean_lo, xmax = mean_hi), width = 0.7,
                  show.legend = FALSE, colour = "#422D4E", linewidth = 0.8,
                  data = gas_paired %>% filter(mean_grp == 2,
                                               alpha_3_code != "GLOBAL"))+
    scale_color_viridis_d(begin = 0.5, end = 0.05,
                          labels = c("Before and on mean year",
                                     "After mean year"))+
    theme(strip.background = element_blank(),
          strip.text = element_blank(),
          plot.title = element_markdown(lineheight = 1.1),
          legend.position = "top",
          # axis.text.x = element_text(angle = 30, hjust = .9),
          axis.text.y = element_blank(),
          panel.grid.minor.x = element_blank()) +
    guides(color = guide_legend(override.aes = list(size = 10))) +
    scale_x_continuous(breaks = c(0.008,0.016,0.032, 0.048, 0.064, 0.12),
                       limits = c(-0.017,0.13))+
    scale_y_discrete(expand = c(0,2))+
    geom_text(mapping = aes(label = str_c(n, " (", n_studies, ")"), x = -0.012), hjust = 1,
              data = gas_paired %>% filter(mean_grp == 1,
                                           alpha_3_code != "GLOBAL"), show.legend = FALSE) +
    geom_text(mapping = aes(label = str_c(n, " (", n_studies, ")"), x = -0.000), hjust = 1,
              data = gas_paired %>% filter(mean_grp == 2,
                                           alpha_3_code != "GLOBAL"), show.legend = FALSE) +
    geom_text(mapping = aes(label = "Number of isolates (studies)", x = -0.01, y= 15),
              show.legend = FALSE, colour = "#5D5D5D") +
    # geom_text(mapping = aes(label = "Before and on\n mean year", x = -0.016, y= 32.25),
    #           show.legend = FALSE, colour = "#5D5D5D") +
    # geom_text(mapping = aes(label = "After \nmean year", x = -0.003, y= 32.25), show.legend = FALSE,
    #           colour = "#5D5D5D") +

    annotate("text", label = "Global before and \non mean year",
             x = 0.004, y = 15.5, colour = "#6EB7B4") +
    annotate("text", label = "Global after \nmean year",
             x = 0.025, y = 15.5, colour = "#88669C") +
    # geom_curve(aes(x = 0.03, xend = 0.024, y = 31.3, yend = 31.5), colour = "#0E131F",
    #            arrow = arrow(length = unit(0.25,"cm"), type = "closed"), curvature = -0.5)+
    # geom_curve(aes(x = 0.014, xend = 0.0204, y = 31.3, yend = 31.5), colour = "#0E131F",
    #            arrow = arrow(length = unit(0.25,"cm"), type = "closed"), curvature = 0.5)+
    labs(x = "Mean MIC",
         y = "",
         size = "Number of Samples",
         colour = "")
  # plot_bubble

  plot_regions = ggplot(gas_paired %>% filter(alpha_3_code != "GLOBAL"),
                        aes(y = fct_rev(entity)))+
    theme_classic() +
    scale_y_discrete(position = "right", expand = c(0,2))+
    theme(strip.background = element_blank(),
          strip.text = element_blank(),
          plot.title = element_markdown(lineheight = 1.1),
          # axis.text.x = element_text(angle = 30, hjust = .9),
          # axis.text.y = element_text(hjust = 0),
          # panel.grid.minor = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank())+
    geom_text(aes(label = who_region, x = 1), colour = "white")+
    geom_text(aes(label = who_region, x = 1), data = gas_paired %>%
                group_by(who_region) %>%
                slice(1) %>%
                ungroup() %>%
                filter(alpha_3_code != "GLOBAL"), hjust = 1, colour = "#5D5D5D", fontface  = "bold")+
    labs(x = "", y = "") +
    coord_cartesian(xlim = c(0,1))

  plot_diffs = ggplot(gas_paired %>% filter(alpha_3_code != "GLOBAL"),
                      aes(y = fct_rev(entity)))+
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.text = element_blank(),
          plot.title = element_markdown(lineheight = 1.1),
          # axis.text.x = element_text(angle = 30, hjust = .9),
          axis.text.y = element_blank(),
          # panel.grid.minor = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank())+
    scale_y_discrete(expand = c(0,2))+
    annotate("text", label = "Difference in \nmean MIC", x = 1, y = 15.5, colour = "#5D5D5D") +
    annotate("text", label = "CI of \ndifference", x = 5.2, y = 15.5, colour = "#5D5D5D") +
    # geom_text(aes(label = "Difference in \nmeans", x = 1, y = 27), colour = "#5D5D5D")+
    # geom_text(aes(label = "CI of \ndifference", x = 5.2, y = 27), colour = "#5D5D5D")+
    geom_text(aes(label = round(change,3), x = 1), colour = "#5D5D5D")+
    geom_text(aes(label = str_c("[", round(ci_lo_change, 3), ", ", round(ci_hi_change,3), "]"), x = 6.5),
              hjust = 1, colour = "#5D5D5D")+
    labs(x = "", y = "") +
    coord_cartesian(xlim = c(0,10))

  layout <- c(
    patchwork::area(t = 1, l = 1, b = 30, r = 2), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
    patchwork::area(t = 0, l = 3, b = 30, r = 16), # middle plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
    patchwork::area(t = 0, l = 16, b = 30, r = 20) # right most plot starts at top of page, begins where middle plot ends (l=9, and middle plot is r=9), goes to bottom of page (b=30), and extends two units wide (r=11)
  )

  plot_regions + plot_bubble + plot_diffs + patchwork::plot_layout(design = layout)

}
ggsave("mean_mic_forest_pairs_mean_nos_raw.tif", path = "figs/", width = 17, height = 9.5)
#===============================================================================
# nos 90'ed + 90s
{
  gas_country = gas_90_combined %>%
    uncount(count) %>%
    group_by(alpha_3_code, mean_grp) %>%
    summarise(mean_mic = mean(mic90_1, na.rm = TRUE),
              sd_mic = sd(mic90_1, na.rm = TRUE),
              n = n(),
              n_studies = length(unique(record_id)),
              mean_hi = mean_mic - qt(0.975, n - 1)*(sd_mic/sqrt(n)),
              mean_lo = mean_mic + qt(0.975, n - 1)*(sd_mic/sqrt(n))) %>%
    mutate(change = mean_mic - lag(mean_mic),
           se_change = sqrt(((lag(sd_mic)^2)/ lag(n)) + ((sd_mic^2)/ n)),
           ci_lo_change = change - qt(0.975, n + lag(n) -1)*se_change,
           ci_hi_change = change + qt(0.975, n + lag(n) -1)*se_change,
           percent_change = round((mean_mic - lag(mean_mic))/lag(mean_mic) * 100, 0)) %>%
    ungroup() %>%
    group_by(alpha_3_code) %>%
    mutate(grpid = cur_group_id(),
           facet_grp = if_else(grpid <= 22, "1", "2"),
           mean_grp = as.factor(mean_grp),
           country_n = sum(n),
           prop = n/country_n,
           alpha_3_code = as_factor(alpha_3_code)) %>%
    ungroup()

  gas_global = gas_90_combined %>%
    uncount(count) %>%
    mutate(global_mean = mean(isolate_yr, na.rm = TRUE),
           mean_grp = as.factor(if_else(isolate_yr <= global_mean, 1, 2))) %>%
    group_by(mean_grp) %>%
    summarise(mean_mic = mean(mic90_1, na.rm = TRUE),
              sd_mic = sd(mic90_1, na.rm = TRUE),
              n = n(),
              n_studies = length(unique(record_id)),
              mean_hi = mean_mic - qt(0.975, n - 1)*(sd_mic/sqrt(n)),
              mean_lo = mean_mic + qt(0.975, n - 1)*(sd_mic/sqrt(n))) %>%
    mutate(change = mean_mic - lag(mean_mic),
           se_change = sqrt(((lag(sd_mic)^2)/ lag(n)) + ((sd_mic^2)/ n)),
           # se_change = () + (lag(sd_mic)/ sqrt(lag(n))),
           ci_lo_change = change - qt(0.975, n + lag(n) -1)*se_change,
           ci_hi_change = change + qt(0.975, n + lag(n) -1)*se_change,
           percent_change = round((mean_mic - lag(mean_mic))/lag(mean_mic) * 100,0),
           global_n = sum(n),
           prop = n/global_n) %>%
    ungroup() %>%
    mutate(grpid = max(gas_country$grpid) + 1,
           facet_grp = as.character(2),
           alpha_3_code = as_factor("GLOBAL"))

  gas_mean = bind_rows(gas_country, gas_global) %>%
    mutate(alpha_3_code = if_else(is.na(alpha_3_code), "UNCLASSIFIED", alpha_3_code)) %>%
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

  gas_mean[which(gas_mean$alpha_3_code == "HKG"), "entity"] = "Hong Kong"
  gas_mean[which(gas_mean$alpha_3_code == "UNCLASSIFIED"), "entity"] = "UNCLASSIFIED"
  gas_mean[which(gas_mean$alpha_3_code == "GLOBAL"), "entity"] = "GLOBAL"
  gas_mean[which(gas_mean$alpha_3_code == "TWN"), "entity"] = "Taiwan"

  paired_countries = (gas_mean %>%
                        count(alpha_3_code) %>%
                        filter(n == 2))$alpha_3_code

  gas_paired = gas_mean %>%
    filter(alpha_3_code %in% paired_countries)

  write_csv(gas_paired, "data/output/forest_data_nos90ed_with_90s.csv")

  plot_bubble = ggplot(gas_paired %>% filter(alpha_3_code != "GLOBAL"),
                       aes(x = mean_mic,
                           y = fct_rev(entity),
                           group = mean_grp,
                           colour = mean_grp)) +
    geom_line(aes(group = entity), colour = "gray", linewidth = 1.1, alpha = 0.65) +
    geom_vline(data = gas_global %>% filter(mean_grp == 1),
               mapping = aes(xintercept = mean_mic),
               colour = "#6EB7B4", linewidth = 1 #linetype = "dotdash",
    ) +
    geom_vline(data = gas_global %>% filter(mean_grp == 2),
               mapping = aes(xintercept = mean_mic),
               colour = "#88669C", linewidth = 1# linetype = "dotted",
    ) +
    theme_minimal() +
    # geom_hline(yintercept = 1.5, linetype = "dashed", colour = "gray", linewidth = 1.2) +
    geom_point(data = gas_paired%>% filter(alpha_3_code != "GLOBAL"), mapping = aes(size = n),
               alpha = 0.65, show.legend = TRUE) +        ## size = 5,
    scale_size_continuous(range = c(4,10)) +   ##, transform = "log10"
    # geom_point(data = gas_paired %>% filter(med_grp == 2),
    #            size = 5.8, alpha = 1, color = "white") +
    # geom_point(data = gas_paired %>% filter(med_grp == 2),
    #            size = 5, alpha = 0.75, show.legend = FALSE) +
    geom_errorbar(mapping = aes(xmin = mean_lo, xmax = mean_hi), width = 0.7,
                  show.legend = FALSE, colour = "#174F4D", linewidth = 0.8,
                  data = gas_paired %>% filter(mean_grp == 1,
                                               alpha_3_code != "GLOBAL"))+
    geom_errorbar(mapping = aes(xmin = mean_lo, xmax = mean_hi), width = 0.7,
                  show.legend = FALSE, colour = "#422D4E", linewidth = 0.8,
                  data = gas_paired %>% filter(mean_grp == 2,
                                               alpha_3_code != "GLOBAL"))+
    scale_color_viridis_d(begin = 0.5, end = 0.05,
                          labels = c("Before and on mean year",
                                     "After mean year"))+
    theme(strip.background = element_blank(),
          strip.text = element_blank(),
          plot.title = element_markdown(lineheight = 1.1),
          legend.position = "top",
          # axis.text.x = element_text(angle = 30, hjust = .9),
          axis.text.y = element_blank(),
          panel.grid.minor.x = element_blank()) +
    guides(color = guide_legend(override.aes = list(size = 10))) +
    scale_x_continuous(breaks = c(0.008,0.016,0.032, 0.048, 0.064, 0.12),
                       limits = c(-0.017,0.13))+
    scale_y_discrete(expand = c(0,2))+
    geom_text(mapping = aes(label = str_c(n, " (", n_studies, ")"), x = -0.012), hjust = 1,
              data = gas_paired %>% filter(mean_grp == 1,
                                           alpha_3_code != "GLOBAL"), show.legend = FALSE) +
    geom_text(mapping = aes(label = str_c(n, " (", n_studies, ")"), x = -0.000), hjust = 1,
              data = gas_paired %>% filter(mean_grp == 2,
                                           alpha_3_code != "GLOBAL"), show.legend = FALSE) +
    geom_text(mapping = aes(label = "Number of isolates (studies)", x = -0.01, y= 31),
              show.legend = FALSE, colour = "#5D5D5D") +
    # geom_text(mapping = aes(label = "Before and on\n mean year", x = -0.016, y= 32.25),
    #           show.legend = FALSE, colour = "#5D5D5D") +
    # geom_text(mapping = aes(label = "After \nmean year", x = -0.003, y= 32.25), show.legend = FALSE,
    #           colour = "#5D5D5D") +

    annotate("text", label = "Global before and \non mean year",
             x = 0.032, y = 31.3, colour = "#6EB7B4") +
    annotate("text", label = "Global after \nmean year",
             x = 0.016, y = 31.3, colour = "#88669C") +
    # geom_curve(aes(x = 0.03, xend = 0.024, y = 31.3, yend = 31.5), colour = "#0E131F",
    #            arrow = arrow(length = unit(0.25,"cm"), type = "closed"), curvature = -0.5)+
    # geom_curve(aes(x = 0.014, xend = 0.0204, y = 31.3, yend = 31.5), colour = "#0E131F",
    #            arrow = arrow(length = unit(0.25,"cm"), type = "closed"), curvature = 0.5)+
    labs(x = "Mean MIC",
         y = "",
         size = "Number of Samples",
         colour = "")
  # plot_bubble

  plot_regions = ggplot(gas_paired %>% filter(alpha_3_code != "GLOBAL"),
                        aes(y = fct_rev(entity)))+
    theme_classic() +
    scale_y_discrete(position = "right", expand = c(0,2))+
    theme(strip.background = element_blank(),
          strip.text = element_blank(),
          plot.title = element_markdown(lineheight = 1.1),
          # axis.text.x = element_text(angle = 30, hjust = .9),
          # axis.text.y = element_text(hjust = 0),
          # panel.grid.minor = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank())+
    geom_text(aes(label = who_region, x = 1), colour = "white")+
    geom_text(aes(label = who_region, x = 1), data = gas_paired %>%
                group_by(who_region) %>%
                slice(1) %>%
                ungroup() %>%
                filter(alpha_3_code != "GLOBAL"), hjust = 1, colour = "#5D5D5D", fontface  = "bold")+
    labs(x = "", y = "") +
    coord_cartesian(xlim = c(0,1))

  plot_diffs = ggplot(gas_paired %>% filter(alpha_3_code != "GLOBAL"),
                      aes(y = fct_rev(entity)))+
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.text = element_blank(),
          plot.title = element_markdown(lineheight = 1.1),
          # axis.text.x = element_text(angle = 30, hjust = .9),
          axis.text.y = element_blank(),
          # panel.grid.minor = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank())+
    scale_y_discrete(expand = c(0,2))+
    annotate("text", label = "Difference in \nmean MIC", x = 1, y = 31.25, colour = "#5D5D5D") +
    annotate("text", label = "CI of \ndifference", x = 5.2, y = 31.25, colour = "#5D5D5D") +
    # geom_text(aes(label = "Difference in \nmeans", x = 1, y = 27), colour = "#5D5D5D")+
    # geom_text(aes(label = "CI of \ndifference", x = 5.2, y = 27), colour = "#5D5D5D")+
    geom_text(aes(label = round(change,3), x = 1), colour = "#5D5D5D")+
    geom_text(aes(label = str_c("[", round(ci_lo_change, 3), ", ", round(ci_hi_change,3), "]"), x = 6.5),
              hjust = 1, colour = "#5D5D5D")+
    labs(x = "", y = "") +
    coord_cartesian(xlim = c(0,10))

  layout <- c(
    patchwork::area(t = 1, l = 1, b = 30, r = 2), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
    patchwork::area(t = 0, l = 3, b = 30, r = 16), # middle plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
    patchwork::area(t = 0, l = 16, b = 30, r = 20) # right most plot starts at top of page, begins where middle plot ends (l=9, and middle plot is r=9), goes to bottom of page (b=30), and extends two units wide (r=11)
  )

  plot_regions + plot_bubble + plot_diffs + patchwork::plot_layout(design = layout)
}

ggsave("mean_mic_forest_pairs_mean_nos90ed_with_90s.tif", path = "figs/", width = 17, height = 9.5)
#===============================================================================
# nos raw + 90s
{
  gas_country = gas_sr %>%
    uncount(count) %>%
    group_by(alpha_3_code, mean_grp) %>%
    summarise(mean_mic = mean(mic90nos, na.rm = TRUE),
              sd_mic = sd(mic90nos, na.rm = TRUE),
              n = n(),
              n_studies = length(unique(record_id)),
              mean_hi = mean_mic - qt(0.975, n - 1)*(sd_mic/sqrt(n)),
              mean_lo = mean_mic + qt(0.975, n - 1)*(sd_mic/sqrt(n))) %>%
    mutate(change = mean_mic - lag(mean_mic),
           se_change = sqrt(((lag(sd_mic)^2)/ lag(n)) + ((sd_mic^2)/ n)),
           ci_lo_change = change - qt(0.975, n + lag(n) -1)*se_change,
           ci_hi_change = change + qt(0.975, n + lag(n) -1)*se_change,
           percent_change = round((mean_mic - lag(mean_mic))/lag(mean_mic) * 100, 0)) %>%
    ungroup() %>%
    group_by(alpha_3_code) %>%
    mutate(grpid = cur_group_id(),
           facet_grp = if_else(grpid <= 22, "1", "2"),
           mean_grp = as.factor(mean_grp),
           country_n = sum(n),
           prop = n/country_n,
           alpha_3_code = as_factor(alpha_3_code)) %>%
    ungroup()

  gas_global = gas_sr %>%
    uncount(count) %>%
    mutate(global_mean = mean(isolate_yr, na.rm = TRUE),
           mean_grp = as.factor(if_else(isolate_yr <= global_mean, 1, 2))) %>%
    group_by(mean_grp) %>%
    summarise(mean_mic = mean(mic90nos, na.rm = TRUE),
              sd_mic = sd(mic90nos, na.rm = TRUE),
              n = n(),
              n_studies = length(unique(record_id)),
              mean_hi = mean_mic - qt(0.975, n - 1)*(sd_mic/sqrt(n)),
              mean_lo = mean_mic + qt(0.975, n - 1)*(sd_mic/sqrt(n))) %>%
    mutate(change = mean_mic - lag(mean_mic),
           se_change = sqrt(((lag(sd_mic)^2)/ lag(n)) + ((sd_mic^2)/ n)),
           # se_change = () + (lag(sd_mic)/ sqrt(lag(n))),
           ci_lo_change = change - qt(0.975, n + lag(n) -1)*se_change,
           ci_hi_change = change + qt(0.975, n + lag(n) -1)*se_change,
           percent_change = round((mean_mic - lag(mean_mic))/lag(mean_mic) * 100,0),
           global_n = sum(n),
           prop = n/global_n) %>%
    ungroup() %>%
    mutate(grpid = max(gas_country$grpid) + 1,
           facet_grp = as.character(2),
           alpha_3_code = as_factor("GLOBAL"))

  gas_mean = bind_rows(gas_country, gas_global) %>%
    mutate(alpha_3_code = if_else(is.na(alpha_3_code), "UNCLASSIFIED", alpha_3_code)) %>%
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

  gas_mean[which(gas_mean$alpha_3_code == "HKG"), "entity"] = "Hong Kong"
  gas_mean[which(gas_mean$alpha_3_code == "UNCLASSIFIED"), "entity"] = "UNCLASSIFIED"
  gas_mean[which(gas_mean$alpha_3_code == "GLOBAL"), "entity"] = "GLOBAL"
  gas_mean[which(gas_mean$alpha_3_code == "TWN"), "entity"] = "Taiwan"

  paired_countries = (gas_mean %>%
                        count(alpha_3_code) %>%
                        filter(n == 2))$alpha_3_code

  gas_paired = gas_mean %>%
    filter(alpha_3_code %in% paired_countries)

  write_csv(gas_paired, "data/output/forest_data_nos_raw_with_90s.csv")

  plot_bubble = ggplot(gas_paired %>% filter(alpha_3_code != "GLOBAL"),
                       aes(x = mean_mic,
                           y = fct_rev(entity),
                           group = mean_grp,
                           colour = mean_grp)) +
    geom_line(aes(group = entity), colour = "gray", linewidth = 1.1, alpha = 0.65) +
    geom_vline(data = gas_global %>% filter(mean_grp == 1),
               mapping = aes(xintercept = mean_mic),
               colour = "#6EB7B4", linewidth = 1 #linetype = "dotdash",
    ) +
    geom_vline(data = gas_global %>% filter(mean_grp == 2),
               mapping = aes(xintercept = mean_mic),
               colour = "#88669C", linewidth = 1# linetype = "dotted",
    ) +
    theme_minimal() +
    # geom_hline(yintercept = 1.5, linetype = "dashed", colour = "gray", linewidth = 1.2) +
    geom_point(data = gas_paired%>% filter(alpha_3_code != "GLOBAL"), mapping = aes(size = n),
               alpha = 0.65, show.legend = TRUE) +        ## size = 5,
    scale_size_continuous(range = c(4,10)) +   ##, transform = "log10"
    # geom_point(data = gas_paired %>% filter(med_grp == 2),
    #            size = 5.8, alpha = 1, color = "white") +
    # geom_point(data = gas_paired %>% filter(med_grp == 2),
    #            size = 5, alpha = 0.75, show.legend = FALSE) +
    geom_errorbar(mapping = aes(xmin = mean_lo, xmax = mean_hi), width = 0.7,
                  show.legend = FALSE, colour = "#174F4D", linewidth = 0.8,
                  data = gas_paired %>% filter(mean_grp == 1,
                                               alpha_3_code != "GLOBAL"))+
    geom_errorbar(mapping = aes(xmin = mean_lo, xmax = mean_hi), width = 0.7,
                  show.legend = FALSE, colour = "#422D4E", linewidth = 0.8,
                  data = gas_paired %>% filter(mean_grp == 2,
                                               alpha_3_code != "GLOBAL"))+
    scale_color_viridis_d(begin = 0.5, end = 0.05,
                          labels = c("Before and on mean year",
                                     "After mean year"))+
    theme(strip.background = element_blank(),
          strip.text = element_blank(),
          plot.title = element_markdown(lineheight = 1.1),
          legend.position = "top",
          # axis.text.x = element_text(angle = 30, hjust = .9),
          axis.text.y = element_blank(),
          panel.grid.minor.x = element_blank()) +
    guides(color = guide_legend(override.aes = list(size = 10))) +
    scale_x_continuous(breaks = c(0.008,0.016,0.032, 0.048, 0.064, 0.12),
                       limits = c(-0.017,0.13))+
    scale_y_discrete(expand = c(0,2))+
    geom_text(mapping = aes(label = str_c(n, " (", n_studies, ")"), x = -0.012), hjust = 1,
              data = gas_paired %>% filter(mean_grp == 1,
                                           alpha_3_code != "GLOBAL"), show.legend = FALSE) +
    geom_text(mapping = aes(label = str_c(n, " (", n_studies, ")"), x = -0.000), hjust = 1,
              data = gas_paired %>% filter(mean_grp == 2,
                                           alpha_3_code != "GLOBAL"), show.legend = FALSE) +
    geom_text(mapping = aes(label = "Number of isolates (studies)", x = -0.01, y= 31),
              show.legend = FALSE, colour = "#5D5D5D") +
    # geom_text(mapping = aes(label = "Before and on\n mean year", x = -0.016, y= 32.25),
    #           show.legend = FALSE, colour = "#5D5D5D") +
    # geom_text(mapping = aes(label = "After \nmean year", x = -0.003, y= 32.25), show.legend = FALSE,
    #           colour = "#5D5D5D") +

    annotate("text", label = "Global before and \non mean year",
             x = 0.035, y = 31.3, colour = "#6EB7B4") +
    annotate("text", label = "Global after \nmean year",
             x = 0.015, y = 31.3, colour = "#88669C") +
    # geom_curve(aes(x = 0.03, xend = 0.024, y = 31.3, yend = 31.5), colour = "#0E131F",
    #            arrow = arrow(length = unit(0.25,"cm"), type = "closed"), curvature = -0.5)+
    # geom_curve(aes(x = 0.014, xend = 0.0204, y = 31.3, yend = 31.5), colour = "#0E131F",
    #            arrow = arrow(length = unit(0.25,"cm"), type = "closed"), curvature = 0.5)+
    labs(x = "Mean MIC",
         y = "",
         size = "Number of Samples",
         colour = "")
  plot_bubble

  plot_regions = ggplot(gas_paired %>% filter(alpha_3_code != "GLOBAL"),
                        aes(y = fct_rev(entity)))+
    theme_classic() +
    scale_y_discrete(position = "right", expand = c(0,2))+
    theme(strip.background = element_blank(),
          strip.text = element_blank(),
          plot.title = element_markdown(lineheight = 1.1),
          # axis.text.x = element_text(angle = 30, hjust = .9),
          # axis.text.y = element_text(hjust = 0),
          # panel.grid.minor = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank())+
    geom_text(aes(label = who_region, x = 1), colour = "white")+
    geom_text(aes(label = who_region, x = 1), data = gas_paired %>%
                group_by(who_region) %>%
                slice(1) %>%
                ungroup() %>%
                filter(alpha_3_code != "GLOBAL"), hjust = 1, colour = "#5D5D5D", fontface  = "bold")+
    labs(x = "", y = "") +
    coord_cartesian(xlim = c(0,1))

  plot_regions

  plot_diffs = ggplot(gas_paired %>% filter(alpha_3_code != "GLOBAL"),
                      aes(y = fct_rev(entity)))+
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.text = element_blank(),
          plot.title = element_markdown(lineheight = 1.1),
          # axis.text.x = element_text(angle = 30, hjust = .9),
          axis.text.y = element_blank(),
          # panel.grid.minor = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank())+
    scale_y_discrete(expand = c(0,2))+
    annotate("text", label = "Difference in \nmean MIC", x = 1, y = 31.25, colour = "#5D5D5D") +
    annotate("text", label = "CI of \ndifference", x = 5.2, y = 31.25, colour = "#5D5D5D") +
    # geom_text(aes(label = "Difference in \nmeans", x = 1, y = 27), colour = "#5D5D5D")+
    # geom_text(aes(label = "CI of \ndifference", x = 5.2, y = 27), colour = "#5D5D5D")+
    geom_text(aes(label = round(change,3), x = 1), colour = "#5D5D5D")+
    geom_text(aes(label = str_c("[", round(ci_lo_change, 3), ", ", round(ci_hi_change,3), "]"), x = 6.5),
              hjust = 1, colour = "#5D5D5D")+
    labs(x = "", y = "") +
    coord_cartesian(xlim = c(0,10))

  plot_diffs

  layout <- c(
    patchwork::area(t = 1, l = 1, b = 30, r = 2), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
    patchwork::area(t = 0, l = 3, b = 30, r = 16), # middle plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
    patchwork::area(t = 0, l = 16, b = 30, r = 20) # right most plot starts at top of page, begins where middle plot ends (l=9, and middle plot is r=9), goes to bottom of page (b=30), and extends two units wide (r=11)
  )

  plot_regions + plot_bubble + plot_diffs + patchwork::plot_layout(design = layout)
}

ggsave("mean_mic_forest_pairs_mean_nos_raw_with_90s.tif", path = "figs/", width = 17, height = 9.5)
