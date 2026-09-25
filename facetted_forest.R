ggplot(gas_mic_agg |> filter(alpha_3_code %in% c(
      "USA", "JPN", "ESP", "SRB", "ITA", "GBR", "FRA", "BRA", "AUT", "TUR", "MEX", "CAN"))%>%
        group_by(alpha_3_code) %>%
        mutate(median_year = floor(median(isolate_yr, na.rm = TRUE)),
                median_grp = as.factor(if_else(isolate_yr <= median_year, 1, 2)),
                mean_year = floor(mean(isolate_yr, na.rm = TRUE)),
                mean_grp = as.factor(if_else(isolate_yr <= mean_year, 1, 2))) %>%
         ungroup(),
       aes(x = mic_90_agg, y = isolate_yr,
           colour = mean_grp)) +
  geom_point(mapping = aes(size = count), alpha = 0.7) +
  theme_bw() +
  scale_color_viridis_d(begin = 0.5, end = 0.05,
                        labels = c("Before and on mean year",
                                   "After mean year")) +
  scale_y_continuous(transform = "reverse") +
  geom_point(data = country_data |> filter(alpha_3_code %in% c(
    "USA", "JPN", "ESP", "SRB", "ITA", "GBR", "FRA", "BRA", "AUT", "TUR", "MEX", "CAN"), mean_grp == 1),
             aes(x = mean_mic_ori, y = 2030, size = n), pch = 18, alpha = 0.7)+
  geom_point(data = country_data |> filter(alpha_3_code %in% c(
    "USA", "JPN", "ESP", "SRB", "ITA", "GBR", "FRA", "BRA", "AUT", "TUR", "MEX", "CAN"), mean_grp == 2),
             aes(x = mean_mic_ori, y = 2030, size = n), pch = 18, alpha = 0.7) +
  geom_hline(mapping = aes(yintercept = 2026), linetype = "dashed") +
  facet_wrap(~alpha_3_code)+
  scale_size_continuous(range = c(2,7),
                        breaks = c(1, 10, 100, 1000, 4000)) +
  geom_line(data = country_data |> filter(alpha_3_code %in% c(
    "USA", "JPN", "ESP", "SRB", "ITA", "GBR", "FRA", "BRA", "AUT", "TUR", "MEX", "CAN"),
    change_mean_ori > 0 | is.na(change_mean_ori)),
    aes(group = alpha_3_code, y = 2030, x = mean_mic_ori), colour = "gray", linewidth = 1.1, alpha = 0.45,
            arrow = arrow(length = unit(0.45, "cm"), type = "closed"), inherit.aes = FALSE) +
  geom_line(data = country_data |> filter(alpha_3_code %in% c(
    "USA", "JPN", "ESP", "SRB", "ITA", "GBR", "FRA", "BRA", "AUT", "TUR", "MEX", "CAN"),
    change_mean_ori < 0 | is.na(change_mean_ori)),
    aes(group = alpha_3_code, y = 2030, x = mean_mic_ori), colour = "gray", linewidth = 1.1, alpha = 0.45,
    arrow = arrow(length = unit(0.45, "cm"), type = "closed", ends = "first"), inherit.aes = FALSE)+
  labs(x = "Mean MIC (log2 scale)",
       y = "Isolate Year",
       size = "Number of Samples",
       colour = "")

ggsave("facet_forest_summarised.pdf", path = "figs/", width = 20, height = 10)
