source("mic_data_load.R")

dd_df = dd_sr %>%
  group_by(alpha_3_code) %>%
  mutate(sum_iso = sum(count, na.rm = TRUE),
         sum_rows = n(),
         sum_studies = length(unique(record_id))) %>%
  slice(1) %>%
  ungroup() %>%
  select(who, country, sum_iso, sum_rows, sum_studies, alpha_3_code, alpha_2_code, income_group)

dd_plot = ggplot(world_map %>%
         filter(alpha_3_code != "ATA"), aes(x = long, y = lat, group = group)) +
  geom_polygon(colour = "black", fill = "#E0E0E0", linewidth = 0.1, show.legend = FALSE) +
  geom_polygon(dd_df %>%
                 left_join(world_map, by = "alpha_3_code"),
               mapping = aes(x = long, y = lat, group = group, fill = sum_studies),
               alpha = 0.85, colour = "black", linewidth = 0.1) +
  geom_polygon(data = world_map %>%
                 filter(alpha_3_code == "LSO"), colour = "black", fill = "#E0E0E0",
               linewidth = 0.1, show.legend = FALSE)+
  scale_fill_viridis_c(values = c(0, 0.1, 0.2, 0.5, 1), na.value = "#E0E0E0") +
  scale_size_continuous(range = c(1,8),
                        breaks = c(100, 1000, 10000, 40000)) +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 13),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.key.height = unit(1, 'cm')) +
  labs(fill = "Number of studies",
       size = "Number of isolates") +
  geom_point(mapping = aes(size = sum_iso, x = centroid_long, y = centroid_lat, group = alpha_3_code),
             data = dd_df %>%
               left_join(centroids, by = c("alpha_3_code")), alpha = .7,
             colour = "#D5573B", pch = 21, stroke = 1.2, fill = "#EAA79A")+
  guides(fill = guide_colorbar(order = 1),
         size = guide_legend(order = 2))

dd_plot  ## 1 row removed in geom_point - 'multiple'

ggsave("isolates_map.tif", path = "figs/", width = 12.1, height = 5.72)


