source("code/preamble.R")

# gas_map = gas_sr %>% 
#   group_by(alpha_3_code) %>% 
#   mutate(sum_iso = sum(count, na.rm = TRUE)) %>% 
#   slice(1) %>% 
#   ungroup()
# 
# gas_plot = ggplot(world_map %>% 
#          filter(alpha_3_code != "ATA"), aes(x = long, y = lat, group = group)) +
#   geom_polygon(colour = "black", fill = "#E0E0E0", linewidth = 0.1) +
#   geom_polygon(gas_map %>% 
#                  left_join(world_map, by = "alpha_3_code"), 
#                mapping = aes(x = long, y = lat, group = group, fill = sum_iso),
#                alpha = 0.85, colour = "black", linewidth = 0.1, show.legend = FALSE) +
#   geom_polygon(data = world_map %>% 
#                  filter(alpha_3_code == "LSO"), colour = "black", fill = "#E0E0E0",
#                linewidth = 0.1)+
#   scale_fill_viridis_c(breaks = scales::breaks_pretty()) +
#   theme(panel.background = element_rect(fill = "white"),
#         plot.background = element_rect(fill = "white"),
#         panel.grid = element_blank(),
#         plot.title = element_text(size = 18, face = "bold"),
#         plot.subtitle = element_text(size = 13),
#         axis.text = element_blank(),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         legend.key.height = unit(1, 'cm'),
#         legend.background = element_rect(fill = "white"),
#         legend.text = element_text(colour = "black"),
#         legend.title = element_text(colour = "black")) +
#   labs(fill = "Number of isolates",
#        title = "Count of isolates by country",
#        subtitle = "Those records with MIC reported") +
#   annotation_custom(ggplotGrob(
#     ggplot(gas_map, aes(x = sum_iso, fill = sum_iso, group = sum_iso)) +
#       geom_histogram(bins = 30, show.legend = FALSE) +
#       theme(panel.background = element_rect(fill = "white"),
#             plot.background = element_rect(fill = "white", colour ="black"),
#             panel.grid = element_blank(),
#             axis.title = element_text(size = 8),
#             legend.key.height = unit(1, 'cm'),
#             legend.background = element_rect(fill = "white"),
#             legend.text = element_text(colour = "black"),
#             legend.title = element_text(colour = "black")) +
#       labs(x = "Isolates per Country",
#            y = "Count") +
#       scale_fill_viridis_c(breaks = scales::breaks_pretty())), 
#     xmin = -200, xmax = -80, ymin = -60, ymax = -20
#   )
# gas_plot
# 
# 
# ggsave("map_mic.tiff", path = "figures/", width = 12.1, height = 5.72)

################################################################################

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
  scale_size_continuous(range = c(2,6)) +
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
               left_join(centroids, by = c("alpha_3_code")), alpha = .8, 
             colour = "#D5573B", pch = 19, stroke = 1.2)+
  guides(fill = guide_colorbar(order = 1),
         size = guide_legend(order = 2))

dd_plot  ## 1 row removed in geom_point - 'multiple' 

ggsave("map_all.tiff", path = "figures/", width = 12.1, height = 5.72)

# +
#   annotation_custom(ggplotGrob(
#     ggplot(dd_df, aes(x = sum_iso, fill = sum_iso, group = sum_iso)) +
#       geom_histogram(bins = 30, show.legend = FALSE) +
#       theme(panel.background = element_rect(fill = "white"),
#             plot.background = element_rect(fill = "white", colour ="black"),
#             panel.grid = element_blank(),
#             axis.title = element_text(size = 8),
#             legend.key.height = unit(1, 'cm'),
#             legend.background = element_rect(fill = "white"),
#             legend.text = element_text(colour = "black"),
#             legend.title = element_text(colour = "black")) +
#       labs(x = "Isolates per Country",
#            y = "Count") +
#       scale_fill_viridis_c(breaks = scales::breaks_pretty())), 
#     xmin = -200, xmax = -80, ymin = -60, ymax = -20
#   )





# dd_plot_studies = ggplot(world_map %>% 
#                    filter(alpha_3_code != "ATA"), aes(x = long, y = lat, group = group)) +
#   geom_polygon(colour = "black", fill = "#E0E0E0", linewidth = 0.1) +
#   geom_polygon(dd_df %>% 
#                  left_join(world_map, by = "alpha_3_code"), 
#                mapping = aes(x = long, y = lat, group = group, fill = sum_studies),
#                alpha = 0.85, colour = "black", linewidth = 0.1, show.legend = FALSE) +
#   geom_polygon(data = world_map %>% 
#                  filter(alpha_3_code == "LSO"), colour = "black", fill = "#E0E0E0",
#                linewidth = 0.1)+
#   scale_fill_viridis_c(breaks = scales::breaks_pretty()) +
#   theme(panel.background = element_rect(fill = "white"),
#         plot.background = element_rect(fill = "white"),
#         panel.grid = element_blank(),
#         plot.title = element_text(size = 18, face = "bold"),
#         plot.subtitle = element_text(size = 13),
#         axis.text = element_blank(),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         legend.key.height = unit(1, 'cm'),
#         legend.background = element_rect(fill = "white"),
#         legend.text = element_text(colour = "black"),
#         legend.title = element_text(colour = "black")) +
#   labs(fill = "Number of studies",
#        title = "Count of studies by country",
#        subtitle = "Those records with SIR reported") +
#   annotation_custom(ggplotGrob(
#     ggplot(dd_df, aes(x = sum_studies, fill = sum_studies, group = sum_studies)) +
#       geom_histogram(bins = 30, show.legend = FALSE) +
#       theme(panel.background = element_rect(fill = "white"),
#             plot.background = element_rect(fill = "white", colour ="black"),
#             panel.grid = element_blank(),
#             axis.title = element_text(size = 8),
#             legend.key.height = unit(1, 'cm'),
#             legend.background = element_rect(fill = "white"),
#             legend.text = element_text(colour = "black"),
#             legend.title = element_text(colour = "black")) +
#       labs(x = "Studies per Country",
#            y = "Count") +
#       scale_fill_viridis_c(breaks = scales::breaks_pretty())), 
#     xmin = -200, xmax = -80, ymin = -60, ymax = -20
#   )
# dd_plot_studies
# 
# ggsave("map_sir_studies.tiff", path = "figures/", width = 12.1, height = 5.72)
