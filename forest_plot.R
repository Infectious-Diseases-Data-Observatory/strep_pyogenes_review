setwd("~/R/Strep A")
source("code/preamble.R")

gas_country = gas_sr %>% 
  uncount(count) %>% 
  group_by(alpha_3_code, med_grp) %>%
  summarise(mean_mic = mean(mic90nos, na.rm = TRUE),
            sd_mic = sd(mic90nos, na.rm = TRUE),
            n = n(),
            n_studies = length(unique(record_id)),
            mean_hi = mean(mic90nos, na.rm = TRUE) - qt(0.975, n() - 1)*(sd(mic90nos, na.rm = TRUE)/sqrt(n())),
            mean_lo = mean(mic90nos, na.rm = TRUE) + qt(0.975, n() - 1)*(sd(mic90nos, na.rm = TRUE)/sqrt(n()))) %>% 
  mutate(change = mean_mic - lag(mean_mic),
         se_change = (sd_mic/sqrt(n)) + (lag(sd_mic)/ sqrt(lag(n))),
         ci_lo_change = change - qt(0.975, n + lag(n) -1)*se_change,
         ci_hi_change = change + qt(0.975, n + lag(n) -1)*se_change,
         percent_change = round((mean_mic - lag(mean_mic))/lag(mean_mic) * 100,0)) %>% 
  ungroup() %>% 
  group_by(alpha_3_code) %>% 
  mutate(grpid = cur_group_id(), 
         facet_grp = if_else(grpid <= 22, "1", "2"),
         alpha_3_code = as_factor(alpha_3_code))

gas_global = gas_sr %>% 
  uncount(count) %>% group_by(year_grp) %>% 
  summarise(mean_mic = mean(mic90nos, na.rm = TRUE),
            sd_mic = sd(mic90nos, na.rm = TRUE),
            n = n(),
            n_studies = length(unique(record_id)),
            mean_hi = mean(mic90nos, na.rm = TRUE) - qt(0.975, n() - 1)*(sd(mic90nos, na.rm = TRUE)/sqrt(n())),
            mean_lo = mean(mic90nos, na.rm = TRUE) + qt(0.975, n() - 1)*(sd(mic90nos, na.rm = TRUE)/sqrt(n()))) %>% 
  mutate(change = mean_mic - lag(mean_mic),
         se_change = (sd_mic/sqrt(n)) + (lag(sd_mic)/ sqrt(lag(n))),
         ci_lo_change = change - qt(0.975, n + lag(n) -1)*se_change,
         ci_hi_change = change + qt(0.975, n + lag(n) -1)*se_change,
         percent_change = round((mean_mic - lag(mean_mic))/lag(mean_mic) * 100,0)) %>% 
  ungroup() %>% mutate(grpid = max(gas_country$grpid)+1, 
                       facet_grp = as.character(2),
                       alpha_3_code = as_factor("GLOBAL"))

gas_mean = bind_rows(gas_country, gas_global) %>% 
  mutate(alpha_3_code = if_else(is.na(alpha_3_code), "UNCLASSIFIED", alpha_3_code)) %>% 
  left_join(who_regions, by = c("alpha_3_code" = "code"))%>% 
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

plot_bubble = ggplot(gas_mean,
       aes(x = mean_mic,
           y = fct_rev(alpha_3_code),
           group = med_grp,
           colour = med_grp)) +
  geom_line(aes(group = alpha_3_code), colour = "gray", linewidth = 1.1) +
  geom_hline(yintercept = 1.4, linetype = "dashed", colour = "gray", linewidth = 1.2) +
  geom_point(data = gas_mean,
             size = 5, alpha = 0.95, show.legend = FALSE) +
  geom_point(data = gas_mean %>% filter(med_grp == 2),
             size = 5.8, alpha = 1, color = "white") +
  geom_point(data = gas_mean %>% filter(med_grp == 2),
             size = 5, alpha = 0.75, show.legend = FALSE) +
  geom_errorbar(mapping = aes(xmin = mean_lo, xmax = mean_hi), width = 0.7, show.legend = FALSE,
                color = "black")+
  scale_color_viridis_d(begin = 0.5, end = 0.05)+
  theme_minimal() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_markdown(lineheight = 1.1),
        # axis.text.x = element_text(angle = 30, hjust = .9),
        axis.text.y = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_x_continuous(breaks = c(0.008,0.016,0.032,0.064,0.12),
                     limits = c(-0.017,0.12))+
  scale_y_discrete(expand = c(0,4))+
  geom_text(mapping = aes(label = str_c(n, " (", n_studies, ")"), x = -0.011), hjust = 1,
            data = gas_mean %>% filter(med_grp == 1), show.legend = FALSE) +
  geom_text(mapping = aes(label = str_c(n, " (", n_studies, ")"), x = -0.000), hjust = 1,
            data = gas_mean %>% filter(med_grp == 2), show.legend = FALSE) +
  geom_text(mapping = aes(label = "Number of isolates (studies)", x = -0.01, y= 47.5), show.legend = FALSE) +
  geom_text(mapping = aes(label = "Before and\non 2000", x = -0.015, y= 46),
            show.legend = FALSE, colour = "#5D5D5D") +
  geom_text(mapping = aes(label = "After \n2000", x = -0.0035, y= 46), show.legend = FALSE,
            colour = "#5D5D5D") +
  labs(x = "Mean MIC",
       y = "")
plot_bubble

plot_regions = ggplot(gas_mean,
       aes(y = fct_rev(alpha_3_code)))+
  theme_classic() +
  scale_y_discrete(position = "right", expand = c(0,4))+
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_markdown(lineheight = 1.1),
        # axis.text.x = element_text(angle = 30, hjust = .9),
        # axis.text.y = element_text(hjust = 1),
        # panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank())+
  geom_text(aes(label = who_region, x = 1), colour = "white")+
  geom_text(aes(label = who_region, x = 1), data = gas_mean %>% 
              group_by(who_region) %>% 
              slice(1) %>% 
              ungroup(), hjust = 1, colour = "#5D5D5D", fontface  = "bold")+
  labs(x = "", y = "") +
  coord_cartesian(xlim = c(0,1))

plot_regions

plot_diffs = ggplot(gas_mean,
       aes(y = fct_rev(alpha_3_code)))+
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
  scale_y_discrete(expand = c(0,4))+
  geom_text(aes(label = "Difference in \nmeans", x = 1, y = 46), colour = "#5D5D5D")+
  geom_text(aes(label = "CI of \ndifference", x = 5, y = 46), colour = "#5D5D5D")+
  geom_text(aes(label = round(change,3), x = 1), colour = "#5D5D5D")+
  geom_text(aes(label = str_c("[", round(ci_lo_change, 3), ", ", round(ci_hi_change, 3), "]"), x = 6.5),
            hjust = 1, colour = "#5D5D5D")+
  labs(x = "", y = "") +
  coord_cartesian(xlim = c(0,10))

layout <- c(
  patchwork::area(t = 1, l = 1, b = 30, r = 3), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
  patchwork::area(t = 0, l = 4, b = 30, r = 16), # middle plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
  patchwork::area(t = 0, l = 16, b = 30, r = 19) # right most plot starts at top of page, begins where middle plot ends (l=9, and middle plot is r=9), goes to bottom of page (b=30), and extends two units wide (r=11)
)

plot_regions + plot_bubble + plot_diffs + patchwork::plot_layout(design = layout)

ggsave("mean_mic_forest.tiff", path = "figures/", width = 17.5, height = 9.5)


#===============================================================================

paired_countries = (gas_mean %>% 
  count(alpha_3_code) %>% 
  filter(n == 2))$alpha_3_code

gas_paired = gas_mean %>% 
  filter(alpha_3_code %in% paired_countries)

write_csv(gas_paired, "data/cleaned/forest_data.csv")

plot_bubble = ggplot(gas_paired,
                     aes(x = mean_mic,
                         y = fct_rev(entity),
                         group = med_grp,
                         colour = med_grp)) +
  geom_line(aes(group = entity), colour = "gray", linewidth = 1.1) +
  geom_hline(yintercept = 1.5, linetype = "dashed", colour = "gray", linewidth = 1.2) +
  geom_point(data = gas_paired,
             size = 5, alpha = 0.95, show.legend = FALSE) +
  geom_point(data = gas_paired %>% filter(med_grp == 2),
             size = 5.8, alpha = 1, color = "white") +
  geom_point(data = gas_paired %>% filter(med_grp == 2),
             size = 5, alpha = 0.75, show.legend = FALSE) +
  geom_errorbar(mapping = aes(xmin = mean_lo, xmax = mean_hi), width = 0.7, show.legend = FALSE,
                color = "black")+
  scale_color_viridis_d(begin = 0.5, end = 0.05)+
  theme_minimal() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_markdown(lineheight = 1.1),
        # axis.text.x = element_text(angle = 30, hjust = .9),
        axis.text.y = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_x_continuous(breaks = c(0.008,0.016,0.032,0.064),
                     limits = c(-0.017,0.07))+
  scale_y_discrete(expand = c(0,4))+
  geom_text(mapping = aes(label = str_c(n, " (", n_studies, ")"), x = -0.011), hjust = 1,
            data = gas_paired %>% filter(med_grp == 1), show.legend = FALSE) +
  geom_text(mapping = aes(label = str_c(n, " (", n_studies, ")"), x = -0.000), hjust = 1,
            data = gas_paired %>% filter(med_grp == 2), show.legend = FALSE) +
  geom_text(mapping = aes(label = "Number of isolates (studies)", x = -0.0085, y= 27.5), show.legend = FALSE) +
  geom_text(mapping = aes(label = "Before and\non 2000", x = -0.014, y= 26.25),
            show.legend = FALSE, colour = "#5D5D5D") +
  geom_text(mapping = aes(label = "After \n2000", x = -0.0025, y= 26.25), show.legend = FALSE,
            colour = "#5D5D5D") +
  labs(x = "Mean MIC",
       y = "")
plot_bubble

plot_regions = ggplot(gas_paired,
                      aes(y = fct_rev(entity)))+
  theme_classic() +
  scale_y_discrete(position = "right", expand = c(0,4))+
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
              ungroup(), hjust = 1, colour = "#5D5D5D", fontface  = "bold")+
  labs(x = "", y = "") +
  coord_cartesian(xlim = c(0,1))

plot_regions

plot_diffs = ggplot(gas_paired,
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
  scale_y_discrete(expand = c(0,4))+
  geom_text(aes(label = "Difference in \nmeans", x = 1, y = 27), colour = "#5D5D5D")+
  geom_text(aes(label = "CI of \ndifference", x = 5.2, y = 27), colour = "#5D5D5D")+
  geom_text(aes(label = round(change,3), x = 1), colour = "#5D5D5D")+
  geom_text(aes(label = str_c("[", round(ci_lo_change, 3), ", ", round(ci_hi_change,3), "]"), x = 6.5),
            hjust = 1, colour = "#5D5D5D")+
  labs(x = "", y = "") +
  coord_cartesian(xlim = c(0,10))

plot_diffs

layout <- c(
  patchwork::area(t = 1, l = 1, b = 30, r = 3), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
  patchwork::area(t = 0, l = 4, b = 30, r = 14), # middle plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
  patchwork::area(t = 0, l = 14, b = 30, r = 17) # right most plot starts at top of page, begins where middle plot ends (l=9, and middle plot is r=9), goes to bottom of page (b=30), and extends two units wide (r=11)
)

plot_regions + plot_bubble + plot_diffs + patchwork::plot_layout(design = layout)



ggsave("mean_mic_forest_pairs.tiff", path = "figures/", width = 17, height = 9.5)

