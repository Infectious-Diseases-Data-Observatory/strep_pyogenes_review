clean_gas_data = function(data){
  data[which(data$country == "(unknown)"), "country"] = "Unknown"
  data[which(data$country == "(not reported)"), "country"] = "Unknown"

  data[which(data$country == "USA"), "country"] = "United States of America (the)"
  data[which(data$country == "South Korea"), "country"] = "Korea (the Republic of)"
  data[which(data$country == "Taiwan"), "country"] = "Taiwan (Province of China)"
  data[which(data$country == "UK"), "country"] = "United Kingdom of Great Britain and Northern Ireland (the)"
  data[which(data$country == "Worldwide"), "country"] = "Multiple"
  data[which(data$country == "worldwide"), "country"] = "Multiple"
  data[which(data$country == "United Kingdom"), "country"] = "United Kingdom of Great Britain and Northern Ireland (the)"
  data[which(data$country == "Slovak Republic"), "country"] = "Slovakia"
  data[which(data$country == "Turkey"), "country"] = "Turkiye"
  data[which(data$country == "Netherlands"), "country"] = "Netherlands (Kingdom of the)"

  # data_join = data %>%
  #   left_join(world_income, by = c("country")) %>%
  #   mutate(country = str_to_title(country)) %>%
  #   group_by(alpha_3_code) %>%
  #   mutate(median_year = median(isolate_yr, na.rm = TRUE),
  #          med_grp = if_else(isolate_yr <= median_year, 1, 2),
  #          midpoint_year = (max(isolate_yr, na.rm = TRUE) - min(isolate_yr, na.rm = TRUE))/2 + min(isolate_yr, na.rm = TRUE),
  #          midpoint_grp = if_else(isolate_yr <= midpoint_year, 1, 2),
  #          mean_year = floor(mean(isolate_yr, na.rm = TRUE)),
  #          mean_grp = as.factor(if_else(isolate_yr <= mean_year, 1, 2))) %>%
  #   ungroup()

  return(data)
}
