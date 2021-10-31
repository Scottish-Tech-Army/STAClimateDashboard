


simp_chart_plot <- function(detail_choice) {
  
  scot_ghg_det_plot <- ghg_sector_total_plot %>%
    filter(sector == detail_choice)
  
  #stacked area plot
  scot_ghg_det_plot %>%
    ggplot() +
    aes(x = year, y = total_emissions) +
    geom_area(fill = sector_list$sector_colors[sector_list$sector == detail_choice]) +
    theme_light() +

    scale_x_continuous(breaks = seq(first(scot_ghg_det_plot$year),
                                    last(scot_ghg_det_plot$year), 5)) +
    
    theme(legend.position = "none",
          panel.grid = element_blank(),
          axis.text = element_text(face = "bold")) +
    labs(x = "Year",
         y = "Total CO2 Emissions (Mt CO2)")
}


