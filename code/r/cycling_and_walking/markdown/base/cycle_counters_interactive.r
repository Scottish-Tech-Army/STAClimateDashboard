## ---- bicycle_counts --------

#options(repr.plot.width = 12, repr.plot.height = 5)

plot1 <- cycle_counter_data_2017_2021 %>%
    filter(traffic_mode == "bicycle") %>%
    group_by(traffic_mode, year, month) %>%
    summarise(average = mean(count), 
              count = sum(count)) %>%
    
    ggplot(aes(month, count, group = year, colour = year, 
               text = paste(count, "bicycles", "-", month, " ", year))) +
        geom_line(size = 0.5) + 
        scale_y_continuous(labels = scales::comma) +
        ylab("Bicycle - Count by Month") + xlab("") +
        cop_cycling_theme +
        theme(legend.title = element_blank())


convertToPlotly(plot1, height = 330, width = 720, 
                yaxis = list(tickfont = list(size = 14), title = list(font = list(size = 18))),
                xaxis = list(tickfont = list(size = 14))
               ) %>%
    config(displayModeBar = FALSE)



## ---- pedestrian_counts --------

plot1 <- cycle_counter_data_2017_2021 %>%
    filter(traffic_mode == "pedestrian") %>%
    group_by(traffic_mode, year, month) %>%
    summarise(average = mean(count), 
              count = sum(count)) %>%
    
    ggplot(aes(month, count, group = year, colour = year, 
               text = paste(count, "pedestrians", "-", month, " ", year))) +
        geom_line(size = 0.5) +
        scale_y_continuous(labels = scales::comma) +
        ylab("Pedestrian - Count by Month") + xlab("") +
        cop_cycling_theme +
        theme(legend.title = element_blank())



convertToPlotly(plot1, height = 330, width = 720, 
                yaxis = list(tickfont = list(size = 14), title = list(font = list(size = 18))),
                xaxis = list(tickfont = list(size = 14))
               ) %>%
    config(displayModeBar = FALSE)



## ---- summary_tables --------

cycle_counter_data_2017_2021 %>%

    select(traffic_mode, year, count) %>%
    group_by(traffic_mode, year) %>%
    summarise(count = sum(count))  %>%
  
    pivot_wider(names_from = year, values_from = count) %>%
    rename_with(~ gsub("_", " ", .x, fixed = TRUE)) %>%
    ungroup() %>%

    mutate("total" = reduce(select(., matches("\\b\\d{4}\\b")), `+`)) %>%
    mutate_if(is.numeric, scales::comma) %>%

    kable(caption = "Total count per year") %>%
    kable_paper("striped", full_width = FALSE, position = "left")


cycle_counter_data_2017_2021 %>%

    select(traffic_mode, year, count) %>%
    group_by(traffic_mode, year) %>%
    summarise(average = round(24 * mean(count, rm.na = TRUE)))  %>%
  
    pivot_wider(names_from = year, values_from = average) %>%
    rename_with(~ gsub("_", " ", .x, fixed = TRUE)) %>%

    kable(caption = "\nAverage count by hour") %>%
    kable_paper("striped", full_width = FALSE, position = "left")


cycle_counter_data_2017_2021 %>%

    select(traffic_mode, year, siteID, date, time, count) %>%
    group_by(traffic_mode, year, date, siteID) %>%
    summarise(count = sum(count, rm.na = TRUE)) %>%

    group_by(traffic_mode, year, date) %>%
    summarise(average = mean(count, rm.na = TRUE)) %>%

    group_by(traffic_mode, year) %>%
    summarise(average = round(mean(average, rm.na = TRUE))) %>%

  
    pivot_wider(names_from = year, values_from = average) %>%
    rename_with(~ gsub("_", " ", .x, fixed = TRUE)) %>%

    kable(caption = "\nAverage count by day (across counters)") %>%
    kable_paper("striped", full_width = FALSE, position = "left")



## ---- avg_count_time_of_day_by_weekday_and_year --------

#options(repr.plot.width = 18, repr.plot.height = 4)

plot1 <- cycle_counter_data_2017_2021 %>%

    filter(traffic_mode == "bicycle") %>%
    group_by(year, weekday, isWeekEnd, time) %>% 
    summarise(total = sum(count),              
              average = mean(count)) %>%
    
    ggplot(aes(time, average, group = weekday, colour = weekday, linetype = isWeekEnd,
               text = paste("Average count -", weekday, time, "-" , round(average, 2)), 
               frame = year, ids = weekday
              )) + 
        geom_line() +
        geom_vline(xintercept = 8, size = 0.5, linetype = "dotdash", alpha = 0.15) + 
        geom_vline(xintercept = 9, size = 0.5, linetype = "dotdash", alpha = 0.15) + 
        geom_vline(xintercept = 14, size = 0.5, linetype = "twodash", alpha = 0.15) + 
        geom_vline(xintercept = 15, size = 0.5, linetype = "twodash", alpha = 0.15) + # "14:00"
        geom_vline(xintercept = 17, size = 0.5, linetype = "dotdash", alpha = 0.15) + 
        geom_vline(xintercept = 18, size = 0.5, linetype = "dotdash", alpha = 0.15) + 
        cop_cycling_theme + 
        scale_colour_brewer(palette = "Dark2", direction = -1) +
        scale_y_continuous(label = scales::number_format(accuracy = 1), breaks = seq(0, 2, 1)) +
        guides(linetype = FALSE) +
        theme(axis.text.x = element_text(size = 11, angle = 45, vjust = 0.5), 
              legend.title = element_blank()) + 
        ggtitle("Bicycle - Average Count by Time of Day") + 
        ylab(NULL) + xlab("")

#plot1 +
#    facet_grid(. ~ year) + 
#    scale_x_discrete(breaks = time_period_axis_breaks)

convertToPlotly(plot1, renameTraces = TRUE, height = 450, width = 800, animated = TRUE, animationPrefix = "Year")
                            


## ---- avg_count_time_of_day_by_year_and_weekday --------
                            
#options(repr.plot.width = 18, repr.plot.height = 4)


plot1 <- cycle_counter_data_2017_2021 %>%

    filter(traffic_mode == "bicycle") %>%
    group_by(year, weekday, isWeekEnd, time) %>% 
    summarise(total = sum(count),              
              average = mean(count)) %>%
    
    ggplot(aes(time, average, group = year, colour = year,
               text = paste("Average count -", weekday, time, "-" , round(average, 2)), 
               frame = weekday, ids = year
              )) + 
        geom_line() +
        cop_cycling_theme + 
        scale_y_continuous(label = scales::number_format(accuracy = 1), breaks = seq(0, 2, 1)) +
        guides(linetype = FALSE) +
        theme(axis.text.x = element_text(size = 11, angle = 45, vjust = 0.5), 
              legend.title = element_blank()) + 
        ggtitle("Bicycle - Average Count by Time of Day") + 
        ylab(NULL) + xlab("")

#plot1 +
#    facet_grid(. ~ weekday) + 
#    scale_x_discrete(breaks = time_period_axis_breaks)

convertToPlotly(plot1, height = 450, width = 800, animated = TRUE, animationPrefix = "Day of the week")



## ---- counts_time_of_day_by_month_and_year --------
counts_by_month <- padding_cycle_counter_data_2017_2021 %>%

            full_join(cycle_counter_data_2017_2021 %>%

                            filter(traffic_mode == "bicycle") %>%
                            group_by(year, month, time) %>% 
                            summarise(total = sum(count),              
                                      average = mean(count)) 
                     ) %>%

            mutate_at(vars(year, time), as.ordered) %>%
            mutate(month = factor(month, levels = month.abb)) %>%
            mutate(pseudo_point = if_else(is.na(average), 0, 1), # need this and next to generate full trace sets and legend or animation breaks
                   tooltip = if_else((pseudo_point == 0), 
                                     "", 
                                     paste("Average count,", month, ",", time, "-" , round(average, 2)))
                   ) %>%
            mutate_at(vars(total, average), ~ replace_na(., -Inf))


## ---- total_count_time_of_day_by_month_and_year --------

plot1 <- counts_by_month %>%

    ggplot(aes(time, total, group = month, colour = month, #alpha = pseudo_point, # need to comment out to prevent legend in ggplotly being faded out ...
               text = tooltip, frame = year, ids = month
              )) + 
        geom_line() +
        cop_cycling_theme + 
        scale_colour_brewer(palette = "Paired", direction = -1) + # Set3
        guides(alpha = FALSE) + 

        theme(axis.text.x = element_text(size = 11, angle = 45, vjust = 0.5), 
              legend.title = element_blank()) + 
        ggtitle("Bicycle - Total Count by Time of Day") + 
        ylab("") + xlab("")


#plot1 + 
#    facet_grid(. ~ year)

convertToPlotly(plot1, renameTraces = TRUE, height = 520, width = 820,
                legend = list(font = list(size = 11)), 
                animated = TRUE, animationPrefix = "Year")





## ---- average_count_time_of_day_by_month_and_year --------

plot1 <- counts_by_month %>%

    ggplot(aes(time, average, group = month, colour = month, #alpha = pseudo_point, # need to comment out to prevent legend in ggplotly being faded out ...
               text = tooltip, frame = year, ids = month
              )) + 
        geom_line() +
        cop_cycling_theme + 
        scale_colour_brewer(palette = "Paired", direction = -1) + # Set3
        guides(alpha = FALSE) + 

        theme(axis.text.x = element_text(size = 11, angle = 45, vjust = 0.5), 
              legend.title = element_blank()) + 
        ggtitle("Bicycle - Average Count by Time of Day") + 
        ylab("") + xlab("")


#plot1 + 
#    #geom_point(aes(alpha = pseudo_point)) +
#    scale_x_discrete(breaks = time_period_axis_breaks) +
#    facet_grid(. ~ year)

convertToPlotly(plot1, renameTraces = TRUE, height = 520, width = 820, 
                legend = list(font = list(size = 11)), 
                animated = TRUE, animationPrefix = "Year")

#rm(counts_by_month)



## ---- average_count_by_location_and_year --------

plot1 <- count_by_location %>%
    mutate_at(vars(average, count), ~replace(., . == -Inf, NA)) %>%
    group_by(LocalAuthority, Location, year) %>%
    summarise(tally = n(),
             average = mean(average, na.rm = TRUE),
             count = sum(count, na.rm = TRUE)) %>%
    mutate_at(vars(count), ~replace(., is.na(average), -Inf))  %>%
    mutate_at(vars(average, count), ~replace(., is.na(.), -Inf)) %>%
    
    
    ggplot(aes(count, average, group = Location, colour = LocalAuthority,
               text = paste(Location, "-", count,  "bikes in", year, ", on average", round(average, 2), "each hour"),
               frame = year, ids = Location,
              )) + 
        geom_point(aes(size = average), alpha = 0.35) + 
        geom_text(aes(label = Location, colour = LocalAuthority)) +
        #scale_x_log10("Total yearly count", labels = scales::unit_format(unit = "K", scale = 1e-3)) + 
        scale_x_log10("Total yearly count", labels = scales::comma) + 
        scale_y_log10("Average hourly count") +
        guides(size = FALSE) + 
        ggtitle("Bicycle Count by Location") + 
            cop_cycling_theme +
            theme(legend.position = "none")

#plot1


convertToPlotly(plot1, height = 680, width = 680, 
                xaxis = list(showgrid = TRUE, tickfont = list(size = 14), title = list(font = list(size = 16))), 
                yaxis = list(tickfont = list(size = 14), title = list(font = list(size = 18))), 
                animated = TRUE, animationPrefix = "Year")



## ---- average_count_by_location_and_month --------

plot1 <- count_by_location %>%
    filter(between(monthOfYear, start_date, end_date)) %>%

    mutate(monthOfYear2 = factor(format(monthOfYear, "%b-%Y"))) %>%
    mutate(monthOfYear2 = fct_reorder(monthOfYear2, monthOfYear)) %>%
    select(-monthOfYear) %>%
    rename(monthOfYear = monthOfYear2) %>%
    relocate(monthOfYear, .after = month) %>%
    mutate_at(vars(average, count), ~replace(., . == -Inf, Inf)) %>%              
                    
    ggplot(aes(count, average, group = Location, colour = LocalAuthority, #alpha = 0.35 * pseudo_point, 
               text = paste(Location, "-", count,  "bikes in", monthOfYear, ", on average", round(average, 2), "each hour"),
               frame = monthOfYear, ids = Location,
              )) + 
        #geom_point() + 
        geom_point(aes(size = average, alpha = 0.35 + pseudo_point)) + 
        geom_text(aes(label = Location, colour = LocalAuthority)) +
        #scale_x_log10("Total monthly count", labels = scales::unit_format(unit = "K", scale = 1e-3)) + 
        scale_x_log10(labels = scales::comma) +  
        scale_y_log10(labels = scales::comma) +
        guides(size = FALSE) + 
        ylab("Average hourly count") + xlab("Total monthly count") + 
        ggtitle("Bicycle Count by Location") + 
            cop_cycling_theme +
            theme(legend.position = "none")

#plot1


convertToPlotly(plot1, height = 680, width = 680, animated = TRUE, animationPrefix = "Month",
                xaxis = list(showgrid = TRUE, tickfont = list(size = 14), title = list(font = list(size = 16))),  
                yaxis = list(tickfont = list(size = 14), title = list(font = list(size = 18)))
               )



## ---- total_count_by_location_and_month --------

# adapted from https://plotly.com/r/sliders

filtered_data <- count_by_location %>%
    filter(between(monthOfYear, start_date, end_date)) %>%
    droplevels()

locations <- levels(filtered_data$Location)


steps <- list()
plot_tmp <- plot_ly(height = 450, width = 820)

for (i in seq_along(locations)) {
 
 plot_tmp <- add_lines(plot_tmp,
                       x = filtered_data$monthOfYear[filtered_data$Location == locations[i]],
                       y = filtered_data$count[filtered_data$Location == locations[i]],
                       #xref = "monthOfYear", yref = "average",
                       text = filtered_data$tooltip[filtered_data$Location == locations[i]],
                       visible = (i == 1),
                       name = locations[i],

                        type = "scatter", 
                        mode = "lines", 
                        hoverinfo = "text", 
                        #line = list(color = "00CED1"), 
                        color = unique(filtered_data$LocalAuthority[filtered_data$Location == locations[i]]), # 
                        #colors = "Blues",
                        showlegend = FALSE)

  step <- list(args = list("visible", rep(FALSE, length(locations))),
               label = #paste0(
                       locations[i], #", ", unique(filtered_data$LocalAuthority[filtered_data$Location == locations[i]])),              
               method = "restyle")
  step$args[[2]][i] <- TRUE  
  steps[[i]] = step 
}  

plot_tmp %>%
  layout(#title = "Monthly Totals", 
          xaxis = list(tickfont = tickFont),
          yaxis = list(tickfont = tickFont, title = list(text = "Monthly Totals", font = list(size = 20))), #, side = "right", rangemode = "tozero"),
          margin = list(l = 5),
          #sliders = list(list(active = 0,
          #                   currentvalue = list(prefix = "City/town: "),
          #                   steps = steps))
          updatemenus = list(list(active = 0, x = 0, y = 1.12,
                                  buttons = steps
                                 )) # end dropdown
    ) # end layout

rm(filtered_data)



## ---- total_count_by_location_grouped_by_year --------

# adapted from https://stackoverflow.com/a/54865094
# ideally would group into traces but this is not possible in the r implementation

filtered_data <- count_by_location %>%
    filter(monthOfYear <= end_date) %>%
    droplevels()

locations <- levels(filtered_data$Location)
years <- levels(filtered_data$year)


steps <- list()
plot_tmp <- plot_ly(height = 450, width = 820)

for (i in seq_along(locations)) {
    for (j in seq_along(years)) {
     
        plot_tmp <- add_lines(plot_tmp, data = filtered_data %>%
                                            filter((Location == locations[i]) & year == years[j]),
                              
                              x = ~ month, 
                              y = ~ count, 
                              text = ~ tooltip, 
                              visible = (i == 1), 
                              name = years[j], #paste0(locations[i], "-", years[j]),

                              type = "scatter",
                              mode = "lines",
                              hoverinfo = "text",
                              color = ~ year,
                              showlegend = TRUE)
        
    } # end iteration over years

    
    step <- list(args = list("visible", rep(FALSE, length(locations) * length(years))),
               label = #paste0(
                       locations[i], #", ", unique(filtered_data$LocalAuthority[filtered_data$Location == locations[i]])),
               method = "restyle")
    for (j in seq_along(years))    
        step$args[[2]][((i - 1) * length(years)) + j] <- TRUE
    steps[[i]] = step

} # end iteration over locations

plot_tmp %>%
    layout(#title = "Monthly Totals",
           xaxis = list(tickfont = tickFont, title = list(text = "")),
           yaxis = list(tickfont = tickFont, title = list(text = "Monthly Totals", font = list(size = 20))), #rangemode = "tozero"),
           margin = list(l = 5),
           #sliders = list(list(active = 0,
           #                    currentvalue = list(prefix = "City/town: "),
           #                    steps = steps))
           updatemenus = list(list(active = 0, x = 0, y = 1.12,
                                  buttons = steps
                                 )) # end dropdown
    ) # end layout

rm(filtered_data)




## ---- counter_installation_data --------

counter_data <- reporting_sites %>%

    distinct(siteID, site, LocalAuthority, Location, RoadName, CycleCounter) %>%
    filter(!is.na(CycleCounter)) %>% # & !is.na(Location)) %>%
    left_join(cycle_counter_data_2017_2021 %>%
                     distinct(siteID, site, Location, RoadName, traffic_mode, total_by_site) %>%
                     filter(traffic_mode == "bicycle") %>%
                     select(-traffic_mode)) %>% 

    mutate(Location = fct_explicit_na(Location, na_level = "Unspecified Location")) %>%
    mutate(LocationLabel = paste0(Location, " (", replace_na(LocalAuthority, ""), ")"))%>%
    mutate_at(vars(LocalAuthority, Location, LocationLabel), as.factor) %>%

    group_by(LocalAuthority, Location, LocationLabel, CycleCounter, total_by_site) %>%
    tally() %>%
    rename(bicycle_counters = n) %>%
    arrange(CycleCounter)


counter_data <- counter_data %>%

    group_by(LocalAuthority, Location, LocationLabel, CycleCounter) %>%
    summarise(bicycle_counters = sum(bicycle_counters)) %>%

    full_join(counter_data %>%

                group_by(LocalAuthority, Location) %>%
                summarise(LatestInstallation = max(CycleCounter))
              ) %>%

    filter(Location != "Unspecified Location")


## counters_installed --------

#options(repr.plot.width = 6.8, #8, # 9.4, 
#        repr.plot.height = 12) # 9.6) # change for printing equal plot size alongside each other    

plot1 <- counter_data %>%


    group_by(LocalAuthority, Location) %>%
    summarise(bicycle_counters = sum(bicycle_counters), 
              LatestInstallation = max(LatestInstallation)) %>%

    ggplot(aes(y = fct_reorder(Location, LatestInstallation), x = bicycle_counters, 
               #frame = CycleCounter, ids = Location, 
               text = paste0(Location, " (", LocalAuthority, ") - ", bicycle_counters) #, " installed ", CycleCounter)
              )) +
        geom_segment(aes(xend = 0, yend = Location), alpha = 0.45) +
        geom_point(aes(colour = LocalAuthority, alpha = 0.45), size = 2) + 
        ylab("") + # Location") +
        xlab("No. of Bicycle Counters Installed\n") + # pad bottom to align x-axes
        #ggtitle("No. of Bicycle Counters Installed") + 
        cop_cycling_theme + 
        theme(axis.text.y = element_text(size = 10),
              legend.position = "none") +
        scale_fill_hue(c = 20)


## counter_installation_dates --------


datebreaks <- seq(min(counter_data$CycleCounter), max(counter_data$CycleCounter), by = "3 months")

#options(repr.plot.width = 6.8, repr.plot.height = 13) # 10)

plot2 <- counter_data %>%


    ggplot(aes(y = fct_reorder(Location, LatestInstallation), x = CycleCounter, 
               frame = CycleCounter, ids = Location, 
               text = paste0(Location, " (", LocalAuthority, ") - ", bicycle_counters, " installed ", CycleCounter)
              )) +
        geom_segment(aes(xend = min(CycleCounter), yend = Location), alpha = 0.45) +
        geom_point(aes(colour = LocalAuthority), size = 2, alpha = 0.45, show.legend = FALSE) + 
        ylab("") + # Location") +
        scale_x_date("Counter Installation Date",
                     breaks = datebreaks, labels = scales::date_format("%b-%Y")) +
        cop_cycling_theme + 
        
scale_fill_hue(c = 20) +
        theme(axis.text.y = element_text(size = 10),
              axis.text.x = element_text(size = 11, angle = 45, vjust = 0.8, hjust = 0.8), 
              legend.position = "none")


options(repr.plot.width = 18, repr.plot.height = 12)
gridExtra::grid.arrange(plot2, plot1, ncol = 2)    





