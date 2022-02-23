
## ---- bicycle_counts --------

label_all_bicycle_providers <- "All Bicycle Counters" #Data Providers"

filtered_data <- padding_cycle_counter_data_from_2017 %>%
    full_join(as.data.frame(levels(cycle_counter_data_from_2017$Provider)),
                  by = character()) %>%
    full_join(as.data.frame(levels(cycle_counter_data_from_2017$traffic_mode)),
                  by = character()) %>%
    rename_with(~c(names(padding_cycle_counter_data_from_2017), "Provider", "traffic_mode")) %>%
    filter((traffic_mode == "bicycle") & (monthOfYear <= end_date)) %>%
    select(- c(monthOfYear, time)) %>%

    full_join(cycle_counter_data_from_2017 %>%
        filter(traffic_mode == "bicycle") %>%
        group_by(Provider, traffic_mode, year, month) %>%
        summarise(average = mean(count, na.rm = TRUE), 
                  count = sum(count, na.rm = TRUE)
                 ) %>%
             
        bind_rows(cycle_counter_data_from_2017 %>%
                    filter(traffic_mode == "bicycle") %>%
                    mutate(Provider = label_all_bicycle_providers) %>%
                    group_by(Provider, traffic_mode, year, month) %>%
                    summarise(average = mean(count, na.rm = TRUE), 
                              count = sum(count, na.rm = TRUE)
                    ) 
                )
             ) %>%

        mutate(month = factor(month, levels = month.abb),
               monthOfYear = parse_date(paste0(month, "-", year), format = "%b-%Y")) %>%
        relocate(monthOfYear, .after = month) %>%
        mutate_at(vars(year), as.ordered) %>%
        mutate_at(vars(Provider, traffic_mode), as.factor) %>%
        mutate_at(vars(Provider), ~ fct_relevel(., c(label_all_bicycle_providers, default_provider))) %>%
        mutate_at(vars(Provider), ~ fct_relevel(., "John Muir Way", after = Inf)) %>%
        mutate(pseudo_point = if_else(is.na(average), 0, 1),
               tooltip = if_else((pseudo_point == 0), 
                                 "", 
                                 paste(Provider, "-", formatNumber(count), paste0(traffic_mode, "s"), "-", month, year))
                   ) %>%
        mutate_at(vars(count, pseudo_point), as.integer) %>%
        mutate_at(vars(average, count), ~ replace_na(., -Inf)) 



providers <- levels(filtered_data$Provider)
years <- levels(filtered_data$year) 

steps <- list()
plot_tmp <- plot_ly(height = 380, width = 700)


for (i in seq_along(providers)) {
    for (j in seq_along(years)) {
        
        plot_tmp <- add_trace( #add_lines(
                        plot_tmp, data = filtered_data %>%
                                                filter((Provider == providers[i]) & year == years[j]),
                              
                              x = ~ month, 
                              y = ~ count, 
                              text = ~ tooltip, 
                              visible = (providers[i] == label_all_bicycle_providers), 
                              name = years[j], 

                              type = "scatter",
                              mode = "lines+markers",
                              hoverinfo = "text",
                              color = ~ year,
                              showlegend = TRUE)

    } # end iteration over years

    
    step <- list(args = list("visible", rep(FALSE, length(providers) * length(years))),
               label = providers[i],
               method = "restyle")
    for (j in seq_along(years))    
        step$args[[2]][((i - 1) * length(years)) + j] <- TRUE
    steps[[i]] = step

} # end iteration over providers


plot_tmp %>%
    layout(xaxis = list(tickfont = tickFont, title = list(text = ""), ticks = "outside", showgrid = FALSE, 
                        zeroline = TRUE, rangemode = "tozero"), # both ignored post restyle :@
           yaxis = list(tickfont = tickFont, title = list(text = "Bicycle", font = list(size = 20)), ticks = "outside"), 
           margin = list(l = 5),
        
           annotations = list(x = -0.075, y = -0.25, text = "<b>Select Data Provider</b>", font = list(size = 14), 
                              yref = "paper", xref = "paper", xanchor = "left", yanchor = "bottom", showarrow = FALSE),
           updatemenus = list(list(active = (which(str_detect(providers, fixed(label_all_bicycle_providers, TRUE))) - 1), 
                                   x = 0.72, y = -0.15, direction = "up",
                                   buttons = steps
                                )) # end dropdown
    ) %>%
    config(displayModeBar = FALSE) 


rm(label_all_bicycle_providers)



## ---- pedestrian_counts_nmf --------

plot1 <- cycle_counter_data_from_2017 %>%
            filter((Provider == default_provider) & (traffic_mode == "pedestrian")) %>%
            group_by(traffic_mode, year, month) %>%
            summarise(average = mean(count), 
                      count = sum(count)) %>%
            mutate(tooltip = paste(formatNumber(count), paste0(traffic_mode, "s"), "-", month, year)) %>%


    ggplot(aes(month, count, group = year, colour = year, text = tooltip)) +
        geom_line(size = 0.5) +
        geom_point(size = 1) +
        scale_y_continuous(labels = scales::label_number_si()) +
        ylab("") + xlab("") + #  - Count by Month# need to set fonts in plotly for consistency with corresponding plot for cycling
        cop_cycling_theme +
        theme(legend.title = element_blank(),
              panel.border = element_blank()) # for consistency with plotly without border settings



convertToPlotly(plot1, height = 350, width = 700, 
                xaxis = list(tickfont = tickFont),
                yaxis = list(tickfont = tickFont, title = list(text = "Pedestrian", font = list(size = 20))),
                legend = list(font = list(size = 12), tracegroupgap = 1)
               ) %>%
    config(displayModeBar = FALSE)



## ---- summary_tables_nmf --------

cycle_counter_data_from_2017 %>%

    filter(Provider == default_provider) %>%

    group_by(Provider, traffic_mode, year) %>%
    summarise(count = sum(count, na.rm = TRUE)) %>%
    arrange(year) %>% # or messes up year order if starts with provider with NAs in any one year - pushes to end
      
    pivot_wider(names_from = year, values_from = count) %>%
    rename_with(~ gsub("_", " ", .x, fixed = TRUE)) %>%
    ungroup() %>%

    #mutate("total" = reduce(select(., matches("\\b\\d{4}\\b")), `+`)) %>% # can't deal with NAs

    rowwise() %>%   
    mutate(total = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>%
    mutate_if(is.numeric, scales::comma) %>%
    mutate_at(vars(matches("\\b\\d{4}\\b")), ~ replace_na(., "-")) %>%
    rename(" " = Provider) %>%
    #column_to_rownames("Provider") %>%

    kable(caption = "Total count per year") %>%
    kable_paper(c("striped", "responsive"), full_width = FALSE, position = "left") 


cycle_counter_data_from_2017 %>%

    filter(Provider == default_provider) %>%

    group_by(Provider, traffic_mode, year, date, siteID, time) %>%
    summarise(count = sum(count, na.rm = TRUE)) %>%

    group_by(Provider, traffic_mode, year, date, siteID) %>%
    summarise(count = sum(count, na.rm = TRUE)) %>%

    group_by(Provider, traffic_mode, year, date) %>%
    summarise(average = mean(count, na.rm = TRUE)) %>%

    group_by(Provider, traffic_mode, year) %>%
    summarise(average = round(mean(average, na.rm = TRUE))) %>%

  
    arrange(year) %>%

    pivot_wider(names_from = year, values_from = average) %>%
    rename_with(~ gsub("_", " ", .x, fixed = TRUE)) %>%
    mutate_at(vars(matches("\\b\\d{4}\\b")), ~ replace_na(., "-")) %>%
    rename(" " = Provider) %>%

    kable(caption = "Average daily count across counters") %>%
    kable_paper(c("striped", "responsive"), full_width = FALSE, position = "left") 


cycle_counter_data_from_2017 %>%

    filter(Provider == default_provider) %>%

    group_by(Provider, traffic_mode, year, date, time) %>%
    summarise(count = sum(count, na.rm = TRUE)) %>%

    group_by(Provider, traffic_mode, year, date) %>%
    summarise(average = mean(count, na.rm = TRUE)) %>%

    group_by(Provider, traffic_mode, year) %>%
    summarise(average = round(mean(average, na.rm = TRUE))) %>%


    arrange(year) %>%
  
    pivot_wider(names_from = year, values_from = average) %>%
    rename_with(~ gsub("_", " ", .x, fixed = TRUE)) %>%
    mutate_at(vars(matches("\\b\\d{4}\\b")), ~ replace_na(., "-")) %>%
    rename(" " = Provider) %>%

    kable(caption = "Average daily count, overall", table.attr = "border-bottom: 15px solid") %>%
    kable_paper(c("striped", "responsive"), full_width = FALSE, position = "left") 


cycle_counter_data_from_2017 %>%

    filter(Provider == default_provider) %>%

    group_by(Provider, traffic_mode, year, date, time) %>%
    summarise(average = mean(count, na.rm = TRUE)) %>%

    group_by(Provider, traffic_mode, year) %>%
    summarise(average = round(mean(average, na.rm = TRUE), 1)) %>%
  

    arrange(year) %>%

    pivot_wider(names_from = year, values_from = average) %>%
    rename_with(~ gsub("_", " ", .x, fixed = TRUE)) %>%
    mutate_at(vars(matches("\\b\\d{4}\\b")), ~ replace_na(., "-")) %>%
    rename(" " = Provider) %>%

    kable(caption = "Average hourly count") %>%
    kable_paper(c("striped", "responsive"), full_width = FALSE, position = "left")



## ---- summary_tables_councils --------

cycle_counter_data_from_2017 %>%

    filter(Provider != default_provider) %>%

    group_by(Provider, traffic_mode, year) %>%
    summarise(count = sum(count, na.rm = TRUE)) %>%
    arrange(year) %>% # or messes up year order if starts with provider with NAs in any one year - pushes to end
      
    pivot_wider(names_from = year, values_from = count) %>%
    rename_with(~ gsub("_", " ", .x, fixed = TRUE)) %>%
    ungroup() %>%

    #mutate("total" = reduce(select(., matches("\\b\\d{4}\\b")), `+`)) %>% # can't deal with NAs

    rowwise() %>%   
    mutate(total = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>%
    mutate_if(is.numeric, scales::comma) %>%
    mutate_at(vars(matches("\\b\\d{4}\\b")), ~ replace_na(., "-")) %>%
    rename(" " = Provider) %>%

    kable(caption = "Total count per year") %>%
    kable_paper(c("striped", "condensed", "responsive"), full_width = FALSE, position = "left") 


cycle_counter_data_from_2017 %>%

    filter(Provider != default_provider) %>%

    group_by(Provider, traffic_mode, year, date, siteID, time) %>%
    summarise(count = sum(count, na.rm = TRUE)) %>%

    group_by(Provider, traffic_mode, year, date, siteID) %>%
    summarise(count = sum(count, na.rm = TRUE)) %>%

    group_by(Provider, traffic_mode, year, date) %>%
    summarise(average = mean(count, na.rm = TRUE)) %>%

    group_by(Provider, traffic_mode, year) %>%
    summarise(average = round(mean(average, na.rm = TRUE))) %>%

  
    arrange(year) %>%

    pivot_wider(names_from = year, values_from = average) %>%
    rename_with(~ gsub("_", " ", .x, fixed = TRUE)) %>%
    mutate_at(vars(matches("\\b\\d{4}\\b")), ~ replace_na(., "-")) %>%
    rename(" " = Provider) %>%

    kable(caption = "Average daily count across counters") %>%
    kable_paper(c("striped", "condensed", "responsive"), full_width = FALSE, position = "left") 


cycle_counter_data_from_2017 %>%

    filter(Provider != default_provider) %>%

    group_by(Provider, traffic_mode, year, date, time) %>%
    summarise(count = sum(count, na.rm = TRUE)) %>%

    group_by(Provider, traffic_mode, year, date) %>%
    summarise(average = mean(count, na.rm = TRUE)) %>%

    group_by(Provider, traffic_mode, year) %>%
    summarise(average = round(mean(average, na.rm = TRUE))) %>%


    arrange(year) %>%
  
    pivot_wider(names_from = year, values_from = average) %>%
    rename_with(~ gsub("_", " ", .x, fixed = TRUE)) %>%
    mutate_at(vars(matches("\\b\\d{4}\\b")), ~ replace_na(., "-")) %>%
    rename(" " = Provider) %>%

    kable(caption = "Average daily count, overall", table.attr = "border-bottom: 15px solid") %>%
    kable_paper(c("striped", "condensed", "responsive"), full_width = FALSE, position = "left") 


cycle_counter_data_from_2017 %>%

    filter(Provider != default_provider) %>%

    group_by(Provider, traffic_mode, year, date, time) %>%
    summarise(average = mean(count, na.rm = TRUE)) %>%

    group_by(Provider, traffic_mode, year) %>%
    summarise(average = round(mean(average, na.rm = TRUE), 1)) %>%
  

    arrange(year) %>%

    pivot_wider(names_from = year, values_from = average) %>%
    rename_with(~ gsub("_", " ", .x, fixed = TRUE)) %>%
    mutate_at(vars(matches("\\b\\d{4}\\b")), ~ replace_na(., "-")) %>%
    rename(" " = Provider) %>%

    kable(caption = "Average hourly count") %>%
    kable_paper(c("striped", "condensed", "responsive"), full_width = FALSE, position = "left")



##
# ---- summary_tables-original - for NMF data - reference only --------
##
cycle_counter_data_from_2017 %>%

    select(traffic_mode, year, count) %>%
    group_by(traffic_mode, year) %>%
    summarise(count = sum(count, na.rm = TRUE))  %>%
  
    pivot_wider(names_from = year, values_from = count) %>%
    rename_with(~ gsub("_", " ", .x, fixed = TRUE)) %>%
    ungroup() %>%

    mutate("total" = reduce(select(., matches("\\b\\d{4}\\b")), `+`)) %>%
    mutate_if(is.numeric, scales::comma) %>%

    kable(caption = "Total count per year") %>%
    kable_paper(c("striped", "responsive"), full_width = FALSE, position = "left")



## ---- summary_tables_max_counts_hourly --------

reporting_sites %>%
    mutate(siteID = coalesce(siteID, externalId)) %>%
    select(siteID, externalId, LocalAuthority, RoadType) %>%
    mutate_at(vars(RoadType), ~ fct_explicit_na(RoadType, na_level = "-")) %>%

    right_join(cycle_counter_data_from_2017 %>%
                group_by(Provider, traffic_mode) %>%
                mutate(max_count = max(count)) %>%
                mutate_at(vars(date), ~ format(date, format = "%d %b %Y")) %>%
                filter(count == max_count)
                  ) %>%
    select(Provider, traffic_mode, LocalAuthority, Location, RoadName, RoadType, date, #year, month, 
           weekday, time, count) %>%
    arrange(traffic_mode, desc(count), date) %>% 

    rename_with(~ snakecase::to_upper_camel_case(.)) %>%

    kable(caption = "Maximum hourly count for any one counter, across data providers") %>%
    kable_paper(c("striped", "condensed", "responsive"), full_width = FALSE, position = "left") 
    


## ---- summary_tables_max_counts_daily --------
                            
reporting_sites %>%
    mutate(siteID = coalesce(siteID, externalId)) %>%
    select(siteID, LocalAuthority, RoadType) %>%
    mutate_at(vars(RoadType), ~ fct_explicit_na(RoadType, na_level = "-")) %>%

    right_join(cycle_counter_data_from_2017 %>%
                group_by(Provider, traffic_mode, Location, RoadName, date) %>%
                mutate(count = sum(count)) %>%
                group_by(Provider, traffic_mode) %>%
                
                mutate(max_count = max(count)) %>%
                mutate_at(vars(date), ~ format(date, format = "%d %b %Y")) %>%
                filter(count == max_count) %>%
                distinct(Provider, siteID, traffic_mode, Location, RoadName, date, weekday, count)
                  ) %>%
    relocate(Provider, .before = siteID) %>%
    relocate(RoadType, .after = RoadName) %>%
    relocate(traffic_mode, .after = Provider) %>%
    select(-count, everything(), count) %>%
    arrange(traffic_mode, desc(count), date) %>% #, month) %>%
    select(-siteID) %>%
    
    rename_with(~ snakecase::to_upper_camel_case(.)) %>%

    kable(caption = "Maximum daily count for any one counter, across data providers") %>%
    kable_paper(c("striped", "condensed", "responsive"), full_width = FALSE, position = "left") 



## ---- filtered_data_counts_time_of_day_by_year_and_weekday --------

filtered_data <-  padding_cycle_counter_data_from_2017 %>%
    full_join(as.data.frame(levels(cycle_counter_data_from_2017$Provider)),
                  by = character()) %>%
    full_join(as.data.frame(levels(cycle_counter_data_from_2017$traffic_mode)),
                  by = character()) %>%
    full_join(as.data.frame(levels(cycle_counter_data_from_2017$weekday)),
                  by = character()) %>%

    rename_with(~c(names(padding_cycle_counter_data_from_2017), "Provider", "traffic_mode", "weekday")) %>%
    filter((traffic_mode == "bicycle") & (monthOfYear <= end_date)) %>%
    select(- c(month, monthOfYear)) %>%

    mutate(end_time = format(as_datetime(time, format = "%H:%M") + hours(1), format = "%H:%M")) %>%
    relocate(end_time, .after = time) %>%

    full_join(cycle_counter_data_from_2017 %>%
        filter(traffic_mode == "bicycle") %>%
        group_by(Provider, traffic_mode, year, weekday, isWeekEnd, time) %>%
        summarise(average = mean(count, na.rm = TRUE), 
                  count = sum(count, na.rm = TRUE)
                 )) %>%

    
        mutate(weekday = factor(weekday, levels = levels(wday(1, label = TRUE)))) %>%
        mutate(isWeekEnd = (as.integer(weekday) %in% c(1, 7))) %>% #between(as.integer(weekday), 6, 7)) %>%
        relocate(isWeekEnd, .after = "weekday") %>%

        mutate_at(vars(year), as.ordered) %>%
        mutate_at(vars(Provider, traffic_mode, time), as.factor) %>%
        mutate_at(vars(Provider), ~ fct_relevel(., default_provider)) %>%
        mutate_at(vars(Provider), ~ fct_relevel(., "John Muir Way", after = Inf)) %>%

        mutate(pseudo_point = if_else(is.na(average), 0, 1),
               tooltip = if_else((pseudo_point == 0), 
                                 "", 
                                 paste(Provider, "- in", year, "on average", round(average, 2), paste0(traffic_mode, "s"), 
                                       weekday, time, "-", end_time))

                   ) %>%
        mutate_at(vars(count, pseudo_point), as.integer) %>%
        mutate_at(vars(average, count), ~ replace_na(., -Inf))


providers <- levels(filtered_data$Provider)
years <- levels(filtered_data$year)
weekdays <- levels(filtered_data$weekday)



## ---- avg_count_time_of_day_by_weekday_and_year --------

steps_by_provider <- list()

plot_tmp <- plot_ly(height = 510, width = 900)


for (i in seq_along(providers)) {
    step_provider <- list(args = list("visible", rep(FALSE, length(providers) * length(years) * length(weekdays))),
                         label = providers[i],
                         method = "restyle")

    for (j in seq_along(years)) { 

        for (k in seq_along(weekdays)) {
            
            plot_tmp <- add_lines(plot_tmp, data = filtered_data %>%
                                                    filter((Provider == providers[i]) & (year == years[j]) & (weekday == weekdays[k])),

                                  x = ~ time, 
                                  y = ~ average, 
                                  text = ~ tooltip, 
                                  visible = (i == 1), 
                                  name = ~ weekday, 

                                  type = "scatter",
                                  mode = "lines",
                                  hoverinfo = "text",
                                  color = ~ weekday,
                                  linetype = ~ isWeekEnd,
                                  showlegend = (j == 1), 
                                  legendgroup = ~ weekday,
                      
                                  transforms = list(
                                      list(type = "filter",
                                           target = ~ Provider,
                                           operation = "=",
                                           value = providers[i]
                                        ),
                                        list(type = "filter",
                                             target = ~ year,
                                             operation = "=",
                                             value = years[1]
                                        )
                                    ) # end transforms 
                                  )


            step_provider$args[[2]][((j - 1) * length(weekdays)) + k + ((i - 1) * length(years) * length(weekdays))] <- TRUE 

        } # end iteration over weekdays
        
    } # end iteration over years
    
    steps_by_provider[[i]] <- step_provider

} # end iteration over providers


plot_tmp %>%
    layout(title = "Bicycle - Average Count by Time of Day",
           xaxis = list(tickfont = tickFont, tickangle = -45, title = list(text = ""), ticks = "outside", showgrid = FALSE, 
                        showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE, zeroline = FALSE), # zeroline setting being ignored ...
           yaxis = list(tickfont = tickFont, title = list(text = "", font = list(size = 20)), 
                        showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE, ticks = "outside"), 
           legend = list(tracegroupgap = 1),
           shapes = list(create_plotly_geom_vline(7, "rgb(217, 217, 217)"), create_plotly_geom_vline(8, "rgb(217, 217, 217)"),
                         create_plotly_geom_vline(13, "rgb(217, 217, 217)"), create_plotly_geom_vline(14, "rgb(217, 217, 217)"), 
                         create_plotly_geom_vline(16, "rgb(217, 217, 217)"), create_plotly_geom_vline(17, "rgb(217, 217, 217)")),
           
           margin = list(l = 5, t = 15),
           sliders = list(list(active = 0, 
                               currentvalue = list(prefix = "Year: "), 
                               pad = list(t = 35), 
                               
                               steps = create_plotly_control_buttons("transforms[1].value", years))
                         ),  
           updatemenus = list(list(active = 0, x = 0.3, y = 1.15, 
                                   buttons = steps_by_provider,
                                   pad = list(t = 10)
                                )) # end dropdown
        
    ) %>%

    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "pan2d"))



## ---- avg_count_time_of_day_by_year_and_weekday --------
                            
steps_by_provider <- list()

plot_tmp <- plot_ly(height = 510, width = 900)


for (i in seq_along(providers)) {
    step_provider <- list(args = list("visible", rep(FALSE, length(providers) * length(weekdays) * length(years))),
                         label = providers[i],
                         method = "restyle")

    for (j in seq_along(weekdays)) { 
                
        for (k in seq_along(years)) {
            
            plot_tmp <- add_lines(plot_tmp, data = filtered_data %>%
                                                    filter((Provider == providers[i]) & (weekday == weekdays[j]) & (year == years[k])),


                                  x = ~ time, 
                                  y = ~ average, 
                                  text = ~ tooltip, 
                                  visible = (i == 1), 
                                  name = ~ year, 

                                  type = "scatter",
                                  mode = "lines",
                                  hoverinfo = "text",
                                  color = ~ year,
                                  showlegend = (j == 1), 
                                  legendgroup = ~ year,
                      
                                  transforms = list(
                                      list(type = "filter",
                                           target = ~ Provider,
                                           operation = "=",
                                           value = providers[i]
                                        ),
                                        list(type = "filter",
                                             target = ~ weekday,
                                             operation = "=",
                                             value = weekdays[1]
                                        )
                                    ) # end transforms 
                                  )


            step_provider$args[[2]][((j - 1) * length(years)) + k + ((i - 1) * length(weekdays) * length(years))] <- TRUE 

        } # end iteration over years
        
    } # end iteration over weekdays
    
    steps_by_provider[[i]] <- step_provider


} # end iteration over providers


plot_tmp %>%
    layout(title = "Bicycle - Average Count by Time of Day",
           xaxis = list(tickfont = tickFont, tickangle = -45, title = list(text = ""), ticks = "outside", showgrid = FALSE, 
                        showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE, zeroline = FALSE), # zeroline setting being ignored ...
           yaxis = list(tickfont = tickFont, title = list(text = "", font = list(size = 20)), 
                        showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE, ticks = "outside"), 
           legend = list(tracegroupgap = 1),
           shapes = list(create_plotly_geom_vline(7, "rgb(217, 217, 217)"), create_plotly_geom_vline(8, "rgb(217, 217, 217)"),
                         create_plotly_geom_vline(13, "rgb(217, 217, 217)"), create_plotly_geom_vline(14, "rgb(217, 217, 217)"), 
                         create_plotly_geom_vline(16, "rgb(217, 217, 217)"), create_plotly_geom_vline(17, "rgb(217, 217, 217)")),
           
           margin = list(l = 5, t = 15),
           sliders = list(list(active = 0, 
                               currentvalue = list(prefix = "Day of the Week: "), 
                               pad = list(t = 35), 
                               
                               steps = create_plotly_control_buttons("transforms[1].value", weekdays))
                         ),  
           updatemenus = list(list(active = 0, x = 0.3, y = 1.15, 
                                   buttons = steps_by_provider,
                                   pad = list(t = 10)
                                )) # end dropdown
    ) %>%

    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "pan2d"))



## ---- filtered_data_counts_time_of_day_by_month_and_year --------

filtered_data <-  padding_cycle_counter_data_from_2017 %>%
    full_join(as.data.frame(levels(cycle_counter_data_from_2017$Provider)),
                  by = character()) %>%
    full_join(as.data.frame(levels(cycle_counter_data_from_2017$traffic_mode)),
                  by = character()) %>%
    rename_with(~c(names(padding_cycle_counter_data_from_2017), "Provider", "traffic_mode")) %>%
    filter((traffic_mode == "bicycle") & (monthOfYear <= end_date)) %>%

    full_join(cycle_counter_data_from_2017 %>%
        filter(traffic_mode == "bicycle") %>%
        group_by(Provider, traffic_mode, year, month, time) %>%
        summarise(average = mean(count, na.rm = TRUE), 
                  count = sum(count, na.rm = TRUE)
                 )) %>%

        mutate(end_time = format(as_datetime(time, format = "%H:%M") + hours(1), format = "%H:%M")) %>%
        relocate(end_time, .after = time) %>%

        mutate(month = factor(month, levels = month.abb)) %>%
        mutate_at(vars(year), as.ordered) %>%
        mutate_at(vars(Provider, traffic_mode, time), as.factor) %>%
        mutate_at(vars(Provider), ~ fct_relevel(., default_provider)) %>%
        mutate_at(vars(Provider), ~ fct_relevel(., "John Muir Way", after = Inf)) %>%

        mutate(pseudo_point = if_else(is.na(average), 0, 1),
               tooltip = if_else((pseudo_point == 0), 
                                 "", 
                                 paste(Provider, "-", month, year, 
                                       "\nOn average", round(average, 2), paste0(traffic_mode, "s,"), time, "-", end_time, 
                                       "\nTotal count:", formatNumber(count)))
                   ) %>%
        mutate_at(vars(count, pseudo_point), as.integer) %>%
        mutate_at(vars(average, count), ~ replace_na(., -Inf)) 


providers <- levels(filtered_data$Provider)
years <- levels(filtered_data$year)
months <- levels(filtered_data$month)


## ---- total_count_time_of_day_by_month_and_year --------

steps_by_provider <- list()

plot_tmp <- plot_ly(height = 510, width = 900)

for (i in seq_along(providers)) {
    step_provider <- list(args = list("visible", rep(FALSE, length(providers) * length(years) * length(months))),
                         label = providers[i],
                         method = "restyle")

    for (j in seq_along(years)) { 

        for (k in seq_along(months)) {

            plot_tmp <- add_lines(plot_tmp, data = filtered_data %>%
                                                    filter((Provider == providers[i]) & (year == years[j]) & (month == months[k])),

                                  x = ~ time, 
                                  y = ~ count, 
                                  text = ~ tooltip, 
                                  visible = (i == 1), 
                                  name = ~ month,  

                                  type = "scatter",
                                  mode = "lines",
                                  hoverinfo = "text",
                                  color = ~ month,
                                  showlegend = (j == 1),
                                  legendgroup = ~ month,
                      
                                  transforms = list(
                                      list(type = "filter",
                                           target = ~ Provider,
                                           operation = "=",
                                           value = providers[i]
                                        ),
                                        list(type = "filter",
                                             target = ~ year,
                                             operation = "=",
                                             value = years[1]
                                        )
                                    ) # end transforms 
                                  )


            step_provider$args[[2]][((j - 1) * length(months)) + k + ((i - 1) * length(years) * length(months))] <- TRUE 

        } # end iteration over months
    
    } # end iteration over years
    
    steps_by_provider[[i]] <- step_provider


} # end iteration over providers



plot_tmp %>%
    layout(title = "Bicycle - Total Count by Time of Day",
           xaxis = list(tickfont = tickFont, tickangle = -45, title = list(text = ""), ticks = "outside", showgrid = FALSE, 
                        showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE, zeroline = FALSE), # zeroline setting being ignored ...
           yaxis = list(tickfont = tickFont, title = list(text = "", font = list(size = 20)), 
                        showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE, ticks = "outside"), 
           legend = list(tracegroupgap = 1),
           margin = list(l = 5, t = 15),
           sliders = list(list(active = 0, 
                               currentvalue = list(prefix = "Year: "), 
                               pad = list(t = 35), 
                               
                               steps = create_plotly_control_buttons("transforms[1].value", years))
                         ),  
           updatemenus = list(list(active = 0, x = 0.3, y = 1.15, 
                                   buttons = steps_by_provider,
                                   pad = list(t = 10)
                                )) # end dropdown
        
    ) %>%

    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "pan2d"))



## ---- average_count_time_of_day_by_month_and_year --------

steps_by_provider <- list()

plot_tmp <- plot_ly(height = 510, width = 900)

for (i in seq_along(providers)) {
    step_provider <- list(args = list("visible", rep(FALSE, length(providers) * length(years) * length(months))),
                         label = providers[i],
                         method = "restyle")

    for (j in seq_along(years)) { 

        for (k in seq_along(months)) {

            plot_tmp <- add_lines(plot_tmp, data = filtered_data %>%
                                                    filter((Provider == providers[i]) & (year == years[j]) & (month == months[k])),

                                  x = ~ time, 
                                  y = ~ average, 
                                  text = ~ tooltip, 
                                  visible = (i == 1), 
                                  name = ~ month,  

                                  type = "scatter",
                                  mode = "lines",
                                  hoverinfo = "text",
                                  color = ~ month,
                                  showlegend = (j == 1),
                                  legendgroup = ~ month,
                      
                                  transforms = list(
                                      list(type = "filter",
                                           target = ~ Provider,
                                           operation = "=",
                                           value = providers[i]
                                        ),
                                        list(type = "filter",
                                             target = ~ year,
                                             operation = "=",
                                             value = years[1]
                                        )
                                    ) # end transforms 
                                  )


            step_provider$args[[2]][((j - 1) * length(months)) + k + ((i - 1) * length(years) * length(months))] <- TRUE 

        } # end iteration over months
    
    } # end iteration over years
    
    steps_by_provider[[i]] <- step_provider


} # end iteration over providers



plot_tmp %>%
    layout(title = "Bicycle - Average Count by Time of Day",
           xaxis = list(tickfont = tickFont, tickangle = -45, title = list(text = ""), ticks = "outside", showgrid = FALSE, 
                        showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE, zeroline = FALSE), # zeroline setting being ignored ...
           yaxis = list(tickfont = tickFont, title = list(text = "", font = list(size = 20)), 
                        showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE, ticks = "outside"), 
           margin = list(l = 5, t = 15),
           legend = list(tracegroupgap = 1),
           sliders = list(list(active = 0,
                               currentvalue = list(prefix = "Year: "), 
                               pad = list(t = 35), 
                               
                               steps = create_plotly_control_buttons("transforms[1].value", years))
                         ),  
           updatemenus = list(list(active = 0, x = 0.3, y = 1.15, 
                                   buttons = steps_by_provider,
                                   pad = list(t = 10)
                                )) # end dropdown
        
    ) %>%

    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "pan2d"))



## ---- average_count_by_location_and_year --------

plot_tmp <- plot_ly(height = 700, width = 900, 
                    data = count_by_location %>%
                                mutate_at(vars(daily_average_by_siteID, count), ~replace(., . == -Inf, NA)) %>%

                                distinct(year, month, LocalAuthority, Location, daily_average, counter_count, daily_average_by_siteID, count) %>%                                
                                group_by(LocalAuthority, Location, year) %>%
                                summarise(tally = n(),
                                         daily_average_by_siteID = mean(daily_average_by_siteID, na.rm = TRUE),
                                         count = sum(count, na.rm = TRUE)) %>%
                                mutate(log_daily_average_by_siteID = if_else(daily_average_by_siteID == 0, 0, log(daily_average_by_siteID)),
                                       tooltip = paste(Location, "-", formatNumber(count),  "bicycles in", year, ", daily average (across counters):", formatNumber(round(daily_average_by_siteID))))%>%
                                mutate_at(vars(count), ~replace(., is.na(daily_average_by_siteID), -Inf)) %>%
                                mutate_at(vars(daily_average_by_siteID, count), ~replace(., is.na(.), -Inf)) %>%
                                mutate_at(vars(log_daily_average_by_siteID), ~replace(., is.na(.), -1))          
                   )


plot_tmp <- add_trace(plot_tmp,
                      
                      x = ~ count,
                      y = ~ daily_average_by_siteID,
                      text = ~ tooltip,               
                      name = ~ LocalAuthority, 
                      frame = ~ year, 
                      ids = ~ Location,

                      type = "scatter", 
                      mode = "markers", 
                      marker = list(sizemode = "diameter"), 
                      sizes = c(1, 25),
                      size = ~ log_daily_average_by_siteID, 
                      fill = ~ "",
                      hoverinfo = "text", 

                      color = ~ LocalAuthority, 
                      #colors = ?,
                      showlegend = TRUE,
                      legendgroup = ~ LocalAuthority
                     )

    
plot_tmp <- add_text(plot_tmp,

                     x = ~ count,
                     y = ~ daily_average_by_siteID,
                     frame = ~ year, 
                     ids = ~ Location,

                     type = "scatter",
                     mode = "text", 
                     text = ~ Location,  
                     textposition = "top middle",
                     color = ~ LocalAuthority, 
                      #textfont = ~ list( size = 8),

                     #name = ~ LocalAuthority,
                     showlegend = FALSE,
                     legendgroup = ~ LocalAuthority
                    )


plot_tmp %>%
  layout(title = list(text = "Bicycle Count by Location", y = 1, x = 0.1, xanchor = "left", yanchor = "top"),
         xaxis = list(type = "log", tickfont = tickFont, title = list(text = "Total yearly count", titlefont = list(size = 20)), 
                      ticks = "outside", showgrid = TRUE, showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE, zeroline = FALSE),
         yaxis = list(type = "log", tickfont = tickFont, ticks = "outside", 
                      title = list(text = "Average daily count - across counters per location", titlefont = list(size = 20)), 
                      ticks = "outside", showgrid = TRUE, showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE), 
         legend = list(title = list(text = "Local Authority", font = list(size = 14)), 
                       itemsizing = "constant", #itemwidth = 30, #sizing ignored ...
                       tracegroupgap = 1), 
         margin = list(l = 5)
          
        ) %>%
        animation_opts(1000, transition = 500, redraw = TRUE) %>%
        animation_slider(currentvalue = list(prefix = paste0("Year: "), font = list(color = "grey"))) 


 
## ---- average_count_by_location_and_month --------


plot_tmp <- plot_ly(height = 650, width = 750, #680, 900, 
                    data = count_by_location %>%

                        filter(between(monthOfYear, start_date, end_date)) %>%

                        mutate(monthOfYear2 = factor(format(monthOfYear, "%b-%Y"))) %>%
                        mutate(monthOfYear2 = fct_reorder(monthOfYear2, monthOfYear)) %>%
                        select(-monthOfYear) %>%
                        rename(monthOfYear = monthOfYear2) %>%
                        relocate(monthOfYear, .after = month) %>%
                        mutate(log_daily_average = if_else(daily_average == 0, 0, log(daily_average)),
                               tooltip = paste(Location, "-", formatNumber(count),  "bicycles in", monthOfYear, ", daily average", round(daily_average, 2))) %>%

                        mutate_at(vars(count), ~replace(., is.na(daily_average), -Inf)) %>%
                        mutate_at(vars(daily_average, count), ~replace(., is.na(.), -Inf)) %>%
                        mutate_at(vars(log_daily_average), ~replace(., is.na(.), -1))
                   )


plot_tmp <- add_trace(plot_tmp,
                      
                      x = ~ count,
                      y = ~ daily_average,
                      text = ~ tooltip,               
                      name = ~ LocalAuthority, 
                      frame = ~ monthOfYear, #year, 
                      ids = ~ Location,

                      type = "scatter", 
                      mode = "markers", 
                      marker = list(sizemode = "diameter"), 
                      sizes = c(1, 25),
                      size = ~ log_daily_average, 
                      fill = ~ "",
                      hoverinfo = "text", 

                      color = ~ LocalAuthority, 
                      #colors = ?,
                      showlegend = TRUE,
                      legendgroup = ~ LocalAuthority
                     )

    
plot_tmp <- add_text(plot_tmp,

                     x = ~ count,
                     y = ~ daily_average,
                     frame = ~ monthOfYear, #year, 
                     ids = ~ Location,

                     type = "scatter",
                     mode = "text", 
                     text = ~ Location,  
                     textposition = "top middle",
                     color = ~ LocalAuthority, 
                      #textfont = ~ list( size = 8),

                     #name = ~ LocalAuthority,
                     showlegend = FALSE,
                     legendgroup = ~ LocalAuthority
                    )


plot_tmp1 <- plot_tmp %>%
  layout(title = list(text = "Bicycle Count by Location", font = list(size = 22), 
                      y = 1, x = 0.1, xanchor = "left", yanchor = "top"),
         xaxis = list(type = "log", tickfont = tickFont, title = list(text = "Total monthly count", font = list(size = 15)), 
                      ticks = "outside", showgrid = TRUE, showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE, zeroline = FALSE),
         yaxis = list(type = "log", tickfont = tickFont, title = list(text = "Average daily count", font = list(size = 15)), 
                      ticks = "outside", showgrid = TRUE, showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE), 
         legend = list(title = list(text = "Local Authority", font = list(size =  14)), font = list(size = 12),
                       itemsizing = "constant", #itemwidth = 30, #sizing ignored ...
                       tracegroupgap = 1), 
         margin = list(l = 5)
          
        ) %>%
        animation_opts(1000, transition = 500, redraw = TRUE) %>%
        animation_slider(currentvalue = list(prefix = paste0("Month: "), font = list(color = "grey"))) 



## ---- average_count_by_siteID_location_and_month --------


plot_tmp <- plot_ly(height = 700, width = 900, #6=50, 750, 
                    data = count_by_location %>%

                        filter(between(monthOfYear, start_date, end_date)) %>%

                        mutate(monthOfYear2 = factor(format(monthOfYear, "%b-%Y"))) %>%
                        mutate(monthOfYear2 = fct_reorder(monthOfYear2, monthOfYear)) %>%
                        select(-monthOfYear) %>%
                        rename(monthOfYear = monthOfYear2) %>%
                        relocate(monthOfYear, .after = month) %>%
                        mutate(log_daily_average_by_siteID = if_else(daily_average_by_siteID == 0, 0, log(daily_average_by_siteID)),
                               tooltip = paste(Location, "-", formatNumber(count),  "bicycles in", monthOfYear, 
                                               ", daily average", formatNumber(round(daily_average_by_siteID)), "(across", 
                                               paste0(counter_count, " counter", if_else(counter_count == 1, ")", "s)"))),
                              ) %>%

                        mutate_at(vars(count), ~replace(., is.na(daily_average_by_siteID), -Inf)) %>%
                        mutate_at(vars(daily_average_by_siteID, count), ~replace(., is.na(.), -Inf)) %>%
                        mutate_at(vars(log_daily_average_by_siteID), ~replace(., is.na(.), -1))
                   )


plot_tmp <- add_trace(plot_tmp,
                      
                      x = ~ count,
                      y = ~ daily_average_by_siteID,
                      text = ~ tooltip,               
                      name = ~ LocalAuthority, 
                      frame = ~ monthOfYear, #year, 
                      ids = ~ Location,

                      type = "scatter", 
                      mode = "markers", 
                      marker = list(sizemode = "diameter"), 
                      sizes = c(1, 25),
                      size = ~ log_daily_average_by_siteID, 
                      fill = ~ "",
                      hoverinfo = "text", 

                      color = ~ LocalAuthority, 
                      #colors = ?,
                      showlegend = TRUE,
                      legendgroup = ~ LocalAuthority
                     )

    
plot_tmp <- add_text(plot_tmp,

                     x = ~ count,
                     y = ~ daily_average_by_siteID,
                     frame = ~ monthOfYear, #year, 
                     ids = ~ Location,

                     type = "scatter",
                     mode = "text", 
                     text = ~ Location,  
                     textposition = "top middle",
                     color = ~ LocalAuthority, 
                      #textfont = ~ list( size = 8),

                     #name = ~ LocalAuthority,
                     showlegend = FALSE,
                     legendgroup = ~ LocalAuthority
                    )


#plot_tmp2 <- 
plot_tmp %>%
  layout(title = list(text = "Bicycle Count by Counter & Location", font = list(size = 18), y = 1, x = 0.1, xanchor = "left", yanchor = "top"),
         xaxis = list(type = "log", tickfont = tickFont, title = list(text = "Total monthly count", font = list(size = 15)), 
                      ticks = "outside", showgrid = TRUE, showline = TRUE, linecolor = "rgb(175, 175, 175)", 
                      mirror = TRUE, zeroline = FALSE),
         yaxis = list(type = "log", tickfont = tickFont, 
                      title = list(text = "Average daily count - across counters per location", font = list(size = 15)), 
                      ticks = "outside", showgrid = TRUE, showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE), 
         legend = list(title = list(text = "Local Authority", font = list(size = 14)), font = list(size = 12),
                       itemsizing = "constant", #itemwidth = 30, #sizing ignored ...
                       tracegroupgap = 1), 
         margin = list(l = 5)
          
        ) %>%
        animation_opts(1000, transition = 500, redraw = TRUE)  %>%
        animation_slider(currentvalue = list(prefix = paste0("Month: "), font = list(color = "grey")))


#bscols(plot_tmp1, plot_tmp2, widths = c(6, 6))



## ---- total_count_by_location_and_month --------

# adapted from https://plotly.com/r/sliders

filtered_data <- count_by_location %>%
    filter(between(monthOfYear, start_date, end_date)) %>%
    droplevels() %>%
    mutate(distinct_location = paste0(Location, "-", site),  
           location_label_site = paste0(Location, " (", site, ")"), 
           location_label_local_authority = paste0(Location, " (", LocalAuthority, ")")) %>%
    mutate(across(c(distinct_location, location_label_site, location_label_local_authority), as.factor)) %>%
    arrange(LocalAuthority, Location) %>%
    mutate(across(starts_with("location_label"), ~ fct_inorder(.))) 

locations <- levels(filtered_data$distinct_location)           


steps <- list()
plot_tmp <- plot_ly(height = 450, width = 950)

for (i in seq_along(locations)) {
    
    plot_tmp <- add_lines(plot_tmp, data = filtered_data %>%
                                            filter(distinct_location == locations[i]),

                       x = ~ monthOfYear, 
                       y = ~ count, 
                       text = ~ tooltip, 
                       visible = (i == 1),
                       name = locations[i],

                        #type = "scatter", 
                        mode = "lines+markers", 
                        hoverinfo = "text", 
                        #line = list(color = "00CED1"), 
                        color = ~ LocalAuthority, 
                        #colors = "Blues",
                        showlegend = FALSE)

  step <- list(args = list("visible", rep(FALSE, length(locations))),
               label = unique(filtered_data$location_label_local_authority[filtered_data$distinct_location == locations[i]]),
               method = "restyle")
  step$args[[2]][i] <- TRUE  
  steps[[i]] <- step 
}  

plot_tmp %>%
  layout(#title = "Monthly Totals", 
          xaxis = list(tickfont = tickFont, tickangle = -45, title = list(text = ""), ticks = "outside", #showgrid = FALSE, 
                       showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE, zeroline = FALSE),
          yaxis = list(tickfont = tickFont, ticks = "outside", title = list(text = ""),
                       showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE, zeroline = FALSE, rangemode = "tozero"),

          margin = list(l = 5),
          #sliders = list(list(active = 0,
          #                   currentvalue = list(prefix = "City/town: "),
          #                   steps = steps))
          updatemenus = list(list(active = 0, x = 0.385, y = 1.12,
                                  buttons = steps
                                 )) # end dropdown
      ) %>% # end layout

      config(modeBarButtonsToRemove = c("zoom2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "pan2d"))

rm(filtered_data)



## ---- historical_weather_scotland_temp --------


plot_tmp <- plot_ly(height = 280, width = 950, line = list(width = 0.65))

plot_tmp <- add_lines(plot_tmp,
                      data = historical_weather_scotland_from_2017 %>%

                                filter((region == "Scotland") & is.na(weather_station) & (metric == "temp")) %>%
                                mutate(tooltip = paste(str_to_title(statistic), paste0(metric, ":"),
                                                       paste0(value, "C"),
                                                       "-", month, year)),
                              
                              y = ~ value,
                              x = ~ monthOfYear,
                              text = ~ tooltip,
                              #width = 1,
                              visible = TRUE,
                              name = ~ if_else(!is.na(statistic), statistic, metric), # order matters

                              type = "scatter",
                              mode = "lines",
                              hoverinfo = "text",
                              color = ~ paste0(if_else(is.na(statistic), "", paste0(statistic, "_")), metric),
                              colors = weather_metrics_colour_scheme,
                              showlegend = TRUE)

plot_tmp %>%
    layout(xaxis = list(title = "", tickfont = tickFont, tickangle = -45, ticks = "outside", #showgrid = FALSE,
                       showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE, zeroline = FALSE),
          yaxis = list(title = "", tickfont = tickFont, ticks = "outside",
                       showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE, zeroline = FALSE, rangemode = "tozero"),

          margin = list(l = 5),
           legend = list(orientation = "h", font = list(size = 14), x = 0, y = 5)
               ) %>%
    config(displayModeBar = FALSE)



## ---- historical_weather_scotland_rainfall --------

plot_tmp <- plot_ly(height = 280, width = 950, line = list(width = 0.65))

plot_tmp <- add_lines(plot_tmp,
                      data = historical_weather_scotland_from_2017 %>%

                                filter((region == "Scotland") & is.na(weather_station) & (metric == "rainfall")) %>%
                                mutate(tooltip = paste(value, "mm", "-", month, year)),

                              y = ~ value,
                              x = ~ monthOfYear,
                              text = ~ tooltip,
                              visible = TRUE,
                              name = ~ if_else(!is.na(statistic), statistic, metric), # order matters

                              type = "scatter",
                              mode = "lines",
                              hoverinfo = "text",
                              color = ~ paste0(if_else(is.na(statistic), "", paste0(statistic, "_")), metric),
                              colors = weather_metrics_colour_scheme,
                              showlegend = TRUE)

plot_tmp %>%
    layout(xaxis = list(title = "", tickfont = tickFont, tickangle = -45, ticks = "outside", #showgrid = FALSE,
                       showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE, zeroline = FALSE),
          yaxis = list(title = "", tickfont = tickFont, ticks = "outside",
                       showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE, zeroline = FALSE, rangemode = "tozero"),

          margin = list(l = 5),
           legend = list(orientation = "h", font = list(size = 14), x = 0, y = 5)
               ) %>%
    config(displayModeBar = FALSE)



## ---- total_count_by_location_grouped_by_year --------

# adapted from https://stackoverflow.com/a/54865094
# ideally would group into traces but this is not possible in the r implementation (as at end 2021)

filtered_data <- count_by_location %>%
    filter(monthOfYear <= end_date) %>%
    droplevels() %>%
    mutate(distinct_location = paste0(Location, "-", site),
           location_label_site = paste0(Location, " (", site, ")"),
           location_label_local_authority = paste0(Location, " (", LocalAuthority, ")")) %>%
    mutate(across(c(distinct_location, location_label_site, location_label_local_authority), as.factor)) %>%
    arrange(LocalAuthority, Location) %>%
    mutate(across(starts_with("location_label"), ~ fct_inorder(.)))

locations <- levels(filtered_data$distinct_location)
years <- levels(filtered_data$year)


steps <- list()
plot_tmp <- plot_ly(height = 450, width = 820)

for (i in seq_along(locations)) {
    for (j in seq_along(years)) {
     
        plot_tmp <- add_lines(plot_tmp, data = filtered_data %>%
                                            filter((distinct_location == locations[i]) & year == years[j]),
                              
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
               label = unique(filtered_data$location_label_local_authority[filtered_data$distinct_location == locations[i]]),
               method = "restyle")
    for (j in seq_along(years))    
        step$args[[2]][((i - 1) * length(years)) + j] <- TRUE
    steps[[i]] <- step

} # end iteration over locations

plot_tmp %>%
    layout(#title = "Monthly Totals",
           xaxis = list(tickfont = tickFont, title = list(text = "")),
           yaxis = list(tickfont = tickFont, title = list(text = "Monthly Totals", font = list(size = 20))), #rangemode = "tozero"),
           margin = list(l = 5),
           #sliders = list(list(active = 0,
           #                    currentvalue = list(prefix = "City/town: "),
           #                    steps = steps))
           updatemenus = list(list(active = 0, x = 0.385, y = 1.12,
                                  buttons = steps
                                 )) # end dropdown
    ) %>% # end layout

    config(modeBarButtonsToRemove = c("autoScale2d", "pan2d"))

rm(filtered_data)



## ---- counters_installed --------

#options(repr.plot.width = 6.8, #8, # 9.4, 
#        repr.plot.height = 12) # 9.6) # change for printing equal plot size alongside each other    

plot1 <- counter_data %>%

    group_by(LocalAuthority, Location) %>%
    summarise(bicycle_counters = sum(bicycle_counters), 
              LatestInstallation = max(LatestInstallation)) %>%

    ggplot(aes(y = fct_reorder2(Location, desc(bicycle_counters), desc(LatestInstallation)), x = bicycle_counters, 
               #frame = CycleCounter, ids = Location, 
              )) +
        geom_segment(aes(xend = 0, yend = Location), alpha = 0.45) +
        geom_point(aes(colour = LocalAuthority, text = paste0(Location, " (", LocalAuthority, ") - ", bicycle_counters)), 
                   alpha = 0.45, size = 1) + 
        guides(col = guide_legend(ncol = 1, keyheight = 0.75)) + 
        scale_x_log10() + 
        ylab("") + # Location") +
        xlab("") + #No. of Bicycle Counters Installed\n\n") + # pad bottom to align x-axes
        #ggtitle("No. of Bicycle Counters Installed") + 
        cop_cycling_theme +
        #scale_fill_hue(c = 20) + 
        theme(axis.text.y = element_text(size = 6),
              axis.text.x = element_text(size = 6),
              axis.title = element_text(size = 11),
              legend.position = "none",
              legend.title = element_text(size = 10),
              legend.text = element_text(size = 8)
             )


## counter_installation_dates --------

datebreaks <- seq(min(counter_data$CycleCounter), max(counter_data$CycleCounter), by = "3 months")


#options(repr.plot.width = 6.8, repr.plot.height = 13) # 10)

plot2 <- counter_data %>%
            mutate(tooltip = paste0(Location, " (", LocalAuthority, ") - ", bicycle_counters, 
                                    " installed ", format(CycleCounter, "%b-%Y"))) %>%

    group_by(LocalAuthority, Location) %>%
    mutate(bicycle_counters = sum(bicycle_counters), 
              LatestInstallation = max(LatestInstallation)) %>%

    ggplot(aes(y = fct_reorder2(Location, desc(bicycle_counters), desc(LatestInstallation)), x = CycleCounter, 
               #frame = CycleCounter, ids = Location, 
              )) +
        geom_segment(aes(xend = min(CycleCounter), yend = Location), alpha = 0.45) +
        geom_point(aes(colour = LocalAuthority, text = tooltip), #shape = Provider), 
                   size = 1, alpha = 0.45, show.legend = FALSE) + 
        ylab("") + # Location") +
        guides(colour = "none") + 
        scale_x_date("", #Counter Installation Date",
                     breaks = datebreaks, labels = scales::date_format("%b-%Y")) +
        cop_cycling_theme +         
        #scale_fill_hue(c = 20) +
        theme(axis.text.y = element_text(size = 6),
              axis.text.x = element_text(size = 6, angle = 45, vjust = 0.8, hjust = 0.8),
              axis.title = element_text(size = 11), 
              legend.position = "none")


#options(repr.plot.width = 12, repr.plot.height = 11) #18/12
#gridExtra::grid.arrange(plot2, 
#                        plot1 + theme(legend.position = "none"), 
#                        create_shared_legend(plot1), 
#                        ncol = 3, widths = c(1.8, 1.8, 1)
#                        )


bscols(
    convertToPlotly(plot2, height = 1000, width = 480, 
                    #xaxis = list(tickfont = tickFont),
                    yaxis = list(tickfont = list(size = 8))           
                   ) %>%
        layout(title = list(text = "\nCounter Installation Dates", font = list(size = 18)), 
               margin = list(l = 5, t = -5)
               ) %>%
        config(displayModeBar = FALSE),

    convertToPlotly(plot1, height = 1000, width = 400, 
                    #xaxis = list(tickfont = tickFont),
                    yaxis = list(tickfont = list(size = 8))           
                   ) %>%
        layout(title = list(text = "\nNo. of Bicycle Counters Installed", font = list(size = 18)), 
               margin = list(l = 5, t = -0.1)
               ) %>%
        config(displayModeBar = FALSE),


    widths = c(6, 5)
)



## ---- total_count_by_location_and_month_lerwick --------

counter_start <- counter_data %>%
    filter(Location == "Lerwick") %>%
    ungroup() %>%
    select(CycleCounter) %>%
    as.integer() %>%
    as_date()

roadNames <- levels(count_by_location_lerwick$RoadName)

steps <- list()
plot_tmp <- plot_ly(height = 450, width = 950)

for (i in seq_along(roadNames)) {
    
    plot_tmp <- add_lines(plot_tmp, data = count_by_location_lerwick %>%
                                              filter(monthOfYear %within% interval(counter_start, end_date)) %>%
                                              filter(RoadName == roadNames[i]),

                       x = ~ monthOfYear, 
                       y = ~ count, 
                       text = ~ tooltip, 
                       visible = (i == 1),
                       name = ~ RoadName,

                        type = "scatter", 
                        mode = "lines", 
                        hoverinfo = "text", 
                        color = ~ RoadName, 
                        showlegend = FALSE)

  step <- list(args = list("visible", rep(FALSE, length(roadNames))),
               label = roadNames[i], 
               method = "restyle")
  step$args[[2]][i] <- TRUE  
  steps[[i]] <- step 
}  

plot_tmp %>%
  layout(#title = "Monthly Totals", 
          xaxis = list(tickfont = tickFont, tickangle = -45, title = list(text = ""), ticks = "outside", #showgrid = FALSE, 
                       showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE, zeroline = FALSE),
          yaxis = list(tickfont = tickFont, ticks = "outside", title = list(text = ""),
                       showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE, zeroline = FALSE, rangemode = "tozero"),

          margin = list(l = 5),
          
          updatemenus = list(list(active = 0, x = 0.185, y = 1.12,
                                  buttons = steps
                                 )) # end dropdown
    ) %>% # end layout

    config(modeBarButtonsToRemove = c("zoom2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "pan2d"))



## ---- historical_weather_lerwick_temp --------

plot_tmp <- plot_ly(height = 280, width = 950, line = list(width = 0.65))

plot_tmp <- add_lines(plot_tmp,
                      data = historical_weather_scotland_from_2017 %>%

                                filter((region == "Scotland") & (weather_station == "Lerwick") & (metric == "temp")) %>%
                                filter(monthOfYear %within% interval(counter_start, end_date)) %>%

                                mutate(tooltip = paste(str_to_title(statistic), paste0(metric, ":"),
                                                       paste0(value, "C"),
                                                       "-", month, year)),
                              
                              y = ~ value,
                              x = ~ monthOfYear,
                              text = ~ tooltip,
                              #width = 1,
                              visible = TRUE,
                              name = ~ if_else(!is.na(statistic), statistic, metric), # order matters

                              type = "scatter",
                              mode = "lines",
                              hoverinfo = "text",
                              color = ~ paste0(if_else(is.na(statistic), "", paste0(statistic, "_")), metric),
                              colors = weather_metrics_colour_scheme,
                              showlegend = TRUE)

plot_tmp %>%
    layout(xaxis = list(title = "", tickfont = tickFont, tickangle = -45, ticks = "outside", #showgrid = FALSE,
                       showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE, zeroline = FALSE),
          yaxis = list(title = "", tickfont = tickFont, ticks = "outside",
                       showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE, zeroline = FALSE, rangemode = "tozero"),

          margin = list(l = 5),
           legend = list(orientation = "h", font = list(size = 14), x = 0, y = 5)
               ) %>%
    config(displayModeBar = FALSE)


## ---- historical_weather_lerwick_rainfall --------

plot_tmp <- plot_ly(height = 280, width = 950, line = list(width = 0.65))

plot_tmp <- add_lines(plot_tmp,
                      data = historical_weather_scotland_from_2017 %>%

                                filter((region == "Scotland") & (weather_station == "Lerwick") & (metric == "rainfall")) %>%
                                filter(monthOfYear %within% interval(counter_start, end_date)) %>%

                                mutate(tooltip = paste(value, "mm", "-", month, year)),

                              y = ~ value,
                              x = ~ monthOfYear,
                              text = ~ tooltip,
                              #width = 1,
                              visible = TRUE,
                              name = ~ if_else(!is.na(statistic), statistic, metric), # order matters

                              type = "scatter",
                              mode = "lines",
                              hoverinfo = "text",
                              color = ~ paste0(if_else(is.na(statistic), "", paste0(statistic, "_")), metric),
                              colors = weather_metrics_colour_scheme,
                              showlegend = TRUE)

plot_tmp %>%
    layout(xaxis = list(title = "", tickfont = tickFont, tickangle = -45, ticks = "outside", #showgrid = FALSE,
                       showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE, zeroline = FALSE),
          yaxis = list(title = "", tickfont = tickFont, ticks = "outside",
                       showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE, zeroline = FALSE, rangemode = "tozero"),

          margin = list(l = 5),
           legend = list(orientation = "h", font = list(size = 14), x = 0, y = 5)
               ) %>%
    config(displayModeBar = FALSE)


## ----  --------


## ----  --------




