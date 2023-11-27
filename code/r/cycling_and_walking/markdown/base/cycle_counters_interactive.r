*** need to replace Infs


## ---- bicycle_counts_summaries --------

bicycle_counts_summaries <- padding_cycle_counter_data_from_2017 %>%
    cross_join(as.data.frame(levels(cycle_counter_data_from_2017$Provider))) %>%
    cross_join(as.data.frame(levels(cycle_counter_data_from_2017$traffic_mode))) %>%

    rename_with(~ c(names(padding_cycle_counter_data_from_2017), "Provider", "traffic_mode")) %>%
    filter((traffic_mode == "bicycle") & (monthOfYear <= end_date)) %>%
    select(- c(monthOfYear, time)) %>%

    full_join(cycle_counter_data_from_2017 %>%
                filter(traffic_mode == "bicycle") %>%

                group_by(Provider, year, month, date) %>%
                summarise(across(count, ~ sum(., na.rm = TRUE))) %>%

                group_by(Provider, year, month) %>%
                summarise(across(count, list(average = mean, median = median, count = sum), .names = "{.fn}"),
                         ) %>%

                full_join(cycle_counter_data_from_2017 %>%
                            filter(traffic_mode == "bicycle") %>%

                            group_by(Provider, year, month, site, date) %>%
                            summarise(across(count, ~ sum(., na.rm = TRUE))) %>%

                            group_by(Provider, year, month) %>%
                            summarise(no_of_sites = n_distinct(site),
                                      across(count, list(average = mean, median = median), .names = "{.fn}_site"),
                                     )
                ) %>%

                full_join(cycle_counter_data_from_2017 %>%
                            filter(traffic_mode == "bicycle") %>%

                            group_by(Provider, year, month, site, siteID, date) %>%
                            summarise(across(count, ~ sum(., na.rm = TRUE))) %>%

                            group_by(Provider, year, month) %>%
                            summarise(no_of_counters = n_distinct(siteID),
                                      across(count, list(average = mean, median = median), .names = "{.fn}_siteId"),
                                     )
                ) %>%


                bind_rows(cycle_counter_data_from_2017 %>%
                            filter(traffic_mode == "bicycle") %>%

                            group_by(Provider, year, month, date) %>%
                            summarise(across(count, ~ sum(., na.rm = TRUE))) %>%

                            mutate(Provider = label_all_bicycle_providers) %>%
                            group_by(Provider, year, month) %>%
                            summarise(across(count, list(average = mean, median = median, count = sum), .names = "{.fn}")) %>%

                            full_join(cycle_counter_data_from_2017 %>%
                                        filter(traffic_mode == "bicycle") %>%

                                        group_by(Provider, year, month, site, date) %>%
                                        summarise(across(count, ~ sum(., na.rm = TRUE))) %>%

                                        mutate(Provider = label_all_bicycle_providers) %>%
                                        group_by(Provider, year, month) %>%
                                        summarise(no_of_sites = n_distinct(site),
                                                  across(count, list(average = mean, median = median), .names = "{.fn}_site"),
                                                 )
                            ) %>%

                            full_join(cycle_counter_data_from_2017 %>%
                                        filter(traffic_mode == "bicycle") %>%

                                        group_by(Provider, year, month, site, siteID, date) %>%
                                        summarise(across(count, ~ sum(., na.rm = TRUE))) %>%

                                        mutate(Provider = label_all_bicycle_providers) %>%
                                        group_by(Provider, year, month) %>%
                                        summarise(no_of_counters = n_distinct(siteID),
                                                  across(count, list(average = mean, median = median), .names = "{.fn}_siteId"))
                            )
                        ) # end bind rows
             ) %>% # end join (to padding)

        mutate(traffic_mode = "bicycle", # need to fill in for "all"
               month = factor(month, levels = month.abb),
               monthOfYear = parse_date(paste0(month, "-", year), format = "%b-%Y"),

               across(traffic_mode, as.factor),
               across(Provider, ~ fct_relevel(., label_all_bicycle_providers, default_provider)),
               across(Provider, ~ fct_relevel(., named_route_providers, after = Inf)),
               across(year, as.ordered),
              ) %>%

        distinct() %>%
        relocate(monthOfYear, .after = month) %>%

        mutate(pseudo_point = if_else(is.na(average) | (average < 0), 0, 1),
               tooltip = if_else((pseudo_point == 0),
                                 "",
                                 paste(Provider, "-", formatNumber(count, 1),
                                       paste0(traffic_mode, if_else((count == 1), "", "s"),
                                              " (", no_of_counters), if_else((no_of_counters == 1), "counter)", "counters)"),
                                       "-", month, year)),
               tooltip_mean = if_else((pseudo_point == 0),
                                 "",
                                 paste(Provider, "- total", paste0(formatNumber(count, 1), "; daily average:"),
                                       formatNumber(average, 0.1), paste0(traffic_mode, if_else((average == 1), "", "s")), "-", month, year)),
               tooltip_median = if_else((pseudo_point == 0),
                                 "",
                                 paste(Provider, "- total", paste0(formatNumber(count, 1), "; typical, daily:"),
                                       formatNumber(median, 1), paste0(traffic_mode, if_else((median == 1), "", "s")), "-", month, year)),
               tooltip_mean_by_counter = if_else((pseudo_point == 0),
                                 "",
                                 paste(Provider, "- total", paste0(formatNumber(count, 1), "; daily average across"),
                                       no_of_counters, if_else((no_of_counters == 1), "counter: ", "counters: "),
                                       formatNumber(average_siteId, 0.1), paste0(traffic_mode, if_else((average_siteId == 1), "", "s")),
                                       if_else((no_of_counters == 1), "-", "each -"), month, year)),
               tooltip_median_by_counter = if_else((pseudo_point == 0),
                                 "",
                                 paste(Provider, "- total", paste0(formatNumber(count, 1), "; typical count daily across"),
                                       no_of_counters, if_else((no_of_counters == 1), "counter: ", "counters: "),
                                       formatNumber(median_siteId, 1), paste0(traffic_mode, if_else((median_siteId == 1), "", "s")),
                                       if_else((no_of_counters == 1), "-", "each -"), month, year)),
               across(c(count, pseudo_point), as.integer),
               across(matches("average|count|median"), ~ replace_na(., -InfInt)),
              )


providers <- levels(bicycle_counts_summaries$Provider)
years <- levels(bicycle_counts_summaries$year)


## ---- bicycle_counts --------

steps <- list()
  plot_tmp <- plot_ly(height = 380, width = 700)


  for (i in seq_along(providers)) {
      for (j in seq_along(years)) {
        
          plot_tmp <- add_lines(
                          plot_tmp, data = bicycle_counts_summaries %>%
                                              mutate(across(count, ~ if_else((. < 0), - Inf, .))) %>%
                                              filter((Provider == providers[i]) & year == years[j]),
                                
                                x = ~ month,
                                y = ~ count,
                                text = ~ tooltip,
                                visible = (providers[i] == label_all_bicycle_providers),
                                name = years[j],

                                type = "scatter",
                                mode = "lines",
                                hoverinfo = "text",
                                color = ~ year,
                                colors = viridis_pal(option = "D", direction = -1, end = 0.95)(length(years)),
                                legendgroup = ~ year,
                                showlegend = TRUE)

      } # end iteration over years

      
      step <- list(args = list("visible", rep(FALSE, length(providers) * length(years))),
                 label = providers[i],
                 method = "restyle")
      for (j in seq_along(years))
          step$args[[2]][((i - 1) * length(years)) + j] <- TRUE
      steps[[i]] = step

  } # end iteration over providers


plot_bicycle_totals <- plot_tmp %>%
      layout(yaxis = list(tickfont = tickFont,
                          title = list(text = "Total Count", font = list(size = 18)), ticks = "outside")
             )


plot_tmp %>%
      layout(xaxis = list(tickfont = tickFont, title = list(text = ""), ticks = "outside", showgrid = FALSE,
                          zeroline = TRUE, rangemode = "tozero"), # both ignored post restyle :@
             yaxis = list(tickfont = tickFont, title = list(text = "Bicycle - Total Count", font = list(size = 20)), ticks = "outside"),
             margin = list(l = 5),

             annotations = list(x = -0.075, y = -0.25, text = "<b>Select Data Provider</b>", font = list(size = 14),
                                yref = "paper", xref = "paper", xanchor = "left", yanchor = "bottom", showarrow = FALSE),
             updatemenus = list(list(active = (which(str_detect(providers, fixed(label_all_bicycle_providers, TRUE))) - 1),
                                     x = 0.72, y = -0.15, direction = "up",
                                     buttons = steps
                                  )) # end dropdown
      ) %>%
      config(displayModeBar = FALSE)



## ---- bicycle_counts_overview --------

plot1 <- bicycle_counts_summaries %>%
    filter(Provider == "All Bicycle Counters") %>%

    ggplot(aes(month, count, group = year, colour = year, text = tooltip_median_by_counter)) +
        geom_line(linewidth = 0.5) +
        scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale(), drop0trailing = TRUE)) +
        scale_color_viridis_d(option = "D", end = 0.95, direction = -1) +
        ylab("") + xlab("") + #  - Count by Month# need to set fonts in plotly for consistency with corresponding plot for cycling
        cop_cycling_theme +
        theme(legend.title = element_blank(),
              panel.border = element_blank()) # for consistency with plotly without border settings



convertToPlotly(plot1, height = 350, width = 700,
                xaxis = list(tickfont = tickFont),
                yaxis = list(tickfont = tickFont, title = list(text = "Bicycle - Total Count", font = list(size = 20))),
                legend = list(title = list(text = ""), font = list(size = 11.5), tracegroupgap = 1.5)
               ) %>%
    config(displayModeBar = FALSE)



## ---- pedestrian_counts_summaries_nmf --------

pedestrian_summaries_nmf <- cycle_counter_data_from_2017 %>%
            filter(traffic_mode == "pedestrian") %>%

            group_by(traffic_mode, year, month, date) %>%
            summarise(across(count, ~ sum(., na.rm = TRUE))) %>%

            group_by(traffic_mode, year, month) %>%
            summarise(average = mean(count, na.rm = TRUE),
                      median = median(count, na.rm = TRUE),
                      count = sum(count, na.rm = TRUE)
                     ) %>%

            full_join(cycle_counter_data_from_2017 %>%
                        filter(traffic_mode == "pedestrian") %>%

                        group_by(traffic_mode, year, month, site, date) %>%
                        summarise(across(count, ~ sum(., na.rm = TRUE))) %>%

                        group_by(traffic_mode, year, month) %>%
                        summarise(no_of_sites = n_distinct(site),
                                  average = mean(count, na.rm = TRUE),
                                  median = median(count, na.rm = TRUE),
                                 ) %>%
                        rename_with(~ paste0(., "_site"), c(average, median))
            ) %>%

            full_join(cycle_counter_data_from_2017 %>%
                        filter(traffic_mode == "pedestrian") %>%

                        group_by(traffic_mode, year, month, site, siteID, date) %>%
                        summarise(across(count, ~ sum(., na.rm = TRUE))) %>%

                        group_by(traffic_mode, year, month) %>%
                        summarise(no_of_counters = n_distinct(siteID),
                                  average = mean(count, na.rm = TRUE),
                                  median = median(count, na.rm = TRUE),
                                 ) %>%
                        rename_with(~ paste0(., "_siteId"), c(average, median))
            ) %>%
            ungroup() %>%


            mutate(tooltip = paste(formatNumber(count, 1), paste0(traffic_mode, "s (", no_of_counters),
                                   " counters): ", "-", month, year),
                   tooltip_mean = paste("total", paste0(formatNumber(count, 1), "; daily average: "),
                                           formatNumber(average, 0.1), paste0(traffic_mode, "s"), "-", month, year),
                   tooltip_median = paste("total", paste0(formatNumber(count, 1), "; typical, daily: "),
                                           formatNumber(median, 1), paste0(traffic_mode, "s"), "-", month, year),
                   tooltip_mean_by_counter = paste("total", paste0(formatNumber(count, 1), "; daily average across "),
                                                   no_of_counters, " counters: ", formatNumber(average_siteId, 0.1),
                                                   paste0(traffic_mode, "s"), "-", month, year),
                   tooltip_median_by_counter = paste("total", paste0(formatNumber(count, 1), ";  typical count daily across "),
                                                       no_of_counters, " counters: ", formatNumber(median_siteId, 1),
                                                       paste0(traffic_mode, "s"), "-", month, year),
                  )



## ---- pedestrian_counts_nmf --------

plot1 <- #cycle_counter_data_from_2017 %>%
         #   filter((Provider == default_provider) & (traffic_mode == "pedestrian")) %>%
         #   group_by(traffic_mode, year, month) %>%
         #   summarise(average = mean(count),
         #             count = sum(count)) %>%
         #   mutate(tooltip = paste(formatNumber(count), paste0(traffic_mode, "s"), "-", month, year)) %>%

  pedestrian_summaries_nmf %>%
    ggplot(aes(month, count, group = year, colour = year, text = tooltip_median_by_counter)) +
    #ggplot(aes(month, count, group = year, colour = year, text = tooltip)) +
        geom_line(linewidth = 0.5) +
        scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale(), drop0trailing = TRUE)) +
        scale_color_viridis_d(option = "D", end = 0.95, direction = -1) +
        ylab("") + xlab("") + #  - Count by Month# need to set fonts in plotly for consistency with corresponding plot for cycling
        cop_cycling_theme +
        theme(legend.title = element_blank(),
              panel.border = element_blank()) # for consistency with plotly without border settings



convertToPlotly(plot1, height = 350, width = 700,
                xaxis = list(tickfont = tickFont),
                yaxis = list(tickfont = tickFont, title = list(text = "Pedestrian - Total Count", font = list(size = 20))),
                legend = list(title = list(text = ""), font = list(size = 11.5), tracegroupgap = 1.5)
               ) %>%
    config(displayModeBar = FALSE)


## ---- summary_tables_nmf_totals --------

cycle_counter_data_from_2017 %>%

    filter(Provider == default_provider) %>%

    group_by(Provider, traffic_mode, year) %>%
    summarise(across(count, ~ sum(., na.rm = TRUE))) %>%
    arrange(year) %>% # or messes up year order if starts with provider with NAs in any one year - pushes to end

    pivot_wider(names_from = year, values_from = count) %>%
    rename_with(~ gsub("_", " ", .x, fixed = TRUE)) %>%
    ungroup() %>%

    #mutate("total" = reduce(select(., matches("\\b\\d{4}\\b")), `+`)) %>% # can't deal with NAs

    rowwise() %>%
    mutate(total = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>%
    mutate_if(is.numeric, scales::comma) %>%
    mutate(across(matches("\\b\\d{4}\\b"), ~ replace_na(., "-"))) %>%
    rename(" " = Provider) %>%
    #column_to_rownames("Provider") %>%

    kable(caption = "Total count per year") %>%
    kable_paper(c("striped", "responsive"), full_width = FALSE, position = "left")


## ---- summary_tables_nmf_average_by_counter --------

cycle_counter_data_from_2017 %>%

    filter(Provider == default_provider) %>%

    group_by(Provider, traffic_mode, year, date, siteID, time) %>%
    summarise(across(count, ~ sum(., na.rm = TRUE))) %>%

    group_by(Provider, traffic_mode, year, date, siteID) %>%
    summarise(across(count, ~ sum(., na.rm = TRUE))) %>%

    group_by(Provider, traffic_mode, year, date) %>%
    summarise(average = mean(count, na.rm = TRUE)) %>%

    group_by(Provider, traffic_mode, year) %>%
    summarise(across(average, ~ round(mean(., na.rm = TRUE)))) %>%


    arrange(year) %>%

    pivot_wider(names_from = year, values_from = average) %>%
    rename_with(~ gsub("_", " ", .x, fixed = TRUE)) %>%
    mutate(across(matches("\\b\\d{4}\\b"), ~ replace_na(., "-"))) %>%
    rename(" " = Provider) %>%

    kable(caption = "Average daily count across counters") %>%
    kable_paper(c("striped", "responsive"), full_width = FALSE, position = "left")


## ---- summary_tables_nmf_averages --------

cycle_counter_data_from_2017 %>%

    filter(Provider == default_provider) %>%

    group_by(Provider, traffic_mode, year, date, time) %>%
    summarise(across(count, ~ sum(., na.rm = TRUE))) %>%

    group_by(Provider, traffic_mode, year, date) %>%
    summarise(average = mean(count, na.rm = TRUE)) %>%

    group_by(Provider, traffic_mode, year) %>%
    summarise(across(average, ~ round(mean(., na.rm = TRUE)))) %>%


    arrange(year) %>%

    pivot_wider(names_from = year, values_from = average) %>%
    rename_with(~ gsub("_", " ", .x, fixed = TRUE)) %>%
    mutate(across(matches("\\b\\d{4}\\b"), ~ replace_na(., "-"))) %>%
    rename(" " = Provider) %>%

    kable(caption = "Average daily count, overall", table.attr = "border-bottom: 15px solid") %>%
    kable_paper(c("striped", "responsive"), full_width = FALSE, position = "left")


## ---- summary_tables_nmf_hourly_averages --------

cycle_counter_data_from_2017 %>%

    filter(Provider == default_provider) %>%

    group_by(Provider, traffic_mode, year, date, time) %>%
    summarise(average = mean(count, na.rm = TRUE)) %>%

    group_by(Provider, traffic_mode, year) %>%
    summarise(across(average, ~ round(mean(., na.rm = TRUE), 1))) %>%


    arrange(year) %>%

    pivot_wider(names_from = year, values_from = average) %>%
    rename_with(~ gsub("_", " ", .x, fixed = TRUE)) %>%
    mutate(across(matches("\\b\\d{4}\\b"), ~ replace_na(., "-"))) %>%
    rename(" " = Provider) %>%

    kable(caption = "Average hourly count") %>%
    kable_paper(c("striped", "responsive"), full_width = FALSE, position = "left")



## ---- summary_tables_councils_totals --------

cycle_counter_data_from_2017 %>%

    filter(Provider != default_provider) %>%

    group_by(Provider, traffic_mode, year) %>%
    summarise(across(count, ~ sum(., na.rm = TRUE))) %>%
    arrange(year) %>% # or messes up year order if starts with provider with NAs in any one year - pushes to end

    pivot_wider(names_from = year, values_from = count) %>%
    rename_with(~ gsub("_", " ", .x, fixed = TRUE)) %>%
    ungroup() %>%

    #mutate("total" = reduce(select(., matches("\\b\\d{4}\\b")), `+`)) %>% # can't deal with NAs

    rowwise() %>%
    mutate(total = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>%
    mutate_if(is.numeric, scales::comma) %>%
    mutate(across(matches("\\b\\d{4}\\b"), ~ replace_na(., "-"))) %>%
    rename(" " = Provider) %>%

    kable(caption = "Total count per year") %>%
    kable_paper(c("striped", "condensed", "responsive"), full_width = FALSE, position = "left")


## ---- summary_tables_councils_average_by_counter --------

cycle_counter_data_from_2017 %>%

    filter(Provider != default_provider) %>%

    group_by(Provider, traffic_mode, year, date, siteID, time) %>%
    summarise(across(count, ~ sum(., na.rm = TRUE))) %>%

    group_by(Provider, traffic_mode, year, date, siteID) %>%
    summarise(across(count, ~ sum(., na.rm = TRUE))) %>%

    group_by(Provider, traffic_mode, year, date) %>%
    summarise(average = mean(count, na.rm = TRUE)) %>%

    group_by(Provider, traffic_mode, year) %>%
    summarise(across(average, ~ round(mean(., na.rm = TRUE)))) %>%


    arrange(year, desc(average)) %>%

    pivot_wider(names_from = year, values_from = average) %>%
    rename_with(~ gsub("_", " ", .x, fixed = TRUE)) %>%
    mutate_if(is.numeric, scales::comma) %>%
    mutate(across(matches("\\b\\d{4}\\b"), ~ replace_na(., "-"))) %>%
    rename(" " = Provider) %>%

    kable(caption = "Average daily count across counters") %>%
    kable_paper(c("striped", "condensed", "responsive"), full_width = FALSE, position = "left")


## ---- summary_tables_councils_averages --------

cycle_counter_data_from_2017 %>%

    filter(Provider != default_provider) %>%

    group_by(Provider, traffic_mode, year, date, time) %>%
    summarise(across(count, ~ sum(., na.rm = TRUE))) %>%

    group_by(Provider, traffic_mode, year, date) %>%
    summarise(average = mean(count, na.rm = TRUE)) %>%

    group_by(Provider, traffic_mode, year) %>%
    summarise(across(average, ~ round(mean(.,, na.rm = TRUE)))) %>%


    arrange(year, desc(average)) %>%

    pivot_wider(names_from = year, values_from = average) %>%
    rename_with(~ gsub("_", " ", .x, fixed = TRUE)) %>%
    mutate_if(is.numeric, scales::comma) %>%
    mutate(across(matches("\\b\\d{4}\\b"), ~ replace_na(., "-"))) %>%
    rename(" " = Provider) %>%

    kable(caption = "Average daily count, overall", table.attr = "border-bottom: 15px solid") %>%
    kable_paper(c("striped", "condensed", "responsive"), full_width = FALSE, position = "left")


## ---- summary_tables_councils_hourly_averages --------

cycle_counter_data_from_2017 %>%

    filter(Provider != default_provider) %>%

    group_by(Provider, traffic_mode, year, date, time) %>%
    summarise(average = mean(count, na.rm = TRUE)) %>%

    group_by(Provider, traffic_mode, year) %>%
    summarise(across(average, ~ round(mean(., na.rm = TRUE), 1))) %>%


    arrange(year, desc(average)) %>%

    pivot_wider(names_from = year, values_from = average) %>%
    rename_with(~ gsub("_", " ", .x, fixed = TRUE)) %>%
    mutate_if(is.numeric, scales::comma) %>%
    mutate(across(matches("\\b\\d{4}\\b"), ~ replace_na(., "-"))) %>%
    rename(" " = Provider) %>%

    kable(caption = "Average hourly count") %>%
    kable_paper(c("striped", "condensed", "responsive"), full_width = FALSE, position = "left")



##
# ---- summary_tables-original - for NMF data - reference only --------
##
cycle_counter_data_from_2017 %>%

    select(traffic_mode, year, count) %>%
    group_by(traffic_mode, year) %>%
    summarise(across(count, ~ sum(., na.rm = TRUE))) %>%

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
    select(site, siteID, externalId, LocalAuthority, RoadType) %>%
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
    select(site, siteID, LocalAuthority, RoadType) %>%
    mutate_at(vars(RoadType), ~ fct_explicit_na(RoadType, na_level = "-")) %>%

    right_join(cycle_counter_data_from_2017 %>%
                group_by(Provider, traffic_mode, Location, RoadName, date) %>%
                mutate(count = sum(count)) %>%
                group_by(Provider, traffic_mode) %>%

                mutate(max_count = max(count)) %>%
                mutate_at(vars(date), ~ format(date, format = "%d %b %Y")) %>%
                filter(count == max_count) %>%
                distinct(Provider, site, siteID, traffic_mode, Location, RoadName, date, weekday, count)
                  ) %>%
    relocate(Provider, .before = siteID) %>%
    relocate(RoadType, .after = RoadName) %>%
    relocate(traffic_mode, .after = Provider) %>%
    select(-count, everything(), count) %>%
    arrange(traffic_mode, desc(count), date) %>% #, month) %>%
    #select(-siteID) %>%

    rename_with(~ snakecase::to_upper_camel_case(.)) %>%

    kable(caption = "Maximum daily count for any one counter, across data providers") %>%
    kable_paper(c("striped", "condensed", "responsive"), full_width = FALSE, position = "left")


## ---- counters_reporting --------

counters_reporting <- counter_filter_zero_daily_counts %>%
    filter((traffic_mode == "bicycle") & month_at_zero) %>%
    distinct(Provider, site, siteID, Location, monthOfYear, month_at_zero) %>%
    count(name = "CountersNotReporting", Provider, site, Location, monthOfYear, month_at_zero) %>%

    full_join(cycle_counter_data_from_2017 %>%
                filter(traffic_mode == "bicycle") %>%
                distinct(Provider, site, siteID, Location, monthOfYear) %>%
                count(name = "CountersReporting", Provider, site, Location, monthOfYear)
              ) %>%

    left_join(reporting_sites %>%
                  select(Provider, site, LocalAuthority, Location)
             ) %>%
    distinct() %>%
    relocate(LocalAuthority, .before = site) %>%

    group_by(LocalAuthority, site, Location, monthOfYear) %>%
    summarise(across(month_at_zero, ~ as.logical(sum(., na.rm = TRUE))),
              across(matches("Reporting"), ~ sum(., na.rm = TRUE))
             ) %>%
    ungroup() %>%
    mutate(tooltip = case_when(month_at_zero ~
                                   case_when((CountersReporting == 0) ~
                                                 if_else((CountersNotReporting == 1),
                                                         paste(CountersNotReporting, "counter, not"),
                                                         paste("none of", CountersNotReporting, "counters")),
                                             TRUE ~ paste(CountersNotReporting, "of", (CountersNotReporting + CountersReporting),
                                                          "counters not")
                                            ),
                               TRUE ~ if_else((CountersReporting == 1),
                                              paste(CountersReporting, "counter"),
                                              paste("all", CountersReporting, "counters"))
                              ),
           across(tooltip, ~ paste0(Location, " (", LocalAuthority, ") - ", monthOfYear, ": ", ., " reporting")),
          )


la_labels <- counters_reporting %>%

    distinct(LocalAuthority, site) %>%
    mutate(across(everything(), as.character),
           across(LocalAuthority, ~ paste0(., " (", site, ")"))) %>%
    deframe()

status_counter_reporting <- c(AllCountersReporting = "AllCountersReporting",
                              NoCountersReporting = "NoCountersReporting",
                              SomeCountersReporting = "SomeCountersReporting"
                             )


filtered_data <- counters_reporting %>%

    mutate(across(LocalAuthority, ~ paste0(., " (", site, ")")),
           across(Location, ~ paste0(., " (", site, ")")),
           proportion_reporting = (CountersReporting / (CountersNotReporting + CountersReporting)),
           across(Location, ~ fct_reorder(., proportion_reporting)),
           across(c(monthOfYear, Location), ~ fct_rev(.)), # order descending does not give order expected ...

           switch_counter_reporting = case_when(!month_at_zero ~ status_counter_reporting["AllCountersReporting"], #all_markers_reporting"
                                                (CountersReporting == 0) ~ status_counter_reporting["NoCountersReporting"], #no_markers_reporting",
                                                if_all(matches("^Counters\\w*Reporting$", ~ (. > 0))) ~ status_counter_reporting["SomeCountersReporting"],
                                               ),
           # numbers don't work here, labels don't when specified as styling
           shape = case_when((switch_counter_reporting == status_counter_reporting["AllCountersReporting"]) ~ "circle",
                             (switch_counter_reporting == status_counter_reporting["SomeCountersReporting"]) ~ "star-diamond-dot",
                             (switch_counter_reporting == status_counter_reporting["NoCountersReporting"]) ~ "hourglass",
                            ),
           unicode_symbol = case_when((switch_counter_reporting == status_counter_reporting["AllCountersReporting"]) ~ "\U25CF",
                                      (switch_counter_reporting == status_counter_reporting["SomeCountersReporting"]) ~ "\U2726",
                                      (switch_counter_reporting == status_counter_reporting["NoCountersReporting"]) ~ "\U29D7",
                                     ),
           marker_size = case_when((switch_counter_reporting == status_counter_reporting["AllCountersReporting"]) ~ 4.5,
                                   (switch_counter_reporting == status_counter_reporting["SomeCountersReporting"]) ~ 5.5,
                                   (switch_counter_reporting == status_counter_reporting["NoCountersReporting"]) ~ 5,
                                  ),
           across(switch_counter_reporting, ~ factor(., levels = c("AllCountersReporting",
                                                                   "SomeCountersReporting",
                                                                   "NoCountersReporting"))),
           across(month_at_zero, as.integer),
          )


local_authorities <-
    filtered_data %>%
        distinct(LocalAuthority) %>%
        deframe()

live_counters <- filtered_data %>%
    distinct(switch_counter_reporting) %>%
    arrange(switch_counter_reporting) %>%

    mutate(label = as.character(switch_counter_reporting),
           across(switch_counter_reporting, as.integer)
           ) %>%
    deframe()

live_counters_unicode <- filtered_data %>%
    arrange(switch_counter_reporting) %>%
    mutate(across(switch_counter_reporting, ~ paste(unicode_symbol, .))) %>% # messes up order
    distinct(switch_counter_reporting) %>%

    mutate(label = as.character(switch_counter_reporting),
           across(switch_counter_reporting, as.integer)
           ) %>%
    deframe()


markers_partially_reporting <- list(target = status_counter_reporting["SomeCountersReporting"],
                                    value = list(marker = list(size = 5.5,
                                                               symbol = "222" #"star-diamond-dot" \U2726 (four-pointed stara)
                                                              )) # iffy with text labels ...
                             )

no_markers_reporting <- list(target = status_counter_reporting["NoCountersReporting"],
                              value = list(marker = list(size = 5,
                                                         symbol = "25" #"hourglass", actual - \U231B; plotly (maths glyph) \U29D7
                                                        ))
                             )

all_markers_reporting <- list(target = status_counter_reporting["AllCountersReporting"],
                              value = list(marker = list(size = 4.5,
                                                         #line = list(width = 1),
                                                         symbol = "0" #"circle" \U25CF
                                                        ))
                          )


## ---- counters_reporting_plotly --------

steps <- list()

plot_tmp <- plot_ly(height = 950, width = 3500)


steps[[1]] <- list(args = list("visible", rep(TRUE, (length(live_counters) + 1) * length(local_authorities))),
                      label = "All Counters",
                      method = "restyle")


for (i in seq_along(live_counters)) {

    step <- list(args = list("visible", rep(FALSE, (length(live_counters) + 1) * length(local_authorities))),
                 label = live_counters_unicode[[i]],
                 method = "restyle")
    
    if (i == 1) { # build legend

        plot_tmp <- add_trace(plot_tmp, data = filtered_data %>%
                                                  group_by(LocalAuthority) %>%
                                                  slice_tail(n = 1) %>%
                                                  ungroup(),

                                  x = ~ Location,
                                  y = ~ monthOfYear,
                                  text = ~ tooltip,
                                  name = ~ LocalAuthority,

                                  type = "scatter",
                                  mode = "markers",
                                  hoverinfo = "skip",

                                  marker = list(size = 0.001,
                                                symbol = "circle-open",
                                                opacity = 0.95
                                               ),

                                  opacity = 0.95,
                                  color = ~ LocalAuthority,
                                  colors = colorRampPalette(colour("soil", force = TRUE)(33))(length(levels(counters_reporting$LocalAuthority))),

                                  legendgroup = ~ site
                             )
    }
    
    for (j in seq_along(local_authorities)) {
      
            plot_tmp <- add_trace(plot_tmp, data = filtered_data %>%
                                      filter((LocalAuthority == local_authorities[j]) & (switch_counter_reporting == live_counters[[i]])),

                                      x = ~ Location,
                                      y = ~ monthOfYear,
                                      text = ~ tooltip,
                                      name = ~ LocalAuthority,

                                      type = "scatter",
                                      mode = "markers",
                                      hoverinfo = "text",
                                  
                                      symbol = ~ I(shape), #switch_counter_reporting,
                                      marker = list(sizemode = "diameter"),
                                      size = ~ I(marker_size),

                                      opacity = 0.95,
                                      color = ~ LocalAuthority,
                                      colors = colorRampPalette(colour("soil", force = TRUE)(33))(length(levels(counters_reporting$LocalAuthority))),
                                      showlegend = FALSE,
                                      legendgroup = ~ site#,
                                  
                                      # not reliable
                                      #transforms = list(list(#name = ~ site,
                                      #                       type = "groupby",
                                      #                        groups = ~ switch_counter_reporting,
                                      #                        styles = list(all_markers_reporting,
                                      #                                      no_markers_reporting,
                                      #                                      markers_partially_reporting
                                      #                        )
                                      #                    )
                                      #                )
                                 )

        step$args[[2]][((i) * length(local_authorities)) + j] <- TRUE
        step$args[[2]][j] <- TRUE

    } # end iteration over local_authorities
    
    steps[[i + 1]] <- step
    
} # end iteration over live_counters


plot_tmp %>%
  layout(title = list(text = "Bicycle Counters - Reporting over Time", y = 1, x = 0.1, xanchor = "left", yanchor = "top"),
         xaxis = list(autotypenumbers = "strict", tickfont = tickFont, title = list(text = "", titlefont = list(size = 20)),
                      autotick = FALSE, type = "category", categoryarray = levels(filtered_data$Location), categoryorder = "array",
                      ticks = "outside", showgrid = TRUE, showline = TRUE, linecolor = "rgba(175, 175, 175, 0.3)", mirror = TRUE,
                      zeroline = FALSE, range = c(- 1, length(levels(filtered_data$Location)) + 1), tickangle = -45, side = "top"
                     ),
         yaxis = list(autotypenumbers = "strict", autorange = "reversed", tickfont = tickFont,
                      title = list(text = "", titlefont = list(size = 20)), autotick = FALSE, dtick = 2,
                      type = "category", categoryarray = levels(filtered_data$monthOfYear), categoryorder = "array",
                      ticks = "outside", range = ~ c(0, length(levels(monthOfYear))),
                      showgrid = TRUE, showline = TRUE, linecolor = "rgba(175, 175, 175, 0.3)", mirror = TRUE),
         legend = list(title = list(text = "Local Authority", font = list(size = 14)), itemdoubleclick = FALSE,
                       itemclick = FALSE, y = 0.89, itemsizing = "constant", #itemwidth = 30, #sizing ignored ...
                       tracegroupgap = 1),
         margin = list(l = 1),
         clickmode = "none",
         dragmode = FALSE,
         updatemenus = list(list(active = 0, x = 1.08, y = 1.05,
                                   buttons = steps
                                 )
                            )
        ) %>%
    config(modeBarButtonsToRemove = c("zoom", "zoomIn2d", "zoomOut2d", "autoScale2d", "pan2d", "select", "lasso"))


## ---- counters_reporting_ggplotly --------

plot1 <- counters_reporting %>%

    mutate(across(LocalAuthority, ~ paste0(., " (", site, ")")),
           across(Location, ~ paste0(., " (", site, ")")),
           proportion_reporting = (CountersReporting / (CountersNotReporting + CountersReporting)),
           across(Location, ~ fct_reorder(., proportion_reporting)),
           across(Location, ~ fct_rev(.)), # order desceding does not give order expected ...

           across(month_at_zero, as.integer),
          ) %>%

    ggplot(aes(Location, monthOfYear, colour = LocalAuthority, text = tooltip)) +
        geom_point(aes(fill = LocalAuthority, alpha = proportion_reporting), size = 1.2, shape = 21) +
        geom_point(aes(alpha = (1 - month_at_zero)), size = 1.2, shape = 21) + # ensure outline shows for most faded
        geom_point(aes(alpha = month_at_zero), #(month_at_zero * (1 - proportion_reporting))),
                       size = 2, shape = 8) +
        guides(alpha = "none", shape = "none") +
        cop_cycling_theme +
        theme(axis.text.y = element_text(size = 11), # misaliged set here - angle = 30), #, hjust = -1, margin = margin(b = -5)),
              axis.text.x = element_text(size = 9),
              plot.title = element_text(size = 22, face = "bold"),
              legend.position = "bottom"
             ) +
        guides(x = guide_axis(angle = 45)) +
        ylab("") + xlab("") +
        #scale_x_date("", breaks = seq(min(datebreaks$monthOfYear), max(datebreaks$monthOfYear), by = "2 months"),
        #             labels = scales::date_format("%b-%Y"), expand = expansion(mult = .02)) +
        scale_x_discrete(position = "top", expand = expansion(mult = c(.005, 0.025))) +

        khroma::scale_colour_romaO(discrete = TRUE) +
        khroma::scale_fill_romaO(discrete = TRUE)

plot_tmp <- convertToPlotly(plot1, height = 1200, width = 3000,
                xaxis = list(tickfont = tickFont, title = list(font = list(size = 16)), tickangle = -45, side = "top"),
                yaxis = list(tickfont = tickFont, title = list(font = list(size = 14))), #orientation = "h",
                legend = list(font = list(size = 11), title = list(text = "Local Authority", font = list(size = 14)),
                              tracegroupgap = 1),
                )

for (i in seq_along(plot_tmp$x$data)) {

    current_trace <- plot_tmp$x$data[[i]]$name

    if (str_detect(current_trace, "\\d"))
        plot_tmp$x$data[[i]]$showlegend <- FALSE

    plot_tmp$x$data[[i]]$legendgroup <- la_labels[which(str_detect(current_trace, la_labels))]
}

plot_tmp
rm(plot1, la_labels)


## ---- bicycle_counters_typical_counts_daily --------

bicycle_counts_summaries_ext <- bicycle_counts_summaries %>%

    # plotly will not fill gap between years with colour as year ...
    rbind(bicycle_counts_summaries %>%
                filter(month == "Jan") %>%
                mutate(month = "Dec-Jan",
                       across(year, ~ (as.integer(as.character(.)) - 1)),
                       across(monthOfYear, ~ (. - 1)), # or will hide Jan tooltip
                       across(matches("tooltip"), ~ "")
                      )
             ) %>%
    filter((Provider != label_all_bicycle_providers) & !(abs(count) %in% c(Inf, InfInt, NA))) %>%

    # TO HANDLE LOG CONVERSION
    mutate(across(where(is.double) & !where(is.Date), ~ case_when((. < 1) ~ 1,
                                                                  (. == 1) ~ (. + 0.01),
                                                                  TRUE ~ .
                                                                 )
                 ),
           ProviderLabel = Provider
          ) %>%
    group_by(Provider) %>%
    mutate(across(ProviderLabel, ~ case_when((monthOfYear %in% c(min(monthOfYear))) ~ ., #max(monthOfYear))) ~ .,
                                            )#, .names = "{.fn}Label"
                  ),
           ) %>%
    ungroup()


# bicycle_counters_total --------

plot_bicycle_totals <- bicycle_counts_summaries_ext %>%

    ggplot(aes(monthOfYear, count, group = Provider, colour = year, text = tooltip, legendgroup = year)) +
        geom_line(linewidth = 0.45) +
        geom_text(aes(label = ProviderLabel), colour = "slategrey", check_overlap = TRUE,
                  hjust = "inward", vjust = "outward", size = 2.75)  +
        scale_y_log10(labels = scales::label_number(scale_cut = cut_short_scale(), drop0trailing = TRUE)) +
        scale_x_date("", breaks = seq(min(bicycle_counts_summaries$monthOfYear),
                                      max(bicycle_counts_summaries$monthOfYear), by = "2 months"),
                     labels = scales::date_format("%b-%Y"), expand = expansion(mult = .02)) +
        scale_color_viridis_d(option = "D", end = 0.95, direction = -1) +
        ylab("Total Bicycle Count") + xlab("") + # need to set fonts in plotly for consistency
                                                # with corresponding plot for cycling
        #guides(colour = "none") +
        cop_cycling_theme +
        theme(legend.title = element_blank(),
              axis.text.x = element_text(size = 14, angle = 45, vjust = 0.8, hjust = 0.8),
              legend.position = "none"
             )

plot_bicycle_totals <- convertToPlotly(plot_bicycle_totals, height = 350, width = 1300,
                yaxis = list(tickfont = tickFont, zeroline = TRUE, rangemode = "tozero",
                             title = list(font = list(size = 14))),
                maxMargin = 20
               )

# bicycle_counters_median --------

plot_median_daily <- bicycle_counts_summaries_ext %>%

    ggplot(aes(monthOfYear, median, group = Provider, colour = year, text = tooltip_median, legendgroup = year)) +
        geom_line(linewidth = 0.45) +
        geom_text(aes(label = ProviderLabel), colour = "slategrey", check_overlap = TRUE,
                  hjust = "inward", vjust = "outward", size = 2.75)  +
        scale_y_log10(labels = scales::label_number(scale_cut = cut_short_scale(), drop0trailing = TRUE)) +
        scale_x_date("", breaks = seq(min(bicycle_counts_summaries$monthOfYear),
                                      max(bicycle_counts_summaries$monthOfYear), by = "2 months"),
                     labels = scales::date_format("%b-%Y"), expand = expansion(mult = .02)) +
        scale_color_viridis_d(option = "D", end = 0.95, direction = -1) +
        ylab("Median Daily Count") + xlab("") +
        #guides(colour = "none") +
        cop_cycling_theme +
        theme(legend.title = element_blank(),
              axis.text.x = element_text(size = 14, angle = 45, vjust = 0.8, hjust = 0.8),
              legend.position = "none"
             )

plot_median_daily <- convertToPlotly(plot_median_daily, height = 350, width = 1300,
                #xaxis = list(tickfont = tickFont),
                yaxis = list(tickfont = tickFont, zeroline = TRUE, rangemode = "tozero",
                             title = list(font = list(size = 14))),
                maxMargin = 20
               )


# bicycle_counters_median_by_counter --------

plot_median_daily_by_counter <- bicycle_counts_summaries_ext %>%

    ggplot(aes(monthOfYear, median_siteId, group = Provider, colour = year, text = tooltip_median_by_counter, legendgroup = year)) +
        geom_line(linewidth = 0.45) +
        geom_text(aes(label = ProviderLabel), colour = "slategrey", check_overlap = TRUE,
                  hjust = "inward", vjust = "outward", size = 2.75)  +
        scale_y_log10(labels = scales::label_number(scale_cut = cut_short_scale(), drop0trailing = TRUE)) +
        scale_x_date("", breaks = seq(min(bicycle_counts_summaries$monthOfYear),
                                      max(bicycle_counts_summaries$monthOfYear), by = "2 months"),
                     labels = scales::date_format("%b-%Y"), expand = expansion(mult = .02)) +
        scale_color_viridis_d(option = "D", end = 0.95, direction = -1) +
        ylab("Median Daily per Counter") + xlab("") +
        #guides(colour = "none") +
        cop_cycling_theme +
        theme(legend.title = element_blank(),
              axis.text.x = element_text(size = 14, angle = 45, vjust = 0.8, hjust = 0.8),
              legend.position = "none"
             )

plot_median_daily_by_counter <- convertToPlotly(plot_median_daily_by_counter, height = 760, width = 1300,
                yaxis = list(tickfont = tickFont, zeroline = TRUE, rangemode = "tozero",
                             title = list(font = list(size = 14))),
                maxMargin = 20
               )


subplot(plot_bicycle_totals, plot_median_daily, plot_median_daily_by_counter,
                    titleY = TRUE, shareX = TRUE,
                    nrows = 3) %>%
    layout(title = list(text = "Bicycle Counts over Time - by Provider & Counter", font = list(size = 20)),
           xaxis = list(tickfont = tickFont, title = list(text = ""), ticks = "outside", showgrid = FALSE,
                        zeroline = TRUE, rangemode = "tozero"), # both ignored post restyle :@
           margin = list(l = 15, t = 30)
           ) %>%
    config(displayModeBar = FALSE)


## ---- filtered_data_counts_time_of_day_by_year_and_weekday --------

filtered_data <- padding_cycle_counter_data_from_2017 %>%
    cross_join(as.data.frame(levels(cycle_counter_data_from_2017$Provider))) %>%
    cross_join(as.data.frame(levels(cycle_counter_data_from_2017$traffic_mode))) %>%
    cross_join(as.data.frame(levels(cycle_counter_data_from_2017$weekday))) %>%

    rename_with(~ c(names(padding_cycle_counter_data_from_2017), "Provider", "traffic_mode", "weekday")) %>%
    filter((traffic_mode == "bicycle") & (monthOfYear <= end_date)) %>%
    select(- c(month, monthOfYear)) %>%

    mutate(end_time = format(as_datetime(time, format = "%H:%M") + hours(1), format = "%H:%M")) %>%
    relocate(end_time, .after = time) %>%

    full_join(cycle_counter_data_from_2017 %>%
              filter(traffic_mode == "bicycle") %>%
              group_by(Provider, traffic_mode, year, weekday, isWeekEnd, time) %>%
              summarise(across(count, list(average = ~ (mean(., na.rm = TRUE)),
                                           median = ~ (median(., na.rm = TRUE)),
                                           count = ~ (sum(., na.rm = TRUE))),
                               .names = "{.fn}")
                 )) %>%

    full_join(cycle_counter_data_from_2017 %>%
              filter(traffic_mode == "bicycle") %>%
              group_by(traffic_mode, year, weekday, isWeekEnd, time) %>%
              summarise(across(count, list(average = ~ (mean(., na.rm = TRUE)),
                                           median = ~ (median(., na.rm = TRUE)),
                                           count = ~ (sum(., na.rm = TRUE))),
                               .names = "{.fn}")
                 ) %>%
              mutate(Provider = label_all_bicycle_providers)
             ) %>%

        mutate(weekday = factor(weekday, levels = levels(wday(1, label = TRUE))),
               isWeekEnd = (as.integer(weekday) %in% c(1, 7)), #between(as.integer(weekday), 6, 7)) %>%

               across(traffic_mode, as.factor),
               across(Provider, ~ fct_relevel(., label_all_bicycle_providers, default_provider)),
               across(Provider, ~ fct_relevel(., named_route_providers, after = Inf)),
               across(year, as.ordered),
              ) %>%
        relocate(isWeekEnd, .after = "weekday") %>%

        mutate(pseudo_point = if_else(is.na(average) | (average < 0), 0, 1),
               tooltip = if_else((pseudo_point == 0),
                                 "",
                                 paste(Provider, "- in", year, "on average", formatNumber(average, 0.1), paste0(traffic_mode, "s"),
                                       weekday, time, "-", end_time)),
               tooltip_median = if_else((pseudo_point == 0),
                                 "",
                                 paste(Provider, "- in", year, "typically", formatNumber(median, 1), paste0(traffic_mode, "s"),
                                       weekday, time, "-", end_time)),

               across(c(count, pseudo_point), as.integer),
               across(c(average, median, count), ~ replace_na(., -InfInt))
               )


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
                                                    filter((Provider == providers[i]) & (year == years[j]) & (weekday == weekdays[k])) %>%
                                                    mutate(across(average, ~ if_else((. < 0), - Inf, .))),

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
                                                    filter((Provider == providers[i]) & (weekday == weekdays[j]) & (year == years[k])) %>%
                                                    mutate(across(average, ~ if_else((. < 0), - Inf, .))),


                                  x = ~ time,
                                  y = ~ average,
                                  text = ~ tooltip,
                                  visible = (i == 1),
                                  name = ~ year,

                                  type = "scatter",
                                  mode = "lines",
                                  hoverinfo = "text",
                                  color = ~ year,
                                  colors = viridis_pal(option = "D", end = 0.95, direction = -1)(length(years)),
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

filtered_data <- padding_cycle_counter_data_from_2017 %>%
    cross_join(as.data.frame(levels(cycle_counter_data_from_2017$Provider))) %>%
    cross_join(as.data.frame(levels(cycle_counter_data_from_2017$traffic_mode))) %>%

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

        mutate(month = factor(month, levels = month.abb),
               across(year, as.ordered),
               across(c(Provider, traffic_mode, time), as.factor),
               across(Provider, ~ fct_relevel(., default_provider)),
               across(Provider, ~ fct_relevel(., named_route_providers, after = Inf)),

               pseudo_point = if_else(is.na(average) | (average < 0), 0, 1),
               tooltip = if_else((pseudo_point == 0),
                                 "",
                                 paste(Provider, "-", month, year,
                                       "\nOn average",
                                       case_when((round(average) < 1) ~ paste("none or one", traffic_mode),
                                                 (round(average) == 1) ~ paste("one", traffic_mode),
                                                 TRUE ~ paste0(formatNumber(average, 0.1), " ", traffic_mode, "s,")
                                                ),
                                       time, "-", end_time,
                                       "\nTotal count:", formatNumber(count, 1))),

               across(c(count, pseudo_point), as.integer),
               across(c(average, count), ~ replace_na(., -InfInt))
              )

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
                                                    filter((Provider == providers[i]) & (year == years[j]) & (month == months[k])) %>%
                                                    mutate(across(count, ~ if_else((. < 0), - Inf, .))),

                                  x = ~ time,
                                  y = ~ count,
                                  text = ~ tooltip,
                                  visible = (i == 1),
                                  name = ~ month,

                                  type = "scatter",
                                  mode = "lines",
                                  hoverinfo = "text",
                                  color = ~ month,
                                  colors = colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(length(months)),
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
           updatemenus = list(list(active = 0, x = 0.25, y = 1.15,
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
                                                    filter((Provider == providers[i]) & (year == years[j]) & (month == months[k])) %>%
                                                    mutate(across(average, ~ if_else((. < 0), - Inf, .))),

                                  x = ~ time,
                                  y = ~ average,
                                  text = ~ tooltip,
                                  visible = (i == 1),
                                  name = ~ month,

                                  type = "scatter",
                                  mode = "lines",
                                  hoverinfo = "text",
                                  color = ~ month,
                                  colors = ~ colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(length(months)),
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


## ---- median_count_by_location_and_year --------

plot_tmp <- plot_ly(height = 750, width = 900,
                    data = count_by_location %>%
                                mutate(across(c(daily_median_by_siteID_and_year, count_by_year), ~ replace(., . == -Inf, NA))) %>%

                                distinct(year, LocalAuthority, Location, counter_count_by_year, count_by_year,
                                         daily_median_by_siteID_and_year, tooltip_median_siteID_by_year) %>%
                                group_by(LocalAuthority, Location, year) %>%
                                add_tally() %>%
                                ungroup() %>%
                                mutate(log_daily_median_by_siteID_and_year = if_else(daily_median_by_siteID_and_year == 0,
                                                                            0,
                                                                            log(daily_median_by_siteID_and_year)),
                                       across(count_by_year, ~ replace(., is.na(daily_median_by_siteID_and_year), -Inf)),
                                       across(c(daily_median_by_siteID_and_year, count_by_year, counter_count_by_year), ~ replace(., is.na(.), -Inf)),
                                       across(log_daily_median_by_siteID_and_year, ~ replace(., is.na(.), -1))
                                       ) %>%
                                filter(!((n > 1) &
                                         if_all(c(daily_median_by_siteID_and_year, counter_count_by_year, count_by_year), ~ (. == -Inf)))
                                       )
                   )


plot_tmp <- add_trace(plot_tmp,

                      x = ~ count_by_year,
                      y = ~ daily_median_by_siteID_and_year,
                      text = ~ tooltip_median_siteID_by_year,
                      name = ~ LocalAuthority,
                      frame = ~ year,
                      ids = ~ Location,

                      type = "scatter",
                      mode = "markers",
                      marker = list(sizemode = "diameter"),
                      sizes = c(1, 25),
                      size = ~ log_daily_median_by_siteID_and_year,
                      fill = ~ "",
                      hoverinfo = "text",

                      opacity = 0.8,
                      color = ~ LocalAuthority,
                      colors = colorRampPalette(colour("soil", force = TRUE)(33))(length(levels(count_by_location$LocalAuthority))),
showlegend = TRUE,
                      legendgroup = ~ LocalAuthority
                     )


plot_tmp <- add_text(plot_tmp,

                     x = ~ count_by_year,
                     y = ~ daily_median_by_siteID_and_year,
                     frame = ~ year,
                     ids = ~ Location,

                     type = "scatter",
                     mode = "text",
                     text = ~ Location,
                     textposition = "top middle",
                     opacity = 0.8,
                     color = ~ LocalAuthority,
                      #textfont = ~ list( size = 8),

                     #name = ~ LocalAuthority,
                     showlegend = FALSE,
                     legendgroup = ~ LocalAuthority
                    )


plot_tmp %>%
  layout(title = list(text = "Bicycle count by Location", y = 1, x = 0.1, xanchor = "left", yanchor = "top"),
         xaxis = list(type = "log", tickfont = tickFont, title = list(text = "Total yearly count", titlefont = list(size = 20)),
                      ticks = "outside", showgrid = TRUE, showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE, zeroline = FALSE),
         yaxis = list(type = "log", tickfont = tickFont, ticks = "outside",
                      title = list(text = "Typical daily count - across counters per location", titlefont = list(size = 20)),
                      ticks = "outside", showgrid = TRUE, showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE),
         legend = list(title = list(text = "Local Authority", font = list(size = 14)),
                       itemsizing = "constant", #itemwidth = 30, #sizing ignored ...
                       tracegroupgap = 1),
         margin = list(l = 5)

        ) %>%
        animation_opts(1000, transition = 500, redraw = TRUE) %>%
        animation_slider(currentvalue = list(prefix = paste0("Year: "), font = list(color = "grey")))



## ---- average_count_by_location_and_month --------

plot_tmp <- plot_ly(height = 850, width = 950, #680, 900,
                    data = count_by_location %>%

                        filter(between(monthOfYear, start_date, end_date)) %>%

                        mutate(monthOfYear2 = factor(format(monthOfYear, "%b-%Y"))) %>%
                        mutate(monthOfYear2 = fct_reorder(monthOfYear2, monthOfYear)) %>%
                        select(- monthOfYear) %>%
                        rename(monthOfYear = monthOfYear2) %>%
                        relocate(monthOfYear, .after = month) %>%
                        mutate(log_daily_average = if_else(daily_average == 0, 0, log(daily_average)),
                               tooltip = paste(Location, "-", formatNumber(count, 1),  "bicycles in", monthOfYear, ", daily average", round(daily_average, 2))) %>%

                        mutate_at(vars(count), ~ replace(., is.na(daily_average), -Inf)) %>%
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
                      colors = ~ colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(length(levels(LocalAuthority))),
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


plot_tmp %>%
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


## ---- median_count_by_siteID_location_and_month --------

plot_tmp <- plot_ly(height = 750, width = 900, #800, width = 1000,
                    data = count_by_location %>%

                                filter(between(monthOfYear, start_date, end_date)) %>%

                                distinct(year, month, monthOfYear, LocalAuthority, Location, counter_count, count,
                                                 daily_median_by_siteID, tooltip_median_siteID_by_month) %>%
                                mutate(monthOfYear2 = factor(format(monthOfYear, "%b-%Y")),
                                       across(monthOfYear2, ~ fct_reorder(., monthOfYear))
                                       ) %>%
                                select(- monthOfYear) %>%
                                rename(monthOfYear = monthOfYear2) %>%
                                relocate(monthOfYear, .after = month) %>%
                                mutate(log_daily_median_by_siteID = if_else(daily_median_by_siteID == 0, 0,
                                                                             log(daily_median_by_siteID)),
                                       across(count, ~ replace(., is.na(daily_median_by_siteID), -Inf)),
                                       across(c(daily_median_by_siteID, count), ~ replace(., is.na(.), -Inf)),
                                       across(log_daily_median_by_siteID, ~ replace(., is.na(.), -1))
                                       )
                   )


plot_tmp <- add_trace(plot_tmp,

                      x = ~ count,
                      y = ~ daily_median_by_siteID,
                      text = ~ tooltip_median_siteID_by_month,
                      name = ~ LocalAuthority,
                      frame = ~ monthOfYear,
                      ids = ~ Location,

                      type = "scatter",
                      mode = "markers",
                      marker = list(sizemode = "diameter"),
                      sizes = c(1, 25),
                      size = ~ log_daily_median_by_siteID,
                      fill = ~ "",
                      hoverinfo = "text",

                      opacity = 0.8,
                      color = ~ LocalAuthority,
                      colors = colorRampPalette(colour("soil", force = TRUE)(33))(length(levels(count_by_location$LocalAuthority))),
                      showlegend = TRUE,
                      legendgroup = ~ LocalAuthority
                     )


plot_tmp <- add_text(plot_tmp,

                     x = ~ count,
                     y = ~ daily_median_by_siteID,
                     frame = ~ monthOfYear,
                     ids = ~ Location,

                     type = "scatter",
                     mode = "text",
                     text = ~ Location,
                     textposition = "top middle",
                     opacity = 0.8,
                     color = ~ LocalAuthority,
                      #textfont = ~ list( size = 8),

                     #name = ~ LocalAuthority,
                     showlegend = FALSE,
                     legendgroup = ~ LocalAuthority
                    )


plot_tmp %>%
  layout(title = list(text = "Bicycle Count by Counter & Location", font = list(size = 18), y = 1, x = 0.1, xanchor = "left", yanchor = "top"),
         xaxis = list(type = "log", tickfont = tickFont, title = list(text = "Total monthly count", font = list(size = 15)),
                      ticks = "outside", showgrid = TRUE, showline = TRUE, linecolor = "rgb(175, 175, 175)",
                      mirror = TRUE, zeroline = FALSE),
         yaxis = list(type = "log", tickfont = tickFont,
                      title = list(text = "Typical daily count - across counters per location", font = list(size = 15)),
                      ticks = "outside", showgrid = TRUE, showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE),
         legend = list(title = list(text = "Local Authority", font = list(size = 14)), font = list(size = 12),
                       itemsizing = "constant", #itemwidth = 30, #sizing ignored ...
                       tracegroupgap = 1),
         margin = list(l = 5)

        ) %>%
        animation_opts(1000, transition = 500, redraw = TRUE)  %>%
        animation_slider(currentvalue = list(prefix = paste0("Month: "), font = list(color = "grey")))


## ---- total_count_by_location_and_month --------

# adapted from https://plotly.com/r/sliders

filtered_data <- count_by_location %>%
    filter(if_any(count, ~ !is.na(.) & (abs(.) != Inf) & (abs(.) != InfInt))) %>%
    filter(between(monthOfYear, start_date, end_date)) %>%
    droplevels() %>%

    arrange(LocalAuthority, Location) %>%
    mutate(across(matches("^location_label"), ~ fct_inorder(.)))

locations <- levels(filtered_data$distinct_location)
years <- levels(filtered_data$year)


# total_counts_as_the_seasons_change --------

#### Monthly Totals - As the Seasons Change

steps <- list()
plot_tmp <- plot_ly(height = 350, width = 1300)

for (i in seq_along(locations)) {
  
    plot_tmp <- add_lines(plot_tmp, data = filtered_data %>%
                                            filter(distinct_location == locations[i]),

                       x = ~ monthOfYear,
                       y = ~ count,
                       text = ~ tooltip,
                       visible = (i == 1),
                       name = locations[i],

                        mode = "lines",
                        hoverinfo = "text",
                        color = ~ LocalAuthority,
                        colors = as.character(colour("soil", force = TRUE)(length(levels(filtered_data$LocalAuthority)))),
                        showlegend = FALSE)

    step <- list(args = list("visible", rep(FALSE, length(locations))),
                 label = unique(filtered_data$location_label_local_authority[filtered_data$distinct_location == locations[i]]),
                 method = "restyle")
    step$args[[2]][i] <- TRUE
    steps[[i]] <- step
}

plot_location_by_month <- plot_tmp %>%
  layout(yaxis = list(tickfont = tickFont, ticks = "outside", title = list(text = "Monthly Totals"),
                      showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE, zerolinecolor = "rgb(168,169,173)", # metallic grey
                      zeroline = TRUE, rangemode = "tozero"),
         updatemenus = list(list(active = 0, x = 0.345, y = 1.065, #1.12,
                                 buttons = steps
                                )) # end dropdown
      )


# daily_medians_as_the_seasons_change --------

steps <- list()
plot_tmp <- plot_ly(height = 350, width = 1300)

for (i in seq_along(locations)) {
  
    plot_tmp <- add_lines(plot_tmp, data = filtered_data %>%
                                            filter(distinct_location == locations[i]),

                       x = ~ monthOfYear,
                       y = ~ daily_median_by_siteID,
                       text = ~ tooltip_median,
                       visible = (i == 1),
                       name = locations[i],

                        mode = "lines",
                        hoverinfo = "text",
                        color = ~ LocalAuthority,
                        colors = as.character(colour("soil", force = TRUE)(length(levels(filtered_data$LocalAuthority)))),
                        showlegend = FALSE)

    step <- list(args = list("visible", rep(FALSE, length(locations))),
               label = unique(filtered_data$location_label_local_authority[filtered_data$distinct_location == locations[i]]),
               method = "restyle")
    step$args[[2]][i] <- TRUE
    steps[[i]] <- step
}

plot_location_by_month_medians <- plot_tmp %>%
  layout(yaxis = list(tickfont = tickFont, ticks = "outside", title = list(text = "Typical Daily by Counter"),
                      showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE, zerolinecolor = "rgb(168,169,173)", # metallic grey
                      zeroline = TRUE, rangemode = "tozero")
      )


# historical_weather_scotland_temp --------
#### Temperature Patterns (C) - Scotland

plot_data <- historical_weather_scotland_from_2017 %>%
    filter((region == "Scotland") & is.na(weather_station) & (metric == "temp")) %>%
    mutate(tooltip = paste(str_to_title(statistic), paste0(metric, ":"),
                           paste0(value, "C"),
                           "-", month, year))


#plot_tmp <- plot_ly(height = 280, width = 1400, line = list(width = 0.65))
plot_tmp <- plot_ly(height = 800, width = 1300, line = list(width = 0.65)) # height  <-  full set of plots
steps <- list()

# need to plot each separately or the statistics bunch into three of the same for each framerather than sequentally,
# regardless how plotted
for (i in seq_along(locations)) {

    plot_tmp <- add_lines(plot_tmp,
                          data = plot_data %>%
                                      filter(statistic == "max"),

                                  y = ~ value,
                                  x = ~ monthOfYear,
                                  text = ~ tooltip,
                                  visible = (i == 1),
                                  name = ~ if_else(!is.na(statistic), statistic, metric), # order matters

                                  type = "scatter",
                                  mode = "lines",
                                  hoverinfo = "text",
                                  color = ~ paste0(if_else(is.na(statistic), "", paste0(statistic, "_")), metric),
                                  colors = weather_metrics_colour_scheme
                         )


    step <- list(args = list("visible", rep(FALSE, length(locations))),
               label = unique(filtered_data$location_label_local_authority[filtered_data$distinct_location == locations[i]]),
               method = "restyle")
    step$args[[2]][i] <- TRUE
    steps[[i]] <- step
}
for (i in seq_along(locations)) {

    plot_tmp <- add_lines(plot_tmp,
                          data = plot_data %>%
                                      filter(statistic == "mean"),

                                  y = ~ value,
                                  x = ~ monthOfYear,
                                  text = ~ tooltip,
                                  visible = (i == 1),
                                  name = ~ if_else(!is.na(statistic), statistic, metric), # order matters

                                  type = "scatter",
                                  mode = "lines",
                                  hoverinfo = "text",
                                  color = ~ paste0(if_else(is.na(statistic), "", paste0(statistic, "_")), metric),
                                  colors = weather_metrics_colour_scheme
                         )


    step <- list(args = list("visible", rep(FALSE, length(locations))),
               label = unique(filtered_data$location_label_local_authority[filtered_data$distinct_location == locations[i]]),
               method = "restyle")
    step$args[[2]][i] <- TRUE
    steps[[i]] <- step
}
for (i in seq_along(locations)) {

    plot_tmp <- add_lines(plot_tmp,
                          data = plot_data %>%
                                      filter(statistic == "min"),

                                  y = ~ value,
                                  x = ~ monthOfYear,
                                  text = ~ tooltip,
                                  visible = (i == 1),
                                  name = ~ if_else(!is.na(statistic), statistic, metric), # order matters

                                  type = "scatter",
                                  mode = "lines",
                                  hoverinfo = "text",
                                  color = ~ paste0(if_else(is.na(statistic), "", paste0(statistic, "_")), metric),
                                  colors = weather_metrics_colour_scheme
                         )


    step <- list(args = list("visible", rep(FALSE, length(locations))),
               label = unique(filtered_data$location_label_local_authority[filtered_data$distinct_location == locations[i]]),
               method = "restyle")
    step$args[[2]][i] <- TRUE
    steps[[i]] <- step
}

plot_weather_temp <- plot_tmp %>%
    layout(xaxis = list(zeroline = TRUE),
           yaxis = list(title = "Avg. Temp (C)", tickfont = tickFont, ticks = "outside",
                        showline = TRUE, linecolor = "rgb(175, 175, 175)",
                        zerolinecolor = "rgb(215, 215, 215)", # lowest row; need to distinguish but need to remove illusion of two horizontal axes
                        mirror = TRUE, zeroline = TRUE, rangemode = "tozero")
           )


# historical_weather_scotland_rainfall --------
#### Rainfall Patterns (mm) - Scotland

#plot_tmp <- plot_ly(height = 280, width = 1400, line = list(width = 0.65))
plot_tmp <- plot_ly(height = 800, width = 1300, line = list(width = 0.65)) # height  <-  full set of plots

plot_data <- historical_weather_scotland_from_2017 %>%
    filter((region == "Scotland") & is.na(weather_station) & (metric == "rainfall")) %>%
    mutate(tooltip = paste(value, "mm", "-", month, year))

steps <- list()
for (i in seq_along(locations)) {

    plot_tmp <- add_lines(plot_tmp, data = plot_data,

                                  y = ~ value,
                                  x = ~ monthOfYear,
                                  text = ~ tooltip,
                                  visible = (i == 1),
                                  name = ~ if_else(!is.na(statistic), statistic, metric), # order matters

                                  type = "scatter",
                                  mode = "lines",
                                  hoverinfo = "text",
                                  color = ~ paste0(if_else(is.na(statistic), "", paste0(statistic, "_")), metric),
                                  colors = weather_metrics_colour_scheme,
                                  showlegend = FALSE
                         )

    step <- list(args = list("visible", rep(FALSE, length(locations))),
               label = unique(filtered_data$location_label_local_authority[filtered_data$distinct_location == locations[i]]),
               method = "restyle")
    step$args[[2]][i] <- TRUE
    steps[[i]] <- step
}

plot_weather_rainfall <- plot_tmp %>%
    layout(yaxis = list(title = "Avg. Rainfall (mm)", tickfont = tickFont, ticks = "outside",
                        showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE, zerolinecolor = "rgb(168,169,173)", # metallic grey
                        zeroline = TRUE, rangemode = "tozero"))


rm(plot_data)



## ---- cycling_as_the_seasons_change --------

subplot(plot_location_by_month, plot_location_by_month_medians, plot_weather_rainfall, plot_weather_temp,
        titleY = TRUE,
        shareX = TRUE,
        heights = c(0.3, 0.3, 0.2, 0.2),
        nrows = 4) %>%

    layout(title = list(text = "Cycling - As the Seasons Change", font = list(size = 22)),
           xaxis = list(title = "", tickfont = tickFont, tickangle = -45, ticks = "outside", #showgrid = FALSE,
                        autotick = FALSE, dtick = "M2", tickformat = "%b-%Y", showline = TRUE,
                        linecolor = "rgb(175, 175, 175)", zeroline = TRUE),
           updatemenus = list(list(active = 0, x = 0.345, y = 0.5, #1.065, #1.12,
                                   buttons = steps
                                  )), # end dropdown,
           legend = list(y = 0.1),
           margin = list(l = 15, t = 2)
           ) %>%
    config(modeBarButtonsToRemove = c("zoom2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "pan2d")) %>%

    suppressWarnings() # can't find where warning about factor is being triggered :(


## ---- total_count_by_location_grouped_by_year --------

# adapted from https://stackoverflow.com/a/54865094
# ideally would group into traces but this is not possible in the r implementation (as at end 2021)

filtered_data <- count_by_location %>%
    filter(monthOfYear <= end_date) %>%
    droplevels() %>%

    arrange(LocalAuthority, Location) %>%
    mutate(across(matches("^location_label"), ~ fct_inorder(.)))


locations <- levels(filtered_data$distinct_location)
years <- levels(filtered_data$year)


steps <- list()
plot_tmp <- plot_ly(height = 450, width = 820)

for (i in seq_along(locations)) {
    for (j in seq_along(years)) {
     
        plot_tmp <- add_lines(plot_tmp, data = filtered_data %>%
                                                  filter((distinct_location == locations[i]) & year == years[j]) %>%
                                                  mutate(across(count, ~ if_else((. == -InfInt), -Inf, .))),
                              
                              x = ~ month,
                              y = ~ count,
                              text = ~ tooltip_median_siteID_by_month,
                              visible = (i == 1),
                              name = years[j], #paste0(locations[i], "-", years[j]),

                              type = "scatter",
                              mode = "lines",
                              hoverinfo = "text",
                              color = ~ year,
                              colors = viridis_pal(option = "D", end = 0.95, direction = -1)(length(years)),
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
           yaxis = list(tickfont = tickFont, title = list(text = "Monthly Totals", font = list(size = 20)),
                        rangemode = "nonnegative" #tozero"
                       ),
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


## ---- counts_by_location_and_month_lerwick --------

# total_counts_as_the_seasons_change_lerwick --------

roadNames <- levels(count_by_location_lerwick$RoadName)

steps <- list()
plot_tmp <- plot_ly(height = 350, width = 1300)

for (i in seq_along(roadNames)) {
  
    plot_tmp <- add_lines(plot_tmp, data = count_by_location_lerwick %>%
                                              mutate(across(count, ~ if_else((. < 0), - Inf, .))) %>%
                                              filter(RoadName == roadNames[i]),

                       x = ~ monthOfYear,
                       y = ~ count,
                       text = ~ tooltip,
                       visible = (i == 1),
                       name = ~ RoadName,

                        mode = "lines",
                        hoverinfo = "text",
                        color = ~ RoadName,
                        colors = as.character(colour("muted", force = TRUE)(length(roadNames))),
                        showlegend = FALSE)

  step <- list(args = list("visible", rep(FALSE, length(roadNames))),
               label = roadNames[i],
               method = "restyle")
  step$args[[2]][i] <- TRUE
  steps[[i]] <- step
}

plot_location_by_month_lerwick <- plot_tmp %>%
  layout(yaxis = list(tickfont = tickFont, ticks = "outside", title = list(text = "Monthly Totals"),
                      showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE,
                      zerolinecolor = "rgb(168,169,173)", zeroline = TRUE, rangemode = "tozero")#,
          #updatemenus = list(list(active = 0, x = 0.345, y = 1.065, #, x = 0.185, y = 1.12,
          #                        buttons = steps
          #                       )) # end dropdown
        )

# daily_medians_as_the_seasons_change_lerwick --------

steps <- list()
plot_tmp <- plot_ly(height = 350, width = 1300)

for (i in seq_along(roadNames)) {
  
    plot_tmp <- add_lines(plot_tmp, data = count_by_location_lerwick %>%
                                              mutate(across(daily_median, ~ if_else((. < 0), - Inf, .))) %>%
                                              filter(RoadName == roadNames[i]),

                       x = ~ monthOfYear,
                       y = ~ daily_median,
                       text = ~ tooltip_median,
                       visible = (i == 1),
                       name = ~ RoadName,

                        mode = "lines",
                        hoverinfo = "text",
                        color = ~ RoadName,
                        colors = as.character(colour("muted", force = TRUE)(length(roadNames))),
                        showlegend = FALSE)

    step <- list(args = list("visible", rep(FALSE, length(roadNames))),
               label = roadNames[i],
               method = "restyle")
    step$args[[2]][i] <- TRUE
    steps[[i]] <- step
}

plot_location_by_month_medians_lerwick <- plot_tmp %>%
  layout(yaxis = list(tickfont = tickFont, ticks = "outside", title = list(text = "Typical Daily Count"),
                      showline = TRUE, linecolor = "rgb(175, 175, 175)", mirror = TRUE,
                      zerolinecolor = "rgb(168,169,173)", zeroline = TRUE, rangemode = "tozero")
        )



# historical_weather_lerwick_temp --------

plot_data <- historical_weather_scotland_from_2017 %>%

    filter((region == "Scotland") & (weather_station == "Lerwick") & (metric == "temp")) %>%

    left_join(cycle_counter_data_from_2017 %>%
                  filter((traffic_mode == "bicycle") & (Location == "Lerwick")) %>%
                  group_by(Location) %>%
                  summarise(across(date, list(start_date = min, end_date = max), .names = "{.fn}")),
              by = c("weather_station" = "Location")
                 ) %>%
    filter(monthOfYear %within% interval(floor_date(start_date, unit = "month"), ceiling_date(end_date, unit = "month"))) %>%

    mutate(tooltip = paste(str_to_title(statistic), paste0(metric, ":"),
                           paste0(value, "C"),
                           "-", month, year))


#plot_tmp <- plot_ly(height = 280, width = 1400, line = list(width = 0.65))
plot_tmp <- plot_ly(height = 800, width = 1300, line = list(width = 0.65)) # height  <-  full set of plots
steps <- list()

for (i in seq_along(roadNames)) {

    plot_tmp <- add_lines(plot_tmp,
                          data = plot_data %>%
                                      filter(statistic == "max"),

                                  y = ~ value,
                                  x = ~ monthOfYear,
                                  text = ~ tooltip,
                                  visible = (i == 1),
                                  name = ~ if_else(!is.na(statistic), statistic, metric), # order matters

                                  type = "scatter",
                                  mode = "lines",
                                  hoverinfo = "text",
                                  color = ~ paste0(if_else(is.na(statistic), "", paste0(statistic, "_")), metric),
                                  colors = weather_metrics_colour_scheme
                         )

    step <- list(args = list("visible", rep(FALSE, length(roadNames))),
               label = roadNames[i],
               method = "restyle")
    step$args[[2]][i] <- TRUE
    steps[[i]] <- step
}
for (i in seq_along(roadNames)) {

    plot_tmp <- add_lines(plot_tmp,
                          data = plot_data %>%
                                      filter(statistic == "min"),

                                  y = ~ value,
                                  x = ~ monthOfYear,
                                  text = ~ tooltip,
                                  visible = (i == 1),
                                  name = ~ if_else(!is.na(statistic), statistic, metric), # order matters

                                  type = "scatter",
                                  mode = "lines",
                                  hoverinfo = "text",
                                  color = ~ paste0(if_else(is.na(statistic), "", paste0(statistic, "_")), metric),
                                  colors = weather_metrics_colour_scheme
                         )

    step <- list(args = list("visible", rep(FALSE, length(roadNames))),
               label = roadNames[i],
               method = "restyle")
    step$args[[2]][i] <- TRUE
    steps[[i]] <- step

}

plot_weather_temp_lerwick <- plot_tmp %>%
    layout(yaxis = list(title = "Avg. Temp (C)", tickfont = tickFont, ticks = "outside", showline = TRUE,
                        linecolor = "rgb(175, 175, 175)", mirror = TRUE, zerolinecolor = "rgb(215, 215, 215)",
                        zeroline = TRUE, rangemode = "tozero")
               )

# historical_weather_lerwick_rainfall --------

plot_data <- historical_weather_scotland_from_2017 %>%

                                filter((region == "Scotland") & (weather_station == "Lerwick") & (metric == "rainfall")) %>%

                                left_join(cycle_counter_data_from_2017 %>%
                                              filter((traffic_mode == "bicycle") & (Location == "Lerwick")) %>%
                                              group_by(Location) %>%
                                              summarise(across(date, list(start_date = min, end_date = max), .names = "{.fn}")),
                                          by = c("weather_station" = "Location")
                                             ) %>%
                                filter(monthOfYear %within% interval(floor_date(start_date, unit = "month"), ceiling_date(end_date, unit = "month"))) %>%

                                mutate(tooltip = paste(value, "mm", "-", month, year))


#plot_tmp <- plot_ly(height = 280, width = 950, line = list(width = 0.65))
plot_tmp <- plot_ly(height = 800, width = 1300, line = list(width = 0.65)) # height  <-  full set of plots
steps <- list()

for (i in seq_along(roadNames)) {

    plot_tmp <- add_lines(plot_tmp,
                          data = plot_data,

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
                                  showlegend = FALSE)

    step <- list(args = list("visible", rep(FALSE, length(roadNames))),
               label = roadNames[i],
               method = "restyle")
    step$args[[2]][i] <- TRUE
    steps[[i]] <- step

}


plot_weather_rainfall_lerwick <- plot_tmp %>%
    layout(yaxis = list(title = "Avg. Rainfall (mm)", tickfont = tickFont, ticks = "outside", showline = TRUE,
                        linecolor = "rgb(175, 175, 175)", mirror = TRUE, zerolinecolor = "rgb(168,169,173)", # metallic grey,
                        zeroline = TRUE, rangemode = "tozero")
               )


rm(plot_data)

## ---- cycling_as_the_seasons_change_lerwick --------

subplot(plot_location_by_month_lerwick, plot_location_by_month_medians_lerwick,
            plot_weather_rainfall_lerwick, plot_weather_temp_lerwick,
        titleY = TRUE,
        shareX = TRUE,
        heights = c(0.3, 0.3, 0.2, 0.2),
        nrows = 4) %>%

    layout(title = list(text = "Cycling in Lerwick - As the Seasons Change", font = list(size = 22)),
           xaxis = list(title = "", tickfont = tickFont, tickangle = -45, ticks = "outside", #showgrid = FALSE,
                        autotick = FALSE, dtick = "M2", tickformat = "%b-%Y", showline = TRUE,
                        linecolor = "rgb(175, 175, 175)", zeroline = TRUE),
           updatemenus = list(list(active = 0, x = 0.245, y = 1.065, #1.12,
                                   buttons = steps
                                  )), # end dropdown,
           legend = list(y = 0.1),
           margin = list(l = 15, t = 5)
           ) %>%
    config(modeBarButtonsToRemove = c("zoom2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "pan2d")) %>%

    suppressWarnings() # can't find where warning about factor is being triggered - somewhere in the loop :(


## ----  --------


## ----  --------
