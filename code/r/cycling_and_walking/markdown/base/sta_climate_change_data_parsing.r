###
#
# having to replace -Inf with -(.Machine$integer.max) -
# defined in sta_climate_change_common_vars as InfInt
# new version of r cant cope with Inf in integer columns and need counts to stay aas ints
#
###

## ---- nmf_count_summaries --------
nmf_count_summaries <- cycle_counter_data_from_2017 %>%
    filter(Provider == default_provider) %>%

    group_by(traffic_mode) %>%
    summarise(count = formatNumber(sum(count), 0.1)) %>%
    deframe()




## ---- filter actual records from paddimg needed for plotly sliders --------
start_date <- min(cycle_counter_data_from_2017$date)
end_date <- max(cycle_counter_data_from_2017$date)


## ---- pad counter data to obtain rectangular dataset --------
padding_cycle_counter_data_from_2017 <-
  as.data.frame(levels(cycle_counter_data_from_2017$year)) %>%
  cross_join(as.data.frame(levels(cycle_counter_data_from_2017$month))) %>%
  cross_join(as.data.frame(levels(as.ordered(cycle_counter_data_from_2017$time)))) %>%
  rename_with(~ c("year", "month", "time")) %>%

  mutate(monthOfYear = parse_date(paste0(month, "-", year), format = "%b-%Y")) #%>%
  #filter(between(monthOfYear, start_date, end_date))



## ---- counter_installation_data --------

counter_data <- reporting_sites %>%

    distinct(Provider, siteID, site, LocalAuthority, Location, RoadName, CycleCounter) %>%
    filter(!is.na(CycleCounter)) %>% # & !is.na(Location)) %>%
    left_join(cycle_counter_data_from_2017 %>%
                     distinct(siteID, site, Location, RoadName, traffic_mode, total_by_site) %>%
                     filter(traffic_mode == "bicycle") %>%
                     select(-traffic_mode)) %>% 

    mutate(across(c(LocalAuthority, Location), as.character),
           LocationLabel = case_when(is.na(LocalAuthority) ~ Location, # really shouldn't be happening
                                     (LocalAuthority == Location) ~ paste0(LocalAuthority, " (Unspecified Location)")),
           Location = fct_na_value_to_level(Location, level = "Unspecified Location"),
           across(LocationLabel, ~ if_else(!is.na(LocationLabel), ., paste0(Location, " (", LocalAuthority, ")"))),
           across(c(LocalAuthority, Location, LocationLabel), as.factor)
          ) %>%

    group_by(Provider, LocalAuthority, Location, LocationLabel, CycleCounter, total_by_site) %>%
    tally() %>%
    rename(bicycle_counters = n) %>%
    arrange(CycleCounter)


counter_data <- counter_data %>%

    group_by(Provider, LocalAuthority, Location, LocationLabel, CycleCounter) %>%
    summarise(bicycle_counters = sum(bicycle_counters)) %>%

    full_join(counter_data %>%

                group_by(Provider, LocalAuthority, Location) %>%
                summarise(LatestInstallation = max(CycleCounter))
              ) %>%
    ungroup() %>%

    filter(Location != "Unspecified Location")



## ---- count_by_location --------

# NOTE - multiple counter installations (CycleCounter) in some months -
# if summarising further need to take distinct rows excluding this variable
#
# Also, Locations (names) not unique, so need to include LocalAuthority (or site) when grouping

counters_reporting_data <- cycle_counter_data_from_2017 %>%
    distinct(siteID, site, Location, Provider) %>%

    semi_join(reporting_sites, by = c("siteID" = "externalId")) %>%
    distinct(siteID) %>% # reported with externalId

    bind_rows(reporting_sites %>%

                semi_join(
                    cycle_counter_data_from_2017 %>%
                        distinct(siteID, site, Location, Provider) %>%

                        anti_join(reporting_sites, by = c("siteID" = "externalId")),
                    
                        by = "siteID"

                    ) %>%
                distinct(siteID)
    ) %>% # reported with siteID

    deframe() %>%
    as.character()


count_by_location <- padding_cycle_counter_data_from_2017 %>%
    distinct(year, month) %>%

    cross_join(reporting_sites %>%
                  filter((externalId %in% counters_reporting_data) |
                         ((siteID %in% counters_reporting_data) & !(siteID %in% externalId))) %>%
                  distinct(site, LocalAuthority, Location), #CycleCounter)
                  ) %>%

    full_join(cycle_counter_data_from_2017 %>%

                filter(traffic_mode == "bicycle") %>%
                group_by(site, Location, year, month, date) %>%
                summarise(count = sum(count, na.rm = TRUE)) %>%

                group_by(site, Location, year, month) %>%
                summarise(across(count, list(average = mean, median = median), .names = "daily_{.fn}"),
                          across(count, sum)
                         ) %>%

                left_join(cycle_counter_data_from_2017 %>%

                            filter(traffic_mode == "bicycle") %>%

                            group_by(siteID, site, Location, year, date) %>%
                            summarise(count = sum(count, na.rm = TRUE)) %>%

                            group_by(site, Location, year) %>%
                            summarise(across(count, list(average = mean, median = median),
                                             .names = "daily_{.fn}_by_siteID_and_year"),
                                      count_by_year = sum(count),
                                      counter_count_by_year = n_distinct(siteID)
                                     )
                ) %>% # end internal join

                left_join(cycle_counter_data_from_2017 %>%

                            filter(traffic_mode == "bicycle") %>%

                            group_by(siteID, site, Location, year, month, date) %>%
                            summarise(count = sum(count, na.rm = TRUE)) %>%

                            group_by(site, Location, year, month) %>%
                            summarise(across(count, list(average = mean, median = median),
                                             .names = "daily_{.fn}_by_siteID"),
                                      across(count, sum),
                                      counter_count = n_distinct(siteID)
                                     )
                ) %>% # end internal join

                relocate(daily_average_by_siteID, daily_average_by_siteID_and_year, daily_median, daily_median_by_siteID,
                           daily_median_by_siteID_and_year, counter_count, .after = daily_average) %>%
                relocate(counter_count_by_year, count_by_year, .after = count) %>%

                #arrange(year, month) %>%

                group_by(site, Location) %>%
                mutate(lag = lag(daily_average_by_siteID),
                       growth_rate_weighted_by_location = (daily_average_by_siteID - lag) / lag) %>%
                relocate(lag, growth_rate_weighted_by_location, .before = counter_count)
             ) %>% # end join


    #mutate(across(where(is.factor), as.character)) %>%
    drop_na(Location, LocalAuthority) %>%


    mutate(monthOfYear = paste0(month, "-", year),
           across(monthOfYear, ~ parse_date(., format = "%b-%Y"))
           ) %>%

    relocate(monthOfYear, .after = month) %>%

    # for now exclude - inspect shows these as count NA so won't show up in charts regardless
    # actual values noted, will need a manual pass for reverse geocode
    #drop_na(LocalAuthority) %>%
    #mutate(across(LocalAuthority), ~replace(., is.na(.), "Not Known")) %>%
#                mutate(across(c(LocalAuthority, site, Location), ~ fct_drop(.))) %>%
    #mutate(across(LocalAuthority), ~ fct_relevel(., "Not Known", after = Inf)) %>%

    mutate(across(c(LocalAuthority, Location), as.character),
           distinct_location = paste0(Location, "-", site),
           location_label_site = paste0(Location, " (", site, ")"),
           location_label_local_authority = case_when(if_all(c(LocalAuthority, Location), ~ (. == "Stirling")) ~ paste0(Location, " (", LocalAuthority, ")"),
                                                      (LocalAuthority == Location) ~ paste0(LocalAuthority, " (Unspecified Location)"),
                                                      TRUE ~ paste0(Location, " (", LocalAuthority, ")")
                                                     ),
           across(c(LocalAuthority, Location, distinct_location) | matches("^location_label"), as.factor)
          ) %>%
    relocate(distinct_location, matches("^location_label"), .after = Location) %>%

    mutate(across(year, as.ordered),
           across(month, ~ factor(., levels = month.abb)),
           pseudo_point = if_else(is.na(daily_average), 1, 0), # need this and next to generate full trace sets and legend or animation breaks
           tooltip = if_else(as.logical(pseudo_point),
                             "",
                             paste(location_label_local_authority, "-", formatNumber(count),
                                   paste0("bicycle", if_else(count == 1, " in", "s in")), monthOfYear,
                                   "\nAverage daily count:", formatNumber(daily_average, 1),
                                   paste0("(across ", counter_count, " counter", if_else(counter_count == 1, ")", "s)")))
                            ),
           tooltip_median = if_else(as.logical(pseudo_point),
                                    "",
                                     paste(location_label_local_authority, "-", formatNumber(count),
                                           paste0("bicycle", if_else(count == 1, " in", "s in")), format(monthOfYear, "%b-%Y"),
                                           "\nTypical daily count:", formatNumber(daily_median, 1),
                                           paste0("(across ", counter_count, " counter", if_else(counter_count == 1, ")", "s)")))
                                   ),
           tooltip_median_siteID_by_month = if_else(as.logical(pseudo_point),
                                                    "",
                                                     paste(location_label_local_authority, "-", formatNumber(count),
                                                           paste0("bicycle", if_else(count == 1, " in", "s in")), format(monthOfYear, "%b-%Y"),
                                                           "\nTypical daily count:", formatNumber(daily_median_by_siteID, 1),
                                                           paste0("(across ", counter_count, " counter", if_else(counter_count == 1, ")", "s)")))
                                                   ),
           tooltip_median_siteID_by_month = if_else(as.logical(pseudo_point),
                                                    "",
                                                     paste(location_label_local_authority, "-", formatNumber(count),
                                                           paste0("bicycle", if_else(count == 1, " in", "s in")), format(monthOfYear, "%b-%Y"),
                                                           "\nTypical daily count:", formatNumber(daily_median_by_siteID, 1),
                                                           paste0("(across ", counter_count, " counter", if_else(counter_count == 1, ")", "s)")))
                                                   ),
           tooltip_median_siteID_by_year = if_else(as.logical(pseudo_point),
                                                     "",
                                                     paste(location_label_local_authority, "-", formatNumber(count_by_year),
                                                           paste0("bicycle", if_else(count_by_year == 1, " in", "s in")), year,
                                                           "\nTypical daily count:", formatNumber(daily_median_by_siteID_and_year, 1),
                                                           paste0("(across ", counter_count_by_year, " counter", if_else(counter_count_by_year == 1, ")", "s)")))
                                                  ),

           across(c(daily_average, daily_median, count, count_by_year), ~ replace_na(., -InfInt))
          )


## ---- counts_time_of_day_by_month_and_year --------
counts_by_month <- padding_cycle_counter_data_from_2017 %>%

            full_join(cycle_counter_data_from_2017 %>%

                            filter(traffic_mode == "bicycle") %>%
                            group_by(year, month, time) %>% 
                            summarise(total = sum(count),              
                                      average = mean(count)) 
                     ) %>%

            mutate_at(vars(year, time), as.ordered) %>%
            mutate(month = factor(month, levels = month.abb)) %>%
            mutate(pseudo_point = if_else(is.na(average), 1, 0), # need this and next to generate full trace sets and legend or animation breaks
                   tooltip = if_else(as.logical(pseudo_point),
                                     "", 
                                     paste("Average count,", month, ",", time, "-" , round(average, 2)))
                   ) %>%
            mutate_at(vars(total, average), ~ replace_na(., -InfInt))



## ---- historical_weather_scotland_from_2017 --------

datebreaks <- seq(min(historical_weather_scotland_from_2017$monthOfYear),
                  max(historical_weather_scotland_from_2017$monthOfYear), by = "2 months")



## ---- count_by_location_lerwick --------

count_by_location_lerwick <- padding_cycle_counter_data_from_2017 %>%
    distinct(year, month) %>%

    cross_join(reporting_sites %>%
                  distinct(LocalAuthority, Location, siteID, RoadName, CycleCounter) %>%
                  filter(Location == "Lerwick")
              ) %>%

    full_join(cycle_counter_data_from_2017 %>%

                filter((traffic_mode == "bicycle") & (Location == "Lerwick")) %>%
                group_by(Location, siteID, RoadName, year, month, date) %>%
                summarise(count = sum(count, na.rm = TRUE)) %>%

                group_by(Location, siteID, RoadName, year, month) %>%
                summarise(across(count, list(average = mean, median = median), .names = "daily_{.fn}"),
                          across(count, sum)
                         ) %>%
                ungroup() %>%

                mutate(lag = lag(count),
                       GrowthRate = (count - lag(count)) / lag(count)) %>%
                relocate(GrowthRate, .after = count) %>%
                relocate(lag, .after = count)
             ) %>% # end join

    #mutate(across(where(is.factor), as.character)) %>%
    drop_na(Location, LocalAuthority) %>%


    mutate(across(c(LocalAuthority, siteID, Location), ~ fct_drop(.)),
           monthOfYear = paste0(month, "-", year),
           across(monthOfYear, ~ parse_date(., format = "%b-%Y"))
           ) %>%

    relocate(monthOfYear, .after = month) %>%

    mutate(across(RoadName, as.factor),
           across(year, as.ordered),
           across(month, ~ factor(., levels = month.abb)),
           pseudo_point = if_else(is.na(daily_average), 1, 0), # need this and next to generate full trace sets and legend or animation breaks
           tooltip = if_else(as.logical(pseudo_point),
                             "",
                             paste(RoadName, "-", month, year,
                                   "\nAverage daily count:", formatNumber(round(daily_average)),
                                   "\nTotal count:", formatNumber(count))),
           tooltip_median = if_else(as.logical(pseudo_point),
                             "",
                             paste(RoadName, "-", month, year,
                                   "\nTypical daily count:", formatNumber(round(daily_median)),
                                   "\nTotal count:", formatNumber(count))),

           across(c(daily_average, daily_median, count), ~ replace_na(., -InfInt))
          ) %>%

    left_join(cycle_counter_data_from_2017 %>%
                  filter((traffic_mode == "bicycle") & (Location == "Lerwick")) %>%
                  group_by(Location) %>%
                  summarise(across(date, list(start_date = min, end_date = max), .names = "{.fn}"))
                 ) %>%

    filter(monthOfYear %within% interval(floor_date(start_date, unit = "month"), ceiling_date(end_date, unit = "month")))


## ----  --------
