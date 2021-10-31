
## ---- filter actual records from paddimg needed for plotly sliders --------
start_date <- min(cycle_counter_data_from_2017$date)
end_date <- max(cycle_counter_data_from_2017$date)


## ---- pad counter data to obtain rectangular dataset --------
padding_cycle_counter_data_from_2017 <-
  as.data.frame(levels(cycle_counter_data_from_2017$year)) %>%
  full_join(as.data.frame(levels(cycle_counter_data_from_2017$month)),
              by = character()) %>%
  full_join(as.data.frame(levels(as.ordered(cycle_counter_data_from_2017$time))),
              by = character()) %>%
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

    mutate(Location = fct_explicit_na(Location, na_level = "Unspecified Location")) %>%
    mutate(LocationLabel = paste0(Location, " (", replace_na(LocalAuthority, ""), ")"))%>%
    mutate_at(vars(LocalAuthority, Location, LocationLabel), as.factor) %>%

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

    filter(Location != "Unspecified Location")



## ---- count_by_location --------

# NOTE - multiple counter installations (CycleCounter) in some months -
# if summarising further need to take distinct rows excluding this variable

count_by_location <- padding_cycle_counter_data_from_2017 %>%
    distinct(year, month) %>%

    full_join(reporting_sites %>%
                  distinct(LocalAuthority, Location, CycleCounter),
              by = character()
             ) %>%

    full_join(cycle_counter_data_from_2017 %>%

                filter(traffic_mode == "bicycle") %>%
                group_by(Location, year, month, date) %>%
                summarise(count = sum(count, na.rm = TRUE)) %>%
              
                group_by(Location, year, month) %>%
                summarise(daily_average = mean(count, na.rm = TRUE),
                          count = sum(count, na.rm = TRUE)) %>%

              
                left_join(cycle_counter_data_from_2017 %>%

                            filter(traffic_mode == "bicycle") %>%
                            group_by(siteID, Location, year, month, date) %>%
                            summarise(count = sum(count, na.rm = TRUE)) %>%
                          
                            group_by(siteID, Location, year, month) %>%
                            summarise(daily_average_by_siteID = mean(count, na.rm = TRUE)) %>%
                          
                            group_by(Location, year, month) %>%
                            summarise(daily_average_by_siteID = mean(daily_average_by_siteID),
                                      counter_count = n())
                          
                ) %>% # end internal join
              
                relocate(daily_average_by_siteID, .after = daily_average) %>%
                relocate(counter_count, .after = daily_average) %>%

              
                ungroup() %>%
                #arrange(year, month) %>%

                mutate(lag = lag(count),
                       GrowthRate = (count - lag(count)) / lag(count)) %>%
                relocate(GrowthRate, .after = count) %>%
                relocate(lag, .after = count)) %>% # end join

                mutate_if(is.factor, as.character) %>%
                drop_na(Location) %>%


                mutate(monthOfYear = paste0(month, "-", year)) %>%
                mutate_at(vars(monthOfYear), ~ parse_date(., format = "%b-%Y")) %>%

                relocate(monthOfYear, .after = month) %>%
    
                # for now exclude - inspect shows these as count NA so won't show up in charts regardless
                # actual values noted, will need a manual pass for reverse geocode
                drop_na(LocalAuthority) %>%
                #mutate_at(vars(LocalAuthority), ~replace(., is.na(.), "Not Known")) %>%
                mutate_at(vars(LocalAuthority, Location), as.factor) %>%
                #mutate_at(vars(LocalAuthority), ~ fct_relevel(., "Not Known", after = Inf)) %>%


                mutate_at(vars(year), as.ordered) %>%
                mutate(month = factor(month, levels = month.abb)) %>%
                mutate(pseudo_point = if_else(is.na(daily_average), 0, 1), # need this and next to generate full trace sets and legend or animation breaks
                       tooltip = if_else((pseudo_point == 0),
                                         "",
                                         paste(Location, "-", month, year,
                                               "\nAverage daily count:", formatNumber(round(daily_average)),
                                               paste0("(", counter_count, " counter", if_else(counter_count == 1, ")", "s)")),
                                               "\nTotal count:", formatNumber(count))),
                       daily_average = replace_na(daily_average, -Inf),
                       count = replace_na(count, -Inf))



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
            mutate(pseudo_point = if_else(is.na(average), 0, 1), # need this and next to generate full trace sets and legend or animation breaks
                   tooltip = if_else((pseudo_point == 0), 
                                     "", 
                                     paste("Average count,", month, ",", time, "-" , round(average, 2)))
                   ) %>%
            mutate_at(vars(total, average), ~ replace_na(., -Inf))



## ---- historical_weather_scotland_from_2017 --------

historical_weather_scotland_from_2017 <- parseMeteoData(here("data/historical_meteo_data/scotland_mean_temp.tsv"), "mean_temp",  start_date, end_date) %>%

    full_join(parseMeteoData(here("data/historical_meteo_data/scotland_max_temp.tsv"), "max_temp",start_date, end_date)
             ) %>%

    full_join(parseMeteoData(here("data/historical_meteo_data/scotland_min_temp.tsv"), "min_temp", start_date, end_date)
             ) %>%

    full_join(parseMeteoData(here("data/historical_meteo_data/scotland_rainfall.tsv"), "rainfall", start_date, end_date)
             ) %>%

    mutate_at(vars(year), as.ordered)

datebreaks <- seq(min(historical_weather_scotland_from_2017$monthOfYear),
                  max(historical_weather_scotland_from_2017$monthOfYear), by = "2 months")



## ---- historical_weather_lerwick_from_2017 --------

historical_weather_lerwick_from_2017 <- read_table(here("data/historical_meteo_data/lerwick.tsv")) %>%
                                                    filter(rowSums(is.na(.)) != ncol(.))

historical_weather_lerwick_from_2017 <- historical_weather_lerwick_from_2017 %>%

    rename_with(~ c("year", "month", "max_temp", "min_temp", "af", "rainfall", "sunshine")) %>%

    mutate_if(negate(is.numeric), parse_number) %>%

    filter(year >= year(start_date))%>%
    mutate_at(vars(year), as.ordered) %>%

    mutate_at(vars(month), ~ month(., label = TRUE)) %>%
    mutate(monthOfYear = parse_date(paste0(month, "-", year), format = "%b-%Y")) %>%
    relocate(monthOfYear, .after = month) %>%

    filter(monthOfYear %within% interval(start_date, end_date))



## ---- count_by_location_lerwick --------

count_by_location_lerwick <- padding_cycle_counter_data_from_2017 %>%
    distinct(year, month) %>%

    full_join(reporting_sites %>%
                  distinct(LocalAuthority, Location, RoadName, CycleCounter) %>%
                  filter(Location == "Lerwick"),
              by = character()
             ) %>%

    full_join(cycle_counter_data_from_2017 %>%

                filter((traffic_mode == "bicycle") & (Location == "Lerwick")) %>%
                group_by(Location, RoadName, year, month, date) %>%
                summarise(count = sum(count)) %>%
                group_by(Location, RoadName, year, month) %>%
                summarise(daily_average = mean(count),
                          count = sum(count)) %>%


                ungroup() %>%

                mutate(lag = lag(count),
                       GrowthRate = (count - lag(count)) / lag(count)) %>%
                relocate(GrowthRate, .after = count) %>%
                relocate(lag, .after = count)) %>% # end join

                mutate_if(is.factor, as.character) %>%
                drop_na(Location) %>%


                mutate(monthOfYear = paste0(month, "-", year)) %>%
                mutate_at(vars(monthOfYear), ~ parse_date(., format = "%b-%Y")) %>%

                relocate(monthOfYear, .after = month) %>%
    
                mutate_at(vars(LocalAuthority, Location, RoadName), as.factor) %>%

                mutate_at(vars(year), as.ordered) %>%
                mutate(month = factor(month, levels = month.abb)) %>%
                mutate(pseudo_point = if_else(is.na(daily_average), 0, 1), # need this and next to generate full trace sets and legend or animation breaks
                       tooltip = if_else((pseudo_point == 0),
                                         "",
                                         paste(RoadName, "-", month, year,
                                               "\nAverage daily count:", round(daily_average, 2),
                                               "\nTotal count:", formatNumber(count))),
                       daily_average = replace_na(daily_average, -Inf),
                       count = replace_na(count, -Inf)) %>%

    filter(between(monthOfYear, start_date, end_date))



## ----  --------

