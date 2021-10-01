
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

    distinct(siteID, site, LocalAuthority, Location, RoadName, CycleCounter) %>%
    filter(!is.na(CycleCounter)) %>% # & !is.na(Location)) %>%
    left_join(cycle_counter_data_from_2017 %>%
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



## ---- count_by_location --------
count_by_location <- padding_cycle_counter_data_from_2017 %>%
    distinct(year, month) %>%

    full_join(reporting_sites %>%
                  distinct(LocalAuthority, Location, CycleCounter),
              by = character()
             ) %>%

    full_join(cycle_counter_data_from_2017 %>%

                filter(traffic_mode == "bicycle") %>%
                group_by(Location, year, month) %>%
                summarise(average = mean(count),
                          count = sum(count)) %>%

                ungroup() %>%
                #arrange(year, month) %>%

                mutate(lag = lag(count),
                       GrowthRate = (count - lag(count)) / lag(count)) %>%
                relocate(GrowthRate, .after = count) %>%
                relocate(lag, .after = count)) %>% # end join

                drop_na(Location) %>%


                mutate(monthOfYear = paste0(month, "-", year)) %>%
                mutate_at(vars(monthOfYear), ~ parse_date(., format = "%b-%Y")) %>%

                relocate(monthOfYear, .after = month) %>%
    
                mutate_at(vars(LocalAuthority), ~replace(., is.na(.), "Not Known")) %>%
                mutate_at(vars(LocalAuthority, Location), as.factor) %>%
                mutate_at(vars(LocalAuthority), ~ fct_relevel(., "Not Known", after = Inf)) %>%

                mutate_at(vars(year), as.ordered) %>%
                mutate(month = factor(month, levels = month.abb)) %>%
                mutate(pseudo_point = if_else(is.na(average), 0, 1), # need this and next to generate full trace sets and legend or animation breaks
                       tooltip = if_else((pseudo_point == 0),
                                         "",
                                         paste("Average count by", monthOfYear, "-", round(average, 2),
                                               "\nTotal count by", monthOfYear, "-", count
                                              )),
                       average = replace_na(average, -Inf),
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



## ----  --------

## ----  --------

