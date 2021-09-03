

dbConn <- dbConnect(RSQLite::SQLite(), "data/cycling-snapshots/traffic-counts-db.sqlite")
dbConn

cycle_counter_locations <- dbGetQuery(dbConn, "SELECT * FROM fixed_counter_locations_metadata")
head(cycle_counter_locations)
reporting_sites <- dbGetQuery(dbConn, "SELECT * FROM fixed_counter_locations")

reporting_sites <- reporting_sites %>%
  rename(siteID = id) %>%
  select(-c(batteryLevel, lastConnected, type, offlineAfterMinutes)) %>%

  full_join(cycle_counter_locations %>%
              mutate_at(vars(SiteId), as.character) %>%
              rename(AltRoadName = RoadName) %>%
              select(-c(Location, Latitude, Longitude, X, Y)),

    by = c("externalId" = "SiteId")
  )

reporting_sites <- reporting_sites %>%
  mutate_at(vars(siteID, status, site, externalId, Location, RoadType), as.factor) %>%

  relocate(LocalAuthority, .before = Location) %>%
  relocate(RoadType, .after = RoadName) %>%
  relocate(RoadNumber, .after = RoadName) %>%
  relocate(AltRoadName, .after = RoadName) %>%
  mutate_at(vars(CycleCounter, PedestrianCounter), ~ parse_date(., "%b-%y"))

str(reporting_sites)
head(reporting_sites)


cycle_counter_data_2017_2021 <- dbGetQuery(dbConn, "SELECT * FROM counter_data_hourly")

dim(cycle_counter_data_2017_2021)
head(cycle_counter_data_2017_2021)

cycle_counter_data_2017_2021 <- cycle_counter_data_2017_2021 %>%
  parseCounterDataFromDB(TRUE)

cycle_counter_data_2017_2021 <- cycle_counter_data_2017_2021 %>%

  left_join(cycle_counter_data_2017_2021 %>%
    group_by(site, Location, traffic_mode) %>%
    summarise(total_by_site = sum(count, na.rm = TRUE))
  ) %>%

  left_join(cycle_counter_data_2017_2021 %>%
    group_by(site, Location, year, traffic_mode) %>%
    summarise(total_by_site_and_year = sum(count, na.rm = TRUE))
  ) %>%

  left_join(cycle_counter_data_2017_2021 %>%
    group_by(siteID, year, traffic_mode) %>% #, Location, RoadName) %>%
    summarise(total_by_counter_and_year = sum(count, na.rm = TRUE))
  )


start_date <- min(cycle_counter_data_2017_2021$date)
end_date <- max(cycle_counter_data_2017_2021$date)


padding_cycle_counter_data_2017_2021 <-
  as.data.frame(levels(cycle_counter_data_2017_2021$year)) %>%
  full_join(as.data.frame(levels(cycle_counter_data_2017_2021$month)),
              by = character()) %>%
  full_join(as.data.frame(levels(as.ordered(cycle_counter_data_2017_2021$time))),
              by = character()) %>%
  rename_with(~ c("year", "month", "time")) %>%

  mutate(monthOfYear = parse_date(paste0(month, "-", year), format = "%b-%Y")) #%>%
  #filter(between(monthOfYear, start_date, end_date))


count_by_location <- padding_cycle_counter_data_2017_2021 %>%
    distinct(year, month) %>%

    full_join(reporting_sites %>%
                  distinct(LocalAuthority, Location, CycleCounter),
              by = character()
             ) %>%

    full_join(cycle_counter_data_2017_2021 %>%

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



dbDisconnect(dbConn)
