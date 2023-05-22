

dbConn <- dbConnect(RSQLite::SQLite(), "data/cycling-snapshots/traffic-counts-db.sqlite")
dbConn

cycle_counter_locations <- dbGetQuery(dbConn, "SELECT * FROM fixed_counter_locations_metadata")
#head(cycle_counter_locations)


reporting_sites <- dbGetQuery(dbConn, "SELECT * FROM fixed_counter_locations")

reporting_sites <- reporting_sites %>%
    rename(siteID = id) %>%
    select(-c(batteryLevel, lastConnected, type, offlineAfterMinutes, Location, RoadName, Latitude, Longitude)) %>%

    left_join(dbGetQuery(dbConn, "SELECT * FROM local_authorities")) %>%
    full_join(cycle_counter_locations %>%
        mutate(across(SiteId, as.character)) %>%
        #rename(AltRoadName = RoadName) %>%
        select(- c(X, Y)),

        by = c("externalId" = "SiteId", "LocalAuthority")
    )

reporting_sites <- reporting_sites %>%
    filter(!is.na(siteID)) %>%
    #mutate(across(c(siteID, status, site, externalId, LocalAuthority, Location, RoadType), as.factor) %>%

    relocate(LocalAuthority, .before = Location) %>%
    relocate(RoadType, .after = RoadName) %>%
    relocate(RoadNumber, .after = RoadName) %>%
    relocate(AltRoadName, .after = RoadName) %>%
    mutate(across(c(CycleCounter, PedestrianCounter), ~ parse_date(., "%b-%y")))

#str(reporting_sites)
#head(reporting_sites)


non_nmf_counter_metadata <- dbGetQuery(dbConn, "SELECT * FROM non_nmf_counter_locations_metadata")


non_nmf_counter_metadata <- non_nmf_counter_metadata %>%
    mutate(across(c(start_date, end_date, ends_with("Counter")), as_date),
           across(c(Provider, siteID, site, LocalAuthority, Location, RoadName, RoadType), as.factor),
           across(Provider, ~ fct_relevel(., "North East Trunk Roads", "North West Trunk Roads", "South East Trunk Roads",
                                          "South West Trunk Roads", "Sustrans", "John Muir Way", after = Inf)),
           ) 

reporting_sites <- reporting_sites %>%
    mutate(Provider = default_provider) %>%

    bind_rows(non_nmf_counter_metadata %>%
                select(- c(start_date, end_date, Notes)) %>%
                rename(externalId = siteID)
              ) %>%
    mutate_if(is.factor, as.character) %>%
    mutate(across(siteID, ~ coalesce(., externalId)),
           across(c(siteID, status, site, externalId, LocalAuthority, Location, RoadType, Provider), as.factor),
           across(Provider, ~ fct_relevel(., default_provider)),
           across(Provider, ~ fct_relevel(., "North East Trunk Roads", "North West Trunk Roads", "South East Trunk Roads",
                                          "South West Trunk Roads", "Sustrans", "John Muir Way", after = Inf)),
           ) #%>%

    # interim to deal with issues with Glasgow spike in Jan 2022
    #filter(!((site == "GLG") & (externalId == "0105")))



cycle_counter_data_from_2017 <- reporting_sites %>%
    select(Provider, site, siteID, Location, RoadName, Latitude, Longitude) %>%

    right_join(dbGetQuery(dbConn, "SELECT * FROM counter_data_hourly") %>%
                   select(- c(Location, RoadName, Latitude, Longitude))
               )


#dim(cycle_counter_data_from_2017)
#head(cycle_counter_data_from_2017)

cycle_counter_data_from_2017 <- cycle_counter_data_from_2017 %>%
    parseCounterDataFromDB() #%>%
    # interim to deal with issues with Glasgow spike in Jan 2022
    #filter(!((Provider == "Glasgow City Council") & (siteID == "0105")))


# apply counter filter - catch unusually high reads

#site_max_cutoff <- 5000
site_mean_cutoff <- 2000
#high_reads_cutoff <- 1000

source("base/wonky_counter_filter.r")

cycle_counter_data_from_2017 <- cycle_counter_data_from_2017 %>%
    filter(traffic_mode == "bicycle") %>%
    mutate(monthOfYear = paste(month, year, sep = "-"),
           across(monthOfYear, as.factor),
           across(monthOfYear, ~ fct_reorder(., as.numeric(date)))
          ) %>%

    anti_join(counter_filter_max_values %>%
                filter((site_max > 1800) | # tmp_cycle_counter_data_from_2017
                       ((la_max > ((la_median + 1) * 100)) & (site_max >= (la_max / 2)) & (site_max >= 2500)))  %>%
                distinct() %>%
                rename(site_count = count) %>%

                left_join(cycle_counter_data_from_2017 %>%
                            filter(traffic_mode == "bicycle") %>%
                            mutate(monthOfYear = paste(month, year, sep = "-"),
                                   across(monthOfYear, as.factor),
                                   across(monthOfYear, ~ fct_reorder(., as.numeric(date)))) %>%
                            select(Provider, siteID, site, Location, RoadName, count, date, time, weekday, monthOfYear) #%>%
                ) %>%
                filter(((count * 10) > la_max) & (count > 1500)) %>%
                distinct(site, siteID, monthOfYear)
    ) %>%

    bind_rows(cycle_counter_data_from_2017 %>%
                filter(traffic_mode == "pedestrian"))


cycle_counter_data_from_2017 <- cycle_counter_data_from_2017  %>%
  anti_join(counter_filter_zero_daily_counts %>%
              filter(month_at_zero) %>%
              distinct(traffic_mode, site, siteID, monthOfYear)
           )

## END - apply counter filter - catch unusually high reads and filter out zero reads spanning a full month


record_total <- nrow(cycle_counter_data_from_2017)

# filter out anything beyond previous month - current month's totals sometimes change, depending on reporting lag
# and also from counter installation to period when counts went over 0
max_reporting_date <- as_date(now())
day(max_reporting_date) <- 1

# will be overwritten after with unfiltered records
counter_reporting_metadata <- cycle_counter_data_from_2017 %>%

    group_by(Provider, traffic_mode, siteID, site, Location, RoadName, date) %>%
    summarise(date = min(date)) %>%
    rename(record_start_date = date) %>%

    arrange(record_start_date) %>%
    slice_head(n = 1) %>%
    ungroup() %>%

    left_join(cycle_counter_data_from_2017 %>%
                group_by(Provider, traffic_mode, siteID, year, month, date) %>%
                summarise(start_date = min(date),
                          end_date = max(date),
                          no_of_counters = n_distinct(siteID),
                          median = median(count, na.rm = TRUE),
                          average = mean(count, na.rm = TRUE),
                          count = sum(count)) %>%

                slice(which(count > 0)) %>%

                group_by(Provider, traffic_mode, siteID) %>%
                summarise(across(start_date, min),
                          across(end_date, max),
                          median = median(count, na.rm = TRUE),
                          average = mean(count, na.rm = TRUE),
                          across(count, sum)
                          ) %>%
                ungroup()
    )


cycle_counter_data_from_2017 <- cycle_counter_data_from_2017 %>%

    left_join(counter_reporting_metadata %>%
                  select(Provider, traffic_mode, siteID, start_date)) %>%

    filter((date >= start_date) & #between(date, start_date, max_reporting_date)) %>% - between throwing an error ...
           (date < max_reporting_date)) %>%
    select(-start_date)

rm(max_reporting_date)
filtered_record_total <- nrow(cycle_counter_data_from_2017)


cycle_counter_data_from_2017 <- cycle_counter_data_from_2017 %>%

    left_join(cycle_counter_data_from_2017 %>%
        group_by(site, Location, traffic_mode) %>%
        summarise(total_by_site = sum(count, na.rm = TRUE))
    ) %>%

    left_join(cycle_counter_data_from_2017 %>%
        group_by(site, Location, year, traffic_mode) %>%
        summarise(total_by_site_and_year = sum(count, na.rm = TRUE))
    ) %>%

    left_join(cycle_counter_data_from_2017 %>%
        group_by(site, siteID, year, traffic_mode) %>% #, Location, RoadName) %>%
        summarise(total_by_counter_and_year = sum(count, na.rm = TRUE))
    ) %>%

    mutate(across(time, as.factor),
           across(Provider, ~ fct_relevel(., default_provider)),
           across(Provider, ~ fct_relevel(., "North East Trunk Roads", "North West Trunk Roads", "South East Trunk Roads",
                                          "South West Trunk Roads", "Sustrans", "John Muir Way", after = Inf)),
          )



historical_weather_scotland_from_2017 <- dbGetQuery(dbConn, "SELECT * FROM historical_weather_scotland_from_2017") %>%
    parseMeteoDataFromDB()

#dbDisconnect(dbConn)




