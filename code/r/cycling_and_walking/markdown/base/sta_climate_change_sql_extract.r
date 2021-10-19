

dbConn <- dbConnect(RSQLite::SQLite(), "data/cycling-snapshots/traffic-counts-db.sqlite")
dbConn

cycle_counter_locations <- dbGetQuery(dbConn, "SELECT * FROM fixed_counter_locations_metadata")
#head(cycle_counter_locations)


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
    filter(!is.na(siteID)) %>%
    #mutate_at(vars(siteID, status, site, externalId, LocalAuthority, Location, RoadType), as.factor) %>%

    relocate(LocalAuthority, .before = Location) %>%
    relocate(RoadType, .after = RoadName) %>%
    relocate(RoadNumber, .after = RoadName) %>%
    relocate(AltRoadName, .after = RoadName) %>%
    mutate_at(vars(CycleCounter, PedestrianCounter), ~ parse_date(., "%b-%y"))

#str(reporting_sites)
#head(reporting_sites)

non_nmf_counter_metadata <- dbGetQuery(dbConn, "SELECT * FROM non_nmf_counter_locations_metadata")

non_nmf_counter_metadata <- non_nmf_counter_metadata %>%
    mutate_at(vars(start_date, end_date, ends_with("Counter")), as_date) %>%
    mutate_at(vars(Provider, siteID, site, LocalAuthority, Location, RoadName, RoadType), as.factor)

reporting_sites <- reporting_sites %>%
    mutate(Provider = default_provider) %>%

    bind_rows(non_nmf_counter_metadata %>%
                select(- c(start_date, end_date, average, count, Notes)) %>%
                rename(externalId = siteID)
              ) %>%
    mutate_if(is.factor, as.character) %>%
    mutate_at(vars(siteID, status, site, externalId, LocalAuthority, Location, RoadType, Provider), as.factor) 



cycle_counter_data_from_2017 <- dbGetQuery(dbConn, "SELECT * FROM counter_data_hourly")

#dim(cycle_counter_data_from_2017)
#head(cycle_counter_data_from_2017)

cycle_counter_data_from_2017 <- cycle_counter_data_from_2017 %>%
    parseCounterDataFromDB()
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
                summarise(start_date = min(start_date),
                          end_date = max(end_date),
                          average = mean(average, na.rm = TRUE),
                          count = sum(count)) %>%
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
        group_by(siteID, year, traffic_mode) %>% #, Location, RoadName) %>%
        summarise(total_by_counter_and_year = sum(count, na.rm = TRUE))
    )


dbDisconnect(dbConn)




