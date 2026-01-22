

dbConn <- dbConnect(RSQLite::SQLite(), "data/cycling-snapshots/traffic-counts-db.sqlite")
dbConn

# issues with input data means original filter query will fail - new query does a negative pull,
# then corrects EndDateTime during parse

all_modes_survey_from_2017 <-
    dbGetQuery(dbConn,
               paste("SELECT * FROM traffic_biannual_snapshots",
                     if_else((length(countInterval) < 1), "", 
                             #paste0("WHERE (countInterval <> '", countInterval, "')")),
                             paste0("WHERE (countInterval NOT IN ('", str_flatten(countInterval, "', '"), "'))")
                            )
                    ))


count_interval <- setdiff(count_intervals, countInterval)
count_interval <- (#60 * 
                   case_when((count_interval == "quarter_hour") ~ 15,
                             (count_interval == "hour") ~ 60,
                             # (count_interval == "half_hour") ~ 30,
                             # no default - include new as defined
                            )
                  ) 

all_modes_survey_from_2017 <- all_modes_survey_from_2017 |>

    left_join(dbGetQuery(dbConn, "SELECT Code, LocalAuthority, Location, RoadName, RoadNumber, RoadType FROM traffic_biannual_snapshots_location_metadata")
    ) |>

    mutate(Date = as_datetime(StartDateTime), 
            CountPeriod = format(Date, "%b-%Y"),

            TimePeriod = format(Date, format = "%H:%M"),
            across(TimePeriod, ~ str_sub(., end = 5)),
            TimePeriodEnd = format((Date + (count_interval * 60)), format = "%H:%M"),
            across(TimePeriodEnd, ~ str_sub(., end = 5)),
            across(TimePeriod, ~ paste0(., "-", TimePeriodEnd)),

            across(Date, ~ as.numeric(as_date(.))),
           ) |>
    select(- TimePeriodEnd) |>
    relocate(LocalAuthority, Location, RoadName, RoadNumber ,RoadType, .after = Code) |>
    relocate(Date, CountPeriod, TimePeriod, .before = countInterval) |>

    parseTrafficSurveyDataFromDB(breakDownDates = TRUE)



transportation_modes <- dbGetQuery(dbConn, "SELECT mode, label FROM transportation_modes") |>
    deframe()

transportation_types <- dbGetQuery(dbConn, "SELECT DISTINCT type FROM transportation_modes") |>
    deframe()

transportation_type <- dbGetQuery(dbConn, "SELECT mode, type FROM transportation_modes") |>
    deframe()

dbDisconnect(dbConn)

#rm(count_interval)



