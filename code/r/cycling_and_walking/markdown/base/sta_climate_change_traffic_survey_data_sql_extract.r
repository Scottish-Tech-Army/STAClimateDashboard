

dbConn <- dbConnect(RSQLite::SQLite(), "data/cycling-snapshots/traffic-counts-db.sqlite")
dbConn


all_modes_survey_from_2017 <- dbGetQuery(dbConn, paste("SELECT * FROM traffic_biannual_snapshots",
                                                       if_else(countInterval == "", "", paste0("WHERE (countInterval = '", countInterval, "')"))
                                                      ))


all_modes_survey_from_2017 <- parseTrafficSurveyDataFromDB(all_modes_survey_from_2017, breakDownDates = TRUE)


dbDisconnect(dbConn)



