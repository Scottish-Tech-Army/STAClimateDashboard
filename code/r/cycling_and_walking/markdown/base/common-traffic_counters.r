library(lubridate)

source("base/common.r")

#options(lubridate.week.start = 1) # need to group week-ends - not set, though...

label_all_bicycle_providers <- "All Bicycle Counters" #Data Providers"
default_provider <- "National Monitoring Framework (CS)"

transportation_modes <- c("Car", "Taxi", "LGV", "HGV", "ServiceBus", "Coach", "MCycle", "Cyclist", "Pedestrian")
traffic_direction_variables <- c("Context", "Direction", "Side", "Easting", "Northing")


cop_cycling_theme <- 
        theme_bw() +
        #theme_void() + 
        theme(panel.grid.major.y = element_line(), #panel.grid.minor.y = element_blank(),
                panel.grid.major.x = element_blank(), #panel.grid.minor.x = element_blank(), 
                axis.title = element_text(size = 18), 
                axis.text.x = element_text(size = 14), # angle = 0, vjust = 0.3),
                axis.text.y = element_text(size = 14), #element_markdown(size = 12), #
                plot.title = element_text(size = 20),
                legend.title = element_text(size = 16), legend.text = element_text(size = 14), 
                strip.text = element_text(size = 20)
            ) 


parseCounterDataFromDB <-
    function(counterData, glimpseContent = FALSE) {
                
        counterData <- counterData %>%
            mutate_at(vars(siteID, site, Location, countInterval, traffic_mode, Provider), as.factor) %>%
            relocate(Latitude, .before = Longitude) %>%  # correction to order, will have no impact if not needed
            relocate(Provider, .before = siteID) %>%
            mutate_at(vars(fromDate, toDate), as_datetime) %>%
            
            mutate(date = as_date(map_chr(str_split(localTimestamp, "T"), 1))) %>%
            mutate(time = format(map_chr(str_split(localTimestamp, "T"), 2), format = "%H:%M:S"),
                   time = str_sub(time, 1, 5),
                   hour = as.ordered(hour(as_datetime(map_chr(str_split(localTimestamp, "T"), 2), format = "%H:%M:%S"))),
                   year = as.ordered(year(date)),
                   month = month(date, label = TRUE),
                   weekday = wday(date, label = TRUE),
                   isWeekEnd = (as.integer(weekday) %in% c(1, 7)), #between(as.integer(weekday), 6, 7)) %>%
                  ) 
        

        if (glimpseContent)
            glimpse(counterData)
        
        invisible(counterData)        
    }


loadAndParseCounterData <-
    function(pathToFile, provider = NULL, glimpseContent = FALSE) {
        
        print(paste0("Parsing file '", pathToFile, "' ..."))
        
        
        data_loaded <- read_csv(pathToFile, trim_ws = T) %>% 
                            filter(rowSums(is.na(.)) != ncol(.))
        
        if (is_null(provider))
            provider <- default_provider


        data_loaded <- bind_cols(

            getMetadata(pathToFile),


            data_loaded %>%   

                pivot_longer(!time, names_to = "date", values_to = "count") %>%
                separate("date", c(NA, "date"), sep = "\\s*\\n") %>%

                mutate(date = parse_date(date, "%d-%b-%Y")) %>%
                mutate(weekday = wday(date, label = TRUE),
                       month = month(date, label = TRUE),
                       year = year(date), 
                       Provider = provider) %>%

                mutate_at(vars(count), as.integer) %>%
                mutate_at(vars(weekday, month, year), as.factor) %>%
                mutate(isWeekEnd = (as.integer(weekday) %in% c(1, 7))) %>% #between(as.integer(weekday), 6, 7)) %>%
                relocate(isWeekEnd, .after = "weekday") %>%

                select(-c(count, Provider), everything(), count, Provider)
            )

        
        if (glimpseContent)
            glimpse(data_loaded)
        
        invisible(data_loaded) 
    }


getMetadata <-
    function(pathToFile) {

        pathToFile <- basename(pathToFile)
        pathToFile

        metadata <- str_split(str_split(pathToFile, "\\.", simplify = TRUE)[1], "-", simplify = TRUE)
        metadata <- as.data.frame(metadata)

        if (length(metadata) == 4) { # site-traffic_mode-month-year
            names(metadata) <- c("site", "traffic_mode", "month", "year")

            metadata <- metadata %>%
                mutate(siteID = NA) %>%
                relocate(siteID, .after = site)
            
        } else if (length(metadata) == 5) { # site-siteID-traffic_mode-month-year
            names(metadata) <- c("site", "siteID", "traffic_mode", "month", "year")
        }

        metadata <- metadata %>%
            mutate_at(vars(site, siteID, traffic_mode), as.factor) %>%
            select(-c(month, year))


        metadata 
    }



# data and file structure differences in output to JSON from API
loadAndParseJsonCounterData <-
    function(pathTofile, localAuthorityData, breakDownDates = FALSE, provider = NULL, glimpseContent = FALSE) {
        
        print(paste0("Parsing file '", pathTofile, "' ..."))

        counter_data <- fromJSON(pathTofile) 
        counter_data <- counter_data$crossingCountPerTimeInterval 
        
        
        if (is_null(provider))
            provider <- default_provider

        counter_data <- counter_data %>%
            mutate(date = as_date(map_chr(str_split(localTimestamp, "T"), 1)), 
                   Provider = provider)
        
        if (breakDownDates) {
            counter_data <- counter_data %>%
                #mutate_at("localTimestamp", as_datetime)
                #mutate(date = as_date(str_split(localTimestamp, "T", simplify = TRUE)[1])) %>% #bthis works, but just switching to map for consistency
                #mutate(date = as_date(map_chr(str_split(localTimestamp, "T"), 1))) %>% # need it as interim regardless

                #mutate(time = str_split(localTimestamp, "T", simplify = TRUE)[2]) # within this only returning position 1 ...
                mutate(time = format(map_chr(str_split(localTimestamp, "T"), 2), format = "%H:%M:S"),
                       hour = as.ordered(hour(as_datetime(map_chr(str_split(localTimestamp, "T"), 2), format = "%H:%M:%S"))),
                       year = as.ordered(year(date)),
                       month = month(date, label = TRUE),
                       weekday = wday(date, label = TRUE),
                       isWeekEnd = (as.integer(weekday) %in% c(1, 7)), #between(as.integer(weekday), 6, 7)) %>%
                      ) 
        }


        counter_data <- bind_cols(getMetadataFromJson(basename(pathTofile)), counter_data) 
        
        counter_data <- localAuthorityData %>%
            #select(c(siteID, site, Location, RoadName, Latitude, Longitude)) %>%
            select(-c(status, externalId, AltRoadName)) %>%

            right_join(counter_data, by = c("siteID" = "siteID")) %>%
        
            # run filter, then remove interim columns
            filter(((traffic_mode == "bicycle") & (date >= CycleCounter)) | 
                   ((traffic_mode == "pedestrian") & (date >= PedestrianCounter))) %>%        
            select(-c(LocalAuthority, RoadNumber, RoadType, CycleCounter, PedestrianCounter)) %>%
            select(-Provider, everything(), Provider)


        if (!breakDownDates) {
            counter_data <- counter_data %>%
                select(-date)
        }

        
        if (glimpseContent)
            glimpse(counter_data)
        
        invisible(counter_data) 
    }


# site5-hour-1012018-31122018-Bicycle-EastBound-lanes
# siteID-countInterval-fromDate-toDate-vehicleClasses-trafficDirections-laneIds

getMetadataFromJson <-
    function(pathToFile) {

        pathToFile <- basename(pathToFile)
        pathToFile

        metadata <- str_split(str_split(pathToFile, "\\.", simplify = TRUE)[1], "-", simplify = TRUE)
        metadata <- as.data.frame(metadata)

        names(metadata) <- c("siteID", "countInterval", "fromDate", "toDate", "vehicleClass", "trafficDirection", "laneId")
        
        metadata <- metadata %>%
            map_df(function(x) {gsub("site", "", x)}) %>%
            mutate_at(vars(everything()), ~ na_if(., "")) %>%
            mutate(fromDate = parse_date_time(str_pad(fromDate, 8, "left", 0), "%d%m%Y"), 
                   toDate = parse_date_time(str_pad(toDate, 8, "left", 0), "%d%m%Y")) %>%
            rename(traffic_mode = vehicleClass) %>%
            mutate_at(vars(traffic_mode), ~ tolower(.))  %>%
            mutate_at(vars(siteID, countInterval, traffic_mode, trafficDirection, laneId), as.factor)
        
        metadata 
    }     


loadAndParseMeteoData <-
    function(dataFile, region, metric, startDateFilter = NULL, endDateFilter = NULL, glimpseContent = FALSE) {
    
        historical_weather <- read_table(dataFile) %>%
                                filter(rowSums(is.na(.)) != ncol(.))

        historical_weather <- historical_weather %>%
            mutate_at(vars(year), as.integer) %>%
            select(c(year, all_of(str_to_lower(month.abb)))) %>%

            mutate_at(vars(!matches("year")), as.numeric) %>% # just in case any issues reading in
            rename_if(is.double, str_to_title) %>%

            pivot_longer(!year, names_to = "month", values_to = metric) %>%
            mutate(month = ordered(month, levels = month.abb)) %>%
 
            pivot_longer(!c(year, month), names_to = "metric", values_to = "value")  %>%
            separate(metric, c("statistic", "metric"), sep = "_", fill = "left") %>%
            relocate(metric, .before = statistic)
 
        
        historical_weather <- historical_weather %>%
            mutate(monthOfYear = parse_date(paste0(month, "-", year), format = "%b-%Y")) %>%
            
            mutate(region = region) %>%
            mutate_at(vars(region, metric, statistic), as.factor) %>%
            relocate(c(monthOfYear, region), .after = month)
        
        # assumes start <= end
        if (!is.null(startDateFilter)) {
            historical_weather <- historical_weather %>%
                filter(monthOfYear >= startDateFilter)
        }
        if (!is.null(endDateFilter)) {
            historical_weather <- historical_weather %>%
                filter(monthOfYear <= endDateFilter)
        }

        
        if (glimpseContent)
            glimpse(historical_weather)
        
        invisible(historical_weather)
}


loadAndParseMeteoStationData <-
    function(dataFile, region, startDateFilter = NULL, endDateFilter = NULL, glimpseContent = FALSE) {
    
        historical_weather <- read_table(dataFile) %>%
                                filter(rowSums(is.na(.)) != ncol(.))
        
        historical_weather <- historical_weather %>%

            rename_with(~ c("year", "month", "max_temp", "min_temp", "af", "rainfall", "sunshine")) %>%
            mutate_if(negate(is.numeric), parse_number) %>%

            filter(year >= year(startDateFilter))%>%
            mutate_at(vars(year), as.ordered) %>%

            mutate_at(vars(month), ~ month(., label = TRUE)) %>%

            pivot_longer(!c(year, month), names_to = "metric", values_to = "value")  %>%
            separate(metric, c("statistic", "metric"), sep = "_", fill = "left") %>%
            relocate(metric, .before = statistic) %>%

            mutate(monthOfYear = parse_date(paste0(month, "-", year), format = "%b-%Y")) %>%
            filter(monthOfYear %within% interval(startDateFilter, endDateFilter)) %>%
            
            mutate(region = region) %>%
            mutate_at(vars(region, metric, statistic), as.factor) %>%
            relocate(c(monthOfYear, region), .after = month)

        
        # assumes start <= end
        if (!is.null(startDateFilter)) {
            historical_weather <- historical_weather %>%
                filter(monthOfYear >= startDateFilter)
        }
        if (!is.null(endDateFilter)) {
            historical_weather <- historical_weather %>%
                filter(monthOfYear <= endDateFilter)
        }

        
        if (glimpseContent)
            glimpse(historical_weather)
        
        invisible(historical_weather)
    }


parseMeteoDataFromDB <-
    function(historicalWeatherData, startDateFilter = NULL, endDateFilter = NULL, glimpseContent = FALSE) {
    
        historicalWeatherData <- historicalWeatherData %>%
            mutate_at(vars(year), as.integer) %>%
            mutate_at(vars(year), as.ordered) %>%
            mutate(month = ordered(month, levels = month.abb)) %>%
        
            mutate_at(vars(region, metric, statistic), as.factor)
 
        
        historicalWeatherData <- historicalWeatherData %>%
            mutate(monthOfYear = parse_date(paste0(month, "-", year), format = "%b-%Y")) %>%
            relocate(monthOfYear, .after = month)
        
        # assumes start <= end
        if (!is.null(startDateFilter)) {
            historicalWeatherData <- historicalWeatherData %>%
                filter(monthOfYear >= startDateFilter)
        }
        if (!is.null(endDateFilter)) {
            historicalWeatherData <- historicalWeatherData %>%
                filter(monthOfYear <= endDateFilter)
        }

        
        if (glimpseContent)
            glimpse(historicalWeatherData)
        
        invisible(historicalWeatherData)

}




loadAndParseTrafficSurveyData <-
    function(pathTofile, localAuthorityData, countInterval = "quarter_hour", breakDownDates = FALSE, selectColumns = NULL, glimpseContent = FALSE) {
        
        print(paste0("Parsing file '", pathTofile, "' ..."))
        
        
        data_loaded <- read_csv(pathTofile, trim_ws = T, na = c("N/A", "No data", "No Data")) %>%
                            filter(rowSums(is.na(.)) != ncol(.))
        
        data_loaded <- data_loaded %>%
                            mutate_at(vars(Code), ~ str_remove(., "Site\\s+")) %>%
                            #rename_with(., ~ (gsub("/|_|\\s+", "", .x)))
                            rename_with(., ~ (gsub("[^a-zA-Z]", "", .x))) #more general
        
        
        
            
        join_params <- c("Code" = "SWSiteID")
        if ("Settlement" %in% names(data_loaded)) # Site in quarter-hourly data, Settlement in hourly ..
            join_params <- c("Settlement" = "LocationPlace", join_params)
        else if ("Site" %in% names(data_loaded))
            join_params <- c("Site" = "LocationPlace", join_params)

        if ("LocalAuthority" %in% names(data_loaded))
            join_params <- c("LocalAuthority" = "LocalAuthority", join_params)

    
        if (!("Date" %in% names(data_loaded))) {
            data_loaded <- data_loaded %>%
                mutate(Date = as.Date(StartDateTime)) %>%
                relocate(Date, .before = StartDateTime)
        }
        if (!("TimePeriod" %in% names(data_loaded))) {

            data_loaded <- data_loaded %>%

                mutate(TimePeriod = format(StartDateTime, format = "%H:%M"),
                       TimePeriod = str_sub(TimePeriod, end = 5),
                       TimePeriodEnd = format(EndDateTime, format = "%H:%M"),
                       TimePeriodEnd = str_sub(TimePeriodEnd, end = 5),
                       TimePeriod = paste(TimePeriod, "-", TimePeriodEnd)) %>%
                select(-TimePeriodEnd) %>%
                relocate(TimePeriod, .before = StartDateTime)
        }
        if ("Total" %in% names(data_loaded)) {

            data_loaded <- data_loaded %>%
                select(-Total)
        }

        
        data_loaded <- data_loaded %>%
        
            inner_join(localAuthorityData %>%
                            select(LocalAuthority, LocationPlace, SWSiteID, RoadNumber, RoadType),
                       by = join_params
                      ) %>%
            mutate(CountPeriod = paste0(month(Date, label = TRUE), "-", year(Date))) %>%
            select(CountPeriod, everything())
        
        
        if (sum(c("Location", "Site") %in% names(data_loaded)) == 2) {
            data_loaded <- data_loaded %>%
                rename_with(~ c("RoadName", "Location"), c("Location", "Site"))
        
        }
        if (sum(c("Street", "Settlement") %in% names(data_loaded)) == 2) {
            data_loaded <- data_loaded %>%
                rename_with(~ c("RoadName", "Location"), c("Street", "Settlement"))
        }
        if ("Area" %in% names(data_loaded)) { # matches LocalAuthority

            data_loaded <- data_loaded %>%
                select(-Area)
        }
        if (!("countInterval" %in% names(data_loaded))) {

            data_loaded <- data_loaded %>%
                mutate(countInterval = countInterval)
        }
        
        
        data_loaded <- data_loaded %>%
            mutate(CountPeriodAsDate = parse_datetime(as.character(CountPeriod), format = "%b-%Y")) %>%
            mutate(CountPeriod = fct_reorder(CountPeriod, CountPeriodAsDate)) %>%
            select(-CountPeriodAsDate)



        if (breakDownDates) {
            data_loaded <- data_loaded %>%

                mutate(hour = as.ordered(hour(as_datetime(map_chr(str_split(TimePeriod, "\\s*-\\s"), 1), format = "%H:%M"))),
                       year = as.ordered(year(Date)),
                       month = month(Date, label = TRUE),
                       weekday = wday(Date, label = TRUE),
                       isWeekEnd = (as.integer(weekday) %in% c(1, 7)), #between(as.integer(weekday), 6, 7)) %>%
                      ) %>%

                relocate(c(hour, year, month, weekday, isWeekEnd), .after = EndDateTime)
        }


        
        params_as_factor <- c("Code", "LocalAuthority", "Location", "RoadName", "Context", "Direction", "Side",
                              "CountPeriod", "TimePeriod", "countInterval", "TransportationMode")
        params_as_count <- c("ID", transportation_modes)

        data_loaded <- data_loaded %>%
        
            mutate_at(intersect(names(data_loaded), params_as_count), as.integer) %>%
            pivot_longer(all_of(transportation_modes), names_to = "TransportationMode", values_to = "count") %>%
            mutate_at(intersect(colnames(.), params_as_factor), as.factor) %>% #- doesn't include new columns

            relocate(LocalAuthority, .before = Location) %>%
            relocate(c(RoadNumber, RoadType), .after = RoadName)
 
        
        if (sum(c("Latitude", "Longitude") %in% names(data_loaded)) < 2) # at least one not set
            data_loaded <- data_loaded %>%
        
                mutate(Latitude = NA,
                       Longitude = NA) %>%
                mutate_at(vars(Latitude, Longitude), as.double)
  
        
        if (!is_null(selectColumns))
            data_loaded <- data_loaded %>%
                select(all_of(selectColumns))
        
        
        if (glimpseContent)
            glimpse(data_loaded)
        
        invisible(data_loaded)
    }



parseTrafficSurveyDataFromDB <-
    function(trafficSurveyData, breakDownDates = FALSE, glimpseContent = FALSE) {
                
        trafficSurveyData <- trafficSurveyData %>%
            select(- (any_of(traffic_direction_variables) & where(~ sum(is.na(.)) == nrow(trafficSurveyData)))) %>%
            mutate_at(vars(CountPeriod, Code, LocalAuthority, Location, RoadName, RoadNumber, RoadType, TimePeriod, countInterval, TransportationMode), as.factor) %>%
            mutate_at(vars(Date), as_date) %>%
            mutate_at(vars(StartDateTime, EndDateTime), as_datetime)
        
        trafficSurveyData <- trafficSurveyData %>%
            mutate(CountPeriodAsDate = parse_datetime(as.character(CountPeriod), format = "%b-%Y")) %>%
            mutate(CountPeriod = fct_reorder(CountPeriod, CountPeriodAsDate)) %>%
            select(-CountPeriodAsDate)

        
        if (breakDownDates) {
            trafficSurveyData <- trafficSurveyData %>%

                mutate(hour = as.ordered(hour(as_datetime(map_chr(str_split(TimePeriod, "\\s*-\\s"), 1), format = "%H:%M"))),
                       year = as.ordered(year(Date)),
                       month = month(Date, label = TRUE),
                       weekday = wday(Date, label = TRUE),
                       isWeekEnd = (as.integer(weekday) %in% c(1, 7)), #between(as.integer(weekday), 6, 7)) %>%
                      ) %>%

                # mutate_at(vars(, as.ordered)) %>%
                relocate(c(hour, year, month, weekday, isWeekEnd), .after = EndDateTime)
        }
        

        if (glimpseContent)
            glimpse(trafficSurveyData)
        
        invisible(trafficSurveyData)
    }

