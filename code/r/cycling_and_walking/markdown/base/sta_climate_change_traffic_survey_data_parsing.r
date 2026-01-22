
start_date <- min(all_modes_survey_from_2017$Date)
end_date <- max(all_modes_survey_from_2017$Date)


## ---- all_modes_survey_from_2017_with_lag --------

# #all_modes_survey_from_2017 <- all_modes_survey_from_2017 |>
#     group_by(countInterval) |>
#     slice_head(n = 355) |>
#     ungroup() |>

#    group_by(CountPeriod, Date) |>
#    reframe(n()) |> # used to reframe/reduce - var itself redundant ...

#    mutate(lag = lag(Date, order_by = Date),
#           lag = if_else(is.na(lag), Date, lag),
#           lag = Date - lag,
#           group_id = cumsum(c(TRUE, diff(lag) > 1))
#          ) |>
#    mutate(across(group_id, as.ordered)) |>
#    select(CountPeriod, group_id, Date) |>
#
#    right_join(all_modes_survey_from_2017)



## ---- all_modes_survey_from_2017_cleaned_and_deduped --------

all_modes_survey_from_2017 <- all_modes_survey_from_2017 |>

    group_by(Code, TransportationMode, across(matches("time"))) |>
    add_tally(name = "duplicate") |>
    ungroup() |>

    # assumes count 0 error in otherwise "duplicate" records
    mutate(across(duplicate, ~ (duplicate > 1)),
           set_to_del = duplicate & (count == 0)) |>
    filter(!set_to_del) |>
    select(- duplicate) |>
    
    
    # based on patterns observed removes most like erroneous value (almost exclusively unusually high breaking pattern) 
    # b/n 1 and 4am - most errors at start 4 or 5 am; 
    # averages for all other start times (duplicate records - max two copies found in all cases)
    group_by(Code, TransportationMode, Date, across(matches("time"))) |>
    add_tally(name = "duplicate") |>
    ungroup() |>

    mutate(across(duplicate, ~ (duplicate > 1)),
           hour = hour(StartDateTime),
           action = case_when(!duplicate ~ NA,
                              between(hour, 1, 4) ~ "min",
                              TRUE ~ "avg"
                             )) |>

    group_by(Code, TransportationMode, Date, across(matches("time"))) |>
    mutate(set_to_del = duplicate & (action == "min") & (min(count, na.rm = TRUE) != count),
           
           across(count, ~ case_when((action == "avg") ~ as.integer(ceiling(mean(., na.rm = TRUE))),
                                     TRUE ~ .
                                    )),
          ) |>
    ungroup() |>

    filter(!set_to_del) |>
    distinct(across(!c(duplicate, hour, action, set_to_del)))


## ---- all_modes_survey_from_2017 --------

all_modes_survey_from_2017 <- all_modes_survey_from_2017 |>
    mutate(CountPeriodExt = month(Date),
           across(CountPeriodExt, ~ paste(case_when((. >= 8) | (. < 3) ~ "Autumn",
                                                    (. < 8) ~ "Spring"
                                                   ), 
                                          year(Date))),

           across(matches("CountPeriod"), ~ fct_reorder(., as.integer(CountPeriod))),
           ) |>

    relocate(CountPeriodExt, .after = CountPeriod)

# note initially had Autumn only from Sep, moved back to Aug
# and Spring back to Mar


all_modes_survey_from_2017 <- all_modes_survey_from_2017 |>

    filter(str_detect(TransportationMode, "^Cycl\\w+")) |>

    mutate(across(TransportationMode, ~ case_when(str_detect(., "^Cycl\\w+") ~ fct_expand("CyclistAllModes"),
                                                  TRUE ~ .
                                                 ))) |>
    group_by(across(!count)) |>
    reframe(across(count, ~ sum(., na.rm = TRUE))) |>
    relocate(count, .before = countInterval) |>

    rbind(all_modes_survey_from_2017) |>

    left_join(enframe(transportation_type) |>
                rename_with(~ c("TransportationMode", "TransportationType"))
    ) |>

    mutate(across(TransportationMode, ~ fct_reorder(., desc(count))),
           
           across(TransportationType, ~ case_when((TransportationMode == "CyclistAllModes") ~ "ActiveTravel",
                                                  TRUE ~ .
                                                 )),
           across(TransportationType, as.factor),
          ) |>
    relocate(TransportationType, .after = TransportationMode) |>

    group_by(Code, TransportationMode, CountPeriodExt) |>
    mutate(mode_count_period_zero = (sum(count) == 0),
          ) |>

    group_by(Code, TransportationType, CountPeriodExt) |>
    mutate(tmp_sum = sum(count),
           count_period_mode_type_zero = case_when((tmp_sum > 0) ~ NA_character_,
                                                    (TransportationType == "ActiveTravel") ~ "ActiveTravelZero",
                                                    (TransportationType == "MotorVehicle") ~ "MotorVehicleZero",
                                                    ),
          ) |>
    select(- tmp_sum) |>

    group_by(Code, CountPeriodExt) |>
    mutate(period_type_with_zero = (sum(!is.na(count_period_mode_type_zero)) > 0)) |>

    group_by(Code, TransportationMode) |>
    mutate(mode_count_zero = (sum(count) == 0)) |>
    ungroup() |>


    # set up filters for (empty) records to exclude
    mutate(across(count, ~ case_when(mode_count_zero ~ NA,
                                     
                                     (Code %in% c("FF", "FG", "FE", "ERC24", "ERC25")) & (TransportationType == "MotorVehicle") & 
                                          (count_period_mode_type_zero == "MotorVehicleZero") ~ NA,
                                     
                                     # CE04a - York Place - covers period of tram extension when all but pedestrian counts 0
                                     # other similar patterns, e.g., CYC003a (Helensburgh), no knowlecge of locality 
                                     # so cannot guess at reason - but treat the same, as outcome not dissimilar
                                     (Code %in% c("CE04a", "CYC003a")) & mode_count_period_zero ~
                                     
                                         case_when((TransportationMode == "Pedestrian") & period_type_with_zero ~ NA, # should be redundant, but keep for reusability
                                                   str_detect(TransportationMode, "^Cycl") & period_type_with_zero ~ NA,
                                                   (TransportationType == "MotorVehicle") & 
                                                           (count_period_mode_type_zero == "MotorVehicleZero") ~ NA,
                                                    
                                                    TRUE ~ . # redundant, but keep to be safe
                                                  ),
                                     
                                     # cycle counts zero, yet pedestrians counted, and no obvious reason for the former 
                                     # - treated as errors or deliberate no counts
                                     (Code %in%  c("SLC03", "ERC13")) & (TransportationType == "ActiveTravel") & 
                                             mode_count_period_zero & 
                                             (coalesce(count_period_mode_type_zero, "") != "ActiveTravelZero") ~ NA,
                                     
                                     TRUE ~ .
                                    ))) |>
    filter(!is.na(count))


## ---- summary_all_modes_survey_from_2017 --------

summary_all_modes_survey_from_2017 <- all_modes_survey_from_2017 |>

    group_by(CountPeriodExt, TransportationMode, TransportationType, count_period_mode_type_zero, mode_count_zero) |>
    mutate(across(count, list(average = ~ mean(., na.rm = TRUE),
                              median = ~ median(., na.rm = TRUE)
                              )
                  , .names = "{.fn}_hourly"),
           ) |>

    group_by(Code, TransportationMode, Date, CountPeriodExt) |>
    mutate(across(count, ~ sum(., na.rm = TRUE))) |>

    distinct(across(!matches("Time|hour$"))) |>
    group_by(TransportationMode, TransportationType, CountPeriodExt, 
             count_period_mode_type_zero, mode_count_zero, across(matches("_hourly"))
            ) |>
    reframe(count_locations = n_distinct(Code),
            across(count, list(average = ~ mean(.),
                               median = ~ median(.)
                              )
                   , .names = "{.fn}_daily"),
            across(count, sum),
           ) |>

    mutate(across(count, ~ case_when(if_all(matches("average|median"), is.na) & (count == 0) ~ NA,
                                     TRUE ~ .
                                    ))) |>

    arrange(CountPeriodExt, desc(TransportationType), TransportationMode) 


## ---- overview_plot_all_modes_survey_from_2017 --------

overview_plot <- summary_all_modes_survey_from_2017 |>

    filter(!(str_detect(TransportationMode, "^Cycl\\w+") & (TransportationMode != "CyclistAllModes"))) |>
    mutate(across(TransportationMode, ~ fct_recode(., Cyclist = "CyclistAllModes"))) |>

    ggplot(aes(CountPeriodExt, median_daily, fill = TransportationMode, group = TransportationMode)) +
        geom_area() +
        scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale())) +
        viridis::scale_fill_viridis(discrete = TRUE, direction = -1, begin = 0, end = 0.85,
                                    labels = transportation_modes) +

        cop_cycling_theme +
        theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 0.5),
              axis.text.y = element_text(size = 14),
              legend.title = element_blank()) +
        ylab("Typical Daily Count across Locations") + xlab("Survey Period")


### overview_plot_colour_scheme

  overview_plot_colour_scheme <- ggplot_build(overview_plot)$data |>
    as.data.frame() |>
    distinct(group, fill) |>

    mutate(TransportationMode = levels(summary_all_modes_survey_from_2017$TransportationMode)[
                                    (levels(summary_all_modes_survey_from_2017$TransportationMode) |>
                                        str_detect("^Cycl\\w+AllModes") |

                                      levels(summary_all_modes_survey_from_2017$TransportationMode) |>
                                          str_detect("^Cycl\\w", negate = TRUE)
                                      )],
           
           across(TransportationMode, ~ case_when(str_detect(., "^Cycl") ~ "Cyclist",
                                                  TRUE ~ .
                                                 )),
          )


tmp <- str_detect(levels(summary_all_modes_survey_from_2017$TransportationMode),
                  regex(paste(names(transportation_type[which(transportation_type == "ActiveTravel")]),
                              collapse = "|"), ignore_case = TRUE))
tmp <- levels(summary_all_modes_survey_from_2017$TransportationMode)[which(tmp)]


overview_plot_colour_scheme <- overview_plot_colour_scheme %>%
    mutate(TransportationMode =
               factor(TransportationMode,
                      levels = levels(fct_relevel(summary_all_modes_survey_from_2017$TransportationMode, tmp, after = Inf)))) %>%
    arrange(TransportationMode) %>%
    mutate(group = seq(1:nrow(.))) %>%
    droplevels()

rm(tmp)


## ---- summary_bicycles_survey_periods_from_2017--------

summary_bicycles_survey_periods_from_2017 <- all_modes_survey_from_2017 |>

    filter(str_detect(TransportationMode, "^Cycl\\w+"))  |>
    #filter(str_starts(CountPeriodExt, "May|Sep")) |>

    group_by(CountPeriodExt, Code, LocalAuthority, Location, RoadType, RoadName, year, Date, TimePeriod) |>
    reframe(count = sum(count, na.rm = TRUE)) |>

    group_by(CountPeriodExt, LocalAuthority, Location, RoadType, RoadName, year, Date) |>
    reframe(across(count, list(average = ~ as.integer(round(mean(.))),
                               median = ~ as.integer(round(median(.)))
                               ), .names = "hourly_{.fn}"),
            across(count, sum),
            ) |>

    left_join(all_modes_survey_from_2017 |>

                filter(str_detect(TransportationMode, "^Cycl\\w+")) |>

                group_by(CountPeriodExt, Code, LocalAuthority, Location, RoadType, RoadName, year, Date) |>
                reframe(across(count, ~ sum(., na.rm = TRUE))) |>

                group_by(CountPeriodExt, LocalAuthority, Location, RoadType, RoadName, year) |>
                reframe(across(count, list(average = ~ as.integer(round(mean(.))),
                                           median = ~ as.integer(round(median(.)))
                                          ), .names = "daily_{.fn}"),
                        count_period = sum(count)
                       )
    ) |>

    left_join(all_modes_survey_from_2017 |>

                filter(str_detect(TransportationMode, "^Cycl\\w+")) |>

                group_by(LocalAuthority, Code, Location, year, RoadType, RoadName) |>
                reframe(across(count, ~ sum(., na.rm = TRUE))) |>

                group_by(LocalAuthority, Location, RoadType, RoadName) |>
                reframe(across(count, list(average = ~ as.integer(round(mean(.))),
                                           median = ~ as.integer(round(median(.)))
                                          ), .names = "{.fn}_year_road_name"),
                        total_road_name = sum(count)
                       )
    ) |>

    left_join(all_modes_survey_from_2017 |>

                filter(str_detect(TransportationMode, "^Cycl\\w+")) |>

                group_by(LocalAuthority, Location, year, RoadType) |>
                reframe(across(count, ~ sum(., na.rm = TRUE))) |>

                group_by(LocalAuthority, Location, RoadType) |>
                reframe(across(count, list(average = ~ as.integer(round(mean(.))),
                                           median = ~ as.integer(round(median(.)))
                                          ), .names = "{.fn}_year_road_type"),
                        total_road_type = sum(count)
                       )
    ) |>

    left_join(all_modes_survey_from_2017 |>

                filter(str_detect(TransportationMode, "^Cycl\\w+")) |>

                group_by(LocalAuthority, Location, year) |>
                reframe(across(count, ~ sum(., na.rm = TRUE))) |>

                group_by(LocalAuthority, Location) |>
                reframe(median_year_location = as.integer(round(median(count))),
                        total_location = sum(count)
                        )
    ) |>


    left_join(all_modes_survey_from_2017 |>

                filter(str_detect(TransportationMode, "^Cycl\\w+")) |>
                distinct(CountPeriodExt, LocalAuthority, Date)
    ) |>

    mutate(tooltip = paste(Location, paste0("(", LocalAuthority, ")"),
                           "<br />- typical, per day,<b>", formatNumber(daily_average), "bicycles</b><br /> on",
                           #if_else(RoadType == "Minor", paste0(RoadType, " Roads,"), paste0(RoadType, "s,")),
                           paste0(RoadName, ",<b>"), CountPeriodExt, "</b>"
                          ),
           tooltip_level4 = paste(Location, paste0("(", LocalAuthority, ") <br />- typical, per survey year:"),
                                              "<br />- <b>", formatNumber(median_year_road_name), "bicycles</b> on", RoadName
                                              ),
           tooltip_level3 = paste(Location, paste0("(", LocalAuthority, ") <br />- typical, per survey year:"),
                                  "<br />- <b>", formatNumber(median_year_road_type), "bicycles</b> on",
                                  if_else(RoadType == "Minor", paste0(RoadType, " Roads"), paste0(RoadType, "s"))
                                  ),
           tooltip_level2 = paste("typical, per survey year,", Location,  paste0("(", LocalAuthority, ")"),
                                  "<br />- <b>", formatNumber(median_year_location), "bicycles</b>"
                                 )
           ) |>


    select(CountPeriodExt, everything()) |>
    ungroup() |>

    mutate(across(LocalAuthority, as.factor))


## ---- sunburst_layout_bicycles --------

headers <- c("level", "id", "label", "parent")

sunburst_layout_bicycles <- summary_bicycles_survey_periods_from_2017 |>
    distinct(LocalAuthority) |>
    mutate(id = snakecase::to_upper_camel_case(as.character(LocalAuthority)),
           level = 1) |>

    select(level, id, LocalAuthority) |>
    mutate(parent = NA) |>
    rename_with(~ headers)


sunburst_layout_bicycles <- sunburst_layout_bicycles |>

    bind_rows(summary_bicycles_survey_periods_from_2017 |>
                distinct(LocalAuthority, Location, tooltip_level2) |>

                left_join(sunburst_layout_bicycles, by = c("LocalAuthority" = "label")) |>
                mutate(parent = id,
                       id = paste0(parent, "_", snakecase::to_upper_camel_case(as.character(Location))),
                       level = max(level) + 1) |>

                select(level, id, Location, parent, tooltip_level2) |>
                rename_with(~ c(headers, "tooltip"))
    )


sunburst_layout_bicycles <- sunburst_layout_bicycles |>

    bind_rows(summary_bicycles_survey_periods_from_2017 |>
                distinct(LocalAuthority, Location, RoadType, tooltip_level3) |>

                left_join(sunburst_layout_bicycles |>
                              filter(level == max(level)),
                          by = c("Location" = "label")
                ) |>

                mutate(parent = id,
                       id = paste0(parent, "_", snakecase::to_upper_camel_case(as.character(RoadType))),
                       level = max(level) + 1) |>

                select(level, id, RoadType, parent, tooltip_level3) |>
                rename_with(~ c(headers, "tooltip"))
    )


sunburst_layout_bicycles <- sunburst_layout_bicycles |>

    bind_rows(summary_bicycles_survey_periods_from_2017 |>
                distinct(LocalAuthority, Location, RoadType, RoadName, tooltip_level4) |>
                mutate(across(c(LocalAuthority, Location, RoadType), ~ snakecase::to_upper_camel_case(as.character(.)))) |>

                left_join(sunburst_layout_bicycles |>
                            select(- tooltip) |>
                            filter(level == max(level)) |>

                            separate(id, c("LocalAuthority", "Location", "RoadType"), sep = "_", remove = FALSE)
                          ) |>

                mutate(parent = id,
                       id = paste0(parent, "_", snakecase::to_upper_camel_case(as.character(RoadName))),
                       level = max(level) + 1) |>

                select(level, id, RoadName, parent, tooltip_level4) |>
                rename_with(~ c(headers, "tooltip"))
    )


sunburst_layout_bicycles <- sunburst_layout_bicycles |>

   bind_rows(summary_bicycles_survey_periods_from_2017 |>
                distinct(CountPeriodExt, LocalAuthority, Location, RoadType, RoadName, daily_average, tooltip) |>
                mutate(across(c(LocalAuthority, Location, RoadType, RoadName), ~ snakecase::to_upper_camel_case(as.character(.)))) |>

                left_join(sunburst_layout_bicycles |>
                            select(- tooltip) |>
                            filter(level == max(level)) |>

                            separate(id, c("LocalAuthority", "Location", "RoadType", "RoadName"), sep = "_", remove = FALSE)
                ) |>

                mutate(parent = id,
                       id = paste0(parent, "_", snakecase::to_upper_camel_case(as.character(CountPeriodExt))),
                       level = max(level) + 1) |>

                select(level, id, CountPeriodExt, parent, daily_average, tooltip) |>
                rename_with(~ c(headers, "value", "tooltip"))
    )



sunburst_layout_bicycles <- sunburst_layout_bicycles |>

    mutate(across(parent, ~ coalesce(., "root")),
           across(label, ~ str_trunc((as.character(.)), 25)),
           across(c(id, parent, label), as.factor),
           across(level, as.integer),
           
           ) |>

    bind_rows(data.frame(id = "root",
                         level = min(sunburst_layout_bicycles$level) - 1,
                         label = fct_expand("<b>Biannual Traffic Survey<br /> Bicycle Counts</b>")) |>
                  mutate(tooltip = paste(label, "<br />",
                                         format(min(summary_bicycles_survey_periods_from_2017$Date), "%b %Y"), "-",
                                         format(max(summary_bicycles_survey_periods_from_2017$Date), "%b %Y")))
             )

rm(headers)


## ---- time_period_axis_breaks --------

time_period_axis_breaks <- levels(all_modes_survey_from_2017$TimePeriod)
sel_idx <- as.logical(seq_len(length(time_period_axis_breaks)) %% 2)

time_period_axis_breaks <- time_period_axis_breaks[sel_idx]


## ---- summary_active_travel --------

summary_active_travel <-
    summary_all_modes_survey_from_2017 |>

        filter(TransportationMode %in% c("CyclistAllModes", "Pedestrian")) |>
        mutate(across(TransportationMode, ~ fct_recode(., Cycling = "CyclistAllModes", Walking = "Pedestrian"))) |>
        distinct(across(!matches("_hourly|_zero"))) |>

        pivot_longer(c(median_daily, average_daily, count, count_locations), names_to = "metric", 
                     
                    values_drop_na = TRUE) |>
        mutate(across(metric, fct_inorder),
               season = gsub("(^\\w+)\\s\\d+", "\\1", CountPeriodExt),
               across(c(TransportationMode, season), fct_rev),
              ) |>
        arrange(TransportationMode, CountPeriodExt, metric) |>

        group_by(TransportationMode, metric) |>
        mutate(growth = lag(value),
               across(growth, ~ (value - .) / .),
              )|>

        group_by(TransportationMode, metric, season) |>
        mutate(growth_seasonal = lag(value),
               across(growth_seasonal, ~ (value - .) / .),
              ) |>
        ungroup() |>
        droplevels()


## ----  --------
