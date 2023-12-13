
start_date <- min(all_modes_survey_from_2017$Date)
end_date <- max(all_modes_survey_from_2017$Date)


## ---- all_modes_survey_from_2017_with_lag --------

#all_modes_survey_from_2017 <- all_modes_survey_from_2017 %>%
#    group_by(CountPeriod, Date, weekday, StartDateTime, EndDateTime) %>%
#    summarise(NoOfHours = n(),
#              count = sum(count, na.rm = TRUE)) %>%
#    group_by(CountPeriod, Date, weekday) %>%
#    summarise(NoOfHours = n()) %>%
#
#    ungroup() %>%
#    mutate(lag = lag(Date, order_by = Date),
#           lag = if_else(is.na(lag), Date, lag),
#           lag = Date - lag,
#           group_id = cumsum(c(TRUE, diff(lag) > 1))
#          ) %>%
#    mutate_at(vars(group_id), as.ordered) %>%
#    select(CountPeriod, group_id, Date) %>%
#
#    right_join(all_modes_survey_from_2017)



## ---- all_modes_survey_from_2017 --------

all_modes_survey_from_2017 <- all_modes_survey_from_2017 %>%
    mutate(CountPeriodExt = month(Date),
           across(CountPeriodExt, ~ case_when((. >= 9) | (. < 5) ~ paste0("Autumn ", year(Date)),
                                            (. < 9) ~ paste0("Spring ", year(Date))
                                           )),

           across(matches("CountPeriod"), ~ fct_reorder(., Date)),
           across(LocalAuthority, ~ gsub("^The\\s(\\w+)\\sCouncil$", "\\1", .)),
           ) %>%

    relocate(c(hour, year, month, weekday, isWeekEnd), .after = EndDateTime) %>%
    relocate(countInterval, .after = count) %>%
    relocate(CountPeriodExt, .after = CountPeriod) 


## ---- summary_all_modes_survey_from_2017 --------

summary_all_modes_survey_from_2017 <- all_modes_survey_from_2017 %>%

    group_by(CountPeriodExt, TransportationMode) %>%
    summarise(average = mean(count),
              count = sum(count)) %>%
    ungroup() %>%

    left_join(enframe(transportation_type) %>%
                rename_with(~ c("TransportationMode", "TransportationType"))
    ) %>%

    mutate_at(vars(TransportationMode, TransportationType), as.factor) %>%
    mutate(across(TransportationMode, ~ fct_reorder(., desc(count))))  %>%
    relocate(TransportationType, .after = TransportationMode) %>%

    arrange(CountPeriodExt, desc(TransportationType), TransportationMode)


## ---- overview_plot_all_modes_survey_from_2017 --------

overview_plot <- summary_all_modes_survey_from_2017 %>%

    ggplot(aes(CountPeriodExt, count, fill = TransportationMode, group = TransportationMode)) +
        geom_area() +
        scale_y_continuous(labels = scales::label_number_si()) +
        viridis::scale_fill_viridis(discrete = TRUE, direction = -1, begin = 0, end = 0.85,
                                    labels = transportation_modes) +

        cop_cycling_theme +
        theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 0.5),
              axis.text.y = element_text(size = 14),
              legend.title = element_blank()) +
        ylab("Total Count") + xlab("Survey Period")


### overview_plot_colour_scheme

overview_plot_colour_scheme <- ggplot_build(overview_plot)$data %>%
    as.data.frame() %>%
    distinct(group, fill) %>%

    mutate(TransportationMode = levels(summary_all_modes_survey_from_2017$TransportationMode))


tmp <- str_detect(levels(summary_all_modes_survey_from_2017$TransportationMode),
                  regex(paste(names(transportation_type[which(transportation_type == "ActiveTravel")]),
                              collapse = "|"), ignore_case = TRUE))
tmp <- levels(summary_all_modes_survey_from_2017$TransportationMode)[which(tmp)]


overview_plot_colour_scheme <- overview_plot_colour_scheme %>%
    mutate(TransportationMode =
               factor(TransportationMode,
                      levels = levels(fct_relevel(summary_all_modes_survey_from_2017$TransportationMode, tmp, after = Inf)))) %>%
    arrange(TransportationMode) %>%
    mutate(group = seq(1:nrow(.)))

rm(tmp)


## ---- summary_bicycles_survey_periods_from_2017--------

summary_bicycles_survey_periods_from_2017 <- all_modes_survey_from_2017 %>%

    filter(TransportationMode == "Cyclist") %>%
    #filter(str_starts(CountPeriodExt, "May|Sep")) %>%

    group_by(CountPeriodExt, LocalAuthority, Location, RoadType, RoadName, year, Date, hour) %>%
    summarise(count = sum(count, na.rm = TRUE)) %>%

    group_by(CountPeriodExt, LocalAuthority, Location, RoadType, RoadName, year, Date) %>%
    summarise(hourly_average = as.integer(round(mean(count))),
              count = sum(count, na.rm = TRUE)) %>%

    left_join(all_modes_survey_from_2017 %>%

                filter(TransportationMode == "Cyclist") %>%

                group_by(CountPeriodExt, LocalAuthority, Location, RoadType, RoadName, year, Date) %>%
                summarise(count = sum(count, na.rm = TRUE)) %>%

                group_by(CountPeriodExt, LocalAuthority, Location, RoadType, RoadName, year) %>%
                summarise(daily_average = as.integer(round(mean(count))),
                          daily_median = as.integer(round(median(count))),
                          count_period = sum(count))
    ) %>%

    left_join(all_modes_survey_from_2017 %>%

                filter(TransportationMode == "Cyclist") %>%

                group_by(LocalAuthority, Location, year, RoadType, RoadName) %>%
                summarise(count = sum(count, na.rm = TRUE)) %>%

                group_by(LocalAuthority, Location, RoadType, RoadName) %>%
                summarise(average_year_road_name = as.integer(round(mean(count))),
                          median_year_road_name = as.integer(round(median(count))),
                          total_road_name = sum(count))
    ) %>%

    left_join(all_modes_survey_from_2017 %>%

                filter(TransportationMode == "Cyclist") %>%

                group_by(LocalAuthority, Location, year, RoadType) %>%
                summarise(count = sum(count, na.rm = TRUE)) %>%

                group_by(LocalAuthority, Location, RoadType) %>%
                summarise(average_year_road_type = as.integer(round(mean(count))),
                          median_year_road_type = as.integer(round(median(count))),
                          total_road_type = sum(count))
    ) %>%

    left_join(all_modes_survey_from_2017 %>%

                filter(TransportationMode == "Cyclist") %>%

                group_by(LocalAuthority, Location, year) %>%
                summarise(count = sum(count, na.rm = TRUE)) %>%

                group_by(LocalAuthority, Location) %>%
                summarise(median_year_location = as.integer(round(median(count))),
                          total_location = sum(count, na.rm = TRUE))
    ) %>%


    left_join(all_modes_survey_from_2017 %>%

                filter(TransportationMode == "Cyclist") %>%
                distinct(CountPeriodExt, LocalAuthority, Date)
    ) %>%

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
           ) %>%


    select(CountPeriodExt, everything()) %>%
    ungroup() %>%

    mutate(across(LocalAuthority, as.factor))


## ---- sunburst_layout_bicycles --------

headers <- c("level", "id", "label", "parent")

sunburst_layout_bicycles <- summary_bicycles_survey_periods_from_2017 %>%
    distinct(LocalAuthority) %>%
    mutate(id = snakecase::to_upper_camel_case(as.character(LocalAuthority)),
           level = 1) %>%

    select(level, id, LocalAuthority) %>%
    mutate(parent = NA) %>%
    rename_with(~ headers)


sunburst_layout_bicycles <- sunburst_layout_bicycles %>%

    bind_rows(summary_bicycles_survey_periods_from_2017 %>%
                distinct(LocalAuthority, Location, tooltip_level2) %>%

                left_join(sunburst_layout_bicycles, by = c("LocalAuthority" = "label")) %>%
                mutate(parent = id,
                       id = paste0(parent, "_", snakecase::to_upper_camel_case(as.character(Location))),
                       level = max(level) + 1) %>%

                select(level, id, Location, parent, tooltip_level2) %>%
                rename_with(~ c(headers, "tooltip"))
    )


sunburst_layout_bicycles <- sunburst_layout_bicycles %>%

    bind_rows(summary_bicycles_survey_periods_from_2017 %>%
                distinct(LocalAuthority, Location, RoadType, tooltip_level3) %>%

                left_join(sunburst_layout_bicycles %>%
                              filter(level == max(level)),
                          by = c("Location" = "label")
                ) %>%

                mutate(parent = id,
                       id = paste0(parent, "_", snakecase::to_upper_camel_case(as.character(RoadType))),
                       level = max(level) + 1) %>%

                select(level, id, RoadType, parent, tooltip_level3) %>%
                rename_with(~ c(headers, "tooltip"))
    )


sunburst_layout_bicycles <- sunburst_layout_bicycles %>%

    bind_rows(summary_bicycles_survey_periods_from_2017 %>%
                distinct(LocalAuthority, Location, RoadType, RoadName, tooltip_level4) %>%
                mutate(across(c(LocalAuthority, Location, RoadType), ~ snakecase::to_upper_camel_case(as.character(.)))) %>%

                left_join(sunburst_layout_bicycles %>%
                            select(- tooltip) %>%
                            filter(level == max(level)) %>%

                            separate(id, c("LocalAuthority", "Location", "RoadType"), sep = "_", remove = FALSE)
                          ) %>%

                mutate(parent = id,
                       id = paste0(parent, "_", snakecase::to_upper_camel_case(as.character(RoadName))),
                       level = max(level) + 1) %>%

                select(level, id, RoadName, parent, tooltip_level4) %>%
                rename_with(~ c(headers, "tooltip"))
    )


sunburst_layout_bicycles <- sunburst_layout_bicycles %>%

   bind_rows(summary_bicycles_survey_periods_from_2017 %>%
                distinct(CountPeriodExt, LocalAuthority, Location, RoadType, RoadName, daily_average, tooltip) %>%
                mutate(across(c(LocalAuthority, Location, RoadType, RoadName), ~ snakecase::to_upper_camel_case(as.character(.)))) %>%

                left_join(sunburst_layout_bicycles %>%
                            select(- tooltip) %>%
                            filter(level == max(level)) %>%

                            separate(id, c("LocalAuthority", "Location", "RoadType", "RoadName"), sep = "_", remove = FALSE)
                ) %>%

                mutate(parent = id,
                       id = paste0(parent, "_", snakecase::to_upper_camel_case(as.character(CountPeriodExt))),
                       level = max(level) + 1) %>%

                select(level, id, CountPeriodExt, parent, daily_average, tooltip) %>%
                rename_with(~ c(headers, "value", "tooltip"))
    )



sunburst_layout_bicycles <- sunburst_layout_bicycles %>%

    mutate(across(parent, ~ coalesce(., "root")),
           across(c(id, parent, label), as.factor),
           across(level, as.integer),
           ) %>%

    bind_rows(data.frame(id = "root",
                         level = min(sunburst_layout_bicycles$level) - 1,
                         label = "<b>Biannual Traffic Survey<br /> Bicycle Counts</b>") %>%
                  mutate(tooltip = paste(label, "<br />",
                                         format(min(summary_bicycles_survey_periods_from_2017$Date), "%b %Y"), "-",
                                         format(max(summary_bicycles_survey_periods_from_2017$Date), "%b %Y")))
             )

rm(headers)


## ---- time_period_axis_breaks --------

time_period_axis_breaks <- levels(all_modes_survey_from_2017$TimePeriod)
sel_idx <- as.logical(seq_len(length(time_period_axis_breaks)) %% 2)

time_period_axis_breaks <- time_period_axis_breaks[sel_idx]


## ----  --------
