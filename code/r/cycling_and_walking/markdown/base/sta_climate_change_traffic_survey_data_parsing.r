

start_date <- min(all_modes_survey_from_2017$Date)
end_date <- max(all_modes_survey_from_2017$Date)


## ---- all_modes_survey_from_2017 --------

all_modes_survey_from_2017 <- all_modes_survey_from_2017 %>%
    group_by(CountPeriod, Date, weekday, StartDateTime, EndDateTime) %>%
    summarise(NoOfHours = n(),
              count = sum(count, na.rm = TRUE)) %>%
    group_by(CountPeriod, Date, weekday) %>%
    summarise(NoOfHours = n()) %>%
    
    ungroup() %>%
    mutate(lag = lag(Date, order_by = Date),
           lag = if_else(is.na(lag), Date, lag),
           lag = Date - lag,
           group_id = cumsum(c(TRUE, diff(lag) > 1))
          ) %>%
    mutate_at(vars(group_id), as.ordered) %>%
    select(CountPeriod, group_id, Date) %>%

    right_join(all_modes_survey_from_2017)


## ---- summary_all_modes_survey_from_2017 --------

summary_all_modes_survey_from_2017 <- all_modes_survey_from_2017 %>%


    filter(str_detect(CountPeriod, regex(paste(c("May", "Sep"), collapse = "|"), ignore_case = TRUE))) %>%
    group_by(CountPeriod, TransportationMode) %>%
    summarise(average = mean(count),
              count = sum(count)) %>%
    ungroup() %>%

    left_join(enframe(transportation_type) %>%
                rename_with(~ c("TransportationMode", "TransportationType"))
    ) %>%

    mutate_at(vars(TransportationMode, TransportationType), as.factor) %>%
    mutate(across(TransportationMode, ~ fct_reorder(., desc(count))))  %>%
    relocate(TransportationType, .after = TransportationMode) %>%

    arrange(CountPeriod, desc(TransportationType), TransportationMode)



## ---- overview_plot_all_modes_survey_from_2017 --------

overview_plot <- summary_all_modes_survey_from_2017 %>%

    ggplot(aes(CountPeriod, count, fill = TransportationMode, group = TransportationMode)) +
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



## ---- summary_bicycles_may_sep_survey_from_2017 --------

summary_bicycles_may_sep_survey_from_2017 <- all_modes_survey_from_2017 %>%

    filter(TransportationMode == "Cyclist") %>%
    filter(str_starts(CountPeriod, "May|Sep")) %>%

    group_by(CountPeriod, LocalAuthority, Location, RoadType, year, Date, hour) %>%
    summarise(count = sum(count, na.rm = TRUE)) %>%

    group_by(CountPeriod, LocalAuthority, Location, RoadType, year, Date) %>%
    summarise(hourly_average = as.integer(round(mean(count))),
              count = sum(count, na.rm = TRUE)) %>%

    left_join(all_modes_survey_from_2017 %>%

                filter(TransportationMode == "Cyclist") %>%

                group_by(CountPeriod, LocalAuthority, Location, RoadType, year, Date) %>%
                summarise(count = sum(count, na.rm = TRUE)) %>%

                group_by(CountPeriod, LocalAuthority, Location, RoadType, year) %>%
                summarise(daily_average = as.integer(round(mean(count))),
                          count_period = sum(count))
    ) %>%

    left_join(all_modes_survey_from_2017 %>%

                filter(TransportationMode == "Cyclist") %>%

                group_by(LocalAuthority, Location, year, RoadType) %>%
                summarise(count = sum(count, na.rm = TRUE)) %>%

                group_by(LocalAuthority, Location, RoadType) %>%
                summarise(average_year_road_type = as.integer(round(mean(count))),
                          total_road_type = sum(count))
    ) %>%

    left_join(all_modes_survey_from_2017 %>%

                filter(TransportationMode == "Cyclist") %>%

                group_by(LocalAuthority, Location) %>%
                summarise(total_location = sum(count, na.rm = TRUE))
    ) %>%


    left_join(all_modes_survey_from_2017 %>%

                filter(TransportationMode == "Cyclist") %>%
                distinct(CountPeriod, LocalAuthority, Date)
    ) %>%

    mutate(tooltip = paste(Location, paste0("(", LocalAuthority, ")"),
                           "- daily average <b>", formatNumber(daily_average), "bicycles</b> on",
                           if_else(RoadType == "Minor", paste0(RoadType, " Roads,"), paste0(RoadType, "s,")),
                           "<b>", CountPeriod, "</b>"
                          ),
           tooltip_level3 = paste(Location, paste0("(", LocalAuthority, ")"),
                                  "- <b>", formatNumber(total_road_type), "bicycles</b> on ",
                                  if_else(RoadType == "Minor", paste0(RoadType, " Roads"), paste0(RoadType, "s"))
                                  ),
           tooltip_level2 = paste("Total count,", Location,  paste0("(", LocalAuthority, ")"),
                                  "- <b>", formatNumber(total_location), "bicycles</b>"
                                 )
           ) %>%


    select(CountPeriod, everything()) %>%
    ungroup()



## ---- sunburst_layout_bicycles --------

headers <- c("level", "id", "label", "parent")

sunburst_layout_bicycles <- summary_bicycles_may_sep_survey_from_2017 %>%
    distinct(LocalAuthority) %>%
    mutate(id = snakecase::to_upper_camel_case(as.character(LocalAuthority)),
           level = 1) %>%

    select(level, id, LocalAuthority) %>%
    rename_with(~ headers) 

           
sunburst_layout_bicycles <- sunburst_layout_bicycles %>%

    bind_rows(summary_bicycles_may_sep_survey_from_2017 %>%
                distinct(LocalAuthority, Location, tooltip_level2) %>%

                left_join(sunburst_layout_bicycles, by = c("LocalAuthority" = "label")) %>%
                mutate(parent = id,
                       id = paste0(parent, "_", snakecase::to_upper_camel_case(as.character(Location))),
                       level = max(level) + 1) %>%
               
                select(level, id, Location, parent, tooltip_level2) %>%
                rename_with(~ c(headers, "tooltip"))
    )


sunburst_layout_bicycles <- sunburst_layout_bicycles %>%

    bind_rows(summary_bicycles_may_sep_survey_from_2017 %>%
                distinct(LocalAuthority, Location, RoadType, tooltip_level3) %>%

                left_join(sunburst_layout_bicycles %>%
                              filter(!is.na(parent)),
                          by = c("Location" = "label")
                ) %>%

                mutate(parent = id,
                       id = paste0(parent, "_", RoadType),
                       level = max(level) + 1) %>%

                select(level, id, RoadType, parent, tooltip_level3) %>%
                rename_with(~ c(headers, "tooltip"))
    )


sunburst_layout_bicycles <- sunburst_layout_bicycles %>%

    bind_rows(summary_bicycles_may_sep_survey_from_2017 %>%
                distinct(CountPeriod, LocalAuthority, Location, RoadType, daily_average, tooltip) %>%
                
                left_join(sunburst_layout_bicycles %>%
                            select(- tooltip) %>%
                            filter(level == max(level)) %>%
                            mutate(idx = id) %>%

                            separate(idx, c("LocalAuthority", "Location", "RoadType"), sep = "_") %>%
                            mutate_at(vars(LocalAuthority, Location), ~ snakecase::to_title_case(.)) %>%
                            select(- any_of(headers), id, level)
                ) %>%

                mutate(parent = id,
                       id = paste0(parent, "_", CountPeriod),
                       level = max(level) + 1) %>%

                select(level, id, CountPeriod, parent, daily_average, tooltip) %>%
                rename_with(~ c(headers, "value", "tooltip"))
    )


sunburst_layout_bicycles <- sunburst_layout_bicycles %>%

    mutate_at(vars(parent), ~ coalesce(., "root")) %>%
    mutate_at(vars(parent), as.factor) %>%
    
    bind_rows(data.frame(id = "root", level = min(sunburst_layout_bicycles$level) - 1, label = "<b>Biannual Traffic Survey<br /> Bicycle Counts</b>") %>%
                  mutate(tooltip = paste(label, "<br />",
                                         format(min(summary_bicycles_may_sep_survey_from_2017$Date), "%b %Y"), "-",
                                         format(max(summary_bicycles_may_sep_survey_from_2017$Date), "%b %Y")))
   )

rm(headers)



## ----  --------

