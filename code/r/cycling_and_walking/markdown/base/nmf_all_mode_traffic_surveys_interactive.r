## ---- overview_traffic_surveys_subplots --------


transportation_mode_plots <- lapply(overview_plot_colour_scheme$group, function(i) {

        summary_all_modes_survey_from_2017 |>

            filter(!(str_detect(TransportationMode, "^Cycl\\w+") & (TransportationMode != "CyclistAllModes"))) |>
            mutate(across(TransportationMode, ~ fct_recode(., Cyclist = "CyclistAllModes"))) |>

            filter(TransportationMode == levels(overview_plot_colour_scheme$TransportationMode)[i]) |>

            ggplot(aes(CountPeriodExt, median_daily, fill = TransportationMode, group = TransportationMode)) + 
                geom_area(fill = overview_plot_colour_scheme$fill[i]) +
                scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale())) +
                guides(fill = "none") +

                cop_cycling_theme +         
                theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.75, hjust = 0.75), 
                      axis.text.y = element_text(size = 12), 
                      #legend.title = element_blank()
                     ) + 
                ylab("Typical Daily Count") + xlab("Survey Period")
    })


## ---- sunburst_layout_bicycles --------

plot_ly(width = 550, #height = 500, 
        data = sunburst_layout_bicycles |>
                mutate_at(vars(value), ~ coalesce(., 0))
       ) |> 

    add_trace(
        type = "sunburst",
        ids = ~ id,
        labels = ~ label,
        parents = ~ parent,
        
        sort = FALSE,
        values = ~ value, 
        branchvalues = "relative",
        #count = "branches+leaves", - not working, need to provide values or doesn't draw
        
        hovertext = ~ tooltip,
        hoverinfo = "text",
        textinfo = ~ label,
        
        domain = list(column = 1),
        maxdepth = 3,
        insidetextorientation = "radial"
        
      ) |>

      layout(sunburstcolorway = viridis_pal(direction = -1, begin = 0, end = 0.85, option = "D")(length(levels(summary_bicycles_survey_periods_from_2017$LocalAuthority))),
             extendsunburstcolors = TRUE, 
             margin = list(l = 2)
          )


## ---- facet_view_survey_periods_cyclist --------

all_modes_survey_from_2017 |>
    filter((TransportationMode == "CyclistAllModes") & !is.na(count)) |>
    mutate(across(TransportationMode, ~ fct_recode(., Cyclist = "CyclistAllModes"))) |>

    ggplot(aes(TimePeriod, count)) +
        geom_boxplot(size = 0.25, notchwidth = 0.25, outlier.size = 0.45, outlier.shape = 16) +
        cop_cycling_theme +
        theme(axis.text.x = element_text(size = 11, angle = 45, vjust = 0.75, hjust = 0.75),
              axis.text.y = element_text(size = 16),
              axis.title.y = element_text(size = 22),
              #strip.text = element_text(size = 36),
              strip.background = element_rect(colour = "#dfdfdf", fill = "grey95", linewidth = 0.5)
             ) +

        ylab(paste0("Hourly Count - Cycling")) +
        xlab("") +
        scale_x_discrete(breaks = time_period_axis_breaks) +
        facet_wrap(~ CountPeriodExt, nrow = 2, dir = "v")


## ---- facet_view_survey_periods_pedestrian --------

all_modes_survey_from_2017 |>
    filter((TransportationMode == "Pedestrian") & !is.na(count)) |>

    ggplot(aes(TimePeriod, count)) +
        geom_boxplot(size = 0.25, notchwidth = 0.25, outlier.size = 0.45, outlier.shape = 16) +
        cop_cycling_theme +
        theme(axis.text.x = element_text(size = 11, angle = 45, vjust = 0.75, hjust = 0.75),
              axis.text.y = element_text(size = 16),
              axis.title.y = element_text(size = 22),
              #strip.text = element_text(size = 36),
              strip.background = element_rect(colour = "#dfdfdf", fill = "grey95", linewidth = 0.5)
             ) +
        ylab(paste0("Hourly Count - Walking")) +
        xlab("") +
        scale_x_discrete(breaks = time_period_axis_breaks) +
        facet_wrap(~ CountPeriodExt, nrow = 2, dir = "v")


## ---- survey_periods --------

survey_periods <- all_modes_survey_from_2017 |>

    mutate(CountPeriodExt = month(Date),
           across(CountPeriodExt, ~ case_when(#(. >= 9) | (. < 5) ~ paste0("Autumn ", year(Date)),
                                              (. >= 8) & (. <= 12) ~ paste0("Autumn ", year(Date)),
                                              (. >= 1) & (. < 4) ~ paste0("Autumn ", year(Date) - 1),
                                              
                                              (. < 8) ~ paste0("Spring ", year(Date))
                                             )),
           across(matches("CountPeriod"), ~ fct_reorder(., Date)),
           ) |>
    distinct(CountPeriod, CountPeriodExt, LocalAuthority, Location) |>
           
    mutate(across(LocalAuthority, ~ snakecase::to_upper_camel_case(as.character(.))),
           across(Location, ~ paste(snakecase::to_upper_camel_case(as.character(.)),
Location, sep = "-")),
           
           SurveyDate = case_when(str_starts(CountPeriod, "May-|Sep-") ~ "SurveyDate", 
                                  str_starts(CountPeriod, "Apr-|Aug-") ~ "EarlySurvey",
                                  TRUE ~ "DelayedSurvey"),
           ) |>
    select(- LocalAuthority)

survey_periods <- survey_periods |>

    count(name = "LocationsByMonth", CountPeriod, CountPeriodExt, SurveyDate) |>
    arrange(CountPeriod) |>

    full_join(survey_periods |>
                distinct(across(!CountPeriod)) |>
                count(name = "NoOfLocations", CountPeriodExt, SurveyDate)
             ) |>

    full_join(survey_periods |>
                  distinct(CountPeriodExt, Location) |>
                  count(name = "TotalLocations", CountPeriodExt)
             ) 


survey_periods |>
    select(matches("CountPeriod")) |>
    pivot_wider(names_from = CountPeriod, values_from = CountPeriod) |>
    unite("CountPeriod", matches("\\-\\d"), na.rm = TRUE, sep = ", ") |>
    mutate(across(CountPeriod, ~ gsub("\\-\\d*", "", .))) |>

    full_join(survey_periods |>
                distinct(across(!c(CountPeriod, LocationsByMonth))) |>
                pivot_wider(names_from = SurveyDate, values_from = NoOfLocations) |>
                mutate(across(c(DelayedSurvey, EarlySurvey), ~ coalesce(., 0)),
                       PartialSurvey = case_when((sum(SurveyDate + DelayedSurvey + EarlySurvey) >= TotalLocations) ~ 0,
                                                 TRUE ~ (SurveyDate - TotalLocations) + (DelayedSurvey + EarlySurvey)
                                                ),
                       across(where(is.double), as.integer),
                      )
             ) |>
    relocate(PartialSurvey, .before = DelayedSurvey) |>
    select_if(~ !all((. == 0))) |> # drop redundant columns - one or more of early, delayed or partial surveys
	
    rename_with(~ c("SurveyPeriod", "SurveyMonths", "NoOfSettlements", "SurveyedOnDate"),
                    c("CountPeriodExt", "CountPeriod", "TotalLocations", "SurveyDate")) |>
    rename_with(snakecase::to_title_case) |>
    mutate(across(where(is.numeric), scales::comma),
           across(matches("Survey$"), ~ gsub("^0$", "-", .)),
          ) |>

    kable(caption = "") |> #No of settlements with partial or delayed surveys per period
    kable_paper(c("striped", "responsive"), full_width = FALSE, position = "left")

#rm(survey_periods)


## ---- facet_view_survey_periods_all_modes --------

summary_all_modes_survey_from_2017 |>

    filter(!(str_detect(TransportationMode, "^Cycl\\w+") & (TransportationMode != "CyclistAllModes"))) |>
    mutate(across(TransportationMode, ~ fct_recode(., Cyclist = "CyclistAllModes"))) |>
    distinct(across(!matches("_hourly"))) |>

    pivot_longer(c(median_daily, average_daily, count#, count_locations
), names_to = "metric", 
                 #values_to = "count",
                 values_drop_na = TRUE) |>
    mutate(across(metric, fct_inorder)) |>

    ggplot(aes(CountPeriodExt, value, fill = TransportationMode, group = TransportationMode)) +
        geom_area() +
        scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale())) +
        viridis::scale_fill_viridis(discrete = TRUE, direction = -1, begin = 0, end = 0.85,
                                    labels = transportation_modes) +

        cop_cycling_theme +
        theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 0.5),
              axis.text.y = element_text(size = 22),
              legend.title = element_blank()) +
        ylab("Count") + xlab("Survey Period") + 
        facet_wrap(~ metric, scales = "free_y", ncol = 1,
                   labeller = labeller(metric = c(average_daily = "Daily Average Count per Location", 
                                                  median_daily = "Typical Daily Count across Locations", 
                                                  count = "Total",
                                                  count_locations = "No. of Locations"
                                                 ))
                  ) 


## ---- facet_view_survey_periods_totals_all_modes --------

summary_all_modes_survey_from_2017 |>

    filter(!(str_detect(TransportationMode, "^Cycl\\w+") & (TransportationMode != "CyclistAllModes"))) |>
    mutate(across(TransportationMode, ~ fct_recode(., Cyclist = "CyclistAllModes")),
           across(TransportationType, fct_rev),
          ) |>

    ggplot(aes(CountPeriodExt, count, colour = TransportationMode, group = TransportationMode)) +
        geom_line(aes(linetype = TransportationType)) +
        geom_point(aes(shape = TransportationMode)) +
        scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale())) +
#        scale_y_continuous(transform = "log10", label = scales::number_format(accuracy = 1)#, breaks = seq(0, 100, 20)) +
#         scale_colour_brewer(palette = "Dark2", direction = -1) +
        scale_colour_manual(values = colorRampPalette(colour("okabe ito")(8))(length(transportation_modes))) +
        scale_shape_manual(values = seq(1:length(transportation_modes))) +
        
        cop_cycling_theme +
        theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 0.5),
              axis.text.y = element_text(size = 22),
              legend.title = element_blank()) +
        ylab("Total Count") + xlab("Survey Period")


## ---- location_count_survey_periods_all_modes --------

summary_all_modes_survey_from_2017 |>

    filter(!(str_detect(TransportationMode, "^Cycl\\w+") & (TransportationMode != "CyclistAllModes"))) |>
    mutate(across(TransportationMode, ~ fct_recode(., Cyclist = "CyclistAllModes")),
           across(TransportationType, fct_rev),) |>
 
    ggplot(aes(CountPeriodExt, count_locations, colour = TransportationMode, group = TransportationMode)) +
        geom_line(aes(linetype = TransportationType)) +
        scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale())) +
#         scale_colour_brewer(palette = "Dark2", direction = -1) +
        scale_colour_manual(values = colorRampPalette(colour("okabe ito")(8))(length(transportation_modes))) +
        
        cop_cycling_theme +
        theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 0.5),
              axis.text.y = element_text(size = 22),
              legend.title = element_blank()) +
        ylab("No. of Locations") + xlab("Survey Period")


## ---- summary_bicycle_daily_patterns --------

max_mid_daily <- summary_active_travel |>
    filter((TransportationMode == "Cycling") & str_detect(metric, "_daily$")) |>
    reframe(across(value, max)) |>
    
    reframe(max_mid_daily = round(max(value), -1)) |>
    deframe()


## ---- bicycle_daily_patterns_median_daily --------

summary_active_travel |>
    filter((TransportationMode == "Cycling") & (metric == "median_daily")) |>

    ggplot(aes(CountPeriodExt, value, colour = TransportationMode, group = TransportationMode)) +
            geom_line() +
            scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale())) +
            scale_colour_manual(values =
                                overview_plot_colour_scheme[which(overview_plot_colour_scheme$TransportationMode == "Cyclist"), "fill"]) +
            guides(colour = FALSE) +

            cop_cycling_theme +
            theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 0.5), #hjust = 1.05),
                  axis.text.y = element_text(size = 22),
                  legend.title = element_blank()) +
            ylab("Typical Daily Count") + xlab("Survey Period") +
            ylim(c(NA, max_mid_daily))


## ---- bicycle_daily_patterns_average_daily --------

summary_active_travel |>
    filter((TransportationMode == "Cycling") & (metric == "average_daily")) |>

    ggplot(aes(CountPeriodExt, value, colour = TransportationMode, group = TransportationMode)) +
            geom_line() +
            scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale())) +
            scale_colour_manual(values = 
                                overview_plot_colour_scheme[which(overview_plot_colour_scheme$TransportationMode == "Cyclist"), "fill"]) +
            guides(colour = FALSE) + 

            cop_cycling_theme +
            theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 0.5), #hjust = 1.05),
                  axis.text.y = element_text(size = 22),
                  legend.title = element_blank()) +
            ylab("Average Count, Daily") + xlab("Survey Period")


## ---- bicycle_daily_patterns_count --------

summary_active_travel |>
    filter((TransportationMode == "Cycling") & (metric == "count")) |>

    ggplot(aes(CountPeriodExt, value, colour = TransportationMode, group = TransportationMode)) +
            geom_line() +
            scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale())) +
            scale_colour_manual(values = 
                                overview_plot_colour_scheme[which(overview_plot_colour_scheme$TransportationMode == "Cyclist"), "fill"]) +
            guides(colour = FALSE) + 

            cop_cycling_theme +
            theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 0.5), #hjust = 1.05),
                  axis.text.y = element_text(size = 22),
                  legend.title = element_blank()) +
            ylab("Total Count") + xlab("Survey Period")


## ---- bicycle_daily_patterns_count_locations --------

summary_active_travel |>
    filter((TransportationMode == "Cycling") & (metric == "count_locations")) |>

    ggplot(aes(CountPeriodExt, value, colour = TransportationMode, group = TransportationMode)) +
            geom_line() +
            scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale())) +
            scale_colour_manual(values = 
                                overview_plot_colour_scheme[which(overview_plot_colour_scheme$TransportationMode == "Cyclist"), "fill"]) +
            guides(colour = FALSE) + 

            cop_cycling_theme +
            theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 0.5), #hjust = 1.05),
                  axis.text.y = element_text(size = 22),
                  legend.title = element_blank()) +
            ylab("No. of Locations") + xlab("Survey Period")


# rm(max_mid_daily, summary_active_travel)


## ---- summary_pedestrians_daily_patterns --------

max_mid_daily <- summary_all_modes_survey_from_2017 |>
    filter(TransportationMode == "Pedestrian") |>
    reframe(across(matches("_daily"), max)) |>
    
    reframe(max_mid_daily = round(max(c_across(everything())), -1)) |>
    deframe()


## ---- pedestrians_daily_patterns_median_daily --------

summary_active_travel |>
    filter((TransportationMode == "Walking") & (metric == "median_daily")) |>

    ggplot(aes(CountPeriodExt, value, colour = TransportationMode, group = TransportationMode)) +
            geom_line() +
            scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale())) +
            scale_colour_manual(values = 
                                overview_plot_colour_scheme[which(overview_plot_colour_scheme$TransportationMode == "Cyclist"), "fill"]) +
            guides(colour = FALSE) + 


            cop_cycling_theme +
            theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 0.5), #hjust = 1.05),
                  axis.text.y = element_text(size = 22),
                  legend.title = element_blank()) +
            ylab("Typical Daily Count") + xlab("Survey Period") +
            ylim(c(NA, max_mid_daily))


## ---- pedestrians_daily_patterns_average_daily --------

summary_active_travel |>
    filter((TransportationMode == "Walking") & (metric == "average_daily")) |>

    ggplot(aes(CountPeriodExt, value, colour = TransportationMode, group = TransportationMode)) +
            geom_line() +
            scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale())) +
            scale_colour_manual(values = 
                                overview_plot_colour_scheme[which(overview_plot_colour_scheme$TransportationMode == "Cyclist"), "fill"]) +
            guides(colour = FALSE) + 

            cop_cycling_theme +
            theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 0.5), #hjust = 1.05),
                  axis.text.y = element_text(size = 22),
                  legend.title = element_blank()) +
            ylab("Average Count, Daily") + xlab("Survey Period") +
            ylim(c(NA, max_mid_daily))


## ---- pedestrians_daily_patterns_count --------

summary_active_travel |>
    filter((TransportationMode == "Walking") & (metric == "count")) |>

    ggplot(aes(CountPeriodExt, value, colour = TransportationMode, group = TransportationMode)) +
            geom_line() +
            scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale())) +
            scale_colour_manual(values = 
                                overview_plot_colour_scheme[which(overview_plot_colour_scheme$TransportationMode == "Cyclist"), "fill"]) +
            guides(colour = FALSE) + 

            cop_cycling_theme +
            theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 0.5), #hjust = 1.05),
                  axis.text.y = element_text(size = 22),
                  legend.title = element_blank()) +
            ylab("Total Count") + xlab("Survey Period")


## ---- pedestrians_daily_patterns_count_locations --------

summary_active_travel |>
    filter((TransportationMode == "Walking") & (metric == "count_locations")) |>

    ggplot(aes(CountPeriodExt, value, colour = TransportationMode, group = TransportationMode)) +
            geom_line() +
            scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale())) +
            scale_colour_manual(values = 
                                overview_plot_colour_scheme[which(overview_plot_colour_scheme$TransportationMode == "Cyclist"), "fill"]) +
            guides(colour = FALSE) + 

            cop_cycling_theme +
            theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 0.5), #hjust = 1.05),
                  axis.text.y = element_text(size = 22),
                  legend.title = element_blank()) +
            ylab("No. of Locations") + xlab("Survey Period")


# rm(max_mid_daily, summary_active_travel)


## ---- growth_daily_patterns_active_travel --------

summary_active_travel |>
    mutate(across(metric, ~ if_else((. == "count_locations"), "baseline", "statistic"), .names = "statistic_type"),
           across(statistic_type, fct_rev),
          ) |>

    ggplot(aes(CountPeriodExt, growth, colour = metric, group = metric)) +
            geom_line(aes(linetype = statistic_type)) +
            scale_y_continuous(labels = scales::label_percent()) +         
            khroma::scale_colour_romaO(discrete = TRUE, 
                                       breaks = c("median_daily", "average_daily", "count", "count_locations"),
                                       labels = snakecase::to_title_case(c("median_daily", "average_daily", "count", "no_of_locations"))
                                      ) +
            guides(linetype = FALSE) +

            cop_cycling_theme +
            theme(axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5), #hjust = 1.05),
                  axis.text.y = element_text(size = 16),
                  legend.title = element_blank(),
                  panel.spacing = unit(4, "lines")
                 ) +
            ylab("Change over Time - Active Travel") + xlab("Survey Period") +
            facet_wrap(~ TransportationMode, #scales = "free_x",
                       dir = "v") 


## ---- growth_daily_patterns_seasonal_active_travel --------

summary_active_travel |>
    mutate(across(metric, ~ if_else((. == "count_locations"), "baseline", "statistic"), .names = "statistic_type"),
           across(statistic_type, fct_rev),
          ) |>

    ggplot(aes(CountPeriodExt, growth_seasonal, colour = metric, group = metric)) +
            geom_line(aes(linetype = statistic_type)) +
            scale_y_continuous(labels = scales::label_percent()) +         
            khroma::scale_colour_romaO(discrete = TRUE, 
                                       breaks = c("median_daily", "average_daily", "count", "count_locations"),
                                       labels = snakecase::to_title_case(c("median_daily", "average_daily", "count", "no_of_locations"))
                                      ) +
            guides(linetype = FALSE) + 

            cop_cycling_theme +
            theme(axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5), #hjust = 1.05),
                  axis.text.y = element_text(size = 16),
                  legend.title = element_blank(),
                  strip.text = element_text(colour = "#333333"),
                  panel.spacing.x = unit(0.75, "lines"),
                  panel.spacing.y = unit(4, "lines")
                 ) +
            ylab("Change by Season - Active Travel") + xlab("Survey Period") + 
            facet_grid(TransportationMode ~ season, scales = "free_x")



## ----  --------




