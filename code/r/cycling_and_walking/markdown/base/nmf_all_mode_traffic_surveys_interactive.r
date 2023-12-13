## ---- overview_traffic_surveys_subplots --------

transportation_mode_plots <- lapply(overview_plot_colour_scheme$group, function(i) {

        summary_all_modes_survey_from_2017 %>%
            filter(TransportationMode == levels(overview_plot_colour_scheme$TransportationMode)[i]) %>%

            ggplot(aes(CountPeriodExt, count, fill = TransportationMode, group = TransportationMode)) + 
                geom_area(fill = overview_plot_colour_scheme$fill[i]) +
                scale_y_continuous(labels = scales::label_number_si()) +         
                guides(fill = "none") +

                cop_cycling_theme +         
                theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.75, hjust = 0.75), 
                      axis.text.y = element_text(size = 12), 
                      #legend.title = element_blank()
                     ) + 
                ylab("Total Count") + xlab("Survey Period")  


    })
    


## ---- sunburst_layout_bicycles --------

plot_ly(width = 550, #height = 500, 
        data = sunburst_layout_bicycles %>%
                mutate_at(vars(value), ~ coalesce(., 0))
       ) %>% 

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
        
      ) %>%

      layout(sunburstcolorway = viridis_pal(direction = -1, begin = 0, end = 0.85, option = "D")(length(levels(summary_bicycles_survey_periods_from_2017$LocalAuthority))),
             extendsunburstcolors = TRUE, 
             margin = list(l = 2)
          )


## ---- facet_view_survey_periods_cyclist --------

selectedTransportationMode <- "Cyclist"

all_modes_survey_from_2017 %>%
    filter((TransportationMode == selectedTransportationMode) & !is.na(count)) %>%

    ggplot(aes(TimePeriod, count)) +
        geom_boxplot(size = 0.25, notchwidth = 0.25, outlier.size = 0.45) +
        cop_cycling_theme +
        theme(axis.text.x = element_text(size = 11, angle = 45, vjust = 0.75, hjust = 0.75),
              axis.text.y = element_text(size = 14),
              axis.title.y = element_text(size = 20),
              #strip.text = element_text(size = 36)
             ) +
        ylab(paste0("Count - ", selectedTransportationMode, "s")) +
        xlab("") +
        scale_x_discrete(breaks = time_period_axis_breaks) +
        facet_wrap(~ CountPeriodExt, nrow = 2, dir = "v")

rm(selectedTransportationMode)


## ---- facet_view_survey_periods_pedestrian --------

selectedTransportationMode <- "Pedestrian"

all_modes_survey_from_2017 %>%
    filter((TransportationMode == selectedTransportationMode) & !is.na(count)) %>%

    ggplot(aes(TimePeriod, count)) +
        geom_boxplot(size = 0.25, notchwidth = 0.25, outlier.size = 0.45) +
        cop_cycling_theme +
        theme(axis.text.x = element_text(size = 11, angle = 45, vjust = 0.75, hjust = 0.75),
              axis.text.y = element_text(size = 14),
              axis.title.y = element_text(size = 20),
              #strip.text = element_text(size = 36)
             ) +
        ylab(paste0("Count - ", selectedTransportationMode, "s")) +
        xlab("") +
        scale_x_discrete(breaks = time_period_axis_breaks) +
        facet_wrap(~ CountPeriodExt, nrow = 2, dir = "v")

rm(selectedTransportationMode)


## ---- survey_periods --------

survey_periods <- all_modes_survey_from_2017 %>%

    mutate(CountPeriodExt = month(Date),
           across(CountPeriodExt, ~ case_when((. >= 9) | (. < 5) ~ paste0("Autumn ", year(Date)),
                                            (. < 9) ~ paste0("Spring ", year(Date))
                                           )),
           across(matches("CountPeriod"), ~ fct_reorder(., Date)),
           ) %>%
    distinct(CountPeriod, CountPeriodExt, Location)


survey_periods <- survey_periods %>%

    mutate(SurveyDate = if_else(str_starts(CountPeriod, "May-|Sep-"), "SurveyDate", "DelayedSurvey")) %>%
    count(name = "NoOfLocations", CountPeriod, CountPeriodExt, SurveyDate) %>%
    arrange(CountPeriod) %>%

    full_join(survey_periods %>%
                  distinct(CountPeriodExt, Location) %>%
                  count(name = "TotalLocations", CountPeriodExt)
             )


survey_periods %>%
    select(matches("CountPeriod")) %>%
    pivot_wider(names_from = CountPeriod, values_from = CountPeriod) %>%
    unite("CountPeriod", matches("\\-\\d"), na.rm = TRUE, sep = ", ") %>%
    mutate(across(CountPeriod, ~ gsub("\\-\\d*", "", .))) %>%

    full_join(survey_periods %>%
                select(- CountPeriod) %>%
                pivot_wider(names_from = SurveyDate, values_from = NoOfLocations) %>%
                mutate(across(DelayedSurvey, ~ coalesce(., 0)),
                       PartialSurvey = (SurveyDate - TotalLocations) + DelayedSurvey,
                       across(c(SurveyDate, DelayedSurvey), ~ (. - PartialSurvey))
                      )
             ) %>%
    relocate(PartialSurvey, .before = DelayedSurvey) %>%
    rename_with(~ c("SurveyPeriod", "SurveyMonths", "NoOfSettlements", "SurveyedOnDate"),
                    c("CountPeriodExt", "CountPeriod", "TotalLocations", "SurveyDate")) %>%
    rename_with(snakecase::to_title_case) %>%
    mutate(across(where(is.numeric), scales::comma),
           across(matches("Survey$"), ~ gsub("^0$", "-", .)),
          ) %>%

    kable(caption = "") %>% #No of settlements with partial or delayed surveys per period
    kable_paper(c("striped", "responsive"), full_width = FALSE, position = "left")

#rm(survey_periods)


## ----  --------



