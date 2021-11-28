## ---- overview_traffic_surveys_subplots --------

transportation_mode_plots <- lapply(overview_plot_colour_scheme$group, function(i) {

        summary_all_modes_survey_from_2017 %>%
            filter(TransportationMode == levels(summary_all_modes_survey_from_2017$TransportationMode)[i]) %>%

            ggplot(aes(CountPeriod, count, fill = TransportationMode, group = TransportationMode)) + 
                geom_area(fill = overview_plot_colour_scheme$fill[i]) +
                scale_y_continuous(labels = scales::label_number_si()) +         
                guides(fill = "none") +

                cop_cycling_theme +         
                theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.5), 
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

    layout(sunburstcolorway = viridis_pal(direction = -1, begin = 0, end = 0.85, option = "D")(length(levels(summary_bicycles_may_sep_survey_from_2017$LocalAuthority))),
           extendsunburstcolors = TRUE, 
           margin = list(l = 2)
        )



## ----  --------


## ----  --------



