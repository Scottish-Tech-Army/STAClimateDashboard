library(lubridate)

library(plotly)
library(crosstalk)


weekdaysAbbrevRegex <- paste0(levels(wday(1, label = TRUE)), collapse = "|")
weekdaysAbbrevRegex

monthsAbbrevRegex <- paste0(month.abb, collapse = "|")
monthsAbbrevRegex

genderRegex <- if_else(exists("gender_options_formatted"), paste0(gender_options_formatted, collapse = "|"), "")


tickFont <- list(family = "Arial, sans-serif", size = 12)



renameTraces <- 
    function (plot_tmp, genderRegex = "", regexpr = NULL) {
      
        if (is.null(regexpr))
            regexpr <- paste0(weekdaysAbbrevRegex, "|", monthsAbbrevRegex, "|", genderRegex)

        for (i in seq_along(plot_tmp$x$data)) {   

            current_trace <- plot_tmp$x$data[[i]]$name

            #print(i)
            #print(current_trace)
            if (!is.null(current_trace))
                plot_tmp$x$data[[i]]$name <- str_extract(current_trace, regex(regexpr))

        }

        return(plot_tmp)
    }


convertToPlotly <-

    function(ggPlot, tooltip = "text", renameTraces = FALSE, regexpr = NULL, height = 1200, width = 900, yaxis = NULL, xaxis = NULL, legend = NULL, hidePlotlyModeBar = FALSE, animated = FALSE, setAnimationOptions = NULL, animationPrefix, maxMargin = 5) {
        
        overwriteLayout <- xaxis
        xaxis <- list(tickfont = tickFont)
        if (!is.null(overwriteLayout))
            xaxis = overwriteLayout # will overwrite as required
            
        overwriteLayout <- yaxis
        yaxis <- list(tickfont = tickFont)
        if (!is.null(overwriteLayout))
            yaxis = overwriteLayout
            
        overwriteLayout <- legend
        legend = list(font = list(family = "Arial, sans-serif", size = 14))
        if (!is.null(overwriteLayout))
            legend <- overwriteLayout
            

        plot_tmp <- ggplotly(ggPlot, tooltip = tooltip, height = height, width = width) %>%

            style(hoveron = "points", hoverinfo = "text", hoverlabel = list(bgcolor = "white")) %>%
            layout(xaxis = xaxis, yaxis = yaxis, legend = legend,
                   title = list(font = list(size = 20)),
                   margin = list(l = maxMargin)
                  ) %>% # needed particularly where markdown in use for labels
            highlight(on = "plotly_hover", off = "plotly_doubleclick") #, selected = attrs_selected(showlegend = FALSE)) - does not reset automtically
        
        if (animated) {
            if (!is.null(animationPrefix))
                plot_tmp <- plot_tmp %>%
                    animation_slider(currentvalue = list(prefix = paste0(animationPrefix, ": "), font = list(color = "grey")))
            
            
            if (is.null(setAnimationOptions))
                plot_tmp <- plot_tmp %>%
                    animation_opts(6000, transition = 3000, redraw = TRUE) # being ignored...
            
        }
        
        if (hidePlotlyModeBar) {
          plot_tmp %>%
              config(displayModeBar = FALSE)
        }
        
        
        plot_tmp <- plot_tmp %>%
            plotly_build()

        if (renameTraces)
            return(renameTraces(plot_tmp, regexpr))
        return(plot_tmp)
    }

