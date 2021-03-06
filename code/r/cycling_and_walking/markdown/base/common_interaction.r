library(lubridate)

library(khroma)
library(plotly)
#library(crosstalk)


weekdaysAbbrevRegex <- paste0(levels(wday(1, label = TRUE)), collapse = "|")
weekdaysAbbrevRegex

monthsAbbrevRegex <- paste0(month.abb, collapse = "|")
monthsAbbrevRegex

# will still fail if first part false... - calls both parts and returns one :@
#genderRegex <- if_else(exists("gender_options_formatted"), paste0(gender_options_formatted, collapse = "|"), "")
genderRegex <- ""
if (exists("gender_options_formatted"))
genderRegex <- paste0(gender_options_formatted, collapse = "|")

# initially restricted to it-402dc, but now shared across all,along with other potentially reusable vars
# colour-blind safe - see https://cran.r-project.org/web/packages/khroma/vignettes/tol.html
gender_colour_scheme <- c("female" = as.character(colour("muted")(7)["wine"]),
                          "male" = as.character(colour("muted")(7)["green"]))
gender_shape_icons <- c("female" = -0x2640L, "male" = -0x2642L) # \u2640 and \u2642 )

# weather_metrics <- c("min_temp", "mean_temp", "max_temp", "rainfall")
weather_metrics_colour_scheme <- c("min_temp" = as.character(colour("high contrast")(3)["blue"]),
                                   "mean_temp" = as.character(colour("high contrast")(3)["yellow"]),
                                   "max_temp" = as.character(colour("high contrast")(3)["red"]),
                                   "rainfall" = as.character(colour("vibrant")(7)["teal"])
                                  )



tickFont <- list(family = "Arial, sans-serif", size = 12)



renameTraces <- 
    function (plot_tmp, genderRegex = "", regexpr = NULL) {
      
        if (is.null(regexpr))
            regexpr <- paste0(weekdaysAbbrevRegex, "|", monthsAbbrevRegex, "|", genderRegex)

        for (i in seq_along(plot_tmp$x$data)) {   

            current_trace <- plot_tmp$x$data[[i]]$name

            #print(i)
            #print(str_extract(current_trace, regex(regexpr)))
            if (!is.null(current_trace)) {
                current_trace <- str_remove_all(current_trace, "[()]")
                plot_tmp$x$data[[i]]$name <- str_extract(current_trace, regex(regexpr))
            }

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
            #partial_bundle(local = FALSE) %>% - need full for transforms to work
            
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


create_plotly_control_buttons <- function(arg_label, values, update_method = NULL) {
    
    lapply(
        values,
        
        FUN = function(value, arg_label) {

          button <- list(
            method = if_else(is_null(update_method), "restyle", update_method),
            args = list(arg_label, value),
            label = sprintf("%s", value)
          )
        },
        arg_label
    )
}

create_plotly_geom_vline <- function(x = 0, colour = "red", width = 1, linetype = "dashdot") {
    list(type = "line",
         y0 = 0, y1 = 1,
         yref = "paper",
         x0 = x, x1 = x,
         line = list(color = colour, width = width, dash = linetype)
    )
}


geo_centroid <- function(geo_coordinates) {
    
    latitude <- geo_coordinates %>%
                    select(Latitude) %>%
                    deframe()
    longitude <- geo_coordinates %>%
                    select(Longitude) %>%
                    deframe()

    latitude <- latitude * pi / 180
    longitude <- longitude * pi / 180
    
    x <- cos(latitude) * cos(longitude)
    y <- cos(latitude) * sin(longitude)
    z <- sin(latitude)
    
    x <- mean(x)
    y <- mean(y)
    z <- mean(z)
    
    
    hypotenuse <- sqrt(x^2 + y^2)
    latitude <- atan2(z, hypotenuse) * 180 / pi
    longitude <- atan2(y, x) * 180 / pi

    
    return(data.frame(Latitude = latitude, Longitude = longitude))
}
