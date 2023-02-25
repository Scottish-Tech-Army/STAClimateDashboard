#nrow(reporting_sites)


if (!exists("site_max_cutoff")) {
   site_max_cutoff <- 5000
} 
if (!exists("site_mean_cutoff")) {
    site_mean_cutoff <- 3000
} 
if (!exists("high_reads_cutoff")) {
    high_reads_cutoff <- 1000
}

#print(site_max_cutoff)
#print(site_mean_cutoff)
#print(high_reads_cutoff)


## ---- counter_filters --------

counter_filter <- #dbGetQuery(dbConn, "SELECT * FROM counter_data_hourly") %>%
    #parseCounterDataFromDB() %>%
    cycle_counter_data_from_2017 %>%
    filter(traffic_mode == "bicycle") %>%
    select(- c(hour, countInterval, trafficDirection, laneId, starts_with("total"), ends_with("itude"), matches("\\w+Date"))) %>%
    #slice_sample(n = 5000) %>%

    mutate(monthOfYear = paste0(month, "-", year),
           across(monthOfYear, as.factor),
           across(monthOfYear, ~ fct_reorder(., date)),
           across(localTimestamp, as_datetime)
           ) %>%
    arrange(site, siteID, desc(monthOfYear))

counter_filter <- counter_filter %>%
    group_by(site, siteID, Location, RoadName) %>%
    summarise(instances = n(),
              across(count, list(sum = sum, mean = mean, median = median, max = max), na.rm = TRUE, .names = "site_{.fn}"),
              counter_start = min(localTimestamp)
             ) %>%

    full_join(counter_filter %>%
                group_by(site, siteID, year) %>%
                summarise(yearly_count = n(),
                          across(count, list(sum = sum, mean = mean, median = median, max = max), na.rm = TRUE, .names = "yearly_{.fn}")
                         )
             ) %>%

    full_join(counter_filter %>%
                group_by(site, siteID, monthOfYear) %>%
                summarise(monthly_count = n(),
                          across(count, list(sum = sum, mean = mean, median = median, max = max), na.rm = TRUE, .names = "{.fn}")
                         )
        ) %>%

    mutate(mean_to_site_mean = mean / site_mean,
           max_to_site_mean = max / site_mean,
          )


counter_filter_summary <- counter_filter %>%

    ungroup() %>%
    select(site, starts_with("site_"), counter_start) %>%
    group_by(site) %>% #across(starts_with("site"))) %>%
    summarise(across(counter_start, min),
              across(site_mean, mean),
              across(site_max, max)
             ) %>%

    distinct() %>%
    mutate(site_max_to_mean = site_max / site_mean) %>%
    arrange(desc(site_max_to_mean), desc(site_mean), desc(site_max)) %>%

    mutate(flag_very_high_reads = if_any(c(site_max_to_mean, site_max), ~ (. >= site_max_cutoff)) | 
                                   ((site_mean > 1) & if_all(c(site_max_to_mean, site_max), ~ (. >= site_mean_cutoff))),
           flag_high_reads = (site_mean > 1) & (site_max_to_mean >= high_reads_cutoff),
           )


counter_filter_max_values <- counter_filter %>%
    distinct(site, Location, RoadName, counter_start, site_mean, site_median, site_max) %>%
    group_by(site, siteID) %>%
    summarise(across(where(is.numeric), max)) %>%

    left_join (counter_filter %>%
                distinct(site, Location, RoadName, counter_start, site_mean, site_median, site_max) %>%
                group_by(site) %>%
                summarise(count = n(),
                          across(where(is.numeric), max)) %>% #, .names = "{.col}_la")) 
                rename_with(~ gsub("site_", "la_", .x))
    ) %>%
    select(- count, everything(), count) %>%
    arrange(site, desc(site_median), desc(site_mean), desc(site_max))

rm(site_max_cutoff, site_mean_cutoff, high_reads_cutoff)



## ---- counter_filter_zero_daily_counts --------

counter_filter_zero_daily_counts <- cycle_counter_data_from_2017 %>%
    #filter(traffic_mode == "bicycle") %>%
    group_by(Provider, traffic_mode, site, siteID, Location, RoadName) %>%
    summarise(days_reporting = n_distinct(date, na.rm = TRUE),
              counter_start = min(date, na.rm = TRUE)) %>%

    right_join(cycle_counter_data_from_2017 %>%
                  #filter(traffic_mode == "bicycle") %>%
                  group_by(traffic_mode, date, site, siteID, Location, RoadName) %>%
                  summarise(across(count, sum, na.rm = TRUE)) %>%
                  filter(count == 0)
              )

counter_filter_zero_daily_counts <- counter_filter_zero_daily_counts %>%

    full_join(counter_filter_zero_daily_counts %>%
                group_by(across(!c(date, count))) %>%
                summarise(days_at_zero = n(),
                          start_date = min(date),
                          end_date = max(date)
                         )
              ) %>%
    full_join(counter_filter_zero_daily_counts %>%
                mutate(monthOfYear = paste(month(date, label = TRUE), year(date), sep = "-"),
                       across(monthOfYear, as.factor),
                       across(monthOfYear, ~ fct_reorder(., date))
                      )
             ) %>%
    ungroup()


counter_filter_zero_daily_counts <- counter_filter_zero_daily_counts %>%
                mutate(monthOfYear = paste(month(date, label = TRUE), year(date), sep = "-"),
                       across(monthOfYear, as.factor),
                       across(monthOfYear, ~ fct_reorder(., date))
                      )


counter_filter_zero_daily_counts <- counter_filter_zero_daily_counts %>%

    left_join(counter_filter_zero_daily_counts %>%
                group_by(site, siteID, monthOfYear, days_at_zero) %>%
                summarise(days_in_month = days_in_month(date),
                          days_at_zero_per_month = n(),
                          month_at_zero = (days_at_zero_per_month == days_in_month),
                          ) %>%
                ungroup() %>%
                distinct(site, siteID, monthOfYear, month_at_zero)
              )

#glimpse(counter_filter_zero_daily_counts)
