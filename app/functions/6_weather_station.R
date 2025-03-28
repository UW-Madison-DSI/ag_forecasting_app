library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)

plot_airtemp_30day <- function(station, end_date) {
  end_date   <- as.Date(end_date)
  start_date <- end_date - 38
  
  base_url <- "https://connect.doit.wisc.edu/pywisconet_wrapper/bulk_measures"
  url      <- file.path(base_url, station)
  
  params <- list(
    start_date   = format(start_date, "%Y-%m-%d"),
    end_date     = format(end_date, "%Y-%m-%d"),
    measurements = "AIRTEMP",
    frequency    = "DAILY"
  )
  
  res <- GET(url, query = params, add_headers('accept' = 'application/json'))
  stop_for_status(res)
  
  raw_data <- content(res, "text", encoding = "UTF-8")
  df <- fromJSON(raw_data, flatten = TRUE)
  
  if (length(df) == 0) {
    message(
      "No data returned for station '", station, 
      "' in the range ", start_date, " to ", end_date
    )
    return(invisible(NULL))
  }
  
  # 1) Pivot the daily data to have columns avg, max, min
  df_processed <- df %>%
    mutate(date = as.Date(collection_time)) %>%
    select(date, qualifier, value) %>%
    pivot_wider(names_from = qualifier, values_from = value) %>%
    arrange(date)
  
  # 2) Convert from Fahrenheit to Celsius for avg, max, and min
  df_processed <- df_processed %>%
    mutate(
      avg = (avg - 32) * 5 / 9,
      max = (max - 32) * 5 / 9,
      min = (min - 32) * 5 / 9
    )
  
  # 3) Compute 30-day rolling means (still in Celsius)
  df_processed <- df_processed %>%
    mutate(
      mov_avg = rollmean(avg, k = 30, fill = NA, align = "right"),
      mov_max = rollmean(max, k = 30, fill = NA, align = "right"),
      mov_min = rollmean(min, k = 30, fill = NA, align = "right")
    )
  
  # 4) Convert to "long" format so each line can have a distinct color & legend
  df_long <- df_processed %>%
    select(date, avg, max, min, mov_avg, mov_max, mov_min) %>%
    pivot_longer(
      cols      = -date,         # All except 'date'
      names_to  = "series",      # New column
      values_to = "temperature"  # Numeric values
    )
  
  # 5) (Optional) Label each series in a user-friendly way
  df_long <- df_long %>%
    mutate(
      series = factor(
        series,
        levels = c("avg", "max", "min", "mov_avg", "mov_max", "mov_min"),
        labels = c(
          "Daily Avg (°C)",   # avg
          "Daily Max (°C)",   # max
          "Daily Min (°C)",   # min
          "30-day Avg (°C)",  # mov_avg
          "30-day Max (°C)",  # mov_max
          "30-day Min (°C)"   # mov_min
        )
      )
    )
  
  # 6) Plot: each "series" gets its own color, and a legend entry
  p <- ggplot(df_long, aes(x = date, y = temperature, color = series)) +
    geom_line() +
    labs(
      title = paste0(
        "Daily Air Temperature (Celsius) from ", format(start_date, "%Y-%m-%d"),
        " to ", format(end_date, "%Y-%m-%d")
      ),
      x     = "Date",
      y     = "Temperature (°C)",
      color = "Series"
    )
  
  print(p)
  
  # Return both the data (in long format) and the ggplot
  return(list(
    data = df_long,
    plot = p
  ))
}
