# Create stream data time series for 2017-2019 with date ranges to view in the Shiny app (viewTimeSeries)
# Events inclued here are only ones that were clustered in the SOM analyses

# Load packages
library("tidyverse")
library("lubridate")
library("data.table")

# Read in data
  # 2017-2019 stream time series with event start and end dates (created in compile_calculate_allVars.R)
  # The data are from/prepared in compile_calculate_allVars.R
  stream <- read_csv("Data/streamData_HF_WD_2017-2019.csv", col_types = cols(rain_start = col_datetime(format = ""),
                                                                             event_start = col_datetime(format = ""),
                                                                             falling_inf_pt = col_datetime(format = ""),
                                                                             event_end = col_datetime(format = ""))) %>% 
    # Convert date
    mutate(timestamp = ymd_hms(timestamp, tz = "Etc/GMT+4"),
           rain_start = ymd_hms(rain_start, tz = "Etc/GMT+4"),
           event_start = ymd_hms(event_start, tz = "Etc/GMT+4"),
           falling_inf_pt = ymd_hms(falling_inf_pt, tz = "Etc/GMT+4"),
           event_end = ymd_hms(event_end, tz = "Etc/GMT+4"))

  # Event start dates of events that were clustered in the SOM (created in Results_yields_summary.Rmd)
  clustered <- read_csv("Data/results_clusters_withLinearRegressions.csv") %>% 
    mutate(event_start = ymd_hms(event_start, tz = "Etc/GMT+4"))

# Create date ranges for events in clustered to be overlapped onto stream data
dates <-
  clustered %>% 
  select(site, event_start) %>% 
  group_by(site, event_start) %>% 
  slice(1:1) %>% 
  left_join(stream %>% 
              group_by(site, event_start) %>% 
              slice(1:1), 
            by = c("site", "event_start")) %>% 
  select(site, rain_start, event_start, falling_inf_pt, event_end) %>% 
  # Extend range around event start and end (so I can graph the event time series plus data on either side of the event)
  mutate(event_start_7dB4 = event_start - days(7),
         event_end_7dAF = event_end + days(7)) %>% 
  # Add an event ID to use in Shiny app
  mutate(siteabbrev = ifelse(site == "Hungerford", "HB", "WB"),
         eventID = paste(siteabbrev, "-", as.character(event_start), sep = "")) %>% 
  select(-siteabbrev)

# Drop event delineation columns from stream
stream <- 
  stream %>% 
  select(-c(rain_start, event_start, falling_inf_pt, event_end))

# Do overlap join of dates to stream data using the new date range surrounding each event
# Convert dfs to data.tables
  # In addition, the function foverlaps requires that the keys in y (dates) have matches in x (stream)
  # so we have to create a 2nd time column (just a copy of timestamp) so that we also have two time columns in the stream df
setDT(stream)[, Time2 := timestamp]
setDT(dates)

# Need to set keys for y (dates) before the overlap join
setkey(dates, site, event_start_7dB4, event_end_7dAF)

# Here we do the overlap join by timestamp_sample_24BS and timestamp_sample
# the [ ] at the end just removes the extra columns added

# Note, because many of the new expanded date ranges will overlap, the resulting alldata df will have more rows than the original stream df b/c it copies rows; this is what we want
alldata <- foverlaps(x = stream, y = dates, by.x = c("site", "timestamp", "Time2"), nomatch = NA)[, "Time2" := NULL]

# Write the time series to a CSV
alldata %>%
  mutate(timestamp = as.character(timestamp),
         rain_start = as.character(rain_start),
         event_start = as.character(event_start),
         falling_inf_pt = as.character(falling_inf_pt),
         event_end = as.character(event_end),
         event_start_7dB4 = as.character(event_start_7dB4),
         event_end_7dAF = as.character(event_end_7dAF)) %>%
  write_csv("viewTimeSeries/Data/streamData_for_viewTimeSeriesApp.csv")
