# Calculate precipitation indices for 2017-2019 events at Hungerford and Wade

# Target dates 8/1/17 (when most soil sensors recording at both sites) to 12/31/2019

# NOTES on time zones (eye roll): most stream data is Etc/GMT+4 (EDT) and ignores DST, so I kept things in EDT
    # soil sensor data is collected in Etc/GMT+5 (EST) and ignores DST, so I converted soil data timestamps to Etc/GMT+4 (EDT)

# TO DO:
  # Calculate soil moisture deficit (need estimated actual ET)
  # Look into where rain_start from data (first rain obs for each event; see rain intensity mean calc below) differs from the rain_start we recorded during event delineations
  # Currently using k = 0.95 for API calc; need to figure out if this is OK for this region
  

# Load packages
  library("tidyverse")
  library("lubridate")
  library("data.table")
  library("zoo")
  library("beepr")

  
# Read in data and tidy ----
# EVENT DELINEATIONS ----
  # 2014-2018
  events_hford_14to18 <- read_csv("../../2_Tasks/Storm Event Delineation/Data/Event delineations 2014-2018/Events_Hungerford_2014to2018.csv", na = c("", "NA", "NaN")) %>% mutate(site = "Hungerford")
  events_wade_14to18 <- read_csv("../../2_Tasks/Storm Event Delineation/Data/Event delineations 2014-2018/Events_Wade_2014to2018.csv", na = c("", "NA", "NaN")) %>% mutate(site = "Wade")
    # Combine and tidy these up
    events_17to18 <- bind_rows(events_hford_14to18, events_wade_14to18) %>% 
      # Rename & rearrange columns
      rename(rain_start = "rainfall start", event_start = "HydRun start", falling_inf_pt = "HIP end", event_end = "HydRun end", multipeak = "multipeak tag", snowmelt = Snowmelt) %>% 
      # Convert dates
      mutate(rain_start = mdy_hm(rain_start, tz = "Etc/GMT+4"),
             event_start = mdy_hm(event_start, tz = "Etc/GMT+4"),
             falling_inf_pt = mdy_hm(falling_inf_pt, tz = "Etc/GMT+4"),
             event_end = mdy_hm(event_end, tz = "Etc/GMT+4")) %>% 
      # Filter date range
      filter(event_start >= ymd_hms("2017-08-01 00:00:00", tz = "Etc/GMT+4")) %>% 
      # Add a storm event ID (1 to total # of events at each site and year)
      group_by(site, year(event_start)) %>% 
      mutate(eventid = seq_along(event_start)) %>% 
      ungroup() %>% 
      # Rearrange columns
      select(-c("HydRun detect?", "year(event_start)")) %>% select(site, eventid, everything())
    rm(events_hford_14to18, events_wade_14to18)
  # 2019
  events_hford_19 <- read_csv("../../2_Tasks/Storm Event Delineation/Data/Event delineations 2019/Events_Hungerford_2019.csv", na = c("", "NA", "NaN")) %>% mutate(site = "Hungerford")
  events_wade_19 <- read_csv("../../2_Tasks/Storm Event Delineation/Data/Event delineations 2019/Events_Wade_2019.csv", na = c("", "NA", "NaN")) %>% mutate(site = "Wade")  
    # Combine and tidy these up
    events_19 <- bind_rows(events_hford_19, events_wade_19) %>% 
      # Rename & rearrange columns
      rename(rain_start = "rainfall start", event_start = "HydRun Start", event_YesNo = "Actual event?", event_start_adj = "Adjusted Start", falling_inf_pt = "HIP end", 
             event_end = "HydRun End", event_end_adj = "Adjusted End", multipeak = "multipeak tag", snowmelt = Snowmelt) %>% 
      # Keep only detected events we determined to be actual events
      filter(event_YesNo == "Yes") %>% 
      # Replace event_start with the event_start_adj if there is a date for the latter; repeat for end dates
      mutate(event_start = ifelse(is.na(event_start_adj), event_start, event_start_adj),
             event_end = ifelse(is.na(event_end_adj), event_end, event_end_adj)) %>% 
      # Convert dates
      mutate(rain_start = mdy_hm(rain_start, tz = "Etc/GMT+4"),
             event_start = mdy_hm(event_start, tz = "Etc/GMT+4"),

             falling_inf_pt = mdy_hm(falling_inf_pt, tz = "Etc/GMT+4"),
             event_end = mdy_hm(event_end, tz = "Etc/GMT+4")) %>% 
      # Add a storm event ID (1 to total # of events at each site and year)
      group_by(site, year(event_start)) %>% 
      mutate(eventid = seq_along(event_start)) %>% 
      ungroup() %>% 
      # Rearrange columns
      select(-c(event_YesNo, event_start_adj, event_end_adj, "HydRun detect?", Notes, "year(event_start)")) %>% select(site, eventid, everything())
    rm(events_hford_19, events_wade_19)
  # Combine event objects into one
  events_all <- bind_rows(events_17to18, events_19) %>% 
    # Fix multipeak column so it's either MP or NO
    mutate(multipeak = replace_na(multipeak, "NO"))
  rm(events_17to18, events_19)
  
  
# PRECIP/SNOW DATA FROM CoCoRaHS
  # ccr <- read_csv("Data/dailyPrecip_cocorahs_compiled.csv") %>% 
  #   mutate(date = ymd(date)) %>% 
  #   rename(precip_in_ccr = TotalPrecipAmt, snow_new_in_ccr = NewSnowDepth, snow_total_in_ccr = TotalSnowDepth)
  
  
# ADD ccr to events_all
  # test <- left_join(events_all %>% mutate(date = date(event_start)), 
  #                   ccr %>% select(date, site, precip_in_ccr, snow_new_in_ccr, snow_total_in_ccr), 
  #                   by = c("site", "date"))
  
# PRECIPITATION AND MET DATA ----
  # Hungerford
    # 2015-2017 tipping bucket data (tz = GMT-4)
    # But trim period from 7/1/17 to end of record 11/19/17 13:15
    precip_hford_15to17 <- read_csv("../../General Site Data/Met data/hungerford_2015-17_precip_data.csv") %>%
      rename(timestamp = "date", precip_mm = "rain_mm") %>% 
      mutate(timestamp = mdy_hm(timestamp, tz = "Etc/GMT+4")) %>% 
      filter(timestamp >= ymd_hms("2017-07-01 00:00:00", tz = "Etc/GMT+4") & timestamp <= ymd_hms("2017-11-10 13:15:00", tz = "Etc/GMT+4")) %>% 
      # There were duplicate observations, so keeping only unique rows <- these are because the bucket can tip multiple times in a minute if rain is heavy!
      # distinct()
      # Instead let's aggregate the data to 5-min data
      # Create a new column that gives the date and hour of the measurement
      mutate(timestamp_5min = ceiling_date(timestamp, unit = "5 min")) %>% 
      # Now total 5-minute data into an hourly total
      group_by(timestamp_5min) %>% 
      summarize(precip_mm = sum(precip_mm)) %>% 
      rename(timestamp = timestamp_5min) %>% 
      mutate(site = "Hungerford")
    # 2017-2019 MET data (downloaded in UTC)
    met_hford_17to19 <- read_csv("../../General Site Data/Met data/hungerford_2017-19_met_data.csv", col_types = cols(atm_mbar = "c")) %>% 
      mutate(site = "Hungerford") %>% 
      # The atm_bar column had a few values with ',' in them and the parser just couldn't deal, so I read it as character & converting to numeric here      
      mutate(atm_mbar = as.numeric(gsub(",", "", atm_mbar)))
    # 2019 MET data (downloaded in UTC)
    met_hford_late19 <- read_csv("../../General Site Data/Met data/hungerford_remaining2019_met_data.csv", col_types = cols("Solar Radiation (S-LIB 10979904:10969902-1), W/m^2, Bree_met_3" = "c")) %>% 
      mutate(site = "Hungerford") %>% 
      rename(PAR_uE = starts_with("PAR"), windSpeed = starts_with("Wind Speed"), gustSpeed = starts_with("Gust Speed"), precip_mm = starts_with("Rain"),
             solarRad_wm2 = starts_with("Solar"), windDir = starts_with("Wind Direction"), atm_mbar = starts_with("Pressure"), temp_C = starts_with("Temperature"),
             Rh = starts_with("RH"), dewPoint = starts_with("Dew"), timestamp = "Date") %>% 
      # The solar radition column had a few values with ',' in them and the parser just couldn't deal, so I read it as character & converting to numeric here
      mutate(solarRad_wm2 = as.numeric(gsub(",", "", solarRad_wm2)))
  
  # Wade
    # 2014-2017 tipping bucket data (tz = GMT-4)
    precip_wade_14to17 <- read_csv("../../General Site Data/Met data/wade_2014-17_precip_data.csv") %>%
      rename(timestamp = "date", precip_mm = "rain_mm") %>% 
      mutate(timestamp = mdy_hm(timestamp, tz = "Etc/GMT+4")) %>% 
      filter(timestamp >= ymd_hms("2017-07-01 00:00:00", tz = "Etc/GMT+4") & timestamp < ymd_hms("2017-08-16 04:00:00", tz = "Etc/GMT+4")) %>% 
      # There were duplicate observations, so keeping only unique rows <- these are because the bucket can tip multiple times in a minute if rain is heavy!
      # distinct()
      # Instead let's aggregate the data to 5-min data
      # Create a new column that gives the date and hour of the measurement
      mutate(timestamp_5min = ceiling_date(timestamp, unit = "5 min")) %>% 
      # Now total 5-minute data into an hourly total
      group_by(timestamp_5min) %>% 
      summarize(precip_mm = sum(precip_mm)) %>% 
      rename(timestamp = timestamp_5min) %>% 
      mutate(site = "Wade")
    # 2017-2019 MET data (downloaded in UTC)
    met_wade_17to19 <- read_csv("../../General Site Data/Met data/wade_2017-19_met_data.csv", col_types = cols(solarRad_wm2 = "c", atm_mbar = "c")) %>% 
      mutate(site = "Wade") %>% 
      mutate(solarRad_wm2 = as.numeric(gsub(",", "", solarRad_wm2)),
             atm_mbar = as.numeric(gsub(",", "", atm_mbar)))
    # 2019 MET data (downloaded in UTC)
    met_wade_late19 <- read_csv("../../General Site Data/Met data/wade_remaining2019_met_data.csv", col_types = cols("Solar Radiation (S-LIB 10979907:10969903-1), W/m^2, Bree_met_6" = "c",
                                                                                                                      "Pressure (S-BPB 10979907:10972865-1), mbar, Bree_met_6" = "c")) %>% 
      mutate(site = "Wade") %>% 
      rename(PAR_uE = starts_with("PAR"), windSpeed = starts_with("Wind Speed"), gustSpeed = starts_with("Gust Speed"), precip_mm = starts_with("Rain"),
             solarRad_wm2 = starts_with("Solar"), windDir = starts_with("Wind Direction"), atm_mbar = starts_with("Pressure"), temp_C = starts_with("Temperature"),
             Rh = starts_with("RH"), dewPoint = starts_with("Dew"), timestamp = "Date") %>% 
      # The solar radition column had a few values with ',' in them and the parser just couldn't deal, so I read it as character & converting to numeric here
      mutate(solarRad_wm2 = as.numeric(gsub(",", "", solarRad_wm2)),
             atm_mbar = as.numeric(gsub(",", "", atm_mbar)))
    
  # Join these into one MET file
    met <- bind_rows(met_hford_17to19, met_hford_late19, met_wade_17to19, met_wade_late19) %>% 
      # The data are downloaded in UTC
      mutate(timestamp = mdy_hm(timestamp, tz = "UTC")) %>% 
      # Convert the times to EDT (GMT-4), because this is what the stream sensor data are in
      mutate(timestamp = force_tz(timestamp, tzone = "Etc/GMT+4")) %>% 
      select(-c(recNum, battVolt, "Line#")) %>% 
      select(site, timestamp, everything()) %>% 
      arrange(site, timestamp) %>% 
      # Filter out the months in 2017 at Hungerford where the tipping bucket was in place at the NN site
      # But keep the months were there was a gap in tipping bucket data (i.e., 8/29/17 11:45 to 9/15/17 11:55)
      filter(!(site == "Hungerford" & timestamp <= ymd_hms("2017-08-29 11:40:00", tz = "Etc/GMT+4"))) %>% 
      filter(!(site == "Hungerford" & (timestamp >= ymd_hms("2017-09-15 12:00:00", tz = "Etc/GMT+4") & timestamp <= ymd_hms("2017-11-10 13:15:00", tz = "Etc/GMT+4"))))
    rm(met_hford_17to19, met_hford_late19, met_wade_17to19, met_wade_late19)
    
    # test <- met %>% 
    #   filter(site == "Wade" & timestamp >= ymd_hms("2017-10-24 12:00:00", tz = "Etc/GMT+4")) 
    # timetest <- ymd_hms("2017-10-24 17:15:00", tz = "UTC")
    # with_tz(timetest, "Etc/GMT+4")
    # timetest2 <- ymd_hms("2017-10-24 17:15:00", tz = "Etc/GMT+4")
    
  # Add 2017 tipping bucket data
    met_all <- bind_rows(met, precip_hford_15to17, precip_wade_14to17) %>% 
      arrange(site, timestamp)
    rm(met, precip_hford_15to17, precip_wade_14to17)
    

  # Trim MET data to first and last event of each year at each site
  # But add a 1-month buffer to beginning of each year so that we can calculate pre-event things for 30 days
  # What are the start and end times for each site and year?
  events_all %>% 
    group_by(site, year(event_start)) %>% 
    summarize(start = first(rain_start),
              stop = last(event_end))
  
  met_all <- met_all %>% 
    filter((site == "Hungerford" & (timestamp >= ymd_hms("2017-07-01 00:00:00", tz = "Etc/GMT+4") & timestamp <= ymd_hms("2017-12-03 23:59:59", tz = "Etc/GMT+4"))) | 
             (site == "Hungerford" & (timestamp >= ymd_hms("2018-02-15 00:00:00", tz = "Etc/GMT+4") & timestamp <= ymd_hms("2018-11-29 23:59:59", tz = "Etc/GMT+4"))) |
             (site == "Hungerford" & (timestamp >= ymd_hms("2019-02-15 00:00:00", tz = "Etc/GMT+4") & timestamp <= ymd_hms("2019-11-02 23:59:59", tz = "Etc/GMT+4"))) |
             (site == "Wade" & (timestamp >= ymd_hms("2017-07-01 00:00:00", tz = "Etc/GMT+4") & timestamp <= ymd_hms("2017-12-07 23:59:59", tz = "Etc/GMT+4"))) |
             (site == "Wade" & (timestamp >= ymd_hms("2018-03-01 00:00:00", tz = "Etc/GMT+4") & timestamp <= ymd_hms("2018-11-11 23:59:59", tz = "Etc/GMT+4"))) |
             (site == "Wade" & (timestamp >= ymd_hms("2019-03-01 00:00:00", tz = "Etc/GMT+4") & timestamp <= ymd_hms("2019-11-02 23:59:59", tz = "Etc/GMT+4")))
           )
  
  # test2 <- test %>% filter(site == "Hungerford")
  # test3 <- test %>% group_by(site, year(timestamp)) %>%
  #   arrange(site, timestamp) %>% 
  #   summarize(first = first(timestamp),
  #             last = last(timestamp))
  
  # Make sure that the data is a continuous 5-min time series to match tipping bucket to MET data
  # First, create a continuous 5 min time series from first Q measurement each year to last Q measurement each year for each site
    # Create time sequence to interpolate
    interp_time <- met_all %>% 
      mutate(year = year(timestamp)) %>% 
      select(site, year, timestamp)
  
    # Create the 5-min sequence
    interp_time <- setDT(interp_time)[, list(timestamp = seq(min(timestamp), max(timestamp), by = "5 min")), by = c("site", "year")]
    # Round the 5-min sequence to nearest 5-min (just in case!)
    interp_time <- interp_time %>% mutate(timestamp = ceiling_date(timestamp, unit = "5 min"))    
    
    # Second, join met_all to interp_time
    # Can set the maxgap parameter to limit how many rows of missing data are interpolated; here we set it at 24 rows at 5 min/row = 2 hrs
    # No interpolation here, b/c that would increase the amount of rain in a given time period
    met_all <- full_join(interp_time, met_all, by = c("site", "timestamp")) %>% 
      arrange(site, timestamp) %>% 
      mutate(precip_mm = replace_na(precip_mm, 0)) %>% 
      select(-year)
    
 
    

# POTENTIAL ET ----
  PET <- read_csv("../../General Site Data/PotentialET_2017-2019_Wade_Hungerford.csv") %>% 
      # Create a timestamp column
      mutate(timestamp = ymd_h(paste(Year, Month, Day, Hr, sep = "-"), tz = "Etc/GMT+4")) %>% 
      rename(Wade = "Wade_PET (mm/hr)", Hungerford = "Hungerford_PET (mm/hr)") %>% 
      pivot_longer(cols = c(Wade, Hungerford), names_to = "site", values_to = "PET_mmHR") %>% 
      select(-c(Year, Month, Day, Hr)) 


# DISCHARGE ----
  # Hungerford
  q_hford_17 <- read_csv("../../Streams/02_site_data/discharge/hungerford_2017_best_q.csv", col_types = cols()) %>% mutate(site = "Hungerford") %>% rename(q_cms = HF_best_q, timestamp = r_timestamp) %>% 
    mutate(timestamp = mdy_hm(timestamp, tz = "Etc/GMT+4"))
  q_hford_18 <- read_csv("../../Streams/02_site_data/discharge/hungerford_2018_best_q.csv", col_types = cols()) %>% mutate(site = "Hungerford") %>% rename(q_cms = HF_best_q, timestamp = r_timestamp) %>% 
    mutate(timestamp = mdy_hm(timestamp, tz = "Etc/GMT+4"))
  q_hford_19 <- read_csv("../../Streams/02_site_data/discharge/hungerford_2019_best_q.csv", col_types = cols()) %>% mutate(site = "Hungerford") %>% rename(q_cms = HF_best_q, timestamp = r_timestamp) %>% 
    select(-c(X1, hobo_stage, offset, hobo_stage_int, corr_stage)) %>% mutate(timestamp = ymd_hms(timestamp, tz = "Etc/GMT+4"))
  # Wade
  q_wade_17 <- read_csv("../../Streams/02_site_data/discharge/wade_2017_best_q.csv", col_types = cols()) %>% mutate(site = "Wade") %>% rename(q_cms = best_q, timestamp = r_timestamp) %>% 
    mutate(timestamp = mdy_hm(timestamp, tz = "Etc/GMT+4"))
  q_wade_18 <- read_csv("../../Streams/02_site_data/discharge/wade_2018_best_q.csv", col_types = cols()) %>% mutate(site = "Wade") %>% rename(q_cms = best_q, timestamp = r_timestamp) %>% 
    mutate(timestamp = mdy_hm(timestamp, tz = "Etc/GMT+4"))
  q_wade_19 <- read_csv("../../Streams/02_site_data/discharge/wade_2019_best_q.csv", col_types = cols()) %>% mutate(site = "Wade") %>% rename(q_cms = best_q, timestamp = r_timestamp) %>% 
    select(site, timestamp, q_cms) %>% mutate(timestamp = ymd_hms(timestamp, tz = "Etc/GMT+4"))
  
  # Bind Q dfs into one
  q_all <- bind_rows(q_hford_17, q_hford_18, q_hford_19, q_wade_17, q_wade_18, q_wade_19) %>% 
    select(site, timestamp, q_cms) %>% 
    # Average q values with duplicate timestamps
    group_by(site, timestamp) %>% 
    dplyr::summarize(q_cms = mean(q_cms, na.rm = T)) %>% 
    ungroup() %>% 
    filter(timestamp >= ymd_hms("2017-07-01 00:00:00", tz = "Etc/GMT+4"))
  rm(q_hford_17, q_hford_18, q_hford_19, q_wade_17, q_wade_18, q_wade_19)
  
    # test <- q_all %>% filter(site == "Wade" & timestamp > ymd_hms("2017-08-03 00:00:00", tz = "Etc/GMT+4"))  
  
  # Create a continuous 15-min time series 
  # First, create a continuous 15 min time series from first Q measurement each year to last Q measurement each year for each site
    # Create time sequence to interpolate
    interp_time_q <- q_all %>% 
      mutate(year = year(timestamp)) %>% 
      select(site, year, timestamp)
  
    # Create the 15-min sequence
    interp_time_q <- setDT(interp_time_q)[, list(timestamp = seq(min(timestamp), max(timestamp), by = "15 min")), by = c("site", "year")]
    # Round the 15-min sequence to nearest 15-min
    interp_time_q <- interp_time_q %>% mutate(timestamp = ceiling_date(timestamp, unit = "15 min"))

    # Join q with the 15-min time series
    q_all <- full_join(interp_time_q, q_all, by = c("site", "timestamp")) %>% 
      # Interpolate NAs using linear interpolation
      arrange(site, timestamp) %>%
      group_by(site, year(timestamp)) %>%
      mutate_at(vars(c(q_cms)),
                list(~ na.approx(., x = timestamp, xout = timestamp, maxgap = 8, na.rm = FALSE))) %>%
      ungroup() %>%
      # Drop extra columns
      select(-c("year(timestamp)", year)) %>% 
      # Filter the dataset to every 15 min
      filter(minute(timestamp) %in% c(0, 15, 30, 45))
   
    
# SOIL SENSOR DATA ----
  # Downloaded from Aquarius on 10/19/2020
  # Should be a continuous 15-min time series (meaning I don't have to join it to a continuous 15-min time series to make one)
  soils <- read_csv("../../Soils/4_data/soilsData_selectSites_2017-2019_forFluxRegimes.csv", col_types = cols(Redox = col_double())) %>% 
    # The data are downloaded in EST (GMT-5)
    mutate(timestamp = ymd_hms(timestamp, tz = "Etc/GMT+5")) %>% 
    # Convert the times to EDT (GMT-4), because this is what the stream sensor data are in
    mutate(timestamp = with_tz(timestamp, tzone = "Etc/GMT+4"))
  # Because HD Pit 1 data post 11/20/19 is missing the previous CSV does not include data >11/20/19 15:30 for ANY pits! Why?! Who knows
  # Here are data from all other pits starting on 11/20/19 15:45
  soils2 <- read_csv("../../Soils/4_data/soilsData_selectSites_2017-2019_forFluxRegimes_2.csv", col_types = cols(Redox = col_double())) %>% 
    # The data are downloaded in EST (GMT-5)
    mutate(timestamp = ymd_hms(timestamp, tz = "Etc/GMT+5")) %>% 
    # Convert the times to EDT (GMT-4), because this is what the stream sensor data are in
    mutate(timestamp = with_tz(timestamp, tzone = "Etc/GMT+4"))
  # Bind these together
  soils_all <- bind_rows(soils, soils2) %>% 
    arrange(transect, pit, depth, timestamp) %>%
    mutate(site = ifelse(transect %in% c("HD", "HW"), "Hungerford", "Wade")) %>% 
    # Remove duplicate values (for some reason they occur on 10/22/19 at all transects) %>% 
    distinct()
  rm(soils, soils2)
  
  # Trim soils data to first and last event of each year at each site
  # What are the start and end times for each site and year?
  events_all %>% 
    group_by(site, year(event_start)) %>% 
    summarize(start = first(rain_start),
              stop = last(event_end))
  
  soils_all <- soils_all %>% 
    filter(((transect == "HD" | transect == "HW") & (timestamp >= ymd_hms("2017-07-01 00:00:00", tz = "Etc/GMT+4") & timestamp <= ymd_hms("2017-12-03 23:59:59", tz = "Etc/GMT+4"))) | 
             ((transect == "HD" | transect == "HW") & (timestamp >= ymd_hms("2018-02-15 00:00:00", tz = "Etc/GMT+4") & timestamp <= ymd_hms("2018-11-29 23:59:59", tz = "Etc/GMT+4"))) |
             ((transect == "HD" | transect == "HW") & (timestamp >= ymd_hms("2019-02-15 00:00:00", tz = "Etc/GMT+4") & timestamp <= ymd_hms("2019-11-02 23:59:59", tz = "Etc/GMT+4"))) |
             ((transect == "WD" | transect == "WW") & (timestamp >= ymd_hms("2017-07-01 00:00:00", tz = "Etc/GMT+4") & timestamp <= ymd_hms("2017-12-07 23:59:59", tz = "Etc/GMT+4"))) |
             ((transect == "WD" | transect == "WW") & (timestamp >= ymd_hms("2018-03-01 00:00:00", tz = "Etc/GMT+4") & timestamp <= ymd_hms("2018-11-11 23:59:59", tz = "Etc/GMT+4"))) |
             ((transect == "WD" | transect == "WW") & (timestamp >= ymd_hms("2019-03-01 00:00:00", tz = "Etc/GMT+4") & timestamp <= ymd_hms("2019-11-02 23:59:59", tz = "Etc/GMT+4")))
           )
  
  # test2 <- test %>% filter(transect == "WW")
  # test3 <- test %>% group_by(transect, pit, year(timestamp)) %>% 
  #   summarize(first = first(timestamp),
  #             last = last(timestamp))
  

# STREAM DATA: s::can nutrients & YSI stream temp and turbidity ----
  # s::can predicted NO3 time series for 2018-2019
  # All time series have been checked and corrected for method detection limit
    # Hungerford
    no3_hf_17 <- read_csv("../../Streams/04_s-can_predicted_chemistry/04_s-can_predicted_timeseries/hford_no3_2017/current_model/HF_no3_predicted_time_series_3cmpt.csv", col_types = cols())
    no3_hf_18 <- read_csv("../../Streams/04_s-can_predicted_chemistry/04_s-can_predicted_timeseries/hford_no3_2018/current_model/HF_no3_predicted_time_series_3cmpt.csv", col_types = cols())
    no3_hf_19 <- read_csv("../../Streams/04_s-can_predicted_chemistry/04_s-can_predicted_timeseries/hford_no3_2019/current_model/HF_no3_predicted_time_series_4cmpt.csv", col_types = cols())
      # Bind these & add site name
      no3_hf <- bind_rows(no3_hf_17, no3_hf_18, no3_hf_19) %>% mutate(site = "Hungerford") %>% rename(NO3_mgNL = predicted_param) %>% select(-parameter)
      rm(no3_hf_17, no3_hf_18, no3_hf_19)

    # Wade
    no3_wade_17 <- read_csv("../../Streams/04_s-can_predicted_chemistry/04_s-can_predicted_timeseries/wade_no3_2017/current model/WB_no3_predicted_time_series_5cmpt.csv", col_types = cols())
    no3_wade_18 <- read_csv("../../Streams/04_s-can_predicted_chemistry/04_s-can_predicted_timeseries/wade_no3_2018/current_model/WB_no3_predicted_time_series_5cmpt.csv", col_types = cols())
    no3_wade_19 <- read_csv("../../Streams/04_s-can_predicted_chemistry/04_s-can_predicted_timeseries/wade_no3_2019/current_model/WB_no3_predicted_time_series_5cmpt.csv", col_types = cols())
      # Bind these & add site name
      no3_wade <- bind_rows(no3_wade_17, no3_wade_18, no3_wade_19) %>% mutate(site = "Wade") %>% rename(NO3_mgNL = predicted_param) %>% select(-parameter)
      rm(no3_wade_17, no3_wade_18, no3_wade_19)
      
    # Bind all site NO3 data into one df
    no3_all <- bind_rows(no3_hf, no3_wade) %>%
      rename(timestamp = r_timestamp) %>% 
      # Convert & round timestamp
      mutate(timestamp = round_date(ymd_hms(timestamp, tz = "Etc/GMT+4"), "minute")) %>% 
      filter(timestamp >= ymd_hms("2017-07-01 00:00:00", tz = "Etc/GMT+4"))
    rm(no3_hf, no3_wade)

  # s::can predicted phosphorus time series for 2018-2019
  # These have been corrected for method detection limits
    # Hungerford
    p_hf_17to19 <- read_csv("../../Streams/04_s-can_predicted_chemistry/04_s-can_predicted_timeseries/hford_p_2014-early2019/HF_phos_predicted_time_series_MDLcorrected.csv", col_types = cols()) %>% 
      mutate(site = "Hungerford") %>% rename(TP_mgPL = TP_mgL, TDP_mgPL = TDP_mgL, SRP_mgPL = SRP_mgL, PP_mgPL = PP_mgL) %>% 
      filter(year(r_timestamp) >= 2017)
    p_hf_late19 <- read_csv("../../Streams/04_s-can_predicted_chemistry/04_s-can_predicted_timeseries/hford_p_late2019/HF_phos_predicted_time_series_MDLcorrected.csv", col_types = cols()) %>% 
      mutate(site = "Hungerford") %>% rename(TP_mgPL = TP_mgL, TDP_mgPL = TDP_mgL, SRP_mgPL = SRP_mgL, PP_mgPL = PP_mgL)
      # Bind these
      p_hf <- bind_rows(p_hf_17to19, p_hf_late19)
      rm(p_hf_17to19, p_hf_late19)
    # Wade
    p_wade_17to19 <- read_csv("../../Streams/04_s-can_predicted_chemistry/04_s-can_predicted_timeseries/wade_p_2014-early2019/wade_phos_predicted_time_series_MDLcorrected.csv", col_types = cols()) %>% 
      mutate(site = "Wade") %>% rename(TP_mgPL = TP_mgL, TDP_mgPL = TDP_mgL, SRP_mgPL = SRP_mgL, PP_mgPL = PP_mgL) %>% 
      filter(year(r_timestamp) >= 2017)
    # Late 2019 data do not include PP; I was having an issue with the script
    p_wade_late19 <- read_csv("../../Streams/04_s-can_predicted_chemistry/04_s-can_predicted_timeseries/wade_p_late2019/wade_phos_predicted_time_series_MDLcorrected.csv", col_types = cols()) %>% 
      mutate(site = "Wade") %>% rename(TP_mgPL = TP_mgL, TDP_mgPL = TDP_mgL, SRP_mgPL = SRP_mgL)
      # Bind these
      p_wade <- bind_rows(p_wade_17to19, p_wade_late19)
      rm(p_wade_17to19, p_wade_late19)    
    
    # Bind these P dfs into one
    p_all <- bind_rows(p_hf, p_wade) %>% 
      rename(timestamp = r_timestamp) %>% 
      # Convert & round timestamp
      mutate(timestamp = round_date(ymd_hms(timestamp, tz = "Etc/GMT+4"), "minute")) %>% 
      filter(timestamp >= ymd_hms("2017-07-01 00:00:00", tz = "Etc/GMT+4"))
    rm(p_hf, p_wade)
   
  # YSI stream temperature and turbidity   
  # Data downloaded from Aquarius on 2020-10-19
  ysi <- read_csv("../../Streams/02_site_data/YSI_allParams_allSites_2014-2019_downloaded_2020-10-19.csv", col_types = cols(DO_mgL = col_double())) %>% 
    # Shorten site names to match all other dfs
    mutate(site = ifelse(site == "Hungerford Brook", "Hungerford", 
                         ifelse(site == "Potash Brook", "Potash", "Wade"))) %>% 
    # Set time zone & round time to nearest minute
    mutate(timestamp = round_date(ymd_hms(timestamp, tz = "Etc/GMT+4"), "minute")) %>% 
    # Make vars lowercase
    rename(fDOM = "fDOM_Corrected", spCond = "SpecCond", temp = "Temp", turb = "Turbidity") %>% 
    # Deal with negative turbidity values
    mutate(turb = ifelse(turb < 0, 0, turb)) %>% 
    # We only need to keep temp and turb for now
    select(site, timestamp, temp, turb)
    
  # Then join s::can predicted data & YSI data to interp_time
  stream <- full_join(interp_time_q, no3_all, by = c("site", "timestamp")) %>% 
      full_join(p_all, by = c("site", "timestamp")) %>%
      full_join(ysi, by = c("site", "timestamp")) %>%
    # Interpolate NAs using linear interpolation
    arrange(site, timestamp) %>%
    group_by(site, year(timestamp)) %>%
    mutate_at(vars(c(NO3_mgNL:turb)),
              list(~ na.approx(., x = timestamp, xout = timestamp, maxgap = 8, na.rm = FALSE))) %>%
    ungroup() %>%
    # Drop extra columns
    select(-"year(timestamp)") %>% 
    select(-year) %>% 
    # Filter the dataset to every 15 min
    filter(minute(timestamp) %in% c(0, 15, 30, 45))
  rm(no3_all, p_all, ysi)
  
  # Join q_all to stream
  stream <- full_join(stream, q_all, by = c("site", "timestamp")) %>% 
    filter(timestamp >= ymd_hms("2017-07-01 00:00:00", tz = "Etc/GMT+4"))
  
  # nodat <- stream %>% filter(site == "Wade" & timestamp >= ymd_hms("2018-06-14 00:00:00", tz = "Etc/GMT+4"))

    
# GW LEVEL DATA ----
  # Data are water table depth below ground surface
  # Hungerford
  gw_hf <- read_csv("../../Groundwater/02_site_data/water level data/Hungerford_GW_levelData_2020-09-10.csv",  col_types = cols(`hw str` = col_double())) %>% 
    mutate(site = "Hungerford") %>%
    rename(timestamp = "datetime", hwSTR = "hw str") %>% 
    # Replace 'hw' prefix w/ 'well'
    rename_at(vars(starts_with("hw")), 
              .funs = list(~ str_replace(., 'hw', 'well')))
  # Wade
  gw_wd <- read_csv("../../Groundwater/02_site_data/water level data/Wade_GW_levelData_2020-09-10.csv",  col_types = cols(`ww str` = col_double(), ww6 = col_double())) %>% 
    mutate(site = "Wade") %>%
    rename(timestamp = "datetime", wwSTR = "ww str") %>% 
    # Replace 'ww' prefix w/ 'well'
    rename_at(vars(starts_with("ww")), 
              .funs = list(~ str_replace(., 'ww', 'well')))
  # Combine into one
  gw_all <- bind_rows(gw_hf, gw_wd) %>% 
    mutate(timestamp = ymd_hms(timestamp, tz = "Etc/GMT+4")) %>% 
    select(timestamp, site, well1, well2, well3, well4, well4a, well4b, everything()) %>% 
    filter(!is.na(timestamp))
  rm(gw_hf, gw_wd)
  
  # Create a continuous 15-min time series
  # Join q with the 15-min time series
  gw_all <- full_join(interp_time_q, gw_all, by = c("site", "timestamp")) %>% 
    # Interpolate NAs using linear interpolation
    arrange(site, timestamp) %>%
    group_by(site, year(timestamp)) %>%
    mutate_at(vars(c(well1:wellSTR)),
              list(~ na.approx(., x = timestamp, xout = timestamp, maxgap = 8, na.rm = FALSE))) %>%
    ungroup() %>%
    # Drop extra columns
    select(-c("year(timestamp)", year)) %>% 
    # Filter the dataset to every 15 min
    filter(minute(timestamp) %in% c(0, 15, 30, 45)) %>% 
    # Remove initial NA values where there are no GW level data
    # I also removed a bunch of initial rows, because the levels seemed dubious 
    filter(timestamp >= ymd_hms("2018-03-28 15:00:00", tz = "Etc/GMT+4"))
  
# Remove unnecessary objects
  rm(interp_time, interp_time_q)

# ----    

  
# Explore missing data ----
  # # Stream sensor data
  # nodat <- stream %>% filter(site == "Wade" & timestamp >= ymd_hms("2019-09-23 12:00:00", tz = "Etc/GMT+4"))
  # nodat <- no3_hf_18 %>% filter(r_timestamp >= ymd_hms("2018-06-20 12:00:00"))
  # 
  # stream %>% 
  #   filter(site == "Hungerford" & (timestamp >= ymd_hms("2018-04-25 10:00:00", tz = "Etc/GMT+4") & timestamp <= ymd_hms("2018-04-27 05:00:00", tz = "Etc/GMT+4"))) %>% 
  #   pivot_longer(cols = c(q_cms, NO3_mgNL, SRP_mgPL), names_to = "var", values_to = "value") %>% 
  #   ggplot(aes(x = timestamp, y = value)) +
  #     facet_wrap(~var, scales = "free_y", ncol = 1) +
  #     geom_point()
  #            
  # stream %>% 
  #   filter(site == "Hungerford" & (timestamp >= ymd_hms("2018-05-15 00:00:00", tz = "Etc/GMT+4") & timestamp <= ymd_hms("2018-05-16 23:00:00", tz = "Etc/GMT+4"))) %>% 
  #   pivot_longer(cols = c(q_cms, NO3_mgNL, SRP_mgPL), names_to = "var", values_to = "value") %>% 
  #   ggplot(aes(x = timestamp, y = value)) +
  #     facet_wrap(~var, scales = "free_y", ncol = 1) +
  #     geom_point()  
  # 
  # stream %>% 
  #   filter(site == "Wade" & (timestamp >= ymd_hms("2019-09-23 12:00:00", tz = "Etc/GMT+4") & timestamp <= ymd_hms("2019-09-25 15:00:00", tz = "Etc/GMT+4"))) %>% 
  #   pivot_longer(cols = c(q_cms, NO3_mgNL, SRP_mgPL), names_to = "var", values_to = "value") %>% 
  #   ggplot(aes(x = timestamp, y = value)) +
  #     facet_wrap(~var, scales = "free_y", ncol = 1) +
  #     geom_point()
  # 
  # # Soil sensor data
  # nodat <- soils_all %>% filter(transect == "WW" & timestamp >= ymd_hms("2018-05-15 10:00:00", tz = "Etc/GMT+4")) %>% 
  #   filter(depth == 15 & pit %in% c(1, 6)) %>% 
  #   pivot_wider(names_from = pit, values_from = c(DO:VWC))
  # 
  # soils_all %>% 
  #   filter(transect == "WW" & pit == 6 & depth == 15 & (timestamp >= ymd_hms("2017-09-01 00:00:00", tz = "Etc/GMT+4") & timestamp <= ymd_hms("2017-10-25 20:00:00", tz = "Etc/GMT+4"))) %>% 
  #   ggplot(aes(x = timestamp, y = Redox)) +
  #   geom_line()
  
# ----  
  
  
  
# CALCULATIONS ----
# Calculate time since last event (interflood duration) ----
  # And round event_start to nearest 15-min mark (some events start on 10-min mark)
  events_all <- events_all %>% 
    ungroup() %>% 
    group_by(site, year(event_start)) %>% 
    mutate(event_start = round_date(ymd_hms(event_start, tz = "Etc/GMT+4"), "15 min")) %>% 
    mutate(time_sinceLastEvent = difftime(event_start, lag(event_end), units = "days")) %>% 
    ungroup() %>% 
    select(-("year(event_start)"))

  
# Calculate rain metrics ----
  # Total rain amount for each event (rain start to event end)
    # First add event data (rain start to event end) to MET data using an overlap join
      # What to do about 
      # Remove events that don't have a rain start
      events_withRain <- events_all %>% filter(!is.na(rain_start))
      # Convert dfs to data.tables
      # In addition, the function foverlaps requires that the keys in y (events_withRain) have matches in x (met_all)
      # so we have to create a 2nd time column (just a copy of timestamp) to match rain_start & event_end in events_withRain
      met_all[, Time2 := timestamp]
      setDT(events_withRain)
      # # Need to set keys for y (events_withRain) before the overlap join
      setkey(events_withRain, site, rain_start, event_end)
      # Here we do the overlap join by site and start and end dates
      # the [ ] at the end just removes the extra columns added from events_withRain
      met_all2 <- foverlaps(x = met_all, y = events_withRain, by.x = c("site", "timestamp", "Time2"), nomatch = NA)[, "Time2" := NULL]
      met_all[, "Time2" := NULL]
    
    rain_event_total <- met_all2 %>% 
      filter(!is.na(event_start)) %>% 
      group_by(site, event_start) %>% 
      summarize(rain_event_total_mm = sum(precip_mm, na.rm = T))
  
  # Total duration of rain (rain start to event end)
    rain_event_duration <- met_all2 %>% 
      filter(!is.na(event_start)) %>% 
      # Only look at observations w/ rain > 0
      filter(!is.na(precip_mm) & precip_mm > 0) %>% 
      group_by(site, event_start) %>%
      mutate(rain_event_hrs = as.numeric(difftime(max(timestamp), min(timestamp), units = "hours"))) %>% 
      select(site, event_start, rain_event_hrs) %>% 
      distinct()
    
  # Join those 2 to see where I'm missing values
    # It's due to the events that have no rain start associated with them
    rain_mets <- full_join(rain_event_total, rain_event_duration, by = c("site", "event_start"))
      
  # Intensity of rain event
    # Max
    rain_event_intensity_max <- met_all2 %>% 
      filter(!is.na(event_start)) %>% 
      group_by(site, event_start) %>%
      summarize(rain_int_mmPERmin_max = max(precip_mm)/5)
    
    # Mean
    # Make a df with rain_start and rain_end times from the data
    # TO DO: look into where rain_start here differs from the rain_start we recorded during event delineations
    rain_start_end <- met_all2 %>% 
      filter(!is.na(event_start)) %>% 
      # Only look at observations w/ rain > 0
      filter(!is.na(precip_mm) & precip_mm >0) %>% 
      group_by(site, event_start) %>%
      filter(row_number() == 1 | row_number() == n()) %>% 
      select(site, event_start, timestamp) %>% 
      mutate(id = row_number()) %>% 
      pivot_wider(names_from = id,
                  values_from = timestamp) %>% 
      rename(rain_start = "1", rain_end = "2") %>% 
      filter(!is.na(rain_end))
    
    met_all3 <- met_all2 %>% select(-c(eventid:time_sinceLastEvent))
    # Convert dfs to data.tables
    # In addition, the function foverlaps requires that the keys in y (rain_start_end) have matches in x (met_all)
    # so we have to create a 2nd time column (just a copy of timestamp) to match rain_start & event_end in rain_start_end
    met_all3[, Time2 := timestamp]
    setDT(rain_start_end)
    # # Need to set keys for y (rain_start_end) before the overlap join
    setkey(rain_start_end, site, rain_start, rain_end)
    # Here we do the overlap join by site and start and end dates
    # the [ ] at the end just removes the extra columns added from rain_start_end
    met_all3 <- foverlaps(x = met_all3, y = rain_start_end, by.x = c("site", "timestamp", "Time2"), nomatch = NA)[, "Time2" := NULL]
    
    rain_event_intensity_mean <- met_all3 %>% 
      filter(!is.na(event_start)) %>% 
      group_by(site, event_start) %>% 
      summarize(rain_int_mmPERmin_mean = mean(precip_mm)/5)
    rm(met_all2, met_all3)
      
    
  # Total rain for 1-4days, 7-days, 14-days, and 30-days prior to event (prior to rain start)
  # Only considering events with a rain_start, so we'll use events_withRain df from above
    # Join rain_start to the met_all data
    events_withRain[, timestamp := rain_start]
    setkey(met_all, site, timestamp)
    setkey(events_withRain, site, timestamp)
    comb_met <- events_withRain[met_all, roll = 1] %>% 
      mutate(year = year(timestamp)) %>% 
      arrange(site, timestamp)
    events_withRain[, "timestamp" := NULL]    
      
    # test <- comb_met %>% filter(site == "Hungerford" & timestamp > ymd_hms("2017-10-04 20:00:00", tz = "Etc/GMT+4"))      
    
    # Calculate cumulative pre-event rain totals
    # 288 rows 5 min/row = 1 day, so 288 rows/day
    rain_preEvent_totals <- setDT(comb_met)[, c("rain_1d", "rain_2d", "rain_3d", "rain_4d", "rain_7d", "rain_14d", "rain_30d") 
                      := list(frollsum(lag(precip_mm, n = 1), n = 288*1, align = "right", fill = NA, na.rm = T),
                              frollsum(lag(precip_mm, n = 1), n = 288*2, align = "right", fill = NA, na.rm = T),
                              frollsum(lag(precip_mm, n = 1), n = 288*3, align = "right", fill = NA, na.rm = T),
                              frollsum(lag(precip_mm, n = 1), n = 288*4, align = "right", fill = NA, na.rm = T),
                              frollsum(lag(precip_mm, n = 1), n = 288*7, align = "right", fill = NA, na.rm = T),
                              frollsum(lag(precip_mm, n = 1), n = 288*14, align = "right", fill = NA, na.rm = T),
                              frollsum(lag(precip_mm, n = 1), n = 288*30, align = "right", fill = NA, na.rm = T)),
                              by = site][!is.na(rain_start)][, c("site", "event_start", "rain_1d", "rain_2d", "rain_3d", "rain_4d", "rain_7d", "rain_14d", "rain_30d")]
    
    # Slower alternative method
    # rain_preEvent_totals <- comb_met %>%
    #   group_by(site) %>% 
    #   # 288 rows 5 min/row = 1 day, so 288 rows/day
    #   mutate(rain_preEvent_1d = ifelse(!is.na(rain_start), rollapply(data = lag(precip_mm, n = 1),
    #                                                                   width = 288*1,
    #                                                                   FUN = sum,
    #                                                                   partial = FALSE,
    #                                                                   align = "right",
    #                                                                   fill = NA,
    #                                                                   na.rm = T), NA),
    #          rain_preEvent_7d = ifelse(!is.na(rain_start), rollapply(data = lag(precip_mm, n = 1),
    #                                                                   width = 288*7,
    #                                                                   FUN = sum,
    #                                                                   partial = FALSE,
    #                                                                   align = "right",
    #                                                                   fill = NA,
    #                                                                   na.rm = T), NA),
    #          rain_preEvent_14d = ifelse(!is.na(rain_start), rollapply(data = lag(precip_mm, n = 1),
    #                                                                   width = 288*14,
    #                                                                   FUN = sum,
    #                                                                   partial = FALSE,
    #                                                                   align = "right",
    #                                                                   fill = NA,
    #                                                                   na.rm = T), NA), 
    #          rain_preEvent_30d = ifelse(!is.na(rain_start), rollapply(data = lag(precip_mm, n = 1),
    #                                                                   width = 288*30,
    #                                                                   FUN = sum,
    #                                                                   partial = FALSE,
    #                                                                   align = "right",
    #                                                                   fill = NA,
    #                                                                   na.rm = T), NA))
    
    # Tidy up
    # rain_preEvent_totals <- rain_preEvent_totals %>% 
    #   filter(!is.na(rain_start)) %>% 
    #   select(site, event_start, rain_preEvent_1d:rain_preEvent_30d)
    
  # Calculate Antecedent Precipitation Index (API) over 4 days (used in Fovet et al 2018)
    # From the website: High values of API mean the catchment is wet so any rain is likely to run off.Low values mean 
      # there hasn’t been much rain lately, the catchment is dry so rain is likely to soak into soil and wet up vegetation 
      # and not make it to a stream.  API has been related to initial loss i.e. the amount of rainfall that is ‘lost’ before 
      # runoff starts (Cordery, 1970a; 1970b).  An example for the Bobo River in northern NSW is shown in Figure 1.  As the 
      # API goes up, the catchment is wetter so the initial loss decreases and more rain will run off.    
    # Set the decay factor k for the API
      # Without rain, the catchment wetness (as measured by API) declines each day by the factor k.  Any rain tops the API up again.
      # The decay parameter k must be less than one and is usually between 0.85 and 0.98 (Lindsay et al., 1975).  Cordery (1970a) recommended 
      # an average value of 0.92 for NSW catchments (in Australia) and found that k varied from 0.98 in winter to 0.86 in summer.  A constant, year round, 
      # value of 0.95 was recommended by Hill et al. (2014).
    
      # Could regress log(soil water content) against time to estimate k maybe?
      k = 0.95
      
    # First, calculate the amount of rain received each day (24 hrs) prior to the event for 4 days
    # 288 rows 5 min/row = 1 day, so 288 rows/day
    rain_API <- setDT(comb_met)[, c("rain_day1", "rain_day2", "rain_day3", "rain_day4") 
                      := list(frollsum(lag(precip_mm, n = 1+288*0), n = 288, align = "right", fill = NA, na.rm = T),
                              frollsum(lag(precip_mm, n = 1+288*1), n = 288, align = "right", fill = NA, na.rm = T),
                              frollsum(lag(precip_mm, n = 1+288*2), n = 288, align = "right", fill = NA, na.rm = T),
                              frollsum(lag(precip_mm, n = 1+288*3), n = 288, align = "right", fill = NA, na.rm = T)),
                              by = site][!is.na(rain_start)][, c("site", "event_start", "rain_day1", "rain_day2", "rain_day3", "rain_day4")]
    
  # Let's join all the rain metrics into one df AND calculate the final 4-day API
    rain_mets <- full_join(rain_event_total, rain_event_duration, by = c("site", "event_start")) %>% 
      full_join(rain_event_intensity_max, by = c("site", "event_start")) %>% 
      full_join(rain_event_intensity_mean, by = c("site", "event_start")) %>%
      full_join(rain_preEvent_totals, by = c("site", "event_start")) %>%    
      full_join(rain_API, by = c("site", "event_start")) %>% 
      # 4-day API calc
      mutate(API_4d = rain_event_total_mm + k^1*rain_day1 + k^2*rain_day2 + k^3*rain_day3 + k^4*rain_day4) %>% 
      select(-c(rain_day1, rain_day2, rain_day3, rain_day4))
    
    rm(rain_event_total, rain_event_duration, rain_event_intensity_max, rain_event_intensity_mean, rain_preEvent_totals, rain_API, rain_start_end)
    
# Calculate MET metrics ----
 # 1-day and 3-day means  
  # 288 rows 5 min/row = 1 day, so 288 rows/day
  airT_means <- setDT(comb_met)[, c("airT_1d", "airT_4d") 
                    := list(frollmean(lag(temp_C, n = 1), n = 288*1, align = "right", fill = NA, na.rm = T),
                            frollmean(lag(temp_C, n = 1), n = 288*4, align = "right", fill = NA, na.rm = T)),
                            by = site][!is.na(event_start)][, c("site", "event_start", "airT_1d", "airT_3d")]
  solarRad_means <- setDT(comb_met)[, c("solarRad_1d", "solarRad_4d") 
                    := list(frollmean(lag(solarRad_wm2, n = 1), n = 288*1, align = "right", fill = NA, na.rm = T),
                            frollmean(lag(solarRad_wm2, n = 1), n = 288*4, align = "right", fill = NA, na.rm = T)),
                            by = site][!is.na(event_start)][, c("site", "event_start", "solarRad_1d", "solarRad_3d")]
  dewPoint_means <- setDT(comb_met)[, c("dewPoint_1d", "dewPoint_4d") 
                    := list(frollmean(lag(dewPoint, n = 1), n = 288*1, align = "right", fill = NA, na.rm = T),
                            frollmean(lag(dewPoint, n = 1), n = 288*4, align = "right", fill = NA, na.rm = T)),
                            by = site][!is.na(event_start)][, c("site", "event_start", "dewPoint_1d", "dewPoint_3d")]
  # Join these
  met_mets <- full_join(airT_means, solarRad_means, by = c("site", "event_start")) %>% 
    full_join(dewPoint_means, by = c("site", "event_start"))
  
  rm(comb_met, met_all, airT_means, solarRad_means, dewPoint_means)

  
# Calculate discharge metrics ----
  # Peak discharge & delta Q (change in Q from beginning to to peak Q)
    # First add event data (event start to event end) to Q data using an overlap join
      # Convert dfs to data.tables
      # In addition, the function foverlaps requires that the keys in y (events_all) have matches in x (q_all)
      # so we have to create a 2nd time column (just a copy of timestamp) to match rain_start & event_end in events_all
      setDT(q_all)[, Time2 := timestamp]
      setDT(events_all)
      # # Need to set keys for y (events_all) before the overlap join
      setkey(events_all, site, event_start, event_end)
      # Here we do the overlap join by site and start and end dates
      # the [ ] at the end just removes the extra columns added from events_all
      q_all2 <- foverlaps(x = q_all, y = events_all, by.x = c("site", "timestamp", "Time2"), nomatch = NA)[, "Time2" := NULL]
      q_all[, "Time2" := NULL]
      
    q_event_max_delta <- q_all2 %>% 
      filter(!is.na(event_start)) %>% 
      group_by(site, event_start) %>% 
      summarize(q_event_max = max(q_cms, na.rm = T),
                q_event_delta = max(q_cms, na.rm = T) - first(q_cms))
    
    # test <- q_all %>% filter(event_start == ymd_hms("2017-08-05 09:15:00", tz = "Etc/GMT+4"))
    # test <- q_all %>% filter(site == "Wade" & timestamp > ymd_hms("2017-08-03 00:00:00", tz = "Etc/GMT+4"))
    
  # Mean rate of change in discharge during flow rise
    q_event_dQRate <- q_all2 %>% 
      group_by(site, event_start) %>% 
      # Slice data from start of storm to max q
      slice(1:which.max(q_cms)) %>% 
      summarize(risingLimb_hrs = as.numeric(difftime(max(timestamp), min(timestamp), units = "hours"))) %>% 
      full_join(q_event_max_delta, by = c("site", "event_start")) %>% 
      mutate(q_event_dQRate_cmsPerHr = q_event_delta/risingLimb_hrs) %>% 
      select(-c(risingLimb_hrs, q_event_max, q_event_delta))
    rm(q_all2)
      
  # Mean antecedent discharge - 1 & 4 days
    # Join event_start to the q_all data
      events_all[, timestamp := event_start]
      setkey(q_all, site, timestamp)
      setkey(events_all, site, timestamp)
      comb_q <- events_all[q_all, roll = 1] %>% 
        mutate(year = year(timestamp)) %>% 
        arrange(site, timestamp)
      events_all[, "timestamp" := NULL]  
      
    # 1440 min/day / 15-min/row = 96 rows/day  
    q_preEvent_means <- setDT(comb_q)[, c("q_1d", "q_4d") 
                      := list(frollmean(lag(q_cms, n = 1), n = 96*1, align = "right", fill = NA, na.rm = T),
                              frollmean(lag(q_cms, n = 1), n = 96*4, align = "right", fill = NA, na.rm = T)),
                              by = site][!is.na(event_start)][, c("site", "event_start", "q_1d", "q_4d")] 
    rm(comb_q, q_all)
    
    
    
# Calculate soil variable pre-event means ---- 
  # Join event_start to the soils_all data
    setDT(soils_all)
    events_all[, timestamp := event_start]
    setkey(soils_all, site, timestamp)
    setkey(events_all, site, timestamp)
    comb <- events_all[soils_all, roll = 1] %>% 
      mutate(year = year(timestamp)) %>% 
      arrange(transect, pit, depth, timestamp)
    events_all[, "timestamp" := NULL]
  
  # Check to make sure that each event aligned 
  test <- comb %>% mutate(timediff = difftime(event_start, timestamp, units = "hours"))
  # This summary should have all 0s
  summary(as.numeric(test$timediff))
  rm(test)
  
  # Replace NA values for soil (e.g., when logging every 30-min instead of 15-min in winter) with last observation
    # data.table solution to do na.locf (last observation carried forward); maxgap = 8 rows at 15 min/row = 2 hours
    # Note that I sorted the columns above
    cols = c("DO", "Redox", "SoilTemp", "VWC")
    setDT(comb)
    comb[, (cols) := na.locf(.SD, maxgap = 8, na.rm = F), by = list(year, transect, pit, depth), .SDcols = cols]
  
  # Calculate pre-event means (up to 5 days prior, but less if time between events is less)
   # https://danieljhocking.wordpress.com/2014/12/03/lags-and-moving-means-in-dplyr/
    # Solution for a single variable/column
    # test <- comb %>%
    #   arrange(transect, pit, depth, timestamp) %>%
    #   group_by(year, transect, pit, depth) %>%
    #   # Here I calculate the width of the window used for the variable mean
    #   # If the time b/w events is >= 5, then we calculate the 5-day mean (5 * 96 15-min rows = 480)
    #   # If the time b/w events is >= 1 & < 5, then we calculate the X-day mean (X * 96 15-min rows; e.g., for 2-days = 2*96 = 192)
    #   # If the time b/w events is < 1, then we calculate the 1-hr mean (4 rows)
    #   # If the time b/w events is NA (i.e., the observed event each year), then we calculate the 0.5-day mean
    #   mutate(widthValue = ifelse(round(time_sinceLastEvent) >= 5, 5*96,
    #                                   ifelse(round(time_sinceLastEvent) >= 1 & round(time_sinceLastEvent) < 5, round(time_sinceLastEvent)*96, 4)),
    #          widthValue = replace(widthValue, is.na(widthValue), 48)) %>% 
    #   mutate(DO_5day_mean = ifelse(!is.na(event_start), rollapply(data = lag(DO, n = 1),
    #                                                               width = widthValue,
    #                                                               FUN = mean,
    #                                                               partial = TRUE,
    #                                                               align = "right",
    #                                                               fill = NA,
    #                                                               na.rm = T), NA))
  

  # nodat <- comb %>% filter(transect == "WW" & (timestamp >= ymd_hms("2018-05-15 00:00:00", tz = "Etc/GMT+4") & timestamp <= ymd_hms("2018-05-20 07:30:00", tz = "Etc/GMT+4"))) %>% 
  #   filter(depth == 15 & pit %in% c(1, 6)) %>% 
  #   pivot_wider(names_from = pit, values_from = c(DO:VWC))
  # summary(nodat)
  
  # test2 <- met_all2 %>% filter(event_start == ymd_hms("2017-08-03 14:39:00", tz = "Etc/GMT+4"))
  
  # Solution for multiple variables/columns at once
  # This is pretty slow (~4 min to run); would like to find a data.table solution at some point
  # start_t <- Sys.time()
  comb <- comb %>%
    arrange(transect, pit, depth, timestamp) %>% 
    group_by(year, transect, pit, depth) %>% 
    # Here I calculate the width of the window used for the variable mean (the window always excludes the time of the event start time)
    # If the time b/w events is >= 4, then we calculate the 4-day mean (4 * 96 15-min rows)
    # If the time b/w events is >= 1 & < 4, then we calculate the X-day mean (X * 96 15-min rows; e.g., for 2-days = 2*96 = 192)
    # If the time b/w events is < 1, then we calculate the 1-hr mean (4 rows)
    # If the time b/w events is NA (i.e., the first observed event each year), then we calculate the 0.5-day mean
    mutate(widthValue = ifelse(round(time_sinceLastEvent) >= 4, 4*96,
                                    ifelse(round(time_sinceLastEvent) >= 1 & round(time_sinceLastEvent) < 4, round(time_sinceLastEvent)*96, 4)),
           widthValue = replace(widthValue, is.na(widthValue), 48)) %>%     
    mutate_at(vars(c(DO, Redox, SoilTemp, VWC)),
              .funs = list("pre" = ~ ifelse(!is.na(event_start), rollapply(data = lag(., n = 1),
                                                                width = widthValue,
                                                                FUN = mean,
                                                                partial = TRUE,
                                                                align = "right",
                                                                fill = NA,
                                                                na.rm = T), NA)))
  # end_t <- Sys.time()
  # end_t - start_t  
  
  # Here are the soil variable means!
  soil_means <- comb %>% 
    ungroup() %>% 
    filter(!is.na(event_start)) %>% 
    select(site, transect, pit, depth, event_start, DO_pre:VWC_pre)
  rm(comb, soils_all)
  
  
  # Test alternative approach
  # test <- comb %>%
  #   arrange(transect, pit, depth, timestamp) %>% 
  #   mutate(widthValue = ifelse(round(time_sinceLastEvent) >= 5, 5*96,
  #                                   ifelse(round(time_sinceLastEvent) >= 1 & round(time_sinceLastEvent) < 5, round(time_sinceLastEvent)*96, 4)),
  #          widthValue = replace(widthValue, is.na(widthValue), 48))
  
  
  # This data.table solution did not work and took almost an hour to run
  # start_t2 <- Sys.time()
  # test2 <- setDT(test)[, c("DO_pre", "redox_pre", "soilT_pre", "VWC_pre") 
  #                   := list(rollapply(lag(DO, n = 1), width = widthValue, FUN = mean, partial = TRUE, align = "right", fill = NA, na.rm = T),
  #                           rollapply(lag(Redox, n = 1), width = widthValue, FUN = mean, partial = TRUE, align = "right", fill = NA, na.rm = T),
  #                           rollapply(lag(SoilTemp, n = 1), width = widthValue, FUN = mean, partial = TRUE, align = "right", fill = NA, na.rm = T),
  #                           rollapply(lag(VWC, n = 1), width = widthValue, FUN = mean, partial = TRUE, align = "right", fill = NA, na.rm = T)),
  #                           by = list(transect, pit, depth, timestamp)][!is.na(event_start)][, c("site", "transect", "pit", "depth", "event_start", "DO_pre", "redox_pre", "soilT_pre", "VWC_pre")]
  # end_t2 <- Sys.time()
  # end_t2 - start_t2
  
  # Did not work
  # soil_means <- setDT(test)[, c("DO_pre", "redox_pre") 
  #                   := list(frollmean(lag(DO, n = 1), n = widthValue, align = "right", adaptive = TRUE, fill = NA, na.rm = T),
  #                           frollmean(lag(Redox, n = 1), n = widthValue, align = "right", adaptive = TRUE, fill = NA, na.rm = T)),
  #                           by = list(transect, pit, depth, timestamp)][!is.na(event_start)][, c("site", "transect", "pit", "depth", "event_start", "DO_pre", "redox_pre")]      

  
# Calculate event solute & turbidity yields, pre-event solute concs, & max turbidity ---- 
  # First add event data (event start to event end) to s::can data using an overlap join
    # Convert dfs to data.tables
    # In addition, the function foverlaps requires that the keys in y (events_all) have matches in x (stream)
    # so we have to create a 2nd time column (just a copy of timestamp) to match rain_start & event_end in events_all
    setDT(stream)[, Time2 := timestamp]
    # # Need to set keys for y (events_all) before the overlap join
    setkey(events_all, site, event_start, event_end)
    # Here we do the overlap join by site and start and end dates
    # the [ ] at the end just removes the extra columns added from events_all
    stream <- foverlaps(x = stream, y = events_all, by.x = c("site", "timestamp", "Time2"), nomatch = NA)[, "Time2" := NULL]
    
  # Add some more information to this df
  stream <- stream %>% 
    # Simplify analyte names from mg/L to just chem. abbrev.
    rename(NO3 = NO3_mgNL, TP = TP_mgPL, TDP = TDP_mgPL, SRP = SRP_mgPL, PP = PP_mgPL) %>% 
    # Add catchment_area as a column (pre-Ravindra values)
    # mutate(catch_area_km2 = ifelse(site == "Hungerford", 48.1, 16.7)) %>% 
    # I'm updating the catchment areas to the new values that Ravindra calculated with the new geo data
    mutate(catch_area_km2 = ifelse(site == "Hungerford", 43.8, 16.7)) %>%     
    # Add a condition column, i.e., event vs. baseflow
    mutate(condition = ifelse(is.na(eventid), "baseflow", "event")) %>%
    # Add a year column
    mutate(year = year(timestamp)) %>% 
    # Add a season column
    mutate(season = ifelse(yday(timestamp) > 59 & yday(timestamp) < 152, "spring",
                           ifelse(yday(timestamp) > 151 & yday(timestamp) < 244, "summer",
                                  ifelse(yday(timestamp) > 243 & yday(timestamp) < 335, "fall", "winter")))) %>% 
    # Rearrange columns
    select(site, catch_area_km2, timestamp, year, season, condition, rain_start:event_end, q_cms, NO3:turb)
  
  # Write this to a CSV
  stream %>%
    mutate(timestamp = as.character(timestamp),
           rain_start = as.character(rain_start),
           event_start = as.character(event_start),
           falling_inf_pt = as.character(falling_inf_pt),
           event_end = as.character(event_end)) %>%
    write_csv("Data/streamData_HF_WD_2017-2019.csv")
  
  # Calculate yield
  # While there are still missing Q and solute/ysi values, this is a continuous 15-min time series meaning that the time difference
  # between each row will always be 15 minutes except at the beginning of the year; thus we do not have to worry about removing rows
  # where the time step is greater than 15 minutes
  stream <- stream %>% 
    # Calculate time difference between rows in seconds
    group_by(site, year) %>% 
    mutate(timediff = difftime(timestamp, lag(timestamp), units = "secs"),
           timediff = replace_na(timediff, 0)) %>% 
    # Convert discharge to mm per sec
    # q / catch_area = (m^3/sec) / (km2 * 10^6) * (1000 mm / 1m)
    mutate(q_mms = q_cms / (catch_area_km2 * 10^6) * 1000) %>% 
    # Calculate export (kg) per time step (typically 5 min or 900 sec) for all solutes
    # conc_mg/L * q_m^3/secs * (1000 L/1 m^3) * timediff_secs * (1*10^-6 kg/1 mg)
    # Note: this doesn't make sense for turbidity, but we're going to do it anyway
    mutate_at(vars(c(NO3, TP, TDP, SRP, PP, turb)),
              .funs = list("kg" = ~as.numeric( . * q_cms * 1000 * timediff * (1*10^-6)))) %>% 
    # Calculate water yield (mm) per time step
    # q_mm/secs * timediff_secs = q_mm
    mutate(q_mm = as.numeric(q_mms * timediff)) %>% 
    # Calculate watershed-area normalized yield (standardize XX_kg by catchment area (km2))
    mutate_at(vars(c(NO3_kg, TP_kg, TDP_kg, SRP_kg, PP_kg, turb_kg)),
              .funs = list("km2" = ~ . / catch_area_km2)) %>%    
    # # Sum export through time
    # mutate_at(vars(c(NO3_kg, SRP_kg, TDP_kg, TP_kg)),
    #           funs("cum" = cumsum(.))) %>%
    # Move q_mms column next to q_cms
    select(site:q_cms, q_mms, everything()) %>% 
    ungroup()
  
  # Summarize event fluxes & calculate event ratios
  # Events with no solute or missing solute data will result in an NA
  stream_eventYields <- stream %>%
    # Sum 15-min yields for each event to get yields for each storm
    gather(key = "var", value = "val", c(NO3_kg_km2, SRP_kg_km2, TDP_kg_km2, TP_kg_km2, PP_kg_km2, turb_kg_km2, q_mm)) %>% 
    group_by(site, event_start, event_end, var) %>% 
    summarize(event_flux = sum(val, na.rm = FALSE)) %>% 
    spread(var, event_flux) %>% 
    # Keep only rows that are associated with an event
    filter(!is.na(event_start)) %>% 
    mutate(event_NO3_SRP = (NO3_kg_km2/14.007) / (SRP_kg_km2/30.974),
           event_NO3_TP = (NO3_kg_km2/14.007) / (TP_kg_km2/30.974)) %>% 
    # Add a season column
    mutate(season = ifelse(yday(event_start) > 59 & yday(event_start) < 152, "spring",
                           ifelse(yday(event_start) > 151 & yday(event_start) < 244, "summer",
                                  ifelse(yday(event_start) > 243 & yday(event_start) < 335, "fall", "winter")))) %>% 
    ungroup() %>% 
    # Let's only look at water, NO3, SRP, and turb yields
    select(site, event_start, season, q_mm, NO3_kg_km2, SRP_kg_km2, event_NO3_SRP, turb_kg_km2)
  
  # Write the yields to a CSV
  stream_eventYields %>% 
    mutate(event_start = as.character(event_start)) %>% 
    write_csv("Data/event_yields_ratios.csv")
  
  # Calculate 1-day pre-event solute means
  # 1440 min/day / 15-min/row = 96 rows/day  
  stream_means <- setDT(stream)[, c("NO3_1d", "SRP_1d", "turb_1d") 
                    := list(frollmean(lag(NO3, n = 1), n = 96*1, align = "right", fill = NA, na.rm = T),
                            frollmean(lag(SRP, n = 1), n = 96*1, align = "right", fill = NA, na.rm = T),
                            frollmean(lag(turb, n = 1), n = 96*1, align = "right", fill = NA, na.rm = T)),
                            by = site][!is.na(event_start)][, c("site", "event_start", "NO3_1d", "SRP_1d", "turb_1d")]
  # Because of how I joined the event data to the stream data, I need to keep just the first row of each site-event_start combo
  stream_means <- stream_means[, .SD[1], by = list(site, event_start)]

  # Calculate max turbidity
  turb_event_max <- stream %>% 
    filter(!is.na(event_start)) %>% 
    group_by(site, event_start) %>% 
    summarize(turb_event_max = max(turb, na.rm = T)) %>% 
    mutate(turb_event_max = ifelse(is.infinite(turb_event_max), NA, turb_event_max))
  
  rm(stream)
  
  # test <- stream %>% filter(event_start == ymd_hms(timestamp = "2018-03-29 11:30:00", tz = "Etc/GMT+4"))
  
  
# Calculate groundwater level metrics ----
  # Peak groundwater level & change in gw level (change in level from beginning to to peak level)
    # First add event data (event start to event end) to gw_all data using an overlap join
      # Convert dfs to data.tables
      # In addition, the function foverlaps requires that the keys in y (events_all) have matches in x (gw_all)
      # so we have to create a 2nd time column (just a copy of timestamp) to match rain_start & event_end in events_all
      setDT(gw_all)[, Time2 := timestamp]
      # # Need to set keys for y (events_all) before the overlap join
      setkey(events_all, site, event_start, event_end)
      # Here we do the overlap join by site and start and end dates
      # the [ ] at the end just removes the extra columns added from events_all
      gw_all2 <- foverlaps(x = gw_all, y = events_all, by.x = c("site", "timestamp", "Time2"), nomatch = NA)[, "Time2" := NULL]
      gw_all[, "Time2" := NULL]
      
    gw_event_max_delta <- gw_all2 %>% 
      filter(!is.na(event_start)) %>% 
      pivot_longer(cols = c(starts_with("well")), names_to = "well", values_to = "depth_m") %>% 
      group_by(site, event_start, well) %>% 
      summarize(gw_event_max = max(depth_m, na.rm = T),
                gw_event_delta = max(depth_m, na.rm = T) - first(depth_m)) %>% 
      mutate(gw_event_max = ifelse(!is.finite(gw_event_max), NA, gw_event_max)) %>% 
      pivot_wider(names_from = well, values_from = c(gw_event_max, gw_event_delta)) %>% 
      # Choose which wells you want to keep
      select(site, event_start, matches("(3|5)$"))
      
    rm(gw_all2)
    
    # test <- gw_all %>% filter(event_start == ymd_hms("2017-08-05 09:15:00", tz = "Etc/GMT+4"))
    # test <- gw_all %>% filter(site == "Wade" & timestamp > ymd_hms("2017-08-03 00:00:00", tz = "Etc/GMT+4"))
      
  # Mean antecedent discharge - 1 & 4 days
    # Join event_start to the gw_all data
      events_all[, timestamp := event_start]
      setkey(gw_all, site, timestamp)
      setkey(events_all, site, timestamp)
      comb_gw <- events_all[gw_all, roll = 1] %>% 
        mutate(year = year(timestamp)) %>% 
        arrange(site, timestamp)
      events_all[, "timestamp" := NULL]  
      
    comb_gw_long <- comb_gw %>%
      pivot_longer(cols = c(well1:wellSTR), names_to = "well", values_to = "depth_m") %>% 
      arrange(site, well, timestamp)
    
    # 1440 min/day / 15-min/row = 96 rows/day
    gw_preEvent_means <- setDT(comb_gw_long)[, c("gw_1d", "gw_4d") 
                      := list(frollmean(lag(depth_m, n = 1), n = 96*1, align = "right", fill = NA, na.rm = T),
                              frollmean(lag(depth_m, n = 1), n = 96*4, align = "right", fill = NA, na.rm = T)),
                              by = list(site, year, well)][!is.na(event_start)][, c("site", "well", "event_start", "gw_1d", "gw_4d")]
    
    # test2 <- gw_preEvent_means %>% filter(site == "Hungerford" & well == "well1" & timestamp >= ymd_hms("2019-04-05 00:00:00", tz = "Etc/GMT+4"))
    # test3 <- gw_all %>% filter(site == "Hungerford" & timestamp >= ymd_hms("2019-04-05 00:00:00", tz = "Etc/GMT+4"))
    
    gw_preEvent_means <- gw_preEvent_means %>% 
      pivot_wider(names_from = well, values_from = c(gw_1d, gw_4d)) %>% 
      # Choose which wells you want to keep
      select(site, event_start, matches("(3|5)$"))
    
    rm(comb_gw, comb_gw_long, gw_all)
    
  # Play sound when done!
  beep(sound = "fanfare") 
      

# ----    

# Join all variables together - don't forget PET ----
  # STILL NEED TO INCLUDE TIME SINCE LAST EVENT!
  allvars <- full_join(rain_mets, q_event_max_delta, by = c("site", "event_start")) %>% 
    full_join(q_event_dQRate, by = c("site", "event_start")) %>%
    full_join(q_preEvent_means, by = c("site", "event_start")) %>% 
    full_join(stream_eventYields, by = c("site", "event_start")) %>% 
    full_join(stream_means, by = c("site", "event_start")) %>% 
    full_join(turb_event_max, by = c("site", "event_start")) %>% 
    full_join(gw_event_max_delta, by = c("site", "event_start")) %>% 
    full_join(gw_preEvent_means, by = c("site", "event_start")) %>%   
    full_join(events_all %>% 
                select(site, event_start, time_sinceLastEvent, multipeak) %>% 
                mutate(time_sinceLastEvent = as.numeric(time_sinceLastEvent)), by = c("site", "event_start")) %>% 
    # Add hourly PET
    mutate(timestamp_hour = floor_date(event_start, unit = "1 hour")) %>%   
    left_join(PET %>% rename(timestamp_hour = timestamp), by = c("site", "timestamp_hour")) %>% 
    select(-timestamp_hour) %>% 
    # Add day of year
    mutate(DOY = yday(event_start)) %>% 
    # Shed rows with no event start
    filter(!is.na(event_start)) %>% 
    # Let's just look at events where all variables are complete
    # na.omit %>% 
    # Rearrange columns
    select(site, event_start, DOY, season, everything())
  
  # Which soil vars to add?
  # Look at which transects, pits and depths we have
  whichSoil <- soil_means %>% 
    select(transect, pit, depth) %>% 
    distinct()
  
  # Let's do 15 cm only at HW 1 & 3 and WW 1 & 6
  # Choosing 15 cm only here b/c we have GW level data to get at deeper processes (e.g., 45 cm)
  # soil_means_sub <- soil_means %>% 
  #   filter((transect == "HW" & depth == 15 & pit %in% c(1, 3)) |
  #          (transect == "WW" & depth == 15 & pit %in% c(1, 6)))
  # Because GW level data is limited to 2018-2019, let's also look at 45 cm (esp. VWC)
  soil_means_sub <- soil_means %>% 
    filter((transect == "HW" & depth %in% c(15, 30, 45) & pit %in% c(1, 3)) |
           (transect == "WW" & depth %in% c(15, 30, 45) & pit %in% c(1, 6)))  

  # Split those into separate dfs for Hungerford and Wade
    # and create columns for each variable, a version for each pit
  soil_means_sub_hford <- soil_means_sub %>% filter(site == "Hungerford") %>% 
    mutate(pit = paste0("pit", pit)) %>% 
    mutate(depth = paste0(depth, "cm")) %>% 
    pivot_longer(cols = DO_pre:VWC_pre, names_to = "var", values_to = "value") %>% 
    pivot_wider(names_from = c(var, pit, depth), values_from = value) %>% 
    select(-c(transect))
  soil_means_sub_wade <- soil_means_sub %>% filter(site == "Wade") %>% 
    mutate(pit = paste0("pit", pit)) %>% 
    mutate(depth = paste0(depth, "cm")) %>% 
    pivot_longer(cols = DO_pre:VWC_pre, names_to = "var", values_to = "value") %>% 
    pivot_wider(names_from = c(var, pit, depth), values_from = value) %>% 
    select(-c(transect))
  
  # Split the allvars into separate dfs for Hungerford and Wade & join the soil variables
  allvars_hford <- allvars %>% filter(site == "Hungerford") %>%
    full_join(soil_means_sub_hford, by = c("site", "event_start")) %>% 
    # Drop unnecessary columns
    select(-c(ends_with("well3")))
    # %>% na.omit  
  allvars_wade <- allvars %>% filter(site == "Wade") %>%
    full_join(soil_means_sub_wade, by = c("site", "event_start")) %>% 
    select(-c(ends_with("well5")))
    # %>% na.omit
  
# # Look at distributions
#   allvars_hford %>% 
#     pivot_longer(cols = -c(site:season, multipeak), names_to = "var", values_to = "value") %>% 
#     ggplot(aes(value)) +
#     facet_wrap(~var, scales = "free") +
#     geom_histogram()
  
  # Checked on high turb_1d value for 2019-03-30 15:45:00	event and the turb timeseries for that time period looks OK
  
  # ysi %>% 
  #   filter(site == "Hungerford") %>% 
  #   filter(timestamp > ymd_hms("2019-03-20 15:45:00", tz = "Etc/GMT+4") & timestamp < ymd_hms("2019-04-10 15:45:00", tz = "Etc/GMT+4")) %>% 
  #   ggplot(aes(x = timestamp, y = turb)) +
  #   geom_point()
  
  # allvars_wade %>% 
  #   pivot_longer(cols = -c(site:season, multipeak), names_to = "var", values_to = "value") %>% 
  #   ggplot(aes(value)) +
  #   facet_wrap(~var, scales = "free") +
  #   geom_histogram()  

# Write metrics to CSV ----
  allvars_hford %>% arrange(event_start) %>% mutate(event_start = as.character(event_start)) %>% write_csv("Data/eventMetrics_hford.csv")
  allvars_wade %>% arrange(event_start) %>% mutate(event_start = as.character(event_start)) %>% write_csv("Data/eventMetrics_wade.csv")
  