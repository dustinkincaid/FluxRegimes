# Calculating the event metrics possible for all 3 BREE stream sites from 2014 to 2019 for Scott Hamshaw's manuscript

# Load packages
  library("tidyverse")
  library("lubridate")
  library("data.table")
  library("zoo")
  library("beepr")
  
# Read in data ----
# Stream data ----
  # These data are a continuous 15-min time series of all stream data from 2014 to 2019 compiled using GapFilling/Scripts/compile_data
  stream <- read_csv("../GapFilling/Data/allStreamData_2014to2019_compiled_2020-04-10.csv", col_types = cols(DO_mgL = col_double())) %>% 
    mutate(timestamp = ymd_hms(timestamp, tz = "Etc/GMT+4"))
  
  
# Precip data ----
  # Hungerford
    # 2015-2017 tipping bucket data (tz = GMT-4) - NO 2014 data
    precip_hford_15to17 <- read_csv("../../General Site Data/Met data/hungerford_2015-17_precip_data.csv") %>%
      rename(timestamp = "date", precip_mm = "rain_mm") %>% 
      mutate(timestamp = mdy_hm(timestamp, tz = "Etc/GMT+4")) %>% 
      # Let's aggregate the data to 5-min data
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
    
  # Potash
    
    # 2015-2019 tipping bucket data downloaded from Aquarius 2020-04-18 - GMT-0400
    precip_pot <- read_csv("../../General Site Data/Met data/potash_2015-19_precip_data_fromAquarius_2020-04-18.csv") %>% 
      rename(timestamp = "Timestamp (UTC-04:00)", precip_mm = "Value") %>% 
      mutate(timestamp = mdy_hm(timestamp, tz = "Etc/GMT+4")) %>% 
      # Let's aggregate the data to 5-min data
      # Create a new column that gives the date and hour of the measurement
      mutate(timestamp_5min = ceiling_date(timestamp, unit = "5 min")) %>% 
      # Now total 5-minute data into an hourly total
      group_by(timestamp_5min) %>% 
      summarize(precip_mm = sum(precip_mm)) %>% 
      rename(timestamp = timestamp_5min) %>% 
      mutate(site = "Potash")
  
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
      mutate(timestamp = with_tz(timestamp, tzone = "Etc/GMT+4")) %>% 
      select(-c(recNum, battVolt, "Line#")) %>% 
      select(site, timestamp, everything()) %>% 
      arrange(site, timestamp) %>% 
      # Filter out the months in 2017 at Hungerford where the tipping bucket was in place at the NN site
      # But keep the months were there was a gap in tipping bucket data (i.e., 8/29/17 11:45 to 9/15/17 11:55)
      filter(!(site == "Hungerford" & timestamp <= ymd_hms("2017-08-29 11:40:00", tz = "Etc/GMT+4"))) %>% 
      filter(!(site == "Hungerford" & (timestamp >= ymd_hms("2017-09-15 12:00:00", tz = "Etc/GMT+4") & timestamp <= ymd_hms("2017-11-10 13:15:00", tz = "Etc/GMT+4"))))
    rm(met_hford_17to19, met_hford_late19, met_wade_17to19, met_wade_late19)
    
  # Add 2017 tipping bucket data
    met_all <- bind_rows(met, precip_hford_15to17, precip_pot, precip_wade_14to17) %>% 
      arrange(site, timestamp)
    rm(met, precip_hford_15to17, precip_pot, precip_wade_14to17)  
    
    
# Events ----
  # 2014-2018
  events_hford_14to18 <- read_csv("../../2_Tasks/Storm Event Delineation/Data/Event delineations 2014-2018/Events_Hungerford_2014to2018.csv", na = c("", "NA", "NaN")) %>% mutate(site = "Hungerford")
  events_pot_14to18 <- read_csv("../../2_Tasks/Storm Event Delineation/Data/Event delineations 2014-2018/Events_Potash_2014to2018.csv", na = c("", "NA", "NaN")) %>% mutate(site = "Potash")  
  events_wade_14to18 <- read_csv("../../2_Tasks/Storm Event Delineation/Data/Event delineations 2014-2018/Events_Wade_2014to2018.csv", na = c("", "NA", "NaN")) %>% mutate(site = "Wade")
    # Combine and tidy these up
    events_14to18 <- bind_rows(events_hford_14to18, events_pot_14to18, events_wade_14to18) %>% 
      # Rename & rearrange columns
      rename(rain_start = "rainfall start", event_start = "HydRun start", falling_inf_pt = "HIP end", event_end = "HydRun end", multipeak = "multipeak tag", snowmelt = Snowmelt) %>% 
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
      select(-c("HydRun detect?", "year(event_start)")) %>% select(site, eventid, everything())
    rm(events_hford_14to18, events_pot_14to18, events_wade_14to18)
    
  # 2019
  events_hford_19 <- read_csv("../../2_Tasks/Storm Event Delineation/Data/Event delineations 2019/Events_Hungerford_2019.csv", na = c("", "NA", "NaN")) %>% mutate(site = "Hungerford")
  events_pot_19 <- read_csv("../../2_Tasks/Storm Event Delineation/Data/Event delineations 2019/Events_Potash_2019.csv", na = c("", "NA", "NaN")) %>% mutate(site = "Potash")
  events_wade_19 <- read_csv("../../2_Tasks/Storm Event Delineation/Data/Event delineations 2019/Events_Wade_2019.csv", na = c("", "NA", "NaN")) %>% mutate(site = "Wade")  
    # Combine and tidy these up
    events_19 <- bind_rows(events_hford_19, events_pot_19, events_wade_19) %>% 
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
    rm(events_hford_19, events_pot_19, events_wade_19)
  # Combine event objects into one
  events_all <- bind_rows(events_14to18, events_19)
  rm(events_14to18, events_19)
  
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
    rm(interp_time)

  
# Calculations ----
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
      # Remove events that don't have a rain start
      events_withRain <- events_all %>% filter(!is.na(rain_start))
      # Convert dfs to data.tables
      # In addition, the function foverlaps requires that the keys in y (events_withRain) have matches in x (met_all)
      # so we have to create a 2nd time column (just a copy of timestamp) to match rain_start & event_end in events_withRain
      setDT(met_all)[, Time2 := timestamp]
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
    # rain_mets <- full_join(rain_event_total, rain_event_duration, by = c("site", "event_start"))
      
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
    setDT(met_all3)[, Time2 := timestamp]
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
      
  # Antecedent precipitation Index (API) over 4 days (used in Fovet et al 2018)
    # See here for information about API formula: https://tonyladson.wordpress.com/tag/antecedent-precipitation-index/
    # It might explain some of the variability we see in N:P yield ratios with small event water yields:
      # From the website: High values of API mean the catchment is wet so any rain is likely to run off.Low values mean 
        # there hasn’t been much rain lately, the catchment is dry so rain is likely to soak into soil and wet up vegetation 
        # and not make it to a stream.  API has been related to initial loss i.e. the amount of rainfall that is ‘lost’ before 
        # runoff starts (Cordery, 1970a; 1970b).  An example for the Bobo River in northern NSW is shown in Figure 1.  As the 
        # API goes up, the catchment is wetter so the initial loss decreases and more rain will run off.
    
  
  # Total rain for 1-day, 7-days, 14-days, and 30-days prior to event (prior to rain start)
  # Only considering events with a rain_start, so we'll use events_withRain df from above
    # Join rain_start to the met_all data
    setDT(met_all)
    setDT(events_withRain)
    events_withRain[, timestamp := rain_start]
    setkey(met_all, site, timestamp)
    setkey(events_withRain, site, timestamp)
    comb_met <- events_withRain[met_all, roll = 1] %>% 
      mutate(year = year(timestamp)) %>% 
      arrange(site, timestamp)
    events_withRain[, "timestamp" := NULL]    
      
    # test <- comb_met %>% filter(site == "Hungerford" & timestamp > ymd_hms("2017-10-04 20:00:00", tz = "Etc/GMT+4"))      
    
    # Calculate cumulative pre-event rain totals
    # This is painfully slow; it would be nice to find a data.table solution
    rain_preEvent_totals <- comb_met %>%
      group_by(site) %>% 
      # 288 rows 5 min/row = 1 day, so 288 rows/day
      mutate(rain_preEvent_1d = ifelse(!is.na(rain_start), rollapply(data = lag(precip_mm, n = 1),
                                                                      width = 288*1,
                                                                      FUN = sum,
                                                                      partial = FALSE,
                                                                      align = "right",
                                                                      fill = NA,
                                                                      na.rm = T), NA),
             rain_preEvent_7d = ifelse(!is.na(rain_start), rollapply(data = lag(precip_mm, n = 1),
                                                                      width = 288*7,
                                                                      FUN = sum,
                                                                      partial = FALSE,
                                                                      align = "right",
                                                                      fill = NA,
                                                                      na.rm = T), NA),
             rain_preEvent_14d = ifelse(!is.na(rain_start), rollapply(data = lag(precip_mm, n = 1),
                                                                      width = 288*14,
                                                                      FUN = sum,
                                                                      partial = FALSE,
                                                                      align = "right",
                                                                      fill = NA,
                                                                      na.rm = T), NA), 
             rain_preEvent_30d = ifelse(!is.na(rain_start), rollapply(data = lag(precip_mm, n = 1),
                                                                      width = 288*30,
                                                                      FUN = sum,
                                                                      partial = FALSE,
                                                                      align = "right",
                                                                      fill = NA,
                                                                      na.rm = T), NA))
    
    rain_preEvent_totals <- rain_preEvent_totals %>% 
      ungroup() %>% 
      filter(!is.na(rain_start)) %>% 
      select(site, event_start, rain_preEvent_1d:rain_preEvent_30d)
    rm(comb_met, met_all)
  
      
# Calculate discharge metrics ----
  # Peak discharge & delta Q (change in Q from beginning to to peak Q)
    # Create q_all from stream
    q_all <- stream %>% 
      select(site, timestamp, q_cms)
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
      # NOTE!
      # I had to filter out NA q_cms values, in order for the slice line to work below,
      # but I haven't thought through how this might affect the calculations!
      filter(!is.na(q_cms)) %>%
      group_by(site, event_start) %>% 
      # Slice data from start of storm to max q
      slice(1:which.max(q_cms)) %>%
      summarize(risingLimb_hrs = as.numeric(difftime(max(timestamp), min(timestamp), units = "hours"))) %>% 
      full_join(q_event_max_delta, by = c("site", "event_start")) %>% 
      mutate(q_event_dQRate_cmsPerHr = q_event_delta/risingLimb_hrs) %>% 
      select(-c(risingLimb_hrs, q_event_max, q_event_delta))
    rm(q_all2)
      
  # Mean antecedent discharge - 1 & 7 days
    # Join event_start to the q_all data
      setDT(q_all)
      setDT(events_all)
      events_all[, timestamp := event_start]
      setkey(q_all, site, timestamp)
      setkey(events_all, site, timestamp)
      comb_q <- events_all[q_all, roll = 1] %>% 
        mutate(year = year(timestamp)) %>% 
        arrange(site, timestamp)
      events_all[, "timestamp" := NULL]  
      
    q_preEvent_means <- comb_q %>%
      group_by(site, year) %>% 
      # 1440 min/day / 15-min/row = 96 rows/day
      mutate(q_preEvent_mean_1d = ifelse(!is.na(event_start), rollapply(data = lag(q_cms, n = 1),
                                                                      width = 96*1,
                                                                      FUN = mean,
                                                                      partial = TRUE,
                                                                      align = "right",
                                                                      fill = NA,
                                                                      na.rm = T), NA),
             q_preEvent_mean_7d = ifelse(!is.na(event_start), rollapply(data = lag(q_cms, n = 1),
                                                                      width = 96*7,
                                                                      FUN = mean,
                                                                      partial = TRUE,
                                                                      align = "right",
                                                                      fill = NA,
                                                                      na.rm = T), NA))      
    
    q_preEvent_means <- q_preEvent_means %>% 
      ungroup() %>% 
      filter(!is.na(event_start)) %>% 
      select(site, event_start, q_preEvent_mean_1d, q_preEvent_mean_7d)
    rm(comb_q, q_all)    

# Calculate event solute & turbidity yields & max turbidity ---- 
  # First add event data (event start to event end) to s::can data using an overlap join
    # Convert dfs to data.tables
    # In addition, the function foverlaps requires that the keys in y (events_all) have matches in x (stream)
    # so we have to create a 2nd time column (just a copy of timestamp) to match rain_start & event_end in events_all
    setDT(stream)[, Time2 := timestamp]
    setDT(events_all)
    # # Need to set keys for y (events_all) before the overlap join
    setkey(events_all, site, event_start, event_end)
    # Here we do the overlap join by site and start and end dates
    # the [ ] at the end just removes the extra columns added from events_all
    stream <- foverlaps(x = stream, y = events_all, by.x = c("site", "timestamp", "Time2"), nomatch = NA)[, "Time2" := NULL]
    
  # Add some more information to this df
  stream <- stream %>% 
    # Simplify analyte names from mg/L to just chem. abbrev.
    rename(NO3 = NO3_mgNL, TP = TP_mgPL, TDP = TDP_mgPL, SRP = SRP_mgPL, PP = PP_mgPL) %>% 
    # Add catchment_area as a column
    mutate(catch_area_km2 = ifelse(site == "Hungerford", 48.1, 16.7)) %>% 
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
  
# Calculate max turbidity
  turb_event_max <- stream %>% 
    filter(!is.na(event_start)) %>% 
    group_by(site, event_start) %>% 
    summarize(turb_event_max = max(turb, na.rm = T)) %>% 
    mutate(turb_event_max = ifelse(is.infinite(turb_event_max), NA, turb_event_max))
  
  rm(stream)
  

# Join all variables together - don't forget PET ----
  allvars <- full_join(rain_event_total, rain_event_duration, by = c("site", "event_start")) %>% 
    full_join(rain_event_intensity_max, by = c("site", "event_start")) %>% 
    full_join(rain_event_intensity_mean, by = c("site", "event_start")) %>% 
    full_join(rain_preEvent_totals, by = c("site", "event_start")) %>%
    full_join(q_event_max_delta, by = c("site", "event_start")) %>%
    full_join(q_event_dQRate, by = c("site", "event_start")) %>%
    full_join(q_preEvent_means, by = c("site", "event_start")) %>% 
    full_join(stream_eventYields, by = c("site", "event_start")) %>% 
    full_join(turb_event_max, by = c("site", "event_start")) %>% 
    full_join(events_all %>% 
                select(site, event_start, time_sinceLastEvent) %>% 
                mutate(time_sinceLastEvent = as.numeric(time_sinceLastEvent)), by = c("site", "event_start")) %>% 
    # Shed rows with no event start
    filter(!is.na(event_start)) %>% 
    # Let's just look at events where all variables are complete
    # na.omit %>% 
    # Rearrange columns
    select(site, event_start, season, everything())  

# Write to CSV
  allvars %>% 
    mutate(event_start = as.character(event_start)) %>% 
    write_csv("Data/eventMetrics_allBREESites_2014to2019.csv")
  