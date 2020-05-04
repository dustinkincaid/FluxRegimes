# Compile daily precipitation data downloaded from https://www.cocorahs.org/ using (http://data.cocorahs.org/cocorahs/export/exportmanager.aspx)
# on 2020-04-20
# Units are in inches


# Load packages
  library("tidyverse")
  library("lubridate")
  
# Read in data
  fra <- read_csv("Data/DailyPrecip_FranklinCoVT_downloadedCOCORAHS_2020-04-20.csv", col_types = cols())
  orl <- read_csv("Data/DailyPrecip_OrleansCoVT_downloadedCOCORAHS_2020-04-20.csv", col_types = cols())
  # Bind together & tidy
  all <- bind_rows(fra, orl) %>% 
    rename(date = ObservationDate) %>% 
    mutate(date = mdy(date)) %>% 
    mutate_at(vars(c(TotalPrecipAmt:TotalSnowSWE)),
              list(~as.numeric(.))) %>% 
    filter(grepl('Swanton|Sheldon|St. Albans|Montgomery|Westfield', StationName)) %>% 
    filter((StationName == "Montgomery 5.0 SE" & (date >= ymd("2014-01-01") & date <= ymd("2014-08-30"))) |
             (StationName == "Westfield 0.7 WNW" & (date > ymd("2014-08-30") & date <= ymd("2019-12-31"))) |
             (StationName == "St. Albans 1.9 NNE" & (date >= ymd("2014-01-13") & date < ymd("2015-10-21"))) |
             (StationName == "Swanton 0.2 E" & (date >= ymd("2015-10-21") & date <= ymd("2016-12-13"))) |
             (StationName == "Swanton 0.5 NNE" & (date >= ymd("2017-02-13") & date <= ymd("2019-12-31")))) %>% 
    mutate(site = ifelse(grepl('Montgomery|Westfield', StationName), "Wade", "Hungerford")) %>% 
    select(date, site, StationNumber, StationName, Latitude, Longitude, TotalPrecipAmt:TotalSnowSWE) %>% 
    arrange(site, date)

# How I chose stations and date ranges above 
  # all %>% 
  #   arrange(date) %>% 
  #   group_by(StationName) %>% 
  #   summarize(first = first(date),
  #             last = last(date))
  
  #   StationName        first      last      
  #   <chr>              <date>     <date>    
  # 1 Montgomery 5.0 SE  2014-01-01 2014-08-30
  # 2 St. Albans 1.9 NNE 2014-01-13 2017-01-30
  # 3 Swanton 0.2 E      2015-10-21 2016-12-13
  # 4 Swanton 0.5 NNE    2017-02-13 2020-04-20
  # 5 Westfield 0.7 WNW  2014-01-01 2020-04-20
  
# Let's look at data for > 2017-08-01 & < 2019-12-31
  # Wade
  all %>%
    filter(site == "Wade") %>% 
    filter(date >= ymd("2017-10-01") & date <= ymd("2019-12-31")) %>% 
    pivot_longer(cols = c(TotalPrecipAmt:TotalSnowSWE), names_to = "var", values_to = "value") %>% 
    ggplot(aes(x = date, y = value)) +
    facet_wrap(~var, scales = "free_y", ncol = 1) +
    geom_point() +
    ylab("Inches") +
    scale_x_date(date_breaks = "1 month", date_labels = "%m-%y")
  
# Write to CSV
  all %>% write_csv("Data/dailyPrecip_cocorahs_compiled.csv")
  