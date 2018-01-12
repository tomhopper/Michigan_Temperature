library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Do we want to save plots to disk? (alternative: display in R)
save_plots <- TRUE

# setup
today_year <- year(Sys.Date())

if(file.exists("data/temp_all.rds")) {
  temp_all_df <- readRDS("data/temp_all.rds")
} else {
  # Data from https://www.ncdc.noaa.gov/cdo-web/search
  # Daily Summaries; Date range max, stations by state, Michigan
  # Open details on search result (https://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/locations/FIPS:26/detail)
  # Open Station List
  # Sort by Start Date, then begin selecting stations with End Dates in 2018-01 and high Completeness ratios
  # Due to 1000 Station-Year limit on downloads, several stations were removed from data set
  temp_all_df <- read_csv("data-raw/1175837.csv", 
                          col_types = cols(DATE = col_date(format = "%Y-%m-%d"), 
                                           TAVG = col_double(), TOBS = col_double()))
  write_rds(x = temp_all_df, path = "data/temp_all.rds")
}
# Summary
summary(temp_all_df)

# Check to see if STATION and NAME are uniquely matched
table(temp_all_df[,1:2])

# Examine dates and coverage by station
temp_all_df %>% 
  select(STATION, NAME, DATE) %>% 
  na.omit() %>% 
  group_by(STATION, NAME) %>% 
  summarise(DATE_start = min(DATE), DATE_end = max(DATE),
            n = n()) %>% 
  mutate(N = as.numeric(DATE_end - DATE_start)) %>% 
  mutate(coverage = n / N) %>% 
  arrange(DATE_start)

# Plot TMAX and TMIN for each station
# WARNING takes time and memory!
all_plot <- temp_all_df %>% 
  select(STATION, NAME, DATE, TMAX, TMIN) %>% 
  gather(temp_measure, temp, -STATION, -NAME, -DATE) %>% 
  na.omit() %>% 
  ggplot(aes(x = DATE, y = temp, colour = temp_measure)) +
  geom_point(size = 0.1, alpha = 0.05) +
  #geom_smooth(method = "loess") +
  geom_smooth(method = "lm", se = FALSE) +
  coord_cartesian(ylim = c(-34, 37)) +
  facet_wrap(~NAME) +
  labs(title = "Trend in Daily High and Low Temperatures",
       x = "Year",
       y = expression(list(Temperature, degree~C)),
       caption = paste(today_year, "Thomas Hopper")) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank())
if(save_plots) png(filename = "figs/Daily_High_Low_Trend.png", width = 8, height = 5, units = "in", res = 300)
print(all_plot)
if(save_plots) dev.off()

# Examine those unusually cold temps in Cheboygan
temp_all_df %>% 
  select(STATION, NAME, DATE, TMAX, TMIN) %>% 
  filter(TMIN < -40)
## 1994, 1997-03-16 correspond to the right dates for record lows, but not the right temps
## Does not match reported record lows. Looks like problems with the instrumentation/data

# For each station, calculate the daily mean TMAX and TMIN for each day of the year, over
# the entire span of years.
daily_mean_df <- temp_all_df %>% 
  select(DATE, STATION, NAME, TMIN, TMAX) %>% 
  na.omit() %>% 
  mutate(Month = month(DATE), Day = day(DATE)) %>% 
  group_by(Month, Day, STATION) %>% 
  summarise(NAME = first(NAME),
            TMAX_mean = mean(TMAX), TMIN_mean = mean(TMIN))

# From the daily mean TMAX and TMIN, calculate the actual temperature anomaly for each
# data TMAX and TMIN
temp_anomaly_df <- temp_all_df %>% 
  select(STATION, NAME, DATE, TMAX, TMIN) %>% 
  na.omit() %>% 
  mutate(Month = month(DATE), Day = day(DATE)) %>% 
  left_join(daily_mean_df) %>% 
  mutate(TMAX_anom = TMAX - TMAX_mean, TMIN_anom = TMIN - TMIN_mean)

# Plot the temperature anomalies for TMAX and TMIN 
anomaly_plot <- temp_anomaly_df %>% 
  select(STATION, NAME, DATE, TMIN_anom, TMAX_anom) %>% 
  mutate(Year = year(DATE)) %>% 
  group_by(STATION, Year) %>% 
  summarise(TMIN_anom = mean(TMIN_anom), TMAX_anom = mean(TMAX_anom),
            NAME = first(NAME), n = n()) %>% 
  filter(n > 250) %>% 
  select(-n) %>% 
  gather(temp_measure, temp, -Year, -STATION, -NAME) %>% 
  ggplot(aes(x = Year, y = temp, colour = temp_measure)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~NAME) +
  labs(title = "Annual Temperature Anomaly",
       x = "Year",
       y = expression(list(Temperature~Anomaly, degree~C)),
       caption = paste(today_year, "Thomas Hopper")) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank())
if(save_plots) png(filename = "figs/Daily_Anomaly.png", width = 8, height = 5, units = "in", res = 300)
print(anomaly_plot)
if(save_plots) dev.off()
