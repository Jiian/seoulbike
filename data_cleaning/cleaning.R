##### MEASUREMENT UNITS
# temperatures: Celcius
# solar: MJ/m^2
# rainfall & snowfall: mm
# speed: m/s
# visibility: m
##### COMMENTS
# 1. Rainfall and snowfall mostly zeroes, but the data is probably correct,
# because the sum is very near to the official data
# 2. Snowfall only occurs in Autumn and Winter.
# 3. Rent_count is zero if and only if open is "No"
#####

library(tidyverse)

raw <- read.csv("../data/SeoulBikeData.csv") %>% as_tibble()

df <- raw %>%
  unite(col = datetime, Date, Hour, remove = FALSE) %>%
  rename("date" = Date, "rent_count" = Rented.Bike.Count, "hour" = Hour,
         "open" = Functioning.Day, "holiday" = Holiday, "season" = Seasons,
         "temperature" = Temperature..C., "humidity" = Humidity...,
         "wind_speed" = Wind.speed..m.s., "visibility" = Visibility..10m.,
         "dewpoint_temp" = Dew.point.temperature..C.,
         "solar_radiation" = Solar.Radiation..MJ.m2., "rainfall" = Rainfall.mm.,
         "snowfall" = Snowfall..cm.) %>%
  mutate(season = as.factor(season), holiday = as.factor(holiday),
         open = as.factor(open),
         datetime = as.POSIXlt(datetime, format = "%d/%m/%Y_%H"),
         date = as.Date(date, format = "%d/%m/%Y")) %>%
  select(datetime, date, hour, season, holiday, open, rent_count,
         everything()) %>%
  mutate(snowfall = 10 * snowfall, visibility = visibility * 10)

summary(df)

