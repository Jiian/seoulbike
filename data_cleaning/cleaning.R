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
# 4. No missing data
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

write.csv(df, "../data/bike_clean.csv")
saveRDS(df, file = "../data/bike_clean.rds")

###

info1 <- read.csv("../data/rental_office_info.csv") %>% select(code, latitude:type)
info2 <- read.csv("../data/rental_office_info2.csv") %>% select(code, latitude:type)
info_c <- rbind(info1, info2) %>%
  as_tibble() %>%
  mutate(install_date = as.Date(install_date, format = "%d/%m/%Y"),
         LCD = as.integer(LCD), QR = as.integer(QR))
write.csv(info_c, "../data/office_info_clean.csv")
saveRDS(info_c, "../data/office_info_clean.rds")

###

library(readxl)

dec18rent <- read_excel("../data/rental_office_(dec2018_to_may2019).xlsx", sheet = 1)
dec18retu <- read_excel("../data/rental_office_(dec2018_to_may2019).xlsx", sheet = 2)

dec18rent1 <- dec18rent %>%
  separate(col = office, into = c("code", "office"), sep = ". ") %>%
  filter(!is.na(office)) %>%
  mutate(month = as.character(month),
         month = gsub("^(.{4})(.{2})$", "\\1_\\2_01", month),
         month = as.Date(month, format = "%Y_%m_%d"))
  
dec18retu1 <- dec18retu %>%
  separate(col = office, into = c("code", "office"), sep = ". ") %>%
  filter(!is.na(office)) %>%
  mutate(month = as.character(month),
         month = gsub("^(.{4})(.{2})$", "\\1_\\2_01", month),
         month = as.Date(month, format = "%Y_%m_%d"))

dec18 <- left_join(dec18rent1, dec18retu1, by = c("code", "month")) %>%
  select(code, month, rentals, returns)

###

jan17raw <- read.csv("../data/rental_office_(jan_to_dec_2017).csv") %>% as_tibble()

jan17 <- jan17raw %>%
  mutate(code = substr(code, 2, nchar(code) - 1),
         month = gsub("^'(.{4})(.{2})'$", "\\1_\\2_01", month),
         month = as.Date(month, format = "%Y_%m_%d")) %>%
  select(2, 1, 4, 5)

###

jan18raw <- read.csv("../data/rental_office_(jan_to_june_2018).csv") %>% as_tibble()

jan18 <- jan18raw %>%
  mutate(code = substr(code, 2, nchar(code) - 1),
         month = gsub("^'(.{4})(.{2})'$", "\\1_\\2_01", month),
         month = as.Date(month, format = "%Y_%m_%d")) %>%
  select(2, 1, 4, 5)

###

jul18raw <- read_excel("../data/rental_office_(july_to_nov_2018).xlsx")

jul18 <- jul18raw %>%
  mutate(month = as.character(month),
         month = gsub("^(.{4})(.{2})$", "\\1_\\2_01", month),
         month = as.Date(month, format = "%Y_%m_%d")) %>%
  select(2, 1, 4, 5)

###

loc_df <- rbind(jan17,jan18, jul18, dec18)
write.csv(loc_df, "../data/location_jan17_may19_clean.csv")
saveRDS(loc_df, "../data/location_jan17_may19_clean.rds")
