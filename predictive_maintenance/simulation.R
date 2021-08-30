library(dplyr); library(magrittr); library(ggplot2); library(itsmr)

#####
rm(list = ls())
df <- readRDS("../data/bike_clean.rds")
df1cat <- df %>%
  select(date, season, holiday) %>%
  group_by(date) %>%
  summarise(season = season[1], holiday = holiday[1], .groups = "drop")
df1num1 <- df %>%
  select(date, snowfall, rainfall, rent_count) %>%
  group_by(date) %>%
  summarise(across(.fns = sum, .names = "{col}"), .groups = "drop")
df1num2 <- df %>%
  select(date, temperature, humidity, wind_speed,
         visibility, dewpoint_temp, solar_radiation) %>%
  group_by(date) %>%
  summarise(across(.fns = median, .names = "{col}"), .groups = "drop")
df1 <- cbind(df1cat, df1num1[2:4], df1num2[2:7]) %>% as_tibble()

winter <- df1$season == "Winter"
holiday <- df1$holiday == "Holiday"

# standardised
df2 <- data.frame(lapply(df1[4:12], function(x) (x - mean(x)) / sd(x))) %>%
  as_tibble()

# do a ytd dataframe
df1_ytd <- df1  %>%
  mutate(across(.fns = lag, .name = "{.cols}"))
df1_ytd[1,] <- df1_ytd[2,]
df2_ytd <- df2 %>%
  mutate(across(.fns = lag, .name = "{.cols}"))
df2_ytd[1,] <- df2_ytd[2,]

# qt dataframe
df1_qt <- df1_ytd  %>%
  mutate(across(.fns = lag, .name = "{.cols}"))
df1_qt[1,] <- df1_qt[2,]
df2_qt <- df2_ytd %>%
  mutate(across(.fns = lag, .name = "{.cols}"))
df2_qt[1,] <- df2_qt[2,]

set.seed(408)
rep_count <- (rnorm(365, mean = 300, sd = 20)
              + df2$temperature * 13
              + df2$wind_speed * 5
              + df2$humidity * 20
              + df2$visibility * -77
              + df2$solar_radiation * 1
              + df2$rainfall * 13
              + df2$snowfall * 73
              + winter * 100
              + holiday * 10
              + df2$rainfall * holiday * 30
              
              + df1_ytd$rent_count * 0.02432
              + df2_ytd$temperature * 6
              + df2_ytd$humidity * 10
              + df2_ytd$rainfall * 6
              + df2_ytd$snowfall * 46
              + df2_ytd$rainfall * 15
              
              + df1_qt$rent_count * 0.03032
              + df2_qt$temperature * 3
              + df2_qt$humidity * 13
              + df2_qt$rainfall * 6
              + df2_qt$snowfall * 13
              )
rep_count %>% summary()


df3 <- df1 %>%
  mutate(rep_count = round(rep_count))

ggplot(df3, aes(x = date, y = rep_count, colour = season)) +
  geom_point() +
  geom_line(mapping = aes(y = smooth.ma(rent_count / 10, q = 5)))

cor(df3[4:13])

write.csv(df3, "../data/bike_clean_rep.csv")
saveRDS(df3, "../data/bike_clean_rep.rds")
