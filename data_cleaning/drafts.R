
tmp <- df %>%
  filter(season != "Winter")

tmp$season[which(tmp$snowfall > 0)]

summary(tmp$snowfall)

ggplot(df) + geom_histogram(aes(x = solar_radiation))

df %>%
  pivot_longer(cols = c(rainfall, snowfall), names_to = "rain_snow",
               values_to = "rain_snow_val") %>%
  ggplot(aes(x = rain_snow, y = "rain_snow_val")) +
  geom_boxplot() +
  geom_point()

plotc(df$Rented.Bike.Count[25:48])
length(unique(df$Date))



df %>%
  group_by(Holiday, Hour) %>%
  summarise(rented_count_med = median(Rented.Bike.Count), .groups = "drop") %>%
  ggplot() +
  geom_line(aes(x = Hour, y = rented_count_med, colour = Holiday))


df1  <- df %>%
  select(Rented.Bike.Count, Temperature..C., Humidity...,
         Wind.speed..m.s., Visibility..10m., Dew.point.temperature..C.,
         Solar.Radiation..MJ.m2., Rainfall.mm., Snowfall..cm.,
         Seasons, Holiday, Functioning.Day)
df1$Seasons <- as.factor(df1$Seasons)
df1$Holiday <- as.factor(df1$Holiday)
df1$Functioning.Day <- as.factor(df$Functioning.Day)

md <- lm(Rented.Bike.Count ~ Temperature..C.+ Humidity...+
           Wind.speed..m.s.+ Visibility..10m.+ Dew.point.temperature..C.+
           Solar.Radiation..MJ.m2.+ Rainfall.mm.+ Snowfall..cm.+
           Seasons+ Holiday+ Functioning.Day, data = df)

step(md, direction = "backward")

df1 <- df1 %>%
  filter(Rented.Bike.Count > 0) %>%
  mutate(logrent = log(Rented.Bike.Count))

md <- lm(logrent ~ Temperature..C.+ Humidity...+
           Wind.speed..m.s.+
           Solar.Radiation..MJ.m2.+ Rainfall.mm.+ Snowfall..cm.+
           Seasons+ Holiday, data = df1)
