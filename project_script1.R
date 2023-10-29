# TWD project 1
# Opis
# Loading libraries and data
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
data <- read_csv("D:/Studia/Semestr 3/TWD/changes-in-food-prices---data-analysis/prc_fsc_idx__custom_8184163_linear.csv.gz")
data  <- data.frame(data)
# Tasks:
#--------------------------------------------------------------------
dataPL <- data %>% 
  filter(geo == "PL") %>% 
  mutate(Year = year(as.Date(paste0(TIME_PERIOD, "-01")))) %>% 
  mutate(Month = month(as.Date(paste0(TIME_PERIOD, "-01")))) %>% 
  mutate(Polrocze = ifelse(Month > 6, 2,1)) %>% 
  filter(Year > 2016)
x <-dataPL %>% 
  group_by(Year, Polrocze) %>% 
  summarise(average = mean(OBS_VALUE, na.rm = TRUE)) %>% 
  ungroup()
x <- x  %>% 
  mutate(Inflacja = ((average - lag(average)) / lag(average)) * 100)
x[1, "Inflacja"] <- 0

x$period = paste(x$Year, x$Polrocze, sep = "-")
x <- x %>% 
  select(period, Inflacja)

dataGER <- data %>% 
  filter(geo == "DE") %>% 
  mutate(Year = year(as.Date(paste0(TIME_PERIOD, "-01")))) %>% 
  mutate(Month = month(as.Date(paste0(TIME_PERIOD, "-01")))) %>% 
  mutate(Polrocze = ifelse(Month > 6, 2,1)) %>% 
  filter(Year > 2016)
GER <-dataGER %>% 
  group_by(Year, Polrocze) %>% 
  summarise(average = mean(OBS_VALUE, na.rm = TRUE)) %>% 
  ungroup()
GER <- GER  %>% 
  mutate(Inflacja = ((average - lag(average)) / lag(average)) * 100)
GER[1, "Inflacja"] <- 0

GER$period = paste(GER$Year, GER$Polrocze, sep = "-")
GER <- GER %>% 
  select(period, Inflacja)
x$country <- 'PL'
GER$country <- 'DE'
POLGER <- rbind(x, GER)

EU <- data %>% 
  mutate(Year = year(as.Date(paste0(TIME_PERIOD, "-01")))) %>% 
  mutate(Month = month(as.Date(paste0(TIME_PERIOD, "-01")))) %>% 
  mutate(Polrocze = ifelse(Month > 6, 2,1)) %>% 
  filter(Year > 2016)
EU <-EU %>% 
  group_by(Year, Polrocze) %>% 
  summarise(average = mean(OBS_VALUE, na.rm = TRUE)) %>% 
  ungroup()
EU <- EU  %>% 
  mutate(Inflacja = ((average - lag(average)) / lag(average)) * 100)
EU[1, "Inflacja"] <- 0

EU$period = paste(EU$Year, EU$Polrocze, sep = "-")
EU <- EU %>% 
  select(period, Inflacja)
EU$country <- "EU"
POLGEREU <- rbind(POLGER, EU)
#--------------------------------------------------------------------
thm <- data %>%
  mutate(Year = year(as.Date(paste0(TIME_PERIOD, "-01")))) %>% 
  filter(Year > 2016) %>%
  group_by(Year, coicop) %>%
  summarise(average = mean(OBS_VALUE, na.rm = TRUE)) %>%
  ungroup() 
thm <- thm %>%
  arrange(coicop, Year) %>% 
  group_by(coicop) %>% 
  mutate(Inflacja = ((average - lag(average)) / lag(average)) * 100)
thm <- na.omit(thm)
#--------------------------------------------------------------------
 map1 <- data %>%
  mutate(Year = year(as.Date(paste0(TIME_PERIOD, "-01")))) %>% 
  filter(Year == 2017) %>% 
  group_by(coicop, geo) %>% 
  summarise(cen2017 = mean(OBS_VALUE, na.rm = TRUE)) %>% 
  ungroup()

map2 <- data %>% 
  mutate(Year = year(as.Date(paste0(TIME_PERIOD, "-01")))) %>% 
  filter(Year == 2022) %>% 
  group_by(coicop, geo) %>% 
  summarise(cen2022 = mean(OBS_VALUE, na.rm = TRUE)) %>% 
  ungroup()
merged_map <- merge(x = map1, y = map2, on = (c(map1$coicop, map1$geo) == c(map2$coicop, map2$geo)))
merged_map <- merged_map %>% 
  filter(!(geo %in% c("EA19", "EA20", "EU27_2020"))) %>% 
  mutate(Inflation = ((cen2022 -cen2017)/cen2017 )* 100) %>% 
  group_by(geo) %>% 
  slice(which.max(Inflation)) %>% 
  select(coicop, geo, Inflation)

dict <- data.frame(
  SKR = c("AT", "BE", "BG", "CH", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU",
          "IE", "IS", "IT", "LT", "LU" ,"LV", "MT" ,"NL", "NO" ,"PL", "PT", "RO", "SE", "SI", "SK",
          "TR"),
  Full = c("Austria", "Belgium", "Bulgaria", "Switzerland", "Czech Republic", "Cyprus", "Germany","Denmark", "Estonia",
           "Greece", "Spain", "Finland", "France", "Croatia", "Hungary", "Ireland", "Iceland", "Italy",
           "Lithuania", "Luxembourg", "Latvia", "Malta", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Sweden", "Slovenia", "Slovakia",
           "Turkey")
)
merged_map <- merged_map %>%
  left_join(dict, by=c("geo" = "SKR")) %>% 
  select(coicop, Full, Inflation)
food_dict <- data.frame(
  SKR = c("CP01154", "CP01174", "CP01147", "CP0121",  "CP01124", "CP0115",  "CP0114"),
  Full_food = c("Other edible oils", "Potatoes", "Eggs", "Coffe, tea, cocoa", "Poultry", "Oil, fats", "Milk, cheese")
)
merged_map <- merged_map %>% 
  left_join(food_dict, by=c("coicop" = "SKR")) %>% 
  select(Full_food, Inflation, Full)


# Plots:
ggplot(POLGEREU, aes(x = period, y = Inflacja, color = country,group = country)) +
  geom_line() +
  geom_point() + 
  labs(title = "Inflation by every half a year", x = "Time", y = "% of inflation") +
  scale_color_manual(values = c("EU" = "blue", "DE" = "black", "PL" = "red"))

ggplot(thm, aes(x = Year, y = coicop, fill = Inflacja)) +
  geom_tile()+
  labs(x = "Year", y = "Group of Products", fill = "Inflation", title = "inflation over the years for various product groups")+
  scale_fill_gradient(low = "white", high = "blue4") + 
  theme_minimal() 
  
map_data_europe <- map_data("world")
dane_mapa <- map_data_europe %>%
  left_join(merged_map, by = c("region" = "Full"))

ggplot(dane_mapa) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Full_food), color = "black") +
  scale_fill_viridis_d() +  
  labs(title = "Food category whose price increased the most compared to the country") +
  theme_minimal() +
  theme(legend.position = "right") +
  coord_cartesian(xlim = c(-30, 45), ylim = c(35, 70))


