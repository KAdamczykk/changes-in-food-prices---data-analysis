# TWD project 1
# Opis
# Loading libraries and data
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
data <- read_csv("prc_fsc_idx__custom_8184163_linear.csv.gz")
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
  