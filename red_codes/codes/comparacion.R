library(tidyverse)
# https://docs.ropensci.org/nasapower/ 

library(nasapower)

nasa <- get_power(
  community = "AG",
  lonlat = c(-58.297615, -37.763263),
  pars = c( "T2M_MIN", "T2M_MAX", "RH2M", "PRECTOT"),
  dates = c("2018-01-01", "2018-12-30"),
  temporal_average = "DAILY"
)

nasa <- nasa %>% 
  mutate(fuente=as.factor("nasapower")) %>% 
  select(YYYYMMDD, DD, MM, YEAR, T2M_MAX, T2M_MIN, RH2M, PRECTOT, fuente)

bce19 <- readxl::read_excel("meteo/meteo_bce_manual.xlsx") %>% 
  mutate(YYYYMMDD = as.Date(YYYYMMDD), 
         fuente=as.factor("balcarce_manual")) %>% 
  select(-etp)

dat <- bind_rows(list(nasa, bce19))

ambos <- # fusionar ambos datasets (serie + campaña)
  bce19 %>%
  left_join(nasa, by = c("YYYYMMDD")) %>%
  mutate(YYYYMMDD = as.Date(YYYYMMDD))

dat %>% 
  ggplot(aes(YYYYMMDD, T2M_MIN, col=fuente))+
  geom_line()

dat %>% 
  ggplot(aes(YYYYMMDD, T2M_MAX, col=fuente))+
  geom_line() 

dat %>% 
  ggplot(aes(YYYYMMDD, PRECTOT, fill=fuente))+
  geom_col() 

bce19 %>%
  group_by(date = if_else(day(date) >= 30,
                          floor_date(date, "20 days"),
                          floor_date(date, "10 days"))) %>%
  summarize(rain_acum_season = sum(rain)) %>% #, days=n()) %>%
  mutate(year = year(date),
         month = month(date)) %>%
  group_by(year) %>%
  mutate(decada = row_number())
