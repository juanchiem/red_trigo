pacman::p_load(tidyverse, lubridate)

library(nasapower)

nasa <- get_power(
  community = "AG",
  lonlat = c(-58.297615, -37.763263),
  pars = c( "T2M_MIN", "T2M_MAX", "RH2M", "PRECTOT"),
  dates = c("2018-01-01", "2018-12-30"),
  temporal_average = "DAILY"
)

bce <- readxl::read_excel(here::here("data/balcarce_clima.xlsx")) %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date))

bce18 <- # Para la reciente campaña
  bce %>%
  filter(date > '2017-12-31', date < '2018-12-31') %>%
  mutate(fuente=as.factor("balcarce_manual"),
         date = lubridate::date(date))

nasa18 <-
  nasa %>%
  select(date=YYYYMMDD, DD, MM, YEAR, rain=PRECTOT) %>%
  mutate(fuente=as.factor("nasapower"))

bind_rows(list(nasa18, bce18)) %>%
  group_by(fuente, date = if_else(day(date) >= 30,
                          floor_date(date, "20 days"),
                          floor_date(date, "10 days"))) %>%
  summarize(rain_acum = sum(rain),
            rainy_days = sum(rain>2)) %>% #, days=n()) %>%
  # mutate(year = year(date),
  #        month = month(date)) %>%
  group_by(fuente) %>%
  mutate(decada = row_number()) %>% View

 group_by(date = if_else(day(date) >= 30,
                          floor_date(date, "20 days"),
                          floor_date(date, "10 days"))) %>%
  summarize(rain_acum = sum(rain),
            rainy_days = sum(rain>2)) %>% #, days=n()) %>%
  mutate(year = year(date),
         month = month(date)) %>%
  group_by(year) %>%
  mutate(decada = row_number()) %>%

  ggplot(aes(decada, rainy_days, fill=fuente))+
  geom_col(alpha=0.5)

