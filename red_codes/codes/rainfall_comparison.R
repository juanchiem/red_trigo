pacman::p_load(tidyverse, lubridate)

library(nasapower)

nasa <- get_power(
  community = "AG",
  lonlat = c(-58.297615, -37.763263),
  pars = c( "T2M_MIN", "T2M_MAX", "RH2M", "PRECTOT"),
  dates = c("2018-01-01", "2018-12-31"),
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

nasa18. <-
  nasa %>%
  select(date=YYYYMMDD, DD, MM, YEAR, tmin=T2M_MIN, tmax=T2M_MAX,rain=PRECTOT)

bind_rows(list(nasa18, bce18)) %>%
  group_by(fuente,
           # date = if_else(day(date) >= 30,
           #                floor_date(date, "20 days"),
           #                floor_date(date, "10 days")),
           mes = month(date)
           ) %>%
  summarize(days=n(),
            rain_acum = sum(rain),
            rainy_days = sum(rain>3)) %>%
  group_by(fuente) %>%
  # mutate(decada = row_number()) %>%
  ggplot(aes(mes, rain_acum, fill=fuente))+ #rainy_days
  geom_col(alpha=0.5, position = "dodge") +
  scale_fill_viridis_d() +
  # facet_wrap(~fuente, ncol=1)+
  scale_x_continuous(breaks=seq(1,36,by=1))+
  scale_y_continuous( breaks = function(x) unique( floor(pretty(seq(0, (max(x) + 1) * 1.1)))))+
  theme_bw()+
  labs(y="lluvias acum (mm)")+
  labs(y="dias con al menos 3 mm")+
  guides(x = guide_axis(angle = 90))

nasa18. %>%
  left_join(bce18, by="date") %>%
  mutate(tmin_dif=(tmin.x-tmin.y)+0.66,
         tmax_dif=(tmax.x-tmax.y)-0.18,
         rain_dif=rain.x-rain.y-0.47) %>%
  select(date, tmin_dif, tmax_dif,rain_dif) %>%
  pivot_longer(-date, names_to = "var", values_to = "val") %>%
  # group_by(var, lubridate::month(date)) %>% summarise(mean(val)) %>% View
ggplot(aes(date, val, col = var))+ #rainy_days
  geom_col(alpha=0.5) +
  facet_wrap(~var, ncol=1, scales = "free_y")+
  geom_line(aes(y = mean(val)), color = "red", linetype = "dotted")+
  theme_bw()+
  labs(y="diferenia nasa - manual")+
  guides(x = guide_axis(angle = 90))
