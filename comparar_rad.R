# https://itsalocke.com/blog/understanding-rolling-calculations-in-r/
# https://inta.gob.ar/sites/default/files/inta_impacto_de_los_factores_ambientales_en_la_definicion_de_los_rendimientos_de_los_cultivos_resultados_de_la_campana_2013_de_trigo.pdf
# http://rafaela.inta.gov.ar/info/miscelaneas/101/trigo2004_n1.pdf 

pacman::p_load(tidyverse, lubridate, zoo)

stations <- tibble::tribble(
  ~station,   ~lat,   ~lon,
  "Balcarce INTA", -37.75,  -58.3,
  "Mar del Plata AERO", -37.93, -57.58,
  "Azul AERO", -36.83, -59.88,
  "Benito Juarez AERO", -37.72, -59.78,
  "Laprida", -37.57, -60.77,
  "Barrow INTA", -38.32, -60.25,
  "TANDIL AERO", -37.23, -59.25
) %>% arrange(station)

dates <- seq.Date(lubridate::dmy("1-1-2020"), lubridate::dmy("30-12-2020"), by = "1 day")
temp <- metR::GetSMNData(dates, type = "daily", bar = TRUE)

rad_nasa <- nasapower::get_power(
  community = "AG",
  lonlat =  as.vector(unlist(stations[1,-1])),
  pars = c("ALLSKY_SFC_SW_DWN"),
  dates = c(min(dates), max(dates)),
  temporal_average = "DAILY"
) 

rad_nasa <- rad %>% 
  rename(date = YYYYMMDD, rad_nasa = ALLSKY_SFC_SW_DWN) 
  
bce20_sheet <- gs4_get(gs4_find("balcarce_clima")$id)

bce20 <- read_sheet(bce20_sheet, sheet="2020") %>% 
  janitor::clean_names() %>% 
  rename(YEAR = ano, MM = mes, DD = dia, 
         rad_bce = radiacion_mj_m_2_dia_1) 
  
bce_nasa <- bce20 %>% 
  left_join(rad_nasa, by = c("DD","MM","YEAR")) %>% 
  mutate(julian = lubridate::yday(date))

bce_nasa %>% 
  ggplot()+
  aes(x=rad_bce, y = rad_nasa)+
  geom_point()+
  # theme(aspect.ratio=1)+
  coord_fixed()+
  lims(x=c(0,30), y =c(0,30))+
  geom_abline(intercept = 0, slope = 1)+
  labs(title = "Comparacion de radiacion\nobtenida en EEA vs NASAPOWER")
  
bce_nasa %>% 
  ggplot(aes(x = julian))+ 
  geom_line(aes(y= rad_nasa, col = "nasa"))+
  geom_line(aes(y= rad_bce, col = "bce"))+
  labs(title = "2020")


filtro <- tibble(y = pracma::hampel(tandil$rad, 5, 3)$y)

p1 + geom_point(data = filtro, 
                aes(x = seq(1, length(y)), y = y), 
                col = "darkred")

tandil <- tandil %>% 
  mutate(tmean = (tmax + tmin)/2, 
         rad_y = filtro$y, 
         q = rad_y*0.5/tmean-4.5) 

tandil %>% 
  ggplot(aes(date, q))+ 
  # geom_line()+
  geom_quantile(quantiles = c(0.5),
                formula = NULL,
                method = "rqss") 

# geom_line(data = . %>%
#               mutate(q = zoo::rollmean(q, 2, align = "right", fill = NA)),
#             col = "darkred")

tandil %>% 
  filter(month(date) %in%  10:12) %>%
  group_by(date=if_else(day(date) >= 30,
                        floor_date(date, "20 days"),
                        floor_date(date, "10 days"))) %>%
  summarize(q10 = mean(q),
            days = n()) ->tan_q

tandil <- 
  tandil %>% 
  # mutate(q_pc = rollapply(q, width = list(-20:10), mean, 
  #                             align = "center", 
  #                             fill = NA, 
  #                             na.rm = T)) 
  mutate(tmean_pc = rollapply(tmean, width = list(-20:10), mean, 
                              align = "center", 
                              fill = NA, 
                              na.rm = T)-4.5, 
         rad_pc = rollapply(rad_y*0.5, 
                            width = list(-20:10), mean, 
                            align = "center", 
                            fill = NA, 
                            na.rm = T), 
         q = rad_pc/tmean_pc)


# saveRDS(nasa, here::here("meteo", paste0(id_trial,"_nasa.rds")))

tandil %>% 
  rename("q10" = q) %>% 
  ggplot(aes(date, q10))+ 
  geom_line()+
  # geom_quantile(quantiles = c(0.5),
  #               formula = NULL,
  #               method = "rqss") +
  geom_point(data = tan_q, aes(x = date, y = q10))

