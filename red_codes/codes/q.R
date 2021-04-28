meta_url <- "https://docs.google.com/spreadsheets/d/1-eCU4YfOiGz5Ox6RHX8CXDaQSUsmImEM_lQHAWtWybs/edit?usp=sharing"
metadata <- gsheet::gsheet2tbl(meta_url)  

id_trial <- metadata[menu(metadata %>% pull(id_ensayo), title="Seleccione ensayo"), 1] %>% pull

ensayos <- metadata %>% filter(id_ensayo %in% c("2019_tr_mad","2019_tr_01","2019_tr_05","2019_tr_03","2019_tr_09"))

mada <- nasapower::get_power(
  community = "AG",
  lonlat = c(ensayos$lon[1], ensayos$lat[1]),
  pars = c( "T2M_MIN", "T2M_MAX","ALLSKY_SFC_SW_DWN", "PRECTOT"),
  dates = c("2019-01-06", "2019-12-31"),
  temporal_average = "DAILY"
)
saveRDS(nasa, here::here("meteo", paste0(id_trial,"_nasa.rds")))

ensayos <- metadata %>% filter(id_ensayo == id_trial) 

condiciones <- ensayos %>% 
  select(localidad, contains("&")) %>% 
  rename_all(list(~str_replace(., "&", "")))

crono <- ensayos %>% 
  select(localidad, contains("@")) %>% 
  pivot_longer(-localidad, names_to = "evento", values_to = "fecha") %>% 
  mutate(evento = gsub(".*@","", evento), 
         fecha = lubridate::dmy(fecha)) %>% 
  group_by(localidad) %>% 
  mutate(dd_siembra = interval(min(fecha), fecha) %/% days(1)) %>%

    mutate(
    dd_spray1=interval(((.)%>% filter(evento=="A1")%>%pull(fecha)),fecha) %/% days(1),
    dd_spray2=interval(((.)%>% filter(evento=="A2")%>%pull(fecha)),fecha) %/% days(1))%>%   arrange(fecha)

feno  <- crono %>% 
  filter(str_detect(evento, 'Z')) %>% 
  mutate(evento = recode(evento, 
                         "Z0"="Siembra",
                         "Z99"="Cosecha"))

fecha_antesis <- filter(feno, evento == "Z65") %>% pull(fecha)

