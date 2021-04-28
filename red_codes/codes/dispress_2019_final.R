pacman::p_load(tidyverse, googlesheets4) 
gs4_deauth()
panel <- gs4_get("1-eCU4YfOiGz5Ox6RHX8CXDaQSUsmImEM_lQHAWtWybs")
# gs4_browse(press)

dispress <- read_sheet(panel, sheet = "pres_enf",  range = "B3:K9") %>% 
  pivot_longer(-localidad, names_to = "var", values_to = "y") %>%   
  separate("var", c("enf", "Z"), sep = "_") %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(enf = as.factor(case_when(
    enf== "MA" ~ "Mancha amarilla",
    enf== "RE" ~ "Roya estriada",
    enf== "RH" ~ "Roya de la hoja")) %>% fct_relevel("Roya estriada", after = 1)
  )
  
dispress %>% 
  ggplot(aes(x=Z, y=y)) +
  geom_point(aes(col = localidad))+
  geom_line(aes(col = localidad, group = localidad), size=1.2)+
  facet_grid(~enf, scales="free")+
  labs(x = "Estadios fenológicos (escala Zadoks)", y = "Severidad media (%)", 
       col = "Localidad",
       title = "Evolución de presión de enfermedad por ensayo", 
       subtitle = "Severidad media de las  enfermedades en los testigos sin protección alrededor de Z32 (inicio encañazón),\nZ39 (hoja bandera expandida) y Z65 (antesis)"
       # caption = "\n¹Intensidad = incidencia * severidad --- (equivalente a severidad considerando tanto hojas enfermas como sanas)"
       )+
  cowplot::theme_minimal_grid() %+replace% 
  theme(plot.caption = element_text(hjust = 0, face= "italic", size=9), #Default is hjust=1
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot") +#NEW paramet
  # scale_x_discrete(expand = c(0.1,0.1))+
  scale_y_continuous(limits = c(0,30), breaks= scales::pretty_breaks())+
  geom_area(data = expand.grid(Z = 1:3, 
                               Presion = c("baja", "media", "alta"),
                               enf = c("Mancha amarilla", "Roya estriada", "Roya de la hoja")) %>%  
              mutate(y = c(rep(c(5,5,20), e=3), rep(rep(c(5,5,20), e=3), t=2)))%>% 
              mutate(Presion= fct_relevel(factor(Presion), "alta", "media", "baja")), 
            aes(fill = Presion), position = 'stack', alpha=0.1) +
  scale_fill_manual(values = c("red", "yellow", "seagreen3"))

ggsave("~/Dropbox/5_Lineas_juan/2 Trigo/red/red_codes/plots_finales/dis_press.png",
       width = 8, height = 4)  

pacman::p_load(tidyverse, googlesheets4) 
gs4_deauth()
press <- gs4_get("1-eCU4YfOiGz5Ox6RHX8CXDaQSUsmImEM_lQHAWtWybs")

yield <- read_sheet(press, sheet = "yield_level") %>% 
  rename(loca= "Localidad", cv= "&cultivar", fs="fecha@Z0", rinde="rinde lote") %>% 
  mutate_if(is.character, as.factor) 

yield %>% 
  mutate(loca = fct_reorder(loca, fs)) %>% 
  ggplot(aes(loca, rinde/100))+
  geom_hline(yintercept = c(30,50),linetype =2 )+
  geom_bar(stat="identity", fill = "steelblue") +
  geom_text(
    aes(x = loca, y =5,
        label=paste0(cv, "\n", fs %>% format('%d-%b'))),
    angle=90, 
    hjust = 'left', 
    position = position_dodge(0.9), 
    size=5, fontface = 2, color="white")+
  geom_text(aes(label=round(rinde/100,0)),
            position=position_dodge(width=0.9), vjust=-0.25)+
  cowplot::theme_minimal_grid() +
  labs(y="qq", x="",
       title = "Nivel de rendimiento", 
       subtitle = "Rendimiento medio del lote del productor, cultivar y fecha de siembra")+
  scale_y_continuous(breaks = seq(0,80,10), limits = c(0,80))

ggsave("~/Dropbox/5_Lineas_juan/Trigo/red/red_codes/red/yield_level.png",
       width =6, height = 5)  

