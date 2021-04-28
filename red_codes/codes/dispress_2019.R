pacman::p_load(tidyverse, googlesheets4) 
gs4_deauth()
press <- gs4_get("1-eCU4YfOiGz5Ox6RHX8CXDaQSUsmImEM_lQHAWtWybs")
# gs4_browse(press)

dat <- read_sheet(press, sheet = "pres_enf",  range = "B3:K9") %>% 
  pivot_longer(-localidad, names_to = "var", values_to = "val") %>%   
  separate("var", c("enf", "Z"), sep = "_")

dat %>% 
  mutate(enf = as.factor(case_when(
    enf== "MA" ~ "Mancha amarilla",
    enf== "RE" ~ "Roya estriada",
    enf== "RH" ~ "Roya de la hoja")) %>% fct_relevel("Roya estriada", after = 1)
    ) %>% 
ggplot()+
  geom_point(aes(x=Z, y = val, col = localidad))+
  geom_line(aes(x=Z, y = val, col = localidad, group = localidad), size=1.2)+
  facet_grid(~enf, scales="free")+
  # p + facet_grid(drv ~ cyl) + theme_bw()
  labs(x = "Estadios fenológicos (escala Zadoks)", y = "%", 
       col = "Localidad", 
       title = "Evolución de presión de enfermedad por ensayo", 
       subtitle = "Intensidad¹ de enfermedad en los testigos sin protección alrededor de Z32 (inicio encañazón),\nZ39 (hoja bandera expandida) y Z65 (antesis)", 
       caption = "¹Intensidad = incidencia * severidad")+
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 5, fill = "green", alpha = .1) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 5, ymax = 10, fill = "yellow", alpha = .1) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 10, ymax = Inf, fill = "red", alpha = .1) +
  cowplot::theme_minimal_grid() %+replace% 
  theme(plot.caption = element_text(hjust = 0, face= "italic", size=9), #Default is hjust=1
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot") +#NEW paramet
  scale_x_discrete(expand = c(0.1,0.1))+
  scale_y_continuous(breaks= scales::pretty_breaks())
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

