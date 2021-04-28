arg <- readRDS(here::here("data", "arg_last.rds")) %>%
  mutate(dias =as.numeric( fecha - min(fecha)))

ARG_bars <- arg %>% 
  dplyr::select(fecha, `COVID positivos` = casos, Fallecidos=fallecidos)%>%  
  pivot_longer(-fecha, names_to = "var", values_to = "val") %>%
  ggplot(aes(fecha, val))+ 
  geom_bar(stat="identity", fill = "steelblue") +
  geom_text(aes(label=val), angle=90,size=3, fontface = 2,
            position = position_dodge(0.9), 
            vjust=0.7, hjust = 1.2, color="white")+
  facet_wrap(~var, scales = "free_y", ncol=1)+
  scale_x_date(limits = c(Sys.Date() - dias_epidemia, Sys.Date()),expand = c(0, 0),
               breaks = "2 days", minor_breaks = "1 day", 
               labels=scales::date_format("%d/%m"))+
  labs(y="", x="",
       title = "Casos confirmados en Argentina", 
       subtitle = paste("Datos disponibles al",format(as.Date(Sys.Date(), format = "%Y%m%d"), "%d/%m/%y")))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
ARG_bars