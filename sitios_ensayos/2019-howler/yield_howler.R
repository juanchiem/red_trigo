pacman::p_load(tidyverse)

dat <- read_csv("https://github.com/juanchiem/agro_data/raw/master/yield_wheat19.csv")
red_t=c(1L, 2L, 3L, 5L, 6L, 8L, 9L)

id_trt <- tibble::tribble(
  ~trt,           ~trat, ~kg_trigo,
     1,         "1.Check",         0,
     2,        "2.MD_z32",       172,
     3,        "3.MD_z39",       172,
     # 4,    "4.MD_z32_z39",       345,
     5,        "4.MT_z32",       280,
     6,        "5.MT_z39",       280,
     # 7,    "7.MT_z32_z39",       560,
     8, "6.MD_z32_MT_z39",       453,
     9, "7.Howler_z32",          205) %>%   
  mutate_at(vars(trt, trat), as.factor)

how = c("balc", "mdp")

red <- dat %>% 
  mutate_at(vars(sitio, trt), as.factor) %>%  
  filter(trt %in% red_t) %>% droplevels() %>%
  filter(sitio %in% how) %>% droplevels() %>% 
  left_join(id_trt, by="trt") %>% 
  mutate(rinde_aj = rinde_aj)%>%
  mutate_at(vars(sitio, trt, trat, bq), as.factor)%>%
  # mutate(sitio = fct_relevel(sitio, c("mada", "neco", "tan", "balc", "mdp")))%>% 
  dplyr::select(sitio, trt, trat, bq, rinde_aj, kg_trigo) 
  
red_g <- red %>% 
  mutate(Localidad = as.factor(case_when(
    sitio=="balc" ~ "Balcarce",
    sitio== "mdp" ~ "MdP")))
    
red_sum <- red %>% 
  group_by(sitio, trat) %>% 
  summarise(yield=mean(rinde_aj, na.rm = T)) %>% 
  ungroup %>% 
  spread(trat, yield) %>% 
  gather(trat, yield, -`1.Check`, -sitio) %>% 
  dplyr::select(-`1.Check`, everything()) %>% 
  arrange(sitio, trat) %>% 
  mutate(dif = yield - `1.Check`) %>% 
  left_join(id_trt, by="trat") 

# red_g %>% 
#   ggplot(aes(x=trat, y = rinde_aj/100)) +
#   stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean, 
#                geom = "crossbar", size = 0.5, aes(col = factor(mezcla)))+  
#   geom_point(size=1)+
#   facet_wrap(~Localidad, scales="fixed")+
#   labs(x="", y="qq/ha", title = "Rendimiento ajustado 14%")+
#   cowplot::theme_minimal_grid()+ ylim(60,90)+
#   guides(col=F)+
#   theme_bw() %+replace% 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1))  
# 
# ggsave("~/Dropbox/5_Lineas_juan/Trigo/red/red_codes/ensayos/howler/howler.png",
#        width = 6, height = 4)

resp_sum <- red_sum %>% 
  group_by(sitio) %>% 
  top_n(n=1, wt = dif) %>% 
  mutate(resp = dif/`1.Check`*100, 
         perdida = dif/yield*100, 
         gan = dif-kg_trigo) %>% 
  dplyr::select(trat,yield,`1.Check`,dif, resp, perdida) #%>% knitr::kable()

red_sum %>% 
  mutate(Localidad = as.factor(case_when(
    sitio=="balc" ~ "Balcarce",
    sitio== "mdp" ~ "MdP")))  %>% 
  ggplot(aes(x=fct_reorder(trat, as.numeric(trt), .desc = T), y =dif))+
  geom_point(aes(col=Localidad))+
  geom_point(aes(x=factor(trat), y=kg_trigo), col="red", shape=3) + 
  stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean, 
               geom = "crossbar", 
               size = 0.1)+  
  coord_flip()+
  labs(title = "Respuesta absoluta de fungicidas en trigo", 
       subtitle= "Sudeste de Buenos Aires - 2019",  
       y ="Diferencia con el control (kg de trigo)",
       x = "")+
  # caption = paste(mezcla_doble,"\n",mezcla_triple))+
  theme_bw() %+replace% 
  theme(#axis.text.x=element_text(angle=60, hjust=1, vjust=1),
    plot.caption = element_text(hjust = 0, size = 10, face= "italic"))

ggsave("~/Dropbox/5_Lineas_juan/Trigo/red/red_codes/ensayos/howler/profit.png",
       width = 7, height = 5)

pacman::p_load(lmerTest, ggeffects, emmeans)

mod_balc <- lmer(rinde_aj ~ trat + (1|bq), 
             data= red, subset=sitio=="balc")
anova(mod_balc)

pred_balc = emmeans(mod_balc, specs = ~ trat, type = "response")
res_balc <- multcomp::cld(pred_balc, Letters = letters, alpha = .1, type = "response", reversed = T)
plot(res_balc)

res_balc %>% #View 
  mutate(letra = as.factor(str_remove_all(.group, " "))) %>% 
  ggplot(aes(x=fct_reorder(trat, emmean, .desc = F), y=emmean)) + 
  geom_pointrange(aes(ymin = lower.CL, ymax = upper.CL,col=letra), size=.3)+
  geom_text(aes(label = str_remove_all(.group, " "),
                y = c(rep(0.17, 7))),
            size=2.5, position = position_dodge(0.9), vjust =0.5, hjust="inward")+
  labs(x=NULL, y="kg/ha", title = "Balcarce - rendimiento", color = "Grupos de\nperformance" )+
  scale_y_continuous(breaks=seq(0, 8000, 2000))+
  theme_bw() %+replace% 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1))  
ggsave("~/Dropbox/5_Lineas_juan/Trigo/red/red_codes/ensayos/howler/balc_yield.png",
       width = 6, height = 4)

mod_mdp <- lmer(rinde_aj ~ trat + (1|bq), 
             data= red, subset=sitio=="mdp")
anova(mod_mdp)
pred_mdp = emmeans(mod_mdp, specs = ~ trat, type = "response")
res_mdp <- multcomp::cld(pred_mdp, Letters = letters, alpha = .1, type = "response", reversed = T)

res_mdp %>% #View 
  mutate(letra = as.factor(str_remove_all(.group, " "))) %>% 
  ggplot(aes(x=fct_reorder(trat, emmean, .desc = F), y=emmean)) + 
  geom_pointrange(aes(ymin = lower.CL, ymax = upper.CL,col=letra), size=.3)+
  geom_text(aes(label = str_remove_all(.group, " "),
                y = c(rep(0.17, 7))),
            size=2.5, position = position_dodge(0.9), vjust =0.5, hjust="inward")+
  labs(x=NULL, y="kg/ha", title = "Mdp - rendimiento", color = "Grupos de\nperformance" )+
  scale_y_continuous(breaks=seq(0, 8000, 2000))+
  theme_bw() %+replace% 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1))  
ggsave("~/Dropbox/5_Lineas_juan/Trigo/red/red_codes/ensayos/howler/mdp_yield.png",
       width = 6, height = 4)

mod1 <- lmer(rinde_aj ~ trat + (1|sitio) + (1|sitio:bq), 
             data= red)
summary(mod1)
emm1 = emmeans(mod1, specs = ~ trat)

#z32 vs z39
z32 = c(0, 1, 0, 0, 1, 0, 0, 0, 0)
z39 = c(0, 0, 1, 0, 0, 1, 0, 0, 0)
contrast(emm1, method = list("z32 - z39" =z32-z39))

#MD vs MT
MD = c(0, 1, 1, 1, 0, 0, 0, 0)
MT = c(0, 0, 0, 0, 1, 1, 1, 0)
contrast(emm1, method = list("MD - MT" =MD-MT))

#Aplicacion simple vs doble
AP1 = c(0, 0.25, 0.25, 0, 0.25, 0.25, 0, 0)
AP2 = c(0, 0, 0, 0.333, 0, 0, 0.333, 0.333)
contrast(emm1, method = list("AP1 - AP2" = AP1 - AP2))

red$trat <- relevel(red$trat, ref='9.Howler_z32')
mod2 <- lmer(rinde_aj ~ trat + (1|sitio/bq), data= red)
# %>% filter(sitio!="tan"))
anova(mod1)
summary(mod2)

# Extract the prediction data frame
pred.mm <- ggpredict(mod1, terms = c("trat"))  # this gives overall predictions for the model
/# Plot the predictions 

pred.mm %>% as_data_frame() %>% 
  mutate(dif_test= predicted-first(predicted), 
         respuesta = ((predicted-first(predicted))/first(predicted)*100)) %>% 
  mutate_if(is.numeric, funs(round(.))) %>% 
  knitr::kable()

pred.mm %>% as_data_frame() %>%
  left_join(id_trt %>% rename(x=trat), by="x") %>% 
  mutate(dif_test= predicted-first(predicted),
         ganancia = dif_test-kg_trigo) %>% 
  select(estrategia = x, Rendimiento = predicted, dif_test, kg_trigo, ganancia) %>% 
  mutate_if(is.numeric, funs(round(.))) %>% 
  knitr::kable()
  
data.frame(pred.mm) %>% 
  ggplot()+ 
  geom_pointrange(aes(x = x, 
                      y = predicted,
                      ymin = conf.low, 
                      ymax = conf.high), 
                  fill = "lightgrey", alpha = 0.5) +  # error band
  geom_point(data =
               red_g%>% group_by(Localidad, trat) %>% 
               summarise(predicted=mean(rinde_aj, na.rm = T)) %>% 
               rename(x=trat), 
             aes(x = x, 
                 y = predicted, col=Localidad)) + 
  lims(y=c(5000,8500))+
  labs(x = "", y ="kg/ha", 
       title= "Rendimiento ajustado 14%", 
       subtitle= "Análisis global", 
       caption= "En negro: media estimada por el modelo y su intervalo de 95% de confianza")+
  cowplot::theme_minimal_grid() %+replace% 
  theme(axis.text.x=element_text(angle=60, hjust=1, vjust=1),
        plot.caption = element_text(hjust = 0, face= "italic",
                                  color = "gray30", size = 10))
ggsave("~/Dropbox/5_Lineas_juan/Trigo/red/red_codes/ensayos/howler/global_yield.png",
       width = 6, height = 4)
