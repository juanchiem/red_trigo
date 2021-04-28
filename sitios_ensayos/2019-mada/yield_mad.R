library(tidyverse)

r_aj <- "https://bit.ly/2P0pDB8"
theme_j <- "https://github.com/juanchiem/R-sources/blob/master/theme_juan.R?raw=TRUE"
assum_lm <- "https://github.com/juanchiem/R-sources-Juan/blob/master/lm_assumptions.R?raw=TRUE"
devtools::source_url(r_aj)
devtools::source_url(theme_j)
devtools::source_url(assum_lm)

# Dataset ####
mada <- googlesheets4::read_sheet("1hC8TSs3hJVMEDTlO_yulRRHL_FlI7wLV2XAdm45jGZw", 
                                 sheet="mada",
                                 col_types="cccddddddd")
mada <- mada %>% 
  mutate(pos=str_sub(par, 2, 3)) %>% 
  # mutate(x=as.numeric(pos)*3, y=as.numeric(bq)*6) %>% 
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(par, pos, everything())  
mada

red=c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L)

mada1 <- mada %>% 
  filter(!(is.na(peso_tot))) %>%  # elimina parcelas perdidas 
  rowwise() %>% 
  mutate(hum_rinde=list(rinde_aj2(m_muestra = m_muestra,      # m
                                  dist_surcos = 17.5, # cm
                                  peso_h = peso_h,     # g
                                  peso_s = peso_s,     # g
                                  tara_s = 2,       # g
                                  peso_tot = peso_tot,   # g
                                  h_deseada = 13.5)))%>% # %
  unnest(hum_rinde) %>% 
  
  mutate(
    area_cosecha = m_muestra*17.5/100,
    rinde = (peso_tot/area_cosecha)*10)
# mada1

# Visualización  ####

# Heatmap campo

mada1 %>% 
  filter(trt %in% red) %>% 
  ggplot(aes(pos, bq, fill= rinde_aj)) + 
  geom_tile() +
  geom_text(aes(label = paste(trt, "\n", round(hum,1), "%")), 
            col = "white", size = 3) +
  viridis::scale_fill_viridis(discrete=FALSE, direction = -1)+
  labs(x="Parcela", 
       y="Bloque", 
       fill = "Rendimiento\n(kg/ha)" )+
  theme_juan(9, "right") 

# Medias y dispersión

mada1 %>% 
  group_by(trt) %>% 
  summarise(yield = mean(rinde_aj), 
            sd = sd(rinde_aj)) %>% 
  arrange(yield) %>% 
  filter(trt %in% red) %>% 
  ggplot(aes(x=fct_reorder(trt, yield, .desc = F), y=yield)) + 
  stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean, 
               geom = "crossbar", 
               size = 0.1)+  
  geom_pointrange(aes(ymin = yield-sd, ymax = yield+sd), size=.1)+
  geom_point(data=mada1 %>% mutate(yield=rinde_aj) %>% filter(trt %in% red), 
             aes(x=trt, y=yield, col = bq))+
  scale_y_continuous(breaks = seq(6000, 9000, by = 500)) +
  labs(x="Tratamiento", y="Kg/ha (aj. 13,5%)", title= "Madariaga", color = "Bloque")+  
  theme_juan(9, "bottom") + 
  coord_flip() -> p_mada
p_mada

library(emmeans)
mod <- lm(rinde_aj ~ trt + bq, data=mada1, subset = trt %in% red)
anova(mod)
summary(mod)
assump(mod)

modm <- lme4::lmer(rinde_aj ~ trt + (1|bq), data=mada1, subset = trt %in% red)
plot(modm)
car::Anova(modm)

pred_mada = emmeans(modm, ~ trt, type = "response")
pwpp(pred_mada, Letters = letters, alpha = .05, type = "response", reversed = T)
pwpp(pred_mada, method = "trt.vs.ctrl1", type = "response", side = ">")
