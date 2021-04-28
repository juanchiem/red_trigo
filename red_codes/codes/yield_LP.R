library(tidyverse)
r_aj <- "https://bit.ly/2P0pDB8"
devtools::source_url(r_aj)
theme_j <- "https://github.com/juanchiem/R-sources/blob/master/theme_juan.R?raw=TRUE"
devtools::source_url(theme_j)
devtools::source_url("https://github.com/juanchiem/R-sources-Juan/blob/master/lm_assumptions.R?raw=TRUE")

# Dataset ####

lp <- googlesheets4::read_sheet("1hC8TSs3hJVMEDTlO_yulRRHL_FlI7wLV2XAdm45jGZw", 
                                 sheet="LP",
                                 col_types="cccdddddd") 
lp <- lp %>% 
  mutate(pos=str_sub(par, 2, 3)) %>% 
  mutate(x=as.numeric(pos)*3, y=as.numeric(bq)*6) %>% 
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(par, pos, everything())  
# lp

lp1 <- lp %>% 
  filter(!(is.na(peso_tot))) %>%  # elimina parcelas perdidas 
  rowwise() %>% 
  mutate(hum_rinde=list(rinde_aj2(m_muestra =  10,      # m
                        dist_surcos = 21, # cm
                        peso_h = peso_h,     # g
                        peso_s = peso_s,     # g
                        tara_s = 2,       # g
                        peso_tot = peso_tot,   # g
                        h_deseada = 13.5)))%>% # %
  unnest(hum_rinde) 

# Visualización  ####
# Heatmap campo
red=c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L)

lp1 %>% 
  ggplot(aes(pos, bq, fill= rinde_aj)) + 
  geom_tile() +
  geom_text(aes(label = paste(trt, "\n", round(hum,1), "%")), 
            col = "white", size = 3) +
  viridis::scale_fill_viridis(discrete=FALSE, direction = -1)+
  labs(x="Parcela", y="Bloque",fill = "Rendimiento\n(kg/ha)" )+
  theme_juan(9, "right") 

# Medias y dispersión

lp1 %>% 
  group_by(trt) %>% 
  summarise(yield = mean(rinde_aj), 
            sd = sd(rinde_aj)) %>% 
  arrange(yield) %>% 
  filter(trt %in% red) %>% 
  ggplot(aes(x=fct_reorder(trt, yield, .desc = F), y=yield)) + 
  stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", 
               size = 0.1)+  
  geom_pointrange(aes(ymin = yield-sd, ymax = yield+sd), size=.1)+
  geom_point(data=lp1 %>% mutate(yield=rinde_aj) %>% filter(trt %in% red), 
             aes(x=trt, y=yield, col = bq))+
  labs(x="Tratamiento", y="Kg/ha (aj. 13,5%)", title= "Los Pinos - Balcarce",
       color = "Bloque")+  
  theme_juan(9, "bottom") + 
  coord_flip() -> p_lp
p_lp

# model
library(nlme)
m0 <- gls(rinde_aj ~ trt + bq, data = lp1)  
plot(Variogram(m0, form=~x+y))
m1 <- update(m0, corr=corSpher(c(15, 0.25), form=~x+y, nugget=TRUE))
anova(m0, m1)


library(emmeans)

mod <- lm(rinde_aj ~ trt + bq, data=lp1)
anova(mod)
summary(mod)
assump(mod)

mod1 <- lm(log(rinde_aj) ~ trt + bq, data=lp1)
anova(mod1)
assump(mod1)
summary(mod1)

modm <- lme4::lmer(log(rinde_aj) ~ trt + (1|bq), data=lp1)
plot(modm)
car::Anova(modm)

pred_lp = emmeans(modm, ~ trt, type = "response")
res_tr <- pwpp(pred_tr, Letters = letters, alpha = .05, type = "response", reversed = T)
emm = emmeans(pigs.lm, ~ percent | source)
pwpp(emm)
pwpp(pred_lp, method = "trt.vs.ctrl1", type = "response", side = ">")

res_tr %>% #View 
  # filter(year==2015) %>% 
  ggplot(aes(x=fct_reorder(trt, emmean, .desc = F), y=emmean)) + 
  geom_pointrange(aes(ymin = lower.CL, ymax = upper.CL), size=.3)+
  geom_text(aes(label = str_remove_all(.group, " "),
                y = c(rep(0.17, 15))),
            size=2.5, position = position_dodge(0.9), vjust =0.5, hjust="inward")+
  # labs(x=NULL, y="Proportion of severely damaged pods")+
  # scale_y_continuous(breaks=seq(0.2, 0.5, 0.05))+
  coord_flip()+
  theme(plot.margin = unit(c(0.1,0.1,0,0), "cm"))
