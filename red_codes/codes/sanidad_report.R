## ---- include=FALSE------------------------------------------------------
pacman::p_load(tidyverse, lmerTest, emmeans)
options(gargle_oauth_email = "edwardsmolina@gmail.com")


## ------------------------------------------------------------------------
url_dis <- "https://docs.google.com/spreadsheets/d/19UFhIkIWhRYS_hnQNGg4yKeNOpmMp0am3GUGfFTw6s0/edit?usp=sharing"
id_trial <- "2019_tr_mad"
raw_data <-googlesheets4::read_sheet(url_dis, sheet=id_trial) 


## ------------------------------------------------------------------------
raw_data %>%
  pivot_longer(-(trt:pl), 
               names_to = "org_var", values_to = "y") %>% 
  separate(org_var, c("org", "var")) %>% 
  mutate_at(vars(trt:var), as.factor) %>% 
  pivot_wider(names_from = var, values_from = y) %>% 
  # filter(stringr::str_detect(org, "b")) %>% # elimina tallo
  # filter(!(is.na(af))) %>%  # elimina hojas perdidas
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% # rellena con 0
  select(everything()) -> dat 


## ------------------------------------------------------------------------
# Retencion foliar en estrato < HB-2
dat %>%
  group_by(trt, bq, org) %>% 
    # mutate(
    # af_p = case_when(
    # af == 0 ~ (25+0)/2,
    # af == 1 ~ (50+25)/2,
    # af == 2 ~ (75+50)/2,
    # af == 3 ~ (100+75)/2, 
    # af = TRUE ~ af)) %>%
  summarise(af_p = mean(af, na.rm = TRUE)) -> leaf_dat


## ------------------------------------------------------------------------
# incidencia media a nivel de parcela/órgano de cada enfermedad

dat %>% select(-af) %>% 
  group_by(trt, bq, org) %>% 
  summarise_if(is.numeric, 
               list(~round(mean(.>0, na.rm = TRUE)*100))) -> inc_dat

# severidad media a nivel de parcela/órgano de cada enfermedad
dat %>% select(-af) %>%
  group_by(trt, bq, org) %>% 
  summarise_if(is.numeric, 
               list(~round(mean(., na.rm = TRUE),1))) -> sev_dat

## Unificar tablas
leaf_dat %>%      # tabla con variables experimentales y af 
  left_join(by = c("trt", "bq", "org"), 
            (inc_dat %>%   # tabla con variables de enfermedad 
              left_join(sev_dat, 
                        by = c("trt", "bq","org"), 
                        suffix=c('_inc', '_sev')))
  ) %>% ungroup %>% 
  mutate(af_a = af_p - rowSums(dplyr::select(., matches('sev'))), 
         af_a = if_else(af_a < 0, 0, af_a), 
         ma_int = (ma_inc/100) * (ma_sev/100),
         re_int = (re_inc/100) * (re_sev/100)) -> dat_foliar


## ------------------------------------------------------------------------
# set de hojas 
h012= c("b0","b1","b2"); 


## ------------------------------------------------------------------------
position_jitterdodge(
  jitter.width = 0.1,
  jitter.height = 0,
  dodge.width = 0.1,
  seed = NA
)
theme_set(theme_bw())

p_inc1 <- dat_foliar %>% 
  filter(org %in% h012) %>%  
  ggplot(aes(x=trt, y=ma_inc))+
  stat_summary(fun.data = "mean_cl_boot", colour = "grey30", 
               size = 0.2)+
  geom_point(aes(fill=org), alpha=0.5, size=2, 
             pch = 21, position = position_jitterdodge())+
  labs(y = "Incidencia %", x="Tratamientos", fill="Hojas")


## ------------------------------------------------------------------------
tran <- make.tran("asin.sqrt", 100)
mod_inc1 <- with(tran, 
                 lmer(linkfun(ma_inc) ~ trt + (1|bq), 
                      data=dat_foliar %>% filter(org %in% h012)))
# anova(mod1)
# emmeans(mod1, ~ trt, type = "response")

# par(mfrow=c(1,2)); plot(mod2, which = 1:2) ; layout(1)


## ------------------------------------------------------------------------
p_sev1 <- dat_foliar %>% 
  filter(org %in% h012) %>% 
  ggplot(aes(x=trt, y=ma_sev))+
  stat_summary(fun.data = "mean_cl_boot", size = 0.2)+
  geom_point(aes(fill=org), alpha=0.5, size=2, 
             pch = 21, position = position_jitterdodge())+
  labs(y = "Severidad %", x="Tratamientos", col="Hojas")


## ------------------------------------------------------------------------
mod_sev1 <- with(tran, 
                 lmer(linkfun(ma_sev) ~ trt + (1|bq), 
                      data=dat_foliar %>% filter(org %in% h012)))
# anova(mod_sev1)
# emmeans(mod_sev1, ~ trt, type = "response")


## ------------------------------------------------------------------------
p_inc2 <- dat_foliar %>% 
  filter(org %in% h012) %>% 
  ggplot(aes(x=trt, y=re_inc))+
  stat_summary(fun.data = "mean_cl_boot", colour = "grey30", size = 0.2)+
  geom_point(aes(fill=org), alpha=0.5, size=2, 
             pch = 21, position = position_jitterdodge())+  
  labs(y = "Incidencia %", x="Tratamientos", col="Hojas")


## ------------------------------------------------------------------------
mod_inc2 <- with(tran, 
                 lmer(linkfun(re_inc) ~ trt + (1|bq), 
                      data=dat_foliar %>% filter(org %in% h012)))
# anova(mod_inc2)
# emmeans(mod_inc2, ~ trt, type = "response")



## ------------------------------------------------------------------------
p_sev2 <- dat_foliar %>% 
  filter(org %in% h012) %>% 
  ggplot(aes(x=trt, y=re_sev))+
  stat_summary(fun.data = "mean_cl_boot", size = 0.2)+
  geom_point(aes(fill=org), alpha=0.5, size=2, 
             pch = 21, position = position_jitterdodge())+  
  labs(y = "Severidad %", x="Tratamientos", col="Hojas")



## ------------------------------------------------------------------------
mod_sev2 <- with(tran, 
                 lmer(linkfun(re_sev) ~ trt + (1|bq), 
                      data=dat_foliar %>% filter(org %in% h012)))
anova(mod_sev2)
emmeans(mod_sev2, ~ trt, type = "response")


## ------------------------------------------------------------------------
p_inc3 <- dat_foliar %>% 
  filter(org %in% h012) %>% 
  ggplot(aes(x=trt, y=rh_inc))+
  stat_summary(fun.data = "mean_cl_boot", colour = "grey30", size = 0.2)+
  geom_point(aes(fill=org), alpha=0.5, size=2, 
             pch = 21, position = position_jitterdodge())+  
  labs(y = "Incidencia %", x="Tratamientos", fill="Hojas")


## ------------------------------------------------------------------------
mod_inc3 <- with(tran, 
                 lmer(linkfun(rh_inc) ~ trt + (1|bq), 
                      data=dat_foliar %>% filter(org %in% h012)))
# anova(mod_inc3)
# emmeans(mod_inc3, ~ trt, type = "response")



## ------------------------------------------------------------------------
p_sev3 <- dat_foliar %>% 
  filter(org %in% h012) %>% 
  ggplot(aes(x=trt, y=rh_sev))+
  stat_summary(fun.data = "mean_cl_boot", colour = "grey30", size = 0.2)+
  geom_point(aes(fill=org), alpha=0.5, size=2, 
             pch = 21, position = position_jitterdodge())+  
  labs(y = "Severidad %", x="Tratamientos", col="Hojas")



## ------------------------------------------------------------------------
mod_sev3 <- with(tran, 
                 lmer(linkfun(rh_sev) ~ trt + (1|bq), 
                      data=dat_foliar %>% filter(org %in% h012)))
# anova(mod_sev3)
# emmeans(mod_sev3, ~ trt, type = "response")


## ------------------------------------------------------------------------
p_af <- leaf_dat %>% 
  filter(org %in% "infe") %>% 
  ggplot(aes(x=trt, y=af_p))+
  stat_summary(fun.data = "mean_cl_boot", colour = "red", 
               size = 0.2)+
  geom_jitter(alpha=0.5, width=0.1)+
  labs(y = "Hojas verdes (media) ", x="Tratamientos")


## ------------------------------------------------------------------------
mada <- googlesheets4::read_sheet("1hC8TSs3hJVMEDTlO_yulRRHL_FlI7wLV2XAdm45jGZw", 
                                 sheet="mada",
                                 col_types="cccddddddd")
devtools::source_url("https://bit.ly/2P0pDB8")#rinde_aj

mada <- mada %>% 
  mutate(pos=str_sub(par, 2, 3)) %>% 
  # mutate(x=as.numeric(pos)*3, y=as.numeric(bq)*6) %>% 
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(par, pos, everything())  
# mada

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


## ------------------------------------------------------------------------
mada1 %>% 
  # filter(trt %in% red) %>% 
  ggplot(aes(pos, bq, fill= rinde_aj)) + 
  geom_tile() +
  geom_text(
    # aes(label = paste(trt, "\n", round(hum,1), "%")), 
    aes(label = trt), 
            col = "white", size = 3) +
  viridis::scale_fill_viridis(discrete=FALSE, direction = -1)+
  labs(x="Parcela", 
       y="Bloque", 
       fill = "Rendimiento\n(kg/ha)" )+
  theme_bw() 


## ------------------------------------------------------------------------
mada1 %>% 
  group_by(trt) %>% 
  summarise(yield = mean(rinde_aj), 
            sd = sd(rinde_aj)) %>% 
  arrange(yield) %>% 
  # filter(trt %in% red) %>% 
  ggplot(aes(x=fct_reorder(trt, yield, .desc = F), y=yield)) + 
  stat_summary(fun = mean, fun.min = mean, fun.max = mean, 
               geom = "crossbar", 
               size = 0.1)+  
  geom_pointrange(aes(ymin = yield-sd, ymax = yield+sd), size=.1)+
  geom_point(data=mada1 %>% mutate(yield=rinde_aj), 
             aes(x=trt, y=yield, col = bq))+
  scale_y_continuous(breaks = seq(6000, 9000, by = 500)) +
  labs(x="Tratamiento", y="Kg/ha (aj. 13,5%)", title= "Madariaga", color = "Bloque")+  
  theme_bw() + 
  coord_flip() -> p_mada
p_mada


## ------------------------------------------------------------------------
mod <- lm(rinde_aj ~ trt + bq, data=mada1 )
anova(mod)
# plot(mod)


## ------------------------------------------------------------------------
pred_mada = emmeans(mod, ~ trt, type = "response")

pred_mada %>% as.data.frame() %>% 
  arrange(-emmean) %>% 
  select(-df,-SE, rend_aj_13=emmean) %>% 
  mutate_if(is.numeric, list(~round(.,0))) %>% 
  knitr::kable()


## ---- eval=FALSE---------------------------------------------------------
## pwpp(pred_mada, type = "response", reversed = T)


## ---- eval=FALSE---------------------------------------------------------
## pwpp(pred_mada, method = "trt.vs.ctrl1", type = "response", side = ">")

