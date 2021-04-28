pacman::p_load(tidyverse, tidyselect, googlesheets4, broom, emmeans)

# Planilla sanitaria
url <- "https://docs.google.com/spreadsheets/d/19UFhIkIWhRYS_hnQNGg4yKeNOpmMp0am3GUGfFTw6s0/edit?usp=sharing"
da <-read_sheet(url, sheet="LP_23-11_Z70", col_types="ccccddddddddddddddddd") 

## Manipulación 
# dejar las observaciones crudas a nivel de submuestra/órgano en cada línea 
# eliminar submuestras no evaluadas 
# reemplazar NA's por 0

da %>%
  pivot_longer(-(par:pl), 
               names_to = "org_var", values_to = "y") %>% 
  separate(org_var, c("org", "var")) %>% 
  mutate_if(is.character, as.factor) %>% 
  pivot_wider(names_from = var, values_from = y) %>% 
  filter(stringr::str_detect(org, "b")) %>% # elimina tallo
  filter(!(is.na(af))) %>%  # elimina hojas perdidas
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% # rellena con 0
  select(-ry) -> datf 

## Cálculos 
# resumir af media a nivel de parcela/órgano 

datf %>%
  group_by(par, org) %>% 
    mutate(
    af_p = case_when(
    af == 0 ~ (25+0)/2,
    af == 1 ~ (50+25)/2,
    af == 2 ~ (75+50)/2,
    af == 3 ~ (100+75)/2, 
    af = TRUE ~ af)) %>%
  summarise(trt= first(trt),
            bq = first(bq), 
            af_p = mean(af_p, na.rm = TRUE)) %>% 
  select(trt, bq, everything()) -> af_dat

# incidencia media a nivel de parcela/órgano de cada enfermedad

datf %>% select(-af) %>% 
  group_by(par, org) %>% 
  summarise_if(is.numeric, 
               funs(round(mean(.>0, na.rm = TRUE)*100))) -> inc_dat

# severidad media a nivel de parcela/órgano de cada enfermedad
datf %>% select(-af) %>%
  group_by(par, org) %>% 
  summarise_if(is.numeric, 
               funs(round(mean(., na.rm = TRUE)))) -> sev_dat

## Unificar tablas
af_dat %>%      # tabla con variables experimentales y af 
  left_join(by = c("par", "org"), 
            (inc_dat %>%   # tabla con variables de enfermedad 
              left_join(sev_dat, 
                        by = c("par", "org"), 
                        suffix=c('_inc', '_sev')))
  ) %>% ungroup %>% 
  mutate(af_a = af_p - rowSums(dplyr::select(., matches('sev'))), 
         af_a = if_else(af_a < 0, 0, af_a), 
         ma_int = (ma_inc/100) * (ma_sev/100),
         re_int = (re_inc/100) * (re_sev/100)) -> dat_foliar

# set de hojas 
h012= c("b0","b1","b2")
h01 = c("b0","b1")
h12 = c("b1","b2")

dat_foliar %>% #
  filter(org %in% h012) %>% 
  GGally::ggpairs(columns = c(5:7, 9,10, 12,13), 
                  mapping = ggplot2::aes(colour=org), 
                  lower = list(continuous = "smooth")) 


assum_lm <- "https://github.com/juanchiem/R-sources-Juan/blob/master/lm_assumptions.R?raw=TRUE"
devtools::source_url(assum_lm)

## retencion foliar

dat_foliar %>% 
  filter(org %in% h012) %>% 
  ggplot(aes(x=trt, y=af_a))+
  geom_boxplot()+
  geom_point(alpha=0.5)


mod <- lm(
  # car::logit(af_a, percents=TRUE) ~ trt + bq, 
  af_a ~ trt + bq, 
          data = dat_foliar %>% filter(org %in% h012))

plot(mod, which = 3)

mod %>% assump

pred_bce = emmeans(mod, ~ trt, type = "response")
pwpp(pred_bce, type = "response")

# Manchas 

dat_foliar %>% #
  filter(org %in% h012) %>% 
  lm(re_int ~ trt + bq, data = .) -> mod1

emmeans(mod1, ~ trt, type = "response") %>% 
  pwpp()
  
  
  datf %>% #
  filter(org %in% h012) %>% 
  group_by(par) %>% 
  summarise(trt = first(trt),
            bq = first(bq),
            n = n(),
            af_v = mean(af>1, na.rm = TRUE)) %>% 
    ungroup() %>%
  mutate(
    order = as.integer(as.character(trt)),
    trt1 = fct_reorder(trt, order)
  ) %>% 
  ggplot(aes(x=trt1, y =100*af_v))+
  geom_boxplot(width = 0.1)+ 
    # stat_summary(fun.data = "mean_cl_boot", size = 0.2)+
  geom_jitter(aes(col=bq), width = 0.2)+
  labs(x="", 
       y="% de hojas HB, HB-1 y HB-2 fotosint. activas\n(>50% verdes)")+
  theme_bw()

dat_foliar %>% #
  filter(org %in% hj_012) %>% 
  group_by(par) %>% 
  summarise(trt = first(trt),
            bq = first(bq),
            n = n(),
            ma_inc = mean(ma_inc, na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(
    order = as.integer(as.character(trt)),
    trt1 = fct_reorder(trt, order)
  ) %>% 
  ggplot(aes(x=trt1, y =ma_inc))+
  geom_boxplot(width = 0.1)+ 
  # stat_summary(fun.data = "mean_cl_boot", size = 0.2)+
  geom_jitter(aes(col=bq), width = 0.2)+
  labs(x="", 
       y="Incidencia de mancha amarilla en HB, HB-1 y HB-2")+
  theme_bw()

hojas <- c(
  `b0` = "Hj B",
  `b1` = "Hj B-1",
  `b2` = "Hj B-2",
  `b3` = "Hj B-3"
)

dat_foliar %>% 
  ungroup() %>%
  mutate(
    order = as.integer(as.character(trt)),
    trt1 = fct_reorder(trt, order)
  ) %>% 
  ggplot(aes(x=trt1, y=af_p))+
  geom_point(size = 0.1) + 
  stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 0.2)+
  facet_grid(org~.,
             labeller = as_labeller(hojas))+
  labs(y="% de área verde", x="Tratamientos")+  
  theme_bw()

#  coord_flip()

mutate(
    trt_id = factor(case_when(
      trt == 1 ~ "Test",
      trt == 2 ~ "D32",
      trt == 3 ~ "D39",
      trt == 4 ~ "D32\nD39",
      trt == 5 ~ "T32",
      trt == 6 ~ "T39",
      trt == 7 ~ "T32\nT39",
      trt == 8 ~ "D32\nT39"
      # trt == 9 ~ "Ox32",             
      # trt == 10 ~ "Ox39",
      # trt == 11 ~ "Ox32\nOx39")
      ),
    trt = fct_relevel(trt, 
                       "Test", 
                       "D32","D39", "D32\nD39", 
                       "T32","T39", "T32\nT39",
                       "D32\nT39"
                       # "Ox32", "Ox39","Ox32\nOx39"
                      ),
  hj = case_when(
    hj == "hb0" ~ "Hb",
    hj == "hb1" ~ "Hb-1",
    hj == "hb2" ~ "Hb-2")) -> dat_sum 

      
dat_sum %>%
  ggplot(aes(x = forcats::fct_rev(hj), inc, fill=enf)) + 
  stat_summary(fun.y=mean,position=position_dodge(width=0.95),geom="bar")+
  coord_flip() +
  facet_grid(trt~.)+
  theme_bw()

dat_sum %>%
  ggplot(aes(x = forcats::fct_rev(hj), sev, fill=hj)) + 
  geom_boxplot()+
  coord_flip() +
  facet_grid(trt~enf)+
  theme_bw()

dat %>%  
  select(id, contains("inf")) %>% 
  # head
  group_by(id) %>% 
  summarise(pl_eval = n(), 
            hj_vd_tot = sum(inf_vde),
            hj_vd_pl = hj_vd_tot/pl_eval,
            c_mancha = sum(inf_ma)/hj_vd_tot,
            c_re = sum(inf_re)/hj_vd_tot, 
            c_rh = sum(inf_rh)/hj_vd_tot) %>% 
  mutate_at(4:7, funs(round(., 1)))-> dat_inf

dat_inf %>% 
  select(-(pl_eval:hj_vd_tot)) %>% 
  pivot_longer(-(id), 
               names_to = "var", values_to = "y") %>%
  separate(id, c("trt", "bq"))  %>%
  ggplot(aes(x = trt, y)) + 
  geom_boxplot()+
  coord_flip() +
  facet_wrap(~var, ncol=4, scales = "free" )+
  theme_bw()