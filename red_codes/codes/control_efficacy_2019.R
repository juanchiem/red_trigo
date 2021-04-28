library(tidyverse) # purrr::map_df(), dplyr::glimpse()
library(googlesheets4)

# gs4_deauth()
# red <- gs4_get("1HSoj5M--kcW-WioiwMbbngrqdkSC-bqpumeDIIlbx18")
# gs4_browse(red)

# red %>% sheet_names() %>% 
#   map_df(~ read_sheet(red, sheet = .)) %>% 
#   bind_rows()
# id_trt <- tibble::tribble(
#   ~trt,      ~trat, ~kg_trigo,
#   1,       "Check",         0,
#   2,        "MD32",       172,
#   3,        "MD39",       172,
#   4,     "MD32_39",       345,
#   5,        "MT32",       280,
#   6,        "MT39",       280,
#   7,     "MT32_39",       560,
#   8,   "MD32_MT39",       453
# )
# 
# dat0 <- data_frame(ensayo = red %>% sheet_names()) %>% 
#   mutate(data = map(ensayo, ~read_sheet(red, sheet = .x))) %>% 
#   tidyr::unnest(cols = c(data))

# dat <- dat0 %>%
#   filter(trt %in% c(1,2,3,5,6,8)) %>%
#   mutate(ensayo = as.factor(case_when(
#     ensayo == "mada" ~ "Madariaga",
#     ensayo == "neco" ~ "Necochea",
#     ensayo == "tan" ~ "Tandil",
#     ensayo == "balc" ~ "Balcarce",
#     ensayo == "mdp" ~ "MdP",
#     ensayo == "barrow" ~ "Barrow")) %>%
#   left_join(id_trt, by="trt")) %>% 
#   mutate_at(vars("trt", "bq"), as.factor)
#   
# dat %>% filter(sitio == "barrow")
#   

dp <- dat %>% filter(trt == 1) %>% 
  group_by(sitio, trt) %>% 
  # summarise(MA = mean(ma_sev)>5,
  #           RE = mean(re_sev)>5,
  #           RH = mean(rh_sev)>5)
  summarise(MA = mean(ma_inc/10*ma_sev/10)>5,
            RE = mean(re_inc/10*re_sev/10)>5,
            RH = mean(rh_inc/10*rh_sev/10)>5)

dat %>% 
  filter(trt %in% c(1)) %>%
  pivot_longer(tidyselect::contains("sev"), names_to = "var", values_to = "val") %>% 
  group_by(sitio, var) %>% 
  summarise(val = mean(val))

dat %>% 
  mutate(ma_int = ma_inc/10*ma_sev/10) %>% 
  ggplot(aes(x=trt, y=ma_int))+
  geom_point(aes(fill=bq), alpha=0.5, size=2, pch = 21, position = position_jitterdodge())+
  stat_summary(fun.data = "mean_cl_boot", size = 0.2)+
  facet_wrap("sitio")+
  labs(title = "Mancha amarilla", y = "%", x="", fill="bloques")

dat %>% #filter(sitio == "MdP")
  ggplot(aes(x=trt, y=re_inc/10*re_sev/10))+
  geom_point(aes(fill=bq), alpha=0.5, size=2, 
             pch = 21, position = position_jitterdodge())+
  stat_summary(fun.data = "mean_cl_boot", size = 0.2)+
  facet_wrap("sitio")+
  labs(title = "Roya estriada", y = "%", x="", fill="bloques")

dat %>% 
  ggplot(aes(x=trt, y=rh_inc/10*rh_sev/10))+
  geom_point(aes(fill=bq), alpha=0.5, size=2, 
             pch = 21, position = position_jitterdodge())+
  stat_summary(fun.data = "mean_cl_boot", size = 0.2)+
  facet_wrap("sitio")+
  labs(title = "Roya de la hoja", y = "%", x="", fill="bloques")

pacman::p_load(lme4, sjPlot, ggeffects)

# MA
MA_TRUE = dp %>% filter(MA == T) %>% pull(sitio) %>% as.character
dat %>%
  filter(sitio %in% MA_TRUE) %>% 
  mutate(ma_int = ma_inc/10*ma_sev/10) %>%
  lmer(ma_int ~ trat +(1|sitio/bq), data =.) -> mod_ma
car::Anova(mod_ma)
summary(mod_ma)  

plot(mod_ma)

# Extract the prediction data frame
pred.ma <- ggpredict(mod_ma, terms = c("trat"))  # this gives overall predictions for the model

# Plot the predictions 
data.frame(pred.ma) %>% 
  ggplot()+ 
  geom_point(data =
               dat %>% filter(sitio %in% MA_TRUE) %>% 
               mutate(ma_int = ma_inc/10*ma_sev/10) %>% 
               group_by(sitio, trat, bq) %>% 
               summarise(predicted=mean(ma_int, na.rm = T)) %>% 
               rename(x=trat), 
             aes(x = x, y = predicted, fill=sitio),
             alpha=0.8, size=2, pch = 21) + 
  geom_pointrange(aes(x = x, 
                      y = predicted,
                      ymin = predicted - std.error, 
                      ymax = predicted + std.error), 
                  fill = "lightgrey", alpha = 1) +  # error band
  labs(x = "Estrategia", y = "Severidad media (%)", fill = "Ensayo", 
       title = "Mancha amarilla") +
  cowplot::theme_minimal_grid() %+replace% 
  theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 0.9))

ggsave("~/Dropbox/5_Lineas_juan/2 Trigo/red/red_codes/plots_finales/mancha_amarilla.png",
       width = 6, height = 4)  
emm1 = emmeans(mod_ma, specs = ~ trat)
Ap2 = c(0,   0,   0,   0, 0, 1)
z32 = c(0,   0.5, 0,   0.5, 0, 0)
z39 = c(0,   0,   0.5,   0, 0.5, 0)
MD  = c(0,   0.5,   0.5,   0, 0, 0)
MT  = c(0,   0,   0,   0.5, 0.5, 0)

# Aplicacion doble vs z32
contrast(emm1, method = list("Ap2 - z32" = Ap2 - z32))
# Aplicacion doble vs z39
contrast(emm1, method = list("Ap2 - z39" = Ap2 - z39))
#z32 vs z39
contrast(emm1, method = list("z32 - z39" = z32-z39))
#MD vs MT
contrast(emm1, method = list("MT - MD"= MT-MD))

multcomp::cld(emm1, type = "response", reverse=T) %>% 
  as_tibble() %>% 
  # Calcular el cambio relativo de cada trt respecto a trt 1 (Control)
  mutate(respuesta = ((first(emmean)-emmean)/first(emmean)*100) %>% round(1) %>% 
           paste0(., "%"))%>%
  # Expresar en % con un decimal
  mutate_if(is.double, list(~round(.,1)))

# ggerrorplot(x = "trat", y = "ma_int", xtickslab.rt = 60,
#             title = "A - Mancha amarilla", xlab = "", ylab = "Intensidad (%)",
#             add = "dotplot", facet.by = "sitio", short.panel.labs = FALSE)+
#   stat_compare_means(method = "anova", label.x.npc = 'middle') +
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank())-> p1

# RE
RE_TRUE = dp %>% filter(RE == T) %>% pull(sitio) %>% as.character

dat %>% 
  filter(sitio %in% RE_TRUE) %>% 
  ggplot(aes(x=trat, y=re_inc/10*re_sev/10))+
  geom_point(aes(fill=bq), 
              alpha=0.5, size=2, pch = 21, position = position_jitterdodge(dodge.width = 0.1))+
  stat_summary(fun.data = "mean_cl_boot", size = 0.2)+
  facet_grid(.~sitio, scales = 'free')+
  labs(title = "Roya estriada", y = "Severidad media %", x="", fill="Bloque")+
  cowplot::theme_minimal_grid() %+replace% 
  theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 0.9))
ggsave("~/Dropbox/5_Lineas_juan/2 Trigo/red/red_codes/plots_finales/roya_estriada.png",
       width = 6, height = 4) 

dat %>%
  filter(sitio %in% "Barrow") %>% 
  mutate(re_int = re_inc/10*re_sev/10) %>% 
  lmer(re_int ~ trat +(1|bq), data =.) -> mod_re1
car::Anova(mod_re1)
summary(mod_re1)  

plot(mod_re)

# Extract the prediction data frame
pred.re1 <- ggpredict(mod_re1, terms = c("trat"))  # this gives overall predictions for the model

# Plot the predictions 
data.frame(pred.ma) %>% 
  ggplot()+ 
  geom_point(data =
               dat %>% filter(sitio %in% MA_TRUE) %>% 
               mutate(re_int = re_inc/10*re_sev/10) %>% 
               group_by(sitio, trat, bq) %>% 
               summarise(predicted=mean(re_int, na.rm = T)) %>% 
               rename(x=trat), 
             aes(x = x, y = predicted, fill=sitio),
             alpha=0.8, size=2, pch = 21) + 
  geom_pointrange(aes(x = x, 
                      y = predicted,
                      ymin = predicted - std.error, 
                      ymax = predicted + std.error), 
                  fill = "lightgrey", alpha = 1) +  # error band
  labs(x = "Estrategia", y = "Intensidad (%)", fill = "Ensayo", 
       title = "Mancha amarilla") +
  cowplot::theme_minimal_grid() %+replace% 
  theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 0.9))

ggsave("~/Dropbox/5_Lineas_juan/2 Trigo/red/red_codes/plots_finales/mancha_amarilla.png",
       width = 6, height = 4)  
emm1 = emmeans(mod_ma, specs = ~ trat)
Ap2 = c(0,   0,   0,   0, 0, 1)
z32 = c(0,   0.5, 0,   0.5, 0, 0)
z39 = c(0,   0,   0.5,   0, 0.5, 0)
MD  = c(0,   0.5,   0.5,   0, 0, 0)
MT  = c(0,   0,   0,   0.5, 0.5, 0)

# Aplicacion doble vs z32
contrast(emm1, method = list("Ap2 - z32" = Ap2 - z32))
# Aplicacion doble vs z39
contrast(emm1, method = list("Ap2 - z39" = Ap2 - z39))
#z32 vs z39
contrast(emm1, method = list("z32 - z39" = z32-z39))
#MD vs MT
contrast(emm1, method = list("MT - MD"= MT-MD))

multcomp::cld(emm1, type = "response", reverse=T) %>% 
  as_tibble() %>% 
  # Calcular el cambio relativo de cada trt respecto a trt 1 (Control)
  mutate(respuesta = ((first(emmean)-emmean)/first(emmean)*100) %>% round(1) %>% 
           paste0(., "%"))%>%
  # Expresar en % con un decimal
  mutate_if(is.double, list(~round(.,1)))


dat %>% 
  filter(sitio %in% RE_TRUE) %>% 
  mutate(re_int = re_inc/10*re_sev/10) %>% 
  ggerrorplot(x = "trat", y = "re_int", xtickslab.rt = 60,
            title = "B - Roya estriada", xlab = "", ylab = "Intensidad (%)",
            add = "dotplot", facet.by = "sitio", short.panel.labs = FALSE)+
  stat_compare_means(method = "anova", label.x.npc = 'middle') -> p2
# ggsave("~/Dropbox/5_Lineas_juan/Trigo/red/red_codes/plots_finales/re.png",
#        width = 7, height = 4)
# RH
# dat %>% 
#   filter(sitio %in% (dp %>% filter(RH==T) %>% pull(sitio))) %>% 
#   ggerrorplot(x = "trat", y = "rh_sev", xtickslab.rt = 60,
#             title = "B - Roya de la hoja", xlab = "", ylab = "",
#             add = "dotplot", facet.by = "sitio", short.panel.labs = FALSE)+
#   stat_compare_means(method = "anova", label.x.npc = 'middle') -> p2
# ggsave("~/Dropbox/5_Lineas_juan/Trigo/red/red_codes/plots_finales/rh.png",
#        width = 6, height = 5)

library(patchwork)
p1 / p2 + ggsave("control.png", width = 6, height = 6)

library(ggpubr)

# library(lmerTest)
# 
# mixed_model <- function(.) {
#   lmer(ma_int ~ trt + (1|bq), data = .)
# }
# 
# fixed_model <- function(.) {
#   lm(ma_int ~ trt + bq, data = .)
# }
# 
# fits_MA <- dat %>% 
#   filter(sitio %in% MA_TRUE) %>% 
#   mutate(ma_int = ma_inc/10*ma_sev/10) %>% 
#   select(c(sitio, trt, bq, ma_int)) %>%
#   nest(data = c(trt, bq, ma_int)) %>% 
#   mutate(model = map(data, fixed_model),
#          model_anova = map(data, ~car::Anova(lm(ma_int ~ trt + bq,.))))
#          # model_anova = map(data, ~car::Anova(lmer(ma_int ~ trt + (1|bq),.))))
# 
# out_fits <- fits_MA %>% 
#   mutate(tidy_model = map(model_anova, broom::tidy)) %>% #,
#   # model_qual = map(model, MuMIn::r.squaredGLMM)) %>% 
#   select(sitio, tidy_model) %>%
#   unnest(c(tidy_model)) 
# 
# out_fits
# 
# ma_tan <- dat %>% filter(sitio %in% "Tandil") %>% 
#   mutate(ma_int = ma_inc/10*ma_sev/10) 
# mod1 = lm(ma_int ~ trt + bq, data = ma_tan) 
# plot(mod1)
# anova(mod1)
# summary(mod1)
# 
# library(emmeans)
# emmeans(mod1, ~trt)
# 
