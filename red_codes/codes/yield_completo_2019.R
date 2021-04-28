library(tidyverse) # purrr::map_df(), dplyr::glimpse()

# gs4_deauth()
# panel <- gs4_get("1-eCU4YfOiGz5Ox6RHX8CXDaQSUsmImEM_lQHAWtWybs")
# gs4_browse(red0)

red %>%  
  ggplot(aes(x=trat, y = rinde_aj/100)) +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean, 
               geom = "crossbar", size = 0.5, aes(col = factor(mezcla)))+  
  geom_point(size=1)+
  facet_wrap(~sitio, scales="fixed")+
  labs(x="", y="qq/ha", title = "Rendimiento ajustado 14% - Ensayo individual",
       subtitle = "MD_: epoxi. + azoxi.\nMT_: epoxi. + piraclo. + fluxapiroxad\n_32:Z32; _39:Z39")+
  cowplot::theme_minimal_grid()%+replace%
  theme(axis.text.x = element_text(angle = 90))+
  guides(col=F)

ggsave("~/Dropbox/5_Lineas_juan/Trigo/red/red_codes/plots_finales/yield.png",
       width = 6, height = 5)

# Model

pacman::p_load(lmerTest, emmeans, ggeffects)

## Análisis individual

# mada <- red %>% filter(sitio=="Madariaga")
# m1 = lm(rinde_aj ~ trat+bq, data=mada) 
# anova(m1)
# plot(m1)

mixed_model <- function(.) {
  lmer(rinde_aj ~ trat + (1|bq), data = .)
}

fits <- red %>% 
  select(-c(mezcla, trt, kg_trigo)) %>%
  nest(data = c(trat, bq, rinde_aj)) %>% 
  mutate(model = map(data, mixed_model),
         model_anova = map(data, ~anova(lmer(rinde_aj ~ trat + (1|bq), .))),
         emm = map(data, ~emmeans(lmer(rinde_aj ~ trat + (1|bq), .), "trat")),
         cv = map(data, ~agricolae::cv.model(aov(rinde_aj~trat+bq, data=.)))
                  )
out_fits <- fits %>% 
  mutate(tidy_model = map(model_anova, broom::tidy)) %>% #,
  # model_qual = map(model, MuMIn::r.squaredGLMM)) %>% 
  select(sitio, tidy_model) %>%
  unnest(c(tidy_model)) %>% 
  left_join((fits %>% 
              select(sitio, cv) %>%
              unnest(cv)), by="sitio")
out_fits

out_fits %>% select(sitio, p.value, cv) %>%  
  sheet_write(ss = panel, sheet = "anovas")
  

fits %>% 
  mutate(emmeans = map(emm, broom::tidy)) %>% 
  select(sitio, emmeans) %>%
  unnest(c(emmeans)) -> estimates

## Análisis conjunto 

# probando interacccion trat - sitio
# mod0 <- lmer(rinde_aj ~ trat*sitio+ (1|sitio:bq), 
#              data=red %>% filter(!sitio=="Barrow")) 
# anova(mod0)
# drop1(mod0)

# sitio efecto aleatorio 
mod1 <- lmer(rinde_aj ~ trat +(1|sitio/bq), 
             data= red %>% filter(!sitio=="Barrow"))
# AIC(mod0, mod1) # mejor el mod0
car::Anova(mod1, test = "F")
summary(mod1)

# Diagnosticos
mod1F <- fortify.merMod(mod1)

cowplot::plot_grid(ggplot(mod1F, aes(.fitted,.resid)) + 
                     geom_point(colour="blue") +
                     geom_hline(yintercept=0),
                   ggplot(mod1F, aes(sample = .resid)) + geom_qq() + stat_qq_line()
)

fixef(mod1)

pred.mm <- ggpredict(mod1, terms = c("trat"))  # this gives overall predictions for the model

pred.mm %>% as_data_frame() %>%
  left_join(id_trt %>% rename(x=trat), by="x") %>% 
  mutate(dif_test= predicted-first(predicted),
         ganancia = dif_test-kg_trigo) %>% 
  select(estrategia = x, Rendimiento = predicted, dif_test, kg_trigo, ganancia) %>% 
  mutate_if(is.numeric, funs(round(.))) %>% 
  knitr::kable()

p1 <- tibble(pred.mm) %>% #filter(sitio == "MdP") %>%
  ggplot()+ 
  geom_pointrange(aes(x = x, 
                      y = predicted,
                      ymin = conf.low, 
                      ymax = conf.high), 
                  fill = "lightgrey", alpha = 0.5) +  # error band
  geom_text(aes(x = x, 
                y = predicted,
                label = round(predicted,0), 
                hjust = 1.2))+
  geom_point(data =
               red %>% filter(!sitio=="Barrow") %>% 
               group_by(sitio, trat) %>% 
               summarise(predicted=mean(rinde_aj, na.rm = T)) %>% 
               ungroup %>% 
               rename(x=trat), 
             aes(x = x, 
                 y = predicted, col=sitio)) + 
  # lims(y=c(5000,8000))+
  labs(x = "", y ="Kg/ha", 
       title= "Análisis global del rendimiento", 
       subtitle= "\nEn negro: rendimiento medio y su IC 95% (ajustado a 14%H)", 
       col = "Sitio Exp.")+
  # caption = paste(mezcla_doble,"\n",mezcla_triple)) +
  cowplot::theme_minimal_grid() %+replace% 
  theme(axis.text.x=element_text(angle=60, hjust=1, vjust=1))#, 
        # legend.position="bottom")

library(patchwork)
p1 + gridExtra::tableGrob(dif_check)

ggsave("~/Dropbox/5_Lineas_juan/2 Trigo/red/red_codes/plots_finales/yield_model.png",
       width = 7, height = 5)

pred.mm %>% #names() 
  select(trat = "x", Rendimiento = "predicted", IC_inf ="conf.low", IC_sup="conf.high") %>% 
  mutate(dif_check = fixef(mod1),
         dif_check = replace(dif_check, trat=="Check", 0)) %>%
  mutate_if(is.numeric, ~round(.)) -> pred_mod

# pred_mod  %>%
#   sheet_write(ss = panel, sheet = "emmeans")

# red$trat <- relevel(red$trat, ref='MT32')
# mod2 <- lmer(rinde_aj ~ trat + (1|sitio/bq), data= red)
# # %>% filter(sitio!="tan"))
# summary(mod2)

emm1 = emmeans(mod1, specs = ~ trat)
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

# economics 
red_sum <- red %>% 
  group_by(sitio, trat) %>% 
  summarise(yield=median(rinde_aj, na.rm = T,)) %>% 
  ungroup %>% 
  spread(trat, yield) %>% 
  gather(trat, yield, -`Check`, -sitio) %>% 
  dplyr::select(-`Check`, everything()) %>% 
  arrange(sitio, trat) %>% 
  mutate(dif_check = yield - `Check`) %>% 
  left_join(id_trt, by="trat") 

mezcla_doble <- "MD: Mezcla doble 0,5 l/ha - epoxiconazole + azoxistrobin"
mezcla_triple <-"MT: Mezcla triple 1,2 l/ha - epoxiconazole + pyraclostrobin + fluxapyroxad"

pred_mod %>% filter(!trat=="Check") %>% 
  left_join(id_trt, 
            # %>% pivot_longer(-(trt:trat), names_to= "precio_trigo", values_to = "valor"), 
            by="trat") %>% 
  mutate(result = case_when(dif_check - kg_trigo > 0 ~ "blue",
                            TRUE~"red"))-> pred_mod1

red_sum %>% filter(!sitio=="Barrow") %>% 
  ggplot(aes(x=trat,
             # x=fct_reorder(trat, as.numeric(trt), .desc = T), 
             y = dif_check))+
  
  geom_point(aes(x=factor(trat), y=kg_trigo), col="black", shape="|", size=10) + 
  coord_flip()+
  geom_point(data = pred_mod1,  shape="-",
             aes(ymin = kg_trigo, ymax = dif_check))+
  geom_linerange(data = pred_mod1, 
                 aes(ymin = IC_inf, ymax = IC_sup, col = result), size=2)+
  
  geom_text(data = pred_mod1, aes(y = dif_check - (dif_check - kg_trigo)/2, 
                                  label = paste0(dif_check - kg_trigo)),
            hjust = 1.5)+
  geom_point(aes(fill=sitio), alpha=0.5, size=2, pch = 21)+
  labs(title = "Costo-beneficio de los tratamientos", 
       subtitle= "En negro: respuesta media (punto) y costo de aplicación (*)",  
       y ="Diferencia con el control (kg de trigo)",
       x = "", fill = "")+
  scale_color_manual(values = c("steelblue", "red"))+
  # caption = paste(mezcla_doble,"\n",mezcla_triple))+
  cowplot::theme_minimal_grid() %+replace% 
  theme(#axis.text.x=element_text(angle=60, hjust=1, vjust=1),
    plot.caption = element_text(hjust = 0, size = 10, face= "italic"))

library(patchwork)

pred_mod1 %>% 
  ggplot(aes(x=fct_reorder(trat, as.numeric(trt), .desc = T), 
             y = dif_check-kg_trigo))+
  geom_col(aes(fill = (dif_check-kg_trigo)>0), size=2)+
  coord_flip()+
  geom_text(aes(y = dif_check - kg_trigo, label = paste0(" ", dif_check - kg_trigo, " ")),
            hjust = c(1.1,1.1,1.1,0.3,0.5), 
            size=4, fontface = 2, 
            col = as.character(factor(c(1,1,1,2,2),1:2, c('white', 'black'))))+
  labs(subtitle = "b) 174 US$/T",
       y ="Beneficio (kg de trigo/ha)",
       x = "", fill = "")+
  scale_fill_manual(values = c("red2", "steelblue"), guide=F)+
  cowplot::theme_minimal_grid() %+replace% 
  theme(
    # axis.title.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),  
    panel.border = element_rect(color='grey'),
    axis.line    = element_line(color='grey')) -> p2

pred_mod1 %>% 
  mutate(kg_trigo_low = c(192L, 192L, 311L, 311L, 503L)) %>%  
  ggplot(aes(x=fct_reorder(trat, as.numeric(trt), .desc = T), 
             y = dif_check-kg_trigo_low))+
  geom_col(aes(fill = (dif_check-kg_trigo_low)>0), size=2)+
  coord_flip()+
  geom_text(aes(y = dif_check - kg_trigo_low, 
                label = paste0(" ", dif_check - kg_trigo_low, " ")),
            hjust = c(1.1,1.1,1.1,-0.1,0.5), 
            size=4, fontface = 2, 
            col = as.character(factor(c(1,1,1,2,2),1:2, c('white', 'black'))))+
  labs(subtitle= "a) 157 US$/T",
       y ="",
       x = "", fill = "")+
  scale_fill_manual(values = c("red2", "steelblue"), guide=F)+
  cowplot::theme_minimal_grid() %+replace% 
  theme(
    panel.border = element_rect(color='grey'),
    axis.line    = element_line(color='grey')) -> p1

pred_mod1 %>% 
  mutate(kg_trigo_up = c(157L, 157L, 255L, 255L, 411L)) %>%  
  ggplot(aes(x=fct_reorder(trat, as.numeric(trt), .desc = T), 
             y = dif_check-kg_trigo_up))+
  geom_col(aes(fill = (dif_check-kg_trigo_up)>0), size=2)+
  coord_flip()+
  geom_text(aes(y = dif_check - kg_trigo_up, label = paste0(" ", dif_check - kg_trigo_up, " ")),
            hjust = c(1.1,1.1,1.1,0.1,1.1),
            size=4, fontface = 2, 
            col = as.character(factor(c(1,1,1,2,1),1:2, c('white', 'black'))))+
  labs(subtitle= "c) 191 US$/T",
       y ="",
       x = "", fill = "")+
  scale_fill_manual(values = c("red2", "steelblue"), guide=F)+
  cowplot::theme_minimal_grid() %+replace% 
  theme(
    axis.title.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),  
    panel.border = element_rect(color='grey'),
    axis.line    = element_line(color='grey')) -> p3


p1 + p2 + p3 + plot_annotation(
  title = 'Resultado económico de las estrategias',
  subtitle = "Costo dosis (US$): MD=19, MT=34 (+5 US$/aplicación)
Cotización trigo: b) Real de Oct-Nov 2019; a)-10%; c)+10%",
  caption = 'Las barras representan la diferencia "respuesta de la estrategia" - "costo de su aplicación"',
  theme = theme(plot.title = element_text(size = 16, face = "bold"))
)
ggsave("~/Dropbox/5_Lineas_juan/2 Trigo/red/red_codes/plots_finales/econom.png",
       width = 7, height = 4)


resp_sum <- 
  red_sum %>% 
  group_by(sitio) %>% 
  top_n(n=1, wt = dif_check) %>% 
  mutate(respuesta = dif_check/`Check`*100, 
         perdida = dif_check/yield*100, 
         ganancia = dif_check-kg_trigo) %>% 
  dplyr::select(trat,yield,`Check`, dif_check, respuesta, perdida) #%>% 
# knitr::kable()

red_sum %>% 
  mutate(gan = dif_check - kg_trigo) %>% 
  group_by(sitio) %>% 
  top_n(n=1, wt = gan)  %>% 
  knitr::kable()

tabla_resp <- red_sum %>% 
  group_by(sitio, trat) %>% 
  # top_n(n=1, wt = dif) %>% 
  mutate(resp = dif_check/`Check`*100,
         gan = dif_check - kg_trigo)

dif_check <- 
  data.frame(round(with(tabla_resp, 
                        tapply(dif_check, 
                               list(sitio=sitio, trat=trat), mean)),1)) %>%
  rownames_to_column(var="Sitio") %>%
  # bind_rows(.,summarise_if(is.numeric, mean, na.rm = TRUE))
  sheet_write(ss = panel, sheet = "dif_check")

panel <- gs4_get("https://docs.google.com/spreadsheets/d/1-eCU4YfOiGz5Ox6RHX8CXDaQSUsmImEM_lQHAWtWybs/edit?usp=sharing")
sheet_write(resp, ss = panel, sheet = "respuesta1")
