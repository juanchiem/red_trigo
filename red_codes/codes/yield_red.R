pacman::p_load(tidyverse)
#https://ourcodingclub.github.io/tutorials/mixed-models/
sheets_to_read <- readxl::excel_sheets("red/red_data.xlsx")

dat <- bind_rows(lapply(1:length(sheets_to_read),
                        function(i)readxl::read_excel("red/red_data.xlsx",
                                                      sheet = sheets_to_read[i]) %>%
                          mutate(sitio = sheets_to_read[i]))) %>%
  select(sitio, everything())
# # write_csv(dat, "yield_wheat19.csv")
# save(dat, file="red_data.Rda")
# load("red/red_data.Rda")
dat <- read_csv("https://github.com/juanchiem/agro_data/raw/master/yield_wheat19.csv")

# red_t=c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L)
red_t=c(1L, 2L, 3L, 5L, 6L, 8L)

id_trt <- tibble::tribble(
  ~trt,      ~trat, ~kg_trigo,
  1,       "Check",         0,
  2,        "MD32",       172,
  3,        "MD39",       172,
  4,     "MD32_39",       345,
  5,        "MT32",       280,
  6,        "MT39",       280,
  7,     "MT32_39",       560,
  8,   "MD32_MT39",       453
) %>%   
  mutate_at(vars(trt, trat), as.factor)

# red <- bind_rows(list(lp1, mada1, mdp1, nec1, tan1), 
#           .id = "id") %>% 
#   mutate(Localidad = as.factor(case_when(
#       id == 1 ~ "Balcarce_1",
#       id == 2 ~ "Madariaga",
#       id == 3 ~ "MdP",
#       id == 4 ~ "Necochea",
#       id == 5 ~ "Tandil",
#     ))) 

red <- dat %>% 
  mutate_at(vars(sitio, trt), as.factor) %>%  
  filter(trt %in% red_t) %>% droplevels() %>% 
  left_join(id_trt, by="trt") %>% 
  mutate_at(vars(sitio, trt, trat, bq), as.factor)%>% 
  mutate(sitio = fct_relevel(sitio, c("mada", "neco", "tan", "balc", "mdp")))%>% 
  dplyr::select(sitio, trt, trat, bq, rinde_aj, kg_trigo) 
  
# levels(red_g$Localidad)
red_g <- red %>% 
  mutate(Localidad = as.factor(case_when(
    sitio== "mada" ~ "Madariaga",
    sitio=="neco" ~ "Necochea",
    sitio == "tan" ~ "Tandil",
    sitio=="balc" ~ "Balcarce",
    sitio== "mdp" ~ "MdP",
    sitio== "barrow" ~ "Barrow")),
    mezcla= case_when(trt==1 ~ "#100000",
                      trt %in% 2:4 ~ "#000000",
                      trt %in% 5:7 ~ "#4682b4",
                      trt== 8 ~"#32cd32")) %>% 
  mutate(Localidad = fct_relevel(Localidad, c("Madariaga",
                                              "Necochea", 
                                              "Tandil", 
                                              "Balcarce", 
                                              "MdP", 
                                              "Barrow")),
         trat = fct_relevel(Localidad, c('Check', "M32", "M39", ))
  
red_sum <- red %>% 
  group_by(sitio, trat) %>% 
  summarise(yield=mean(rinde_aj, na.rm = T)) %>% 
  ungroup %>% 
  spread(trat, yield) %>% 
  gather(trat, yield, -`Check`, -sitio) %>% 
  dplyr::select(-`Check`, everything()) %>% 
  arrange(sitio, trat) %>% 
  mutate(dif = yield - `Check`) %>% 
  left_join(id_trt, by="trat") 

# write_csv(red_sum, file.path(here::here(red), "red_sum.csv"))
# pacman::p_load(openxlsx)
# list_of_datasets <- list("red" = red, 
#                          "red_sum" = red_sum)
# write.xlsx(list_of_datasets, file = "red_data2.xlsx")

red_g %>% 
  ggplot(aes(x=trat, y = rinde_aj)) +
  stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean, 
               geom = "crossbar", size = 0.5, aes(col = factor(mezcla)))+  
  geom_point(size=1)+
  facet_wrap(~Localidad, scales="fixed")+
  labs(x="", y="kg/ha", title = "Rendimiento ajustado 14%")+
  cowplot::theme_minimal_grid()%+replace%
  theme(axis.text.x = element_text(angle = 90))+
  guides(col=F)

ggsave("~/Dropbox/5_Lineas_juan/Trigo/red/pres_zoom/individual.png",
       width = 6, height = 4)

# Model
pacman::p_load(lmerTest, emmeans, ggeffects)

## Analisis individual

balc <- red %>% filter(sitio=="balc")
m1 = lm(rinde_aj ~ trat+bq, data=balc) 
anova(m1)
plot(m1, which=3)
summary(m1)

ref_grid(m1)
emm1 = emmeans(m1, specs = ~ trat)

mixed_model <- function(.) {
  lmer(rinde_aj ~ trat + (1|bq), data = .)
}

fits <- red %>% 
  select(-c(trt, kg_trigo)) %>%
  nest(data = c(trat, bq, rinde_aj)) %>% 
  mutate(model = map(data, mixed_model),
         model_anova = map(data, ~anova(lmer(rinde_aj ~ trat + (1|bq), .))),
         emm = map(data, ~emmeans(lmer(rinde_aj ~ trat + (1|bq), .), "trat")))

out_fits <- fits %>% 
  mutate(tidy_model = map(model_anova, broom::tidy)) %>% #,
  # model_qual = map(model, MuMIn::r.squaredGLMM)) %>% 
  select(sitio, tidy_model) %>%
  unnest(c(tidy_model)) 
out_fits %>% knitr::kable()

fits %>% 
  mutate(emmeans = map(emm, broom::tidy)) %>% 
  select(sitio, emmeans) %>%
  unnest(c(emmeans)) %>% 
  filter(sitio %in% c(
    # "mada"
    # "neco"
    # "tan"
    # "balc"
    # "mdp"
    "barrow"
    ))

resp_sum <- 
  red_sum %>% 
  group_by(sitio) %>% 
  top_n(n=1, wt = dif) %>% 
  mutate(resp = dif/`1.Check`*100, 
         perdida = dif/yield*100, 
         gan = dif-kg_trigo) %>% 
  dplyr::select(trat,yield,`1.Check`,dif, resp, perdida) #%>% 
  # knitr::kable()

red_sum %>% 
  mutate(gan = dif - kg_trigo) %>% 
  group_by(sitio) %>% 
  top_n(n=1, wt = gan)  %>% 
  knitr::kable()

tabla_resp <- red_sum %>% 
  group_by(sitio, trat) %>% 
  # top_n(n=1, wt = dif) %>% 
  mutate(resp = dif/`1.Check`*100,
         gan = dif - kg_trigo)

knitr::kable(round(with(tabla_resp, 
     tapply(resp, 
            list(sitio=sitio, trat=trat), mean)),1))

knitr::kable(round(with(tabla_resp, 
     tapply(gan, 
            list(sitio=sitio, trat=trat), mean)),1))

tabla_resp %>% 
  ggplot(aes(x=resp)) + facet_wrap("trat")+
  geom_density()+geom_rug(size=1.5, alpha=0.5, col="red") +
  geom_vline(xintercept = 0, linetype = 2)


mezcla_doble <- "MD: Mezcla doble 0,5 l/ha - epoxiconazole + azoxistrobin"
mezcla_triple <-"MT: Mezcla triple 1,2 l/ha - epoxiconazole + pyraclostrobin + fluxapyroxad"

red_sum %>% 
  mutate(Localidad = as.factor(case_when(
    sitio=="balc" ~ "Balcarce",
    sitio== "mada" ~ "Madariaga",
    sitio== "mdp" ~ "MdP",
    sitio=="neco" ~ "Necochea",
    sitio == "tan" ~ "Tandil")))  %>% 
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
  cowplot::theme_minimal_grid() %+replace% 
  theme(#axis.text.x=element_text(angle=60, hjust=1, vjust=1),
        plot.caption = element_text(hjust = 0, size = 10, face= "italic"))

ggsave("~/Dropbox/5_Lineas_juan/Trigo/red/pres_zoom/profit.png",
       width = 7, height = 5)


#%>% filter(sitio!="tan"))

mod0 <- lmer(rinde_aj ~ trat*sitio+ (1|sitio:bq), 
             data=red) 
anova(mod0)
summary(mod0)

mod1 <- lmer(rinde_aj ~ trat + (1|sitio) + (1|sitio:bq), 
             data= red)
AIC(mod0, mod)
anova(mod1)
summary(mod1)
plot(mod1)
qqnorm(resid(mod1))
qqline(resid(mod1))  # points fall nicely onto the line - good!

# https://aosmith.rbind.io/2019/04/15/custom-contrasts-emmeans/
library(emmeans)
emm1 = emmeans(mod1, specs = ~ trat)

#z32 vs z39
z32 = c(0, 1, 0, 0, 1, 0, 0, 0)
z39 = c(0, 0, 1, 0, 0, 1, 0, 0)
contrast(emm1, method = list("z32 - z39" =z32-z39))

#MD vs MT
MD = c(0, 1, 1, 1, 0, 0, 0, 0)
MT = c(0, 0, 0, 0, 1, 1, 1, 0)
contrast(emm1, method = list("MD - MT" =MD-MT))

#Aplicacion simple vs doble
AP1 = c(0, 0.25, 0.25, 0, 0.25, 0.25, 0, 0)
AP2 = c(0, 0, 0, 0.333, 0, 0, 0.333, 0.333)
contrast(emm1, method = list("AP1 - AP2" = AP1 - AP2))

red$trat <- relevel(red$trat, ref='7.MT_z32_z39')
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
               red %>% group_by(Localidad, trat) %>% 
               summarise(predicted=mean(rinde_aj, na.rm = T)) %>% 
               rename(x=trat), 
             aes(x = x, 
                 y = predicted, col=Localidad)) + 
  lims(y=c(5000,8000))+
  labs(x = "", y ="kg/ha", title= "Rendimiento ajustado 14%", 
       subtitle= "Análisis global\n En negro: media estimada por el modelo y su intervalo de 95% de confianza")+
       # caption = paste(mezcla_doble,"\n",mezcla_triple)) +
  cowplot::theme_minimal_grid() %+replace% 
  theme(axis.text.x=element_text(angle=60, hjust=1, vjust=1))

plot_model(mod1, type = "eff")
plot_model(mod1, type = "re")
609614/(609614 + 10772)  # ~60 %

sjp.lmer(mod)
sjp.setTheme(theme = "forest")

tmod <- tab_model(mod)
tmod
tidy(mod)
