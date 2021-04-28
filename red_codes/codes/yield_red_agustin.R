dat <- read_csv("https://github.com/juanchiem/agro_data/raw/master/yield_wheat19.csv")

#Acondicionamiento del dataset
red_t=c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L)
id_trt <- tibble::tribble(
  ~trt,           ~trat, ~kg_trigo,
  1,         "1.Check",         0,
  2,        "2.MD_z32",       172,
  3,        "3.MD_z39",       172,
  4,    "4.MD_z32_z39",       345,
  5,        "5.MT_z32",       280,
  6,        "6.MT_z39",       280,
  7,    "7.MT_z32_z39",       560,
  8, "8.MD_z32_MT_z39",       453
) %>%   
  mutate_at(vars(trt, trat), as.factor)

red <- dat %>% 
  mutate_at(vars(sitio, trt), as.factor) %>% 
  filter(trt %in% red_t) %>% droplevels() %>% 
  left_join(id_trt, by="trt") %>% 
  mutate_at(vars(sitio, trt, trat, bq), as.factor)%>% 
  dplyr::select(sitio, trt, trat, bq, rinde_aj)

# Grafico exploratorio 

red %>% 
  ggplot(aes(x=trt, y = rinde_aj)) +
  stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean, 
               geom = "crossbar", 
               size = 0.1)+  
  geom_point()+
  facet_wrap(~sitio, scales="free")+
  theme_bw()

# Cual fue la respuesta maxima (o la perdida maxima) en cada campo
# resp = (mejor_trat - check) / check * 100 
# perd =  (mejor_trat - check) / mejor_trat * 100

red_sum %>% 
  group_by(sitio) %>% 
  top_n(n=1, wt = dif) %>% 
  mutate(resp = dif/`1.Check`*100, 
         perdida = dif/yield*100) %>% 
  select(trat,yield,`1.Check`,dif, resp, perdida) 

# Ejemplo un sitio solo = balcarce

balc <- red %>% filter(sitio=="balc")
m1 = lm(rinde_aj ~ trat+bq, data=balc) 
anova(m1)
plot(m1)

pacman::p_load(lme4, sjPlot, ggeffects)

mixed_model <- function(.) {
  lmer(rinde_aj ~ trat + (1|bq), data = .)
}

fits <- red %>% 
  select(-trt) %>%
  nest(data = c(trat, bq, rinde_aj)) %>% 
  mutate(model = map(data, mixed_model),
         model_anova = map(data, ~car::Anova(lmer(rinde_aj ~ trat + (1|bq), .))))

out_fits <- fits %>% 
  mutate(tidy_model = map(model_anova, broom::tidy)) %>% #,
  # model_qual = map(model, MuMIn::r.squaredGLMM)) %>% 
  select(sitio, tidy_model) %>%
  unnest(c(tidy_model)) 
out_fits

# Analisis combinado 

# probando interacccion trat - sitio
mod0 <- lmer(rinde_aj ~ trat*sitio+ (1|sitio:bq), 
             data=red) 

car::Anova(mod0)

# sitio efecto aleatorio 
mod1 <- lmer(rinde_aj ~ trat + (1|sitio/bq), 
             data= red)
AIC(mod0, mod1) # mejor el mod0

anova(mod1)
summary(mod1)
plot(mod1)
qqnorm(resid(mod1))
qqline(resid(mod1))  # points fall nicely onto the line - good!

# usando el mejor trat como referencia
red$trat <- relevel(red$trat, ref='7.MT_z32_z39')
mod2 <- lmer(rinde_aj ~ trat + (1|sitio/bq), data= red)
# %>% filter(sitio!="tan"))
summary(mod2)

# quedan conformados 2 grupos 

plot_model(mod1, type = "eff")
plot_model(mod1, type = "re")
