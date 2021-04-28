red=c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L)

lp1 %>% 
  filter(trt %in% red) ->lp2

lp2 %>% 
  ggplot(aes(x= trt, y =rinde_aj)) + 
  geom_point(alpha=0.5)

lp2 %>% 
  # group_by(trt) %>% 
  summarise(raster::cv(rinde_aj)) %>% 
  pull->cv

pacman::p_load(sjstats)
fit <- lm(rinde_aj ~ trt + bq, data=lp2)
anova(fit)
cv(fit)

lp2 %>% 
  group_by(trt) %>% 
  summarise(mu=mean(rinde_aj)) %>% 
  summarise(sd(mu)) %>% 
  pull->between_groups_sd
between_groups_sd^2

lp2 %>% 
  group_by(trt) %>% 
  summarise(sigma=sd(rinde_aj)) %>% 
  summarise(mean(sigma))%>% 
  pull->within_groups_sd
within_groups_sd^2

library(lme4)
fit <- lmer(rinde_aj ~ trt + (1|bq), data=lp2)
# assump(fit)
summary(fit)
anova(fit)
library(emmeans)
pred_lp = emmeans(fit, ~ trt, type = "response")
pwpp(pred_lp, method = "trt.vs.ctrl1", type = "response", side = ">")

res <-broom::augment(fit)
DE <- sd(res$.resid)

pacman::p_load(pwr2)
# browseURL("https://www.rpubs.com/Mareiza/378540")
pwr.1way(k=2, n=3, alpha=0.1, delta=300, sigma=DE)

## Example 1
ss.1way(k=8, alpha=0.05, beta=0.2, f=0.5, B=100)

## Example 2
ss.1way(k=5, alpha=0.05, beta=0.1, delta=1.5, sigma=1, B=100)
ss.1way(k=5, alpha=0.05, beta=0.1, f=NULL, delta=1.5, sigma=1, B=100)

pacman::p_load(pwr)
browseURL("https://www.statmethods.net/stats/power.html")
browseURL("https://cran.r-project.org/web/packages/pwr/vignettes/pwr-vignette.html")

pwr.anova.test(k = 3, f = 5/3, sig.level = 0.01, power = 0.9)
pwr.anova.test(k = 8, f = between_groups/within_groups, 
               sig.level = 0.05, power = 0.8)

library(stats) #no es necesario llamarlo
# https://stats.idre.ucla.edu/r/dae/one-way-anova-power-analysis/

power.anova.test(groups = 8, 
                 between.var = between_groups^2, 
                 within.var = within_groups^2, 
                 power=NULL, 
                 sig.level=0.1,
                 n=3)

power.anova.test(groups = 8, 
                 between.var = between_groups^2, 
                 within.var = within_groups^2, 
                 power=0.8, 
                 sig.level=0.1,
                 n=NULL)

pacman::p_load(MuMIn)
MuMIn::r.squaredGLMM(lme4::lmer(data=airquality, Ozone ~ 1 + (1|Month)))
