m2 = mean(tabla_resp %>% filter(trt==2) %>% pull(gan))
sd2 = sd(tabla_resp %>% filter(trt==2) %>% pull(gan))
1 - pnorm(m2/sd2)
pnorm(0, m2, sd2)

(trt2 = pnorm(0,
      mean(tabla_resp %>% filter(trt==2) %>% pull(gan)),
      sd(tabla_resp %>% filter(trt==2) %>% pull(gan))))
(trt3 = pnorm(0,
      mean(tabla_resp %>% filter(trt==3) %>% pull(gan)),
      sd(tabla_resp %>% filter(trt==3) %>% pull(gan))))
(trt5 = pnorm(0,
      mean(tabla_resp %>% filter(trt==5) %>% pull(gan)),
      sd(tabla_resp %>% filter(trt==5) %>% pull(gan))))
(trt6 = pnorm(0,
      mean(tabla_resp %>% filter(trt==6) %>% pull(gan)),
      sd(tabla_resp %>% filter(trt==6) %>% pull(gan))))
(trt8 = pnorm(0,
      mean(tabla_resp %>% filter(trt==8) %>% pull(gan)),
      sd(tabla_resp %>% filter(trt==8) %>% pull(gan))))

tabla_resp %>% 
  ggplot(aes(x=gan)) + facet_wrap("trat")+
  geom_density()+geom_rug(size=1.5, alpha=0.5, col="red") +
  geom_vline(xintercept = 0, linetype = 2)#+
  geom_area(aes(fill = gan > 0), alpha = 0.4, colour = "navy") +
  scale_fill_manual(values = c("thistle", "navy"), guide = FALSE)

densityDF <- 
  data.frame(density(NHANES$Pulse, na.rm = TRUE)[1:2]) 
head(densityDF)