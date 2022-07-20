pacman::p_load(broom, metafor, janitor)

dat_mean <- dat %>% 
  group_by(id_ensayo, trt) %>% 
  summarise(
    n=sum(!is.na(kg_ha)),
    mean_kg_ha = mean(kg_ha, na.rm=TRUE))
dat_mean

dat_qm <- dat %>%  
  select(id_ensayo, trt, bk, kg_ha) %>% 
  group_by(id_ensayo) %>% 
  do(tidy(aov(.$kg_ha ~ .$trt + factor(.$bk)))) %>% 
  filter(term == "Residuals") %>% 
  select(1,5) %>% 
  set_names(c("id_ensayo", "v_yld"))

dat_qm

dat_meta <- dat_mean %>% 
  left_join(dat_qm) %>% 
  left_join(info) %>% 
  filter(!id_ensayo=="2019_BARROW") %>% 
  mutate(vi_yld=v_yld/n)
dat_meta
# sum(is.na(dat_meta))

dat_meta %>% tabyl(id_ensayo, dis_1)

mv_yld <- rma.mv(mean_kg_ha, vi_yld,
                 mods = ~ trt,
                 random = list(~trt|id_ensayo),
                 struct = "UN",
                 method = "ML",
                 control = list(optimizer = "nlm"),
                 data = dat_meta
)
summary(mv_yld)

mv_yld_dis <- rma.mv(mean_kg_ha, vi_yld,
                        mods = ~trt * factor(ry),
                        random = list(~trt | id_ensayo),
                        struct = "UN",
                        method = "ML",
                        control = list(optimizer = "nlm"),
                        data = dat_meta)

anova(mv_yld_dis, btt = 8:12)
summary(mv_yld_dis)

# calcular test con ry 0
# 5752.9323
test.r0 <- data.frame(predict(mv_yld_dis, addx=F, 
                              newmods = c(0,0,0,0,0,
                                          0,
                                          0,0,0,0,0)))
# 5752.9323 + 397.8728
rub_Z32.r0 <- data.frame(predict(mv_yld_dis, addx=F, 
                                 newmods = c(1,0,0,0,0,
                                             0,
                                             0,0,0,0,0)))

orq_Z32.r0 <- data.frame(predict(mv_yld_dis, addx=F, 
                                 newmods = c(0,1,0,0,0,
                                             0,
                                             0,0,0,0,0)))

rub_Z39.r0 <- data.frame(predict(mv_yld_dis, addx=F, 
                                 newmods = c(0,0,1,0,0,
                                             0,
                                             0,0,0,0,0)))

orq_Z39.r0 <- data.frame(predict(mv_yld_dis, addx=F, 
                                 newmods = c(0,0,0,1,0,
                                             0,
                                             0,0,0,0,0)))

rub_Z32_orq_Z39.r0 <- data.frame(predict(mv_yld_dis, addx=F, 
                                        newmods = c(0,0,0,0,1,
                                                    0,
                                                    0,0,0,0,0)))
# calcular test con ry 1 
test.r1<- data.frame(predict(mv_yld_dis, addx=F, 
                              newmods = c(0,0,0,0,0,
                                          1,
                                          0,0,0,0,0)))
rub_Z32.r1 <- data.frame(predict(mv_yld_dis, addx=F, 
                                 newmods = c(1,0,0,0,0,
                                             1,
                                             1,0,0,0,0)))
orq_Z32.r1 <- data.frame(predict(mv_yld_dis, addx=F, 
                                 newmods = c(0,1,0,0,0,
                                             1,
                                             0,1,0,0,0)))
rub_Z39.r1 <- data.frame(predict(mv_yld_dis, addx=F, 
                                 newmods = c(0,0,1,0,0,
                                             1,
                                             0,0,1,0,0)))
orq_Z39.r1 <- data.frame(predict(mv_yld_dis, addx=F, 
                                 newmods = c(0,0,0,1,0,
                                             1,
                                             0,0,1,1,0)))
rub_Z32_orq_Z39.r1 <- data.frame(predict(mv_yld_dis, addx=F, 
                                        newmods = c(0,0,0,0,1,
                                                    1,
                                                    0,0,0,0,1)))
res <- bind_rows(
  lst(test.r0, rub_Z32.r0, orq_Z32.r0, rub_Z39.r0, orq_Z39.r0, 
      rub_Z32_orq_Z39.r0,
      test.r1, rub_Z32.r1, orq_Z32.r1, rub_Z39.r1, 
      orq_Z39.r1, rub_Z32_orq_Z39.r1),  .id = "id" ) %>% 
  separate(id, c("trt", "ry"),  sep="\\.", convert = TRUE, extra = "merge")
res

orden_trt <- c("test", "rub_Z32", "orq_Z32", "rub_Z39", "orq_Z39", "rub_Z32_orq_Z39")

res %>%   
  mutate(trt = fct_relevel(trt, orden_trt)) %>% 
  ggplot() + 
  aes(x=trt, y=pred) + 
  geom_errorbar(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.8, width=0, size= 0.8)+
  # geom_errorbarh(aes(xmin = efficacy_lw, xmax = efficacy_up, color = fungicide), alpha = 0.8, height= 0, size= 0.8)+
  geom_point(size = 3)+
  facet_wrap("ry") + coord_flip()
