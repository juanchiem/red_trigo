```{r}
roles <- function(x) sub("[^_]*_","", x)# used to create x-axis label restoring original name of role
dat_mean %>% #count(id_ensayo)
  filter(!trt %in% c("test")) %>% 
  mutate(V4=paste(trt, id_ensayo, sep="_")) %>%
  ggplot(aes(x=reorder(V4, dif_test), y=dif_test) ) + 
  geom_bar(
    # aes(fill = enfermedad_1),
    stat = "identity") +
  # geom_errorbar(width=0.2, size= 0.3, 
  #               aes(ymax = D + sqrt(vi2/sqrt(bk)), 
  #                   ymin=D - sqrt(vi2/sqrt(bk))))+
  facet_wrap(~ trt,  ncol=2, scales = "free_x") +
  scale_x_discrete(labels=roles)+
  labs(y="Dif. test (kg/ha)", x = "Ensayo (ordenado por dif_test) ")+
  theme_bw2+ theme(axis.text.x=element_blank() )+
  geom_abline(intercept = 0)
```


```{r}
dat %>% #count(id_ensayo)
  mutate(trt = fct_relevel(trt, c("test", "rub_Z32", "rub_Z39", "orq_Z32", "orq_Z39", "rub_Z32_orq_Z39"))) %>% 
  filter(id_ensayo == "2020_PABLO_ACOSTA") %>% 
  ggplot()+
  aes(x=trt, y=kg_ha)+
  geom_point(alpha=0.3)+
  # facet_wrap("id_ensayo")+
  stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 0.5)+
  # geom_text(aes)
  stat_summary(data = dat_mean %>% 
                 filter(id_ensayo == "2020_PABLO_ACOSTA"), 
               aes(x=trt, y=kg_ha,
                   label=dif_test %>% round), 
               fun=mean, geom="text", size=4,vjust = -0.5)+
  scale_y_continuous(breaks= scales::pretty_breaks())+
  coord_flip()+
  theme_bw(base_size = 14)
```




```{r}
roles <- function(x) sub("[^_]*_","",x )
n_fun <- function(x){return(data.frame(y = -Inf, label = paste0("n = ", length(x))))}

dat_mean %>% 
  filter(!trt %in% c("test", "rub_Z32_orq_Z39")) %>% 
  mutate(id=paste(trt, id_ensayo, sep="_")) %>% 
  ggplot(aes(x=reorder(id, dif_doble), y=dif_doble) ) + 
  geom_bar(
    # aes(fill = enfermedad_1),
    stat = "identity") +
  # geom_errorbar(width=0.2, size= 0.3, 
  #               aes(ymax = D + sqrt(vi2/sqrt(bk)), 
  #                   ymin=D - sqrt(vi2/sqrt(bk))))+
  facet_grid(.~ trt, scales = "free_x") +
  scale_x_discrete(labels=roles)+
  labs(y="Dif. test (kg/ha)", x = "Ensayo (ordenado por dif_test) ")+
  theme_bw2 + 
  theme(axis.text.x=element_blank())+
  geom_abline(intercept = 200)+
  geom_abline(intercept = 0)+
  geom_text(aes(x= id, y=dif_doble, label = dif_doble %>% round(), 
                fill = NA), angle = 0, size =2)
# geom_text(aes(x= id, y=0, label = id_ensayo, fill = NA), angle = 90, size =2) 

```

```{r}
dat %>% 
  ggplot()+
  aes(trt, kg_ha)+
  geom_point()+
  stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 0.5)+
  facet_grid(. ~ id_ensayo, scales = "free_y")+
  stat_summary(aes(label=round(..y..,0)), 
               fun=mean, geom="text", size=4, vjust = -0.5)+ 
  coord_flip()+
  theme_dens1# +
# stat_summary(fun.data = n_fun, geom = "text", vjust = -0.5)+
```

```{r}
dat_fmc <- red20 %>% 
  filter(trt %in% fmc) 

test_yd <- dat_mean %>% 
  group_by(id_ensayo, trt) %>% 
  summarise(kg_ha = mean(kg_ha, na.rm = T)) %>% 
  filter(trt %in% c("test")) %>% 
  pivot_wider(names_from = trt, values_from = kg_ha) %>% 
  rename(rinde_test = test)

mean_yd <- dat_mean %>% 
  group_by(id_ensayo, trt) %>% 
  summarise(kg_ha = mean(kg_ha, na.rm = T)) %>%  
  left_join(test_yd, by = c("id_ensayo")) %>% 
  rowwise() %>% 
  mutate(dif_test = kg_ha -rinde_test)
# dat_mean %>%filter(id_ensayo == "2020_INTA_BCE")

# load(here::here("data/data.Rdata"))
```

```{r eval=T}

dispress <- dat %>% 
  group_by(id_ensayo) %>% 
  summarise(
    year=first(year),
    trt=factor("test"),
    kg_ha=1,
    enfermedad_1=first(dis_32), 
    enfermedad_2=first(dis_39)) %>%
  unite("enf", enfermedad_1:enfermedad_2)

dat %>% 
  filter(year == 2020) %>%
  ggplot()+
  aes(trt, kg_ha)+
  geom_point()+
  stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 0.5)+
  facet_grid(.~id_ensayo)+
  stat_summary(data = mean_yd, 
               # %>% filter(year == "2020") ,
               aes(x=trt, y=kg_ha, label=dif_test %>% round), 
               fun=mean, geom="text", size=4,vjust = -0.5)+
  geom_rect(data = dispress, 
            # %>% filter(year == "2020"),
            aes(fill = enf),
            xmin = -Inf, xmax = Inf,
            ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  labs(title= "Resultados 2020", 
       y="kg/ha", x = "", 
       fill = "Enfermedades\nZ39_Z65", 
       caption = 
         "
       Enfermedades: MA = mancha amarilla, RE = roya estriada, RH = roya de la hoja\n
       Tratamientos (g/ha):\n 
                rub = epoxi. 50 g + azoxistr. 100 g\n
                orq = epoxi. 50 g + piraclostr. 97 g + fluxapiroxad 60 g
       ") + 
  # scale_fill_manual(name="Enfermedades\nZ39_Z65"
  #                   # values = c("orange", "yellow"),
  #                   # labels = c("Mancha amarilla", "Roya estriada")
  # )+
  geom_vline(xintercept = c(1.5,3.5,5.5), linetype = "dashed")+
  coord_flip()+
  theme_dens1
```

