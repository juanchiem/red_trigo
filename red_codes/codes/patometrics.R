pacman::p_load(tidyverse)

url <- "https://docs.google.com/spreadsheets/d/19UFhIkIWhRYS_hnQNGg4yKeNOpmMp0am3GUGfFTw6s0/edit?usp=sharing"
# browseURL(url)

dat <- googlesheets4::read_sheet(url, 
                                 sheet="Madariaga_11_1", 
                                 col_types="ccccddddddddddddd"
) 

dat <- dat %>% 
  mutate_if(is.character, as.factor) %>% 
  drop_na(inf_vde) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) 


dat %>% 
  select(id, trt, contains("hb")) %>% 
  pivot_longer(-(id:trt), 
               names_to = "hj_enf", values_to = "y") %>% 
  separate(hj_enf, c("hj", "enf")) %>%   
  mutate_if(is.character, as.factor) %>% 
  group_by(id, hj, enf) %>% 
  summarise(inc = round(mean(y > 0, na.rm = T), 2), 
            sev = mean(y[y > 0], na.rm = T)) %>% 
  ungroup %>%  
  separate(id, c("trt", "bq"))  %>%
  mutate(
    trt = factor(case_when(
      trt == 1 ~ "Test",
      trt == 2 ~ "D32",
      trt == 3 ~ "D39",
      trt == 4 ~ "D32\nD39",
      trt == 5 ~ "T32",
      trt == 6 ~ "T39",
      trt == 7 ~ "T32\nT39",
      trt == 8 ~ "D32\nT39",
      trt == 9 ~ "Ox32",             
      trt == 10 ~ "Ox39",
      trt == 11 ~ "Ox32\nOx39")),
    trt = fct_relevel(trt, 
                       "Test", 
                       "D32","D39", "D32\nD39", 
                       "T32","T39", "T32\nT39",
                       "D32\nT39",
                       "Ox32", "Ox39","Ox32\nOx39"),
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

  
