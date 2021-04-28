pacman::p_load(tidyverse)
url <- "https://docs.google.com/spreadsheets/d/1hC8TSs3hJVMEDTlO_yulRRHL_FlI7wLV2XAdm45jGZw/edit#gid=862176781"
browseURL(url)
source('codes/rend_aj.R', 
       encoding = 'UTF-8')

# neco ####
neco <- googlesheets4::read_sheet("1hC8TSs3hJVMEDTlO_yulRRHL_FlI7wLV2XAdm45jGZw", 
                                sheet="neco")
neco1 <- neco %>% 
  filter(!(is.na(peso_tot))) %>%  # elimina parcelas perdidas 
  rowwise() %>% 
  mutate(list(rinde_aj2(surcos = 4,
                        largo = 3,      # m
                        dist_surcos = 17.5, # cm
                        peso_h = peso_h,     # g
                        peso_s = peso_s,     # g
                        tara_s = 2,       # g
                        peso_tot = peso_tot,   # g
                        h_deseada = 13.5)))%>% # %
  unnest()

neco1 %>% 
  ggplot(aes(factor(trt), rinde_aj))+
  stat_summary(fun.data = "mean_cl_boot", colour = "black", size = 0.2)+
  geom_jitter(aes(col=factor(bq)), width = 0.2)+
  labs(x=NULL, y="Kg/ha", title= "Necochea")-> neco_p

# Mada ####

mada <- googlesheets4::read_sheet("1hC8TSs3hJVMEDTlO_yulRRHL_FlI7wLV2XAdm45jGZw", 
                                  sheet="mada")
mada1 <- mada %>% 
  filter(!(is.na(peso_tot))) %>%  # elimina parcelas perdidas 
  rowwise() %>% 
  mutate(list(rinde_aj2(surcos = surcos, 
                             largo = largo,      # m
                             dist_surcos = 17.5, # cm
                             peso_h = peso_h,     # g
                             peso_s = peso_s,     # g
                             tara_s = 2,       # g 
                             peso_tot = peso_tot,   # g
                             h_deseada = 13.5)))%>% # %
  unnest()

dudosas = c("6.3","11.2")

mada1 %>% 
  filter(!par %in% dudosas) %>% 
  ggplot(aes(factor(trt), rinde_aj))+
  stat_summary(fun.data = "mean_cl_boot", colour = "black", size = 0.2)+
  geom_jitter(aes(col=factor(bq)), width = 0.2) +  
  labs(x=NULL, y="Kg/ha", title= "Madariaga")->mada_p

# los pinos ####

lp <- googlesheets4::read_sheet("1hC8TSs3hJVMEDTlO_yulRRHL_FlI7wLV2XAdm45jGZw", 
                                  sheet="LP")
lp1 <- lp %>% 
  filter(!(is.na(peso_tot))) %>%  # elimina parcelas perdidas 
  rowwise() %>% 
  mutate(list(rinde_aj2(surcos = 3, 
                        largo = 3.33,      # m
                        dist_surcos = 21, # cm
                        peso_h = peso_h,     # g
                        peso_s = peso_s,     # g
                        tara_s = 2,   # g
                        peso_tot = peso_tot,   # g
                        h_deseada = 13.5)))%>% # %
  unnest()

# dudosas = c("6.3","11.2")

lp1 %>% 
  # filter(!par %in% dudosas) %>% 
  ggplot(aes(factor(trt), rinde_aj))+
  stat_summary(fun.data = "mean_cl_boot", colour = "black", size = 0.2)+
  geom_jitter(aes(col=factor(bq)), width = 0.2) +  
  labs(x=NULL, y="Kg/ha", title= "Los Pinos",  color = "Bloque")->lp_p

lp_p

# Balcarce ####

bce <- googlesheets4::read_sheet("1hC8TSs3hJVMEDTlO_yulRRHL_FlI7wLV2XAdm45jGZw", 
                                sheet="bce")
bce1 <- bce %>% 
  filter(!(is.na(peso_tot))) %>%  # elimina parcelas perdidas
  rowwise() %>% 
  mutate(list(rinde_aj2(surcos = 7, 
                                largo = 6,      # m
                                dist_surcos = 17.5, # cm
                                peso_h = 30,     # g
                                peso_s = 28,     # g
                                tara_s = 0,       # g 
                                peso_tot = peso_tot,   # g
                        h_deseada = 13.5)))%>% # %
  unnest()

# dudosas = c("6.3","11.2")

bce1 %>% 
  # filter(!par %in% dudosas) %>% 
  ggplot(aes(factor(trt), rinde_aj))+
  stat_summary(fun.data = "mean_cl_boot", colour = "black", size = 0.2)+
  geom_jitter(aes(col=factor(bq)), width = 0.2)+   
  labs(x=NULL, y="Kg/ha", title= "Balcarce",  color = "Bloque")-> bce_p

# MdP ####

mdp <- googlesheets4::read_sheet("1hC8TSs3hJVMEDTlO_yulRRHL_FlI7wLV2XAdm45jGZw", 
                                 sheet="mdp")
mdp1 <- mdp %>% 
  filter(!(is.na(peso_tot))) %>%  # elimina parcelas perdidas
  rowwise() %>% 
  mutate(list(rinde_aj2(surcos = 3, 
                        largo = 3.33,      # m
                        dist_surcos = 21, # cm
                        peso_h = peso_h,     # g
                        peso_s = peso_s,     # g
                        tara_s = 2,       # g 
                        peso_tot = peso_tot,   # g
                        h_deseada = 13.5)))%>% # %
  unnest()

mdp1 %>% 
  # filter(!bq==4) %>% 
  ggplot(aes(factor(trt), rinde_aj))+
  stat_summary(fun.data = "mean_cl_boot", colour = "black", size = 0.2)+
  geom_jitter(aes(col=factor(bq)), width = 0.2) +   
  labs(x=NULL, y="Kg/ha", title= "MdP",
       color = "Bloque")-> mdp_p

mdp_p

# Plots ####

cowplot::plot_grid(mada_p, neco_p, mdp_p, lp_p, bce_p)
