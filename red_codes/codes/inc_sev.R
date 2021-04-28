u <- "https://docs.google.com/spreadsheets/d/19UFhIkIWhRYS_hnQNGg4yKeNOpmMp0am3GUGfFTw6s0/edit#gid=0"
raw_data <- gsheet::gsheet2tbl(u)

raw_data %>%
  pivot_longer(-(trt:pl), 
               names_to = "org_var", values_to = "y") %>% 
  separate(org_var, c("org", "var")) %>% 
  mutate_at(vars(trt:var), as.factor) %>% 
  pivot_wider(names_from = var, values_from = y) %>% 
  filter(stringr::str_detect(org, "b")) %>% # elimina tallo
  # filter(!(is.na(af))) %>%  # elimina hojas perdidas
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% # rellena con 0
  select(everything()) -> dat 

h012= c("b0","b1","b2") 

# Nivel tratamiento
dat %>% 
  filter(org %in% h012) %>% 
  select(-(bq:org)) %>% 
  group_by(trt) %>% 
  summarise_each(., funs(
    inc = mean(.>0, na.rm = TRUE)*100,
    sev = round(mean(., na.rm = TRUE),1)), 
    everything(), -af) %>%  
  mutate_if(is.numeric, round, 1)

# Nivel parcela
dat %>% 
  filter(org %in% h012) %>% 
  group_by(trt, bq) %>%
  summarise_each(., funs(
    inc = mean(.>0, na.rm = TRUE)*100,
    sev = round(mean(., na.rm = TRUE),1)), 
    everything(),  -af, -pl, -org) %>% 
  mutate_if(is.numeric, round, 0)

# Nivel de órgano
dat %>% 
  filter(org %in% h012) %>% 
  group_by(trt, bq, org) %>%
  summarise_each(., funs(
    inc = mean(.>0, na.rm = TRUE)*100,
    sev = round(mean(., na.rm = TRUE),1)), 
    everything(),  -af, -pl) %>% 
  mutate_if(is.numeric, round, 0)

