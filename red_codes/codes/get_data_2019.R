library(tidyverse) 
library(googlesheets4)

red0 <- gs4_get("1HSoj5M--kcW-WioiwMbbngrqdkSC-bqpumeDIIlbx18")
# gs4_browse(red0)

# dat0 <- tibble(sitio = red0 %>% sheet_names()) %>%
#   mutate(data = map(sitio, ~read_sheet(red0, sheet = .x))) %>%
#   tidyr::unnest(cols = c(data))
# 
# dat0 %>%
#   mutate(rinde_aj = case_when(
#     sitio=="mdp" ~ rinde_aj-1000,
#     TRUE ~ rinde_aj)) %>%
#   sheet_write(ss = red0, sheet = "2019")

dat0 <- read_sheet(red0, sheet = "2019")
# dat0 %>% filter(sitio == "balc")

id_trt <- tibble::tribble(
  ~trt,      ~trat, ~kg_trigo,
  1,       "Check",         0,
  2,        "MD32",       172,
  3,        "MD39",       172,
  4,     "MD32_39",       345,
  5,        "MT32",       280,
  6,        "MT39",       280,
  7,     "MT32_39",       560,
  8,   "MD32_MT39",       453) #%>% mutate(kg_trigo10up = kg_trigo*1.1,
         # kg_trigo10down = kg_trigo*0.9)
red_t=c(1L, 2L, 3L, 5L, 6L, 8L)

dat <- 
  dat0 %>% 
  filter(trt %in% red_t) %>% droplevels() %>% 
  mutate(sitio = as.factor(case_when(
    sitio == "mada" ~ "Madariaga",
    sitio == "neco" ~ "Necochea",
    sitio == "tan" ~ "Tandil",
    sitio == "balc" ~ "Balcarce",
    sitio == "mdp" ~ "MdP",
    sitio == "barrow" ~ "Barrow"))) %>% 
  left_join(id_trt, by="trt")  %>% 
  mutate_at(vars("trat", "bq"), as.factor) %>% 
  mutate(trat = fct_relevel(trat, 'MD32_MT39', after = Inf))

# dat %>% filter(sitio == "MdP")

red <-
  dat %>%
  mutate_at(vars(sitio, trt, trat, bq), as.factor)%>%
  # mutate(sitio = fct_relevel(sitio, c("mada", "neco", "tan", "balc", "mdp")))%>%
  dplyr::select(sitio, trt, trat, bq, rinde_aj, kg_trigo) %>%
  mutate(trat = fct_relevel(trat, c('Check', "MD32", "MD39", "MT32", "MT39",
                                    "MD32_MT39")),
         mezcla= case_when(trt==1 ~ "#100000",
                           trt %in% 2:4 ~ "#000000",
                           trt %in% 5:7 ~ "#4682b4",
                           trt== 8 ~"#32cd32"),
         sitio = fct_relevel(sitio, c("Madariaga",
                                      "Necochea",
                                      "Tandil",
                                      "Balcarce",
                                      "MdP",
                                      "Barrow")))
# red %>% filter(sitio == "MdP")
