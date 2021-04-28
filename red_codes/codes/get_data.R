pacman::p_load(openxlsx)

list_of_datasets <- list("mada" = mada1, 
                         "neco" = neco1, 
                         "balc" = balc1, 
                         "mdp" = mdp1, 
                         "tan" = tan1)
write.xlsx(list_of_datasets, file = "red_data.xlsx")

mada1 <- mada %>% left_join(dis_dat, by = c("trt", "bq"))

neco1 = neco1 %>% 
  select(par, trt, bq, rinde_aj)

balc1 = lp1 %>% 
  select(par, trt, bq, rinde_aj)

mdp1 = mdp1 %>% 
  select(par, trt, bq, rinde_aj)

tan1 = tan1 %>% 
  select(par, trt, bq, rinde_aj)


sev_dat <- dis_dat %>% 
  filter(org %in% c("b0", "b1")) %>% 
  select(-org) %>% 
  group_by(trt, bq) %>%
  summarise_each(., funs( 
    # incidencia media a nivel de parcela/órgano de cada enfermedad
    # inc = sum(.>0, na.rm = TRUE),
    # severidad media a nivel de parcela/órgano de cada enfermedad
    sev = round(mean(., na.rm = TRUE),1)), 
    everything())


# https://stackoverflow.com/questions/51200887/how-to-import-multiple-sheets-from-multiple-excel-files-into-one-list-readxl-r
library(tidyverse)
# library(readxl)
# library(fs)
# 
# archivos <- dir_ls(glob = "red_*") 
# #(here::here("data", "balcarce"), glob = "*.xlsx")
# dat <- map_df(archivos, function(x){  
#   sheet_names <- excel_sheets(x)
#   raw_data <- map_df(sheet_names, ~read_excel(x, sheet = .x)) 
#   return(raw_data)})


sheets_to_read <- readxl::excel_sheets("red_data.xlsx")
dat <- bind_rows(lapply(1:length(sheets_to_read),
                        function(i)readxl::read_excel("red_data.xlsx",
                                                      sheet = sheets_to_read[i]) %>%
                          mutate(sitio = sheets_to_read[i]))) %>% 
  select(sitio, everything())

dat


save(dat, file="red_data.Rda")



