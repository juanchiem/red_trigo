## packages I want loaded for all pages of my site
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

## knitr options I want set as default for all ('global') code chunks
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE, fig.show='hold')

options(scipen=999)

ggplot2::theme_set(cowplot::theme_minimal_grid() %+replace% 
                     theme(plot.caption = element_text(hjust = 0, face= "italic",
                                                       color = "gray30", size = 10)))

meta_url <- "https://docs.google.com/spreadsheets/d/1-eCU4YfOiGz5Ox6RHX8CXDaQSUsmImEM_lQHAWtWybs/edit?usp=sharing"
metadata <- gsheet::gsheet2tbl(meta_url)  
# sheets_deauth() 

# id_trial <- metadata[menu(metadata %>% pull(id_ensayo), title="Seleccione ensayo"), 1] %>% pull
# id_trial

