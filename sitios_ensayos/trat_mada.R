trats <- tibble::tribble(
  ~Trt_id,        ~Z32,        ~Z39,
  1L,         ".",         ".",
  2L,     "T + E",         ".",
  3L,         ".",     "T + E",
  4L,     "T + E",     "T + E",
  5L, "T + E + C",         ".",
  6L,         ".", "T + E + C",
  7L, "T + E + C", "T + E + C",
  8L,     "T + E", "T + E + C",
  9L,       "Oxo",         ".",
  10L,         ".",       "Oxo",
  11L,       "Oxo",       "Oxo"
)  

pacman::p_load(kableExtra)

table_trt_mada <- knitr::kable(trats, align = "c") %>% 
  kable_styling(full_width = F) %>%
  footnote(number = c("T + E: Rubric Max 0,5 l/ha	(epoxiconazole 10% ~ 50 g/ha +  azoxistrobina 20% ~ 100 g/ha)",
                      "T + E + C: Orquesta Ultra 1,2 l/ha (epoxiconazole 5% ~ 60 g/ha + pyraclostrobin 8,1% ~ 97 g/ha +	fluxapyroxad 5% ~ 60 g/ha)",
                      "Oxo:	Oxocat 0,5% + Agro Turbo 0,1%"))
