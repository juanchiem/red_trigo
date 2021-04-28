econo <- tibble::tribble(
  ~id,                ~trat,  ~cost_prod1,  ~cost_prod2, ~sprays,
    1,              "Check",            0,            0,       0,
    2,           "Doble_32",           19,            0,       1,
    3,           "Doble_39",           19,            0,       1,
    4,        "Doble_32_39",           19,           19,       2,
    5,          "Triple_32",           34,            0,       1,
    6,          "Triple_39",           34,            0,       1,
    7,       "Triple_32_39",           34,           34,       2,
    8, "Doble_32_Triple_39",           19,           34,       2
  )

econo
rend_indif <- function(cost_prod1, cost_prod2, sprays, mosq, prec_trigo){
  costo = cost_prod1+cost_prod1+(mosq*sprays)
  ingreso = 1000/(prec_trigo*0.8)
  rend_ind = costo/ingreso
  bind_rows(setNames(c(costo, ingreso, rend_ind), 
                     c("costo", "ingreso", "kg_trigo")))

}

rend_indif(cost_prod1 = 19, 
           cost_prod2 = 0, 
           sprays = 1, 
           mosq=5, 
           prec_trigo=174)

econo %>% 
  mutate(ri= list(rend_indif(cost_prod1, 
                             cost_prod2, 
                             sprays, 
                             mosq=5, 
                             prec_trigo=174))) %>% 
  unnest(ri)

