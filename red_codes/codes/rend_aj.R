# area_cosecha <- function(# m2
#   surcos, 
#   largo, # m
#   dist_surcos # cm
# ){
#   surcos*largo*dist_surcos/100
# }
# # area_cosecha(4,3,17.5)
# 
# h_muestra <- function(peso_h, # g
#                       peso_s, # g
#                       tara    # g
# ){
#   (peso_h-(peso_s-tara))/peso_h
# }
# # h_muestra(48.92, 47.85, 2)
# 
# contenido_h <- function(peso_tot, h_muestra){
#   peso_tot*h_muestra
# }
# # contenido_h(1297, h_muestra(48.92, 47.85, 2))
# 
# rinde_s <- function(peso_tot, contenido_h, area_cosecha){ # kg/ha
#   ((peso_tot-contenido_h)/area_cosecha)*10
# }
# 
# rinde_s(1297, 
#         contenido_h(1297, h_muestra(48.92, 47.85, 2)), 
#         area_cosecha(4,3,17.5))
# 
# rinde_aj <- function( # kg/ha
#   rinde_s,  # kg/ha
#   h_deseada  # %
# ){ 
#   round(rinde_s/(1-(h_deseada/100)), 0)
# }
# 
# rinde_aj(rinde_s(1297, 
#                  contenido_h(1297, h_muestra(48.92, 47.85, 2)), 
#                  area_cosecha(4,3,17.5)),
#          12)

# rinde_aj <- function( # kg/ha
#   surcos, 
#   largo,      # m
#   dist_surcos, # cm
#   peso_h,     # g
#   peso_s,     # g
#   tara_s,       # g 
#   peso_tot,   # g
#   h_deseada   # %
# ){ 
#   area_cosecha = surcos*largo*dist_surcos/100
#   
#   h_muestra = (peso_h-(peso_s-tara_s))/peso_h
#   
#   contenido_h = peso_tot*h_muestra
#   
#   rinde_s = ((peso_tot-contenido_h)/area_cosecha)*10 
#   
#   round(rinde_s/(1-(h_deseada/100)), 0) 
  
  # print(paste0("El rinde medio del ensayo fue de",
  #              round(mean(rinde_s*100)), 
  #              "con humedad media de cosecha fue de ", 
  #              round(mean(h_muestra*100)), 
  #              "% (cv:", raster::cv(h_muestra)), "%)")
# }

# rinde_aj(surcos = 3, 
#          largo = 4 ,      # m
#          dist_surcos = 17.5, # cm
#          peso_h = 48.92,     # g
#          peso_s = 47.85,     # g
#          tara_s = 2,       # g 
#          peso_tot = 1297,   # g
#          h_deseada = 12  # %
# )
#https://stackoverflow.com/questions/29614849/dplyrmutate-to-add-multiple-values

rinde_aj2 <- function( # kg/ha
  surcos, 
  largo,      # m
  dist_surcos, # cm
  peso_h,     # g
  peso_s,     # g
  tara_s,       # g 
  peso_tot,   # g
  h_deseada   # %
){ 
  area_cosecha = surcos*largo*dist_surcos/100
  
  h_muestra = (peso_h-(peso_s-tara_s))/peso_h
  
  contenido_h = peso_tot*h_muestra
  
  rinde_s = ((peso_tot-contenido_h)/area_cosecha)*10 
  
  rinde_aj = round(rinde_s/(1-(h_deseada/100)), 0) 
  
  bind_rows(setNames(c(h_muestra*100, rinde_aj), 
                     c("hum", "rinde_aj")))
  }
# rinde_aj2(surcos = 3,
#          largo = 4 ,      # m
#          dist_surcos = 17.5, # cm
#          peso_h = 48,     # g
#          peso_s = 47,     # g
#          tara_s = 2,       # g
#          peso_tot = 1297,   # g
#          h_deseada = 12  # %
# )

