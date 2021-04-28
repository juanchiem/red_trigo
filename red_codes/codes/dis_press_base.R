df <- tibble::tribble(
  ~enf, ~press,   ~x,    ~y,
  "MA",  "baja",   0,     5,
  "MA",  "baja",   1,     5,
  "MA",  "media",  0,     5,
  "MA",  "media",  1,     5,
  "MA",  "alta",   0,     90,
  "MA",  "alta",   1,     90,

  "RE",  "baja",   0,     10,
  "RE",  "baja",   1,     10,
  "RE",  "media",  0,     10,
  "RE",  "media",  1,     10,
  "RE",  "alta",   0,     80,
  "RE",  "alta",   1,     80,
  
  "RH",  "baja",   0,     10,
  "RH",  "baja",   1,     10,
  "RH",  "media",  0,     10,
  "RH",  "media",  1,     10,
  "RH",  "alta",   0,     80,
  "RH",  "alta",   1,     80) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(press= fct_relevel(press, "alta", "media", "baja"))

df %>%  
  ggplot(aes(x = x, y = y, fill = press)) +
  geom_area(position = 'stack', alpha=0.2, outline.type = "lower")+
  facet_wrap(~enf)
