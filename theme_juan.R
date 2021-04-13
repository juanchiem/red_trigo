theme_juan <- function (base_size = base_size, legen_pos = legen_pos) {
  theme_bw(base_size = base_size) %+replace% 
    theme(
      axis.text = element_text(colour = "black"),
      axis.title.x = element_text(colour = "black"),
      axis.title.y = element_text(colour = "black", angle=90),
      
      # strip.background = element_blank(), 
      # strip.text = element_text(size = rel(1.1)),#,face = "bold"),
      
      # panel.border = element_blank(),
      axis.line    = element_line(color='grey'),
      
      panel.grid.minor = element_blank(),
      # panel.grid.major = element_blank(),
      panel.grid.major = element_line(linetype =  "dotted"),
      
      # panel.grid.major.y = element_line(linetype =  "dotted"),
      # panel.grid.major.x = element_line(linetype =  "dotted") ,
      # legend
      legend.position=legen_pos,
      plot.caption = element_text(hjust = 0.5, 
                                  vjust = -1.5, 
                                  face = "italic", 
                                  size=rel(0.8)),
      plot.title = element_text(hjust = 0.5, 
                                vjust = 2, 
                                face = "bold", 
                                size=rel(1.2)),
      
      panel.spacing = unit(0.1,"lines"), 
    )   
}
