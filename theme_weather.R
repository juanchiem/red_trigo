library(ggplot2)

theme_set(theme_bw() +
            theme(axis.text.x = element_text(hjust=-1.5),
                  panel.grid.minor = element_blank(),
                  panel.grid.major = element_line(linetype="dotted"),
                  plot.title = element_text(size = 11, face = "bold"),
                  plot.caption = element_text(hjust=1)))

#ht = function(d, n=6) rbind(head(d, n), tail(d, n))
