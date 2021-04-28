library(ggplot2)
data(presidential)
data(economics)
events <- presidential[-(1:3),]
baseline = min(economics$unemploy)
delta = 0.05 * diff(range(economics$unemploy))
events$ymin = baseline
events$timelapse = c(diff(events$start),Inf)
events$bump = events$timelapse < 4*370 # ~4 years
offsets <- rle(events$bump)
events$offset <- unlist(mapply(function(l,v) {if(v){(l:1)+1}else{rep(1,l)}}, l=offsets$lengths, v=offsets$values, USE.NAMES=FALSE))
events$ymax <- events$ymin + events$offset * delta
xrange = range(c(economics$date, events$start))

p1 <- ggplot(data=economics, mapping=aes(x=date, y=unemploy)) +
  geom_line() +
  scale_x_date("", limits=xrange) +  
  scale_y_continuous(name="unemployed [1000's]") +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())

ylims <- c(0, (max(events$offset)+1)*delta) + baseline

p2 <- ggplot(events, aes(x=start, color=party)) +
  geom_segment(mapping=aes(y=ymin, xend=start, yend=ymax)) +
  geom_point(mapping=aes(y=ymax), size=1) +
  geom_text(mapping=aes(y=ymax, label=name), hjust=-0.1, vjust=0.1, size=3) +
  scale_x_date("time", limits=xrange) +
  scale_y_continuous("", breaks=NULL, limits=ylims)

library(patchwork)
p1/ p2 + 
  plot_layout( heights = unit(c(5, 1), c('cm', 'null')))

