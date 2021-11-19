####### Dendogram ###

library(ggplot2)
library(ggdendro)

hc <- hclust(dist(USArrests),'ave')
hc
ggdendrogram(hc,rotate = FALSE,size=2)



## Here I show how to make full control over the data ###

model <- hclust(dist(USArrests),'ave')

model

dhc <- as.dendrogram(model)
dhc

#### Rectangular lines 

ddata <- dendro_data(dhc,type = 'rectangle')


p <- ggplot(segment(ddata))+
  geom_segment(aes(x=x,y=y,xend=xend,yend=yend))+
  coord_flip()+
  scale_y_reverse(expand=c(0.2,0))

### Modifying the theme   ###

p+theme_test()

