## Graphs for Powerpoint presentation

g1 <- ggplot(hunt, aes(x = position.cat, y = production)) +
  geom_boxplot(fill = "grey60", alpha = 0.6, color = "grey60") +
  scale_x_discrete() + 
  xlab("Position") +
  ylab("Production") +
  theme_minimal(base_family = "Raleway", base_size = 22) +
  theme(panel.background = element_rect(fill = "#FCFCFC", colour = NA),
        plot.background = element_rect(fill = "#FCFCFC", colour = NA))

## Add lines
dat <- ggplot_build(g1)$data[[1]]
g1 <- g1 + geom_segment(data=dat, aes(x=xmin, xend=xmax, y=middle, yend=middle), color = "coral2", size=1) 



g2 <- ggplot(hunt, aes(cut2(phi.h, g = 3), y = production)) +
  geom_boxplot(fill = "grey60", alpha = 0.6, color = "grey60") +
  scale_x_discrete() + 
  xlab("phi.h") +
  ylab("Production") +
  theme_minimal(base_family = "Raleway", base_size = 22) +
  theme(panel.background = element_rect(fill = "#FCFCFC", colour = NA),
        plot.background = element_rect(fill = "#FCFCFC", colour = NA))

## Add lines
dat <- ggplot_build(g2)$data[[1]]
g2 <- g2 + geom_segment(data=dat, aes(x=xmin, xend=xmax, y=middle, yend=middle), color = "coral2", size=1) 




g3 <- ggplot(hunt, aes(cut2(pressure, g = 3), y = production)) +
  geom_boxplot(fill = "grey60", alpha = 0.6, color = "grey60") +
  scale_x_discrete() + 
  xlab("Pressure") +
  ylab("Production") +
  theme_minimal(base_family = "Raleway", base_size = 22) +
  theme(panel.background = element_rect(fill = "#FCFCFC", colour = NA),
        plot.background = element_rect(fill = "#FCFCFC", colour = NA))

## Add lines
dat <- ggplot_build(g3)$data[[1]]
g3 <- g3 + geom_segment(data=dat, aes(x=xmin, xend=xmax, y=middle, yend=middle), color = "coral2", size=1) 

library(patchwork)

g1+g2+g3
