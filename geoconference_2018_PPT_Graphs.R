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



##
ggplot(hunt, aes(gross.pay, phi.h)) + 
  geom_point(color = "coral2", alpha = 0.8, size = 6) +   
  geom_smooth(method='lm', color = "grey40") +
  xlab("Gross Pay") +
  ylab("phi.h") +
  theme_minimal(base_family = "Raleway", base_size = 50) +
  theme(panel.background = element_rect(fill = "#FCFCFC", colour = NA),
        plot.background = element_rect(fill = "#FCFCFC", colour = NA))


##
ggplot(Predict(f1, fun = bar),
       colfill = "gray50",
       adj.subtitle = FALSE,
       layout = c(1,3),
       addlayer = theme_minimal(base_family = "Raleway", base_size = 30) +
         labs(y = "Production"))
##
heatmap <- Predict(f1, phi.h, pressure, position.cat, fun = bar, np = 90)
heatmap <- as.data.frame(heatmap)

ggplot(heatmap, aes(phi.h, pressure)) + 
                   geom_tile(aes(fill = yhat), color = "white")  + 
                   scale_fill_viridis(discrete = FALSE, option = "D") +
                   facet_grid(.~position.cat) +
                   labs(x = "\nphi.h",
                        y = "Pressure",
                        fill = "Predicted\nMean\nProduction",
                        title = "Predicted Production",
                        subtitle = "Predicted mean production as a function of pressure, phi.h and position",
                        caption = "Insufficient data to estimate production when pressure is 15.") + 
                   scale_x_continuous(breaks = seq(0, 160, 20)) +
                   scale_y_continuous(breaks = seq(0, 20, 2)) +
                   theme_minimal() +
                   theme(plot.title = element_text(face = "bold", hjust = 0, vjust = 0.8, colour = "#3C3C3C", size = 40, family = "Raleway")) +
                   theme(plot.subtitle = element_text(hjust = 0, vjust = 0.8, colour = "#3C3C3C", size = 20, family = "Raleway")) +
                   theme(plot.caption = element_text(size = 16, family = "Raleway")) +
                   theme(axis.title = element_text()) +
                   theme(axis.title.y = element_text(size = 16, angle = 90, family = "Raleway")) +
                   theme(axis.title.x = element_text(size = 16, angle = 0, family = "Raleway")) +
                   theme(axis.text.x = element_text(size = 14, family = "Raleway", color = "black", margin = margin(r=0))) +
                   theme(axis.text.y = element_text(size = 14, family = "Raleway", color = "black")) +
                   theme(legend.text = element_text(size = 12, family = "Raleway")) +
                   theme(legend.title = element_text(size = 13, family = "Raleway", hjust = 0.5)) +
                   theme(legend.position = "right") +
                   theme(axis.line = element_blank(), 
                         axis.ticks.y = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         panel.border = element_blank()) +
                   guides(colour = guide_legend(ncol = 1)) +
                   theme(legend.title.align=0) +
                   theme(panel.background = element_rect(fill = "#FCFCFC", colour = NA), plot.background = element_rect(fill = "#FCFCFC", colour = NA))
