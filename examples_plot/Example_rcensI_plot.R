library(tidyverse)
library(ggplot2)

#source("R/rcensI.R")
library(rcens)

# Example rcensI ------
#Example Weibull - Uniform----
alpha = 2
beta = 1
scale = beta ** (-1/alpha )
theta = .5


Data = rcensI(rdistrX = rweibull, rdistrC = runif,
              param_X = list("shape" = alpha, "scale" = scale),
              param_C = list("min" = 0, "max" = 1),
              n = 1e02, theta = theta)


## Plot Intervalar Censoring Data ------

color_line = "#E14D2A"
color_point_i = "red"# "#8B2323"
color_point_f = "blue"#"#3E6D9C"
color_pt = "#008B00"


A %>%
  select(Lower, Upper) %>%
  arrange(Lower, Upper) %>%
  mutate(inicio = 0, index = desc(row_number())) %>%
  ggplot() +

  # Segments
  geom_segment(aes(x = inicio, y = index, xend = Lower, yend = index), color = color_line) +
  geom_segment(aes(x = Lower, y = index, xend = Upper, yend = index), color = color_pt, linetype = "dashed") +

  # Points with color and legend mapping
  geom_point(aes(x = Lower, y = index, color = "Lower Bound")) +  # Map to 'Cota inferior'
  geom_point(aes(x = Upper, y = index, color = "Upper Bound")) +  # Map to 'Cota superior'

  # Theme
  theme(
    plot.title = element_text(color = "black", hjust = 0.5, size = rel(2)),
    plot.title.position = "panel",
    panel.background = element_rect(fill = "transparent"),
    panel.grid.major.x = element_line(color = "gray", linetype = "dashed"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Borde negro
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = rel(2)),
    axis.title.x = element_text(size = rel(1)),
    legend.text = element_text(size = 15),
    legend.position = c(0.9, 0.9),
    legend.title = element_blank(),  # No legend title
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.01), "npc")
  ) +

  # Labels
  labs(title = "Intervalar Censoring Data") +
  xlab("Time") +

  # Custom colors for the legend
  scale_color_manual(values = c("Lower Bound" = "red", "Upper Bound" = "blue"))
