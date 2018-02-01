# this file creates a figure depicting the effect of different weights
# of two base estimators in the nearest neighbor ensemble

library("ggplot2")

# Funktion zur Erzeugung der Daten
pp <- function (n,r=4) {
  x <- seq(0, 1, len=n)
  df <- expand.grid(x=x, y=x)
  df$equal <- (df$x + df$y) / 2
  df$x_two_thirds <-  (2 * df$x + df$y) / 3
  df
}
p <- ggplot(pp(50), aes(x=x,y=y)) +
  theme_gray(30)


# figure for equal weights
p_equal <- p + geom_tile(aes(fill=equal)) +
  scale_fill_gradient2("ensemble-\nestimation\n",
                       low = "firebrick3", mid = "white",
                       high = "steelblue3", midpoint = 0.5, space = "Lab",
                       na.value = "grey50", guide = "colourbar") +
  xlab("estimation 1 (weight 1)") + ylab("estimation 2 (weight 1)")
p_equal

# for weights 2:1 
p_x_two_thirds <-  p + geom_tile(aes(fill=x_two_thirds)) +
  scale_fill_gradient2("ensemble-\nestimation\n",
                       low = "firebrick3", mid = "white",
                       high = "steelblue3", midpoint = 0.5, space = "Lab",
                       na.value = "grey50", guide = "colourbar") +
  xlab("estimation 1 (weight 2/3)") + ylab("estimation 2 (weight 1/3)")
p_x_two_thirds

# for weights 1:0
p_x <- p + geom_tile(aes(fill=x)) +
  scale_fill_gradient2("ensemble-\nestimation\n",
                       low = "firebrick3", mid = "white",
                       high = "steelblue3", midpoint = 0.5, space = "Lab",
                       na.value = "grey50", guide = "colourbar") +
  xlab("estimation 1 (weight 1)") + ylab("estimation 2 (weight 0)")
p_x



# Abspeichern der Grafiken ohne Legende
ggsave(filename = "Grafiken/weight_plot_p_x_two_thirds_no_legend.pdf", 
       plot = p_x_two_thirds + guides(fill = "none"), height = 7, width = 7.5)

ggsave(filename = "Grafiken/weight_plot_p_equal_no_legend.pdf", 
       plot = p_equal + guides(fill = "none"), height = 7, width = 7.5)

ggsave(filename = "Grafiken/weight_plot_p_x_no_legend.pdf", 
       plot = p_x + guides(fill = "none"), height = 7, width = 7.5)

# Abspeichern mit Legende
ggsave(filename = "Grafiken/weight_plot_p_x.pdf", 
       plot = p_x, height = 7, width = 10)



