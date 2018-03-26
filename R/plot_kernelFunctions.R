# create plot for kernel functions in paper
library("classiFunc")
library("fda.usc")
library("ggplot2")
library("reshape2")
mytheme = theme_bw(15)

kers = kerChoices()
kers = kers[kers %in% c("Ker.norm", "Ker.cos", "Ker.epa", "Ker.tri", "Ker.quar",
                        "Ker.unif")]

# create data to be plotted
x = seq(-1.1, 1.1, by = 0.01)
dat = sapply(kers, function(ker) {
  do.call(ker, list(x))
})
dat = as.data.frame(dat)
colnames(dat) = c("Gaussian", "Cosine", "Epanechnikov", "Triweight", 
                  "Quartic", "Uniform")
dat$x = x

# convert to long format for ggplot
longDat <- melt(dat, id.vars = c("x"))



# plot
ggplot(longDat) +
  geom_line(aes(x = x, y = value, color = variable), size = 1) +
  scale_color_discrete("") +
  xlab("u") +
  ylab("K(u)") +
  mytheme
ggsave("Plots/kernelFunctions.pdf", width = 7, height = 4)
