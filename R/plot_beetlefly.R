# extract contour of a 2d image and map it to th distance of the center
source("http://bioconductor.org/biocLite.R")
# biocLite()
# biocLite("EBImage")

library("EBImage")
# x = readImage(system.file("images", "shapes.png", package = "EBImage"))
x = readImage("Plots/beetlefly.png")
colorMode(x) = Grayscale

# for one beetle
b = x[1:58, 20:122, 1]
writeImage(b, "Plots/Beetle1.JPG")

# invert colors
b = abs(b - 1)
ocb = ocontour(b)

# plot outline
jpeg("Plots/Beetle1_outline.JPG")
plot(ocb[[1]], type = 'l')
points(ocb[[1]], col = 2)

# find center (maybe use median instead for robustness)
center = colMeans(ocb$`1`)
points(center[1], center[2])

# mark "start point"
points(ocb$`1`[1,1], ocb$`1`[1,2], col = "green")
dev.off()

# plot distance to outline
jpeg("Plots/Beetle1_dist_outline.JPG")
plot(dist_to_center)
dev.off()

# for one fly
f = x[1:80, 122:234, 1]
writeImage(f, "Plots/Fly1.PNG")
# invert colors
f = abs(f - 1)
f = rotate(f, angle = 280)

display(x)
oc = ocontour(x)
plot(oc[[1]], type = 'l')
points(oc[[1]], col = 2)

# find center (maybe use median instead for robustness)
center = colMeans(oc$`1`)
points(center[1], center[2])

# mark "start point"
points(oc$`1`[1,1], oc$`1`[1,2], col = "green")

# find distance to centerpoint
# apply(oc$`1`[1:2,], 1, function(x) {proxy::dist(x, center)})
dist_to_center = proxy::dist(oc$`1`, matrix(center, nrow = 1), by_rows = TRUE)

plot(dist_to_center)
