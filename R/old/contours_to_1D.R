# extract contour of a 2d image and map it to th distance of the center
source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite("EBImage")

library("EBImage")
x = readImage(system.file("images", "shapes.png", package = "EBImage"))
x = x[1:120, 50:120]
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
apply(oc$`1`[1:2,], 1, function(x) {proxy::dist(x, center)})
dist_to_center = dist(oc$`1`, matrix(center, nrow = 1), by_rows = TRUE)

plot(dist_to_center)
