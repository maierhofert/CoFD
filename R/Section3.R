# this is the R Code that will be used in Section 3 of the paper

################################################################################
### Section 3, classiFunc package

# Chunk 1

classiKnn(classes, fdata, grid = 1:ncol(fdata), knn = 1L, 
          metric = "L2", nderiv = 0L, ...)

classiKernel(classes, fdata, grid = 1:ncol(fdata), h = 1, 
             metric = "L2", ker = "Ker.norm", 
             nderiv = 0L, ...)


# Chunk 2

# install classiFunc package once
# install.packages("classiFunc")
devtools::install_github("maierhofert/classiFunc", ref = "devel")

# load package in every new R-session
library("classiFunc")

# load the example data
data("BeetleFly")
dat = BeetleFly

# random test/train split
set.seed(1)
train.rows = sample(c(TRUE, FALSE), size = nrow(dat), 
                    replace = TRUE, prob = c(0.5, 0.5))

# create nearest neighbor estimator with default values
nn.mod = classiKnn(classes = dat[train.rows, "target"], 
                   fdata = dat[train.rows, 1:512])

# create kernel estimator with manually set bandwidth
ker.mod = classiKernel(classes = dat.train[train.rows, "target"], 
                       fdata = dat.train[train.rows, 1:512],
                       h = 10)

# Chunk 3

# predict nearest neighbor estimators
# hyperparameters (knn, h, ker, nderiv, ...) are stored in model
# and do not have to be specified again
pred.nn = predict(nn.mod, newdata = dat.test[!train.rows, 1:512])
pred.ker = predict(ker.mod, newdata = dat.test[!train.rows, 1:512])

# Chunk 4

# confusion matrix for nn estimator
table(pred = pred.nn, true = dat.test[, "target"])
# confusion matrix for kernel estimator
table(pred = pred.ker, true = dat.test[, "target"])

################################################################################

