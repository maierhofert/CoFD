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
# install.packages("classiFunc") # after new CRAN publication
devtools::install_github("maierhofert/classiFunc", ref = "devel")
library("classiFunc")

# Chunk 3
data("BeetleFly")
set.seed(1)
train.rows = sample(c(TRUE, FALSE), size = nrow(BeetleFly), 
                    replace = TRUE, prob = c(0.5, 0.5))

# Chunk 4
nn.mod = classiKnn(classes = BeetleFly[train.rows, "target"], 
                   fdata = BeetleFly[train.rows, 1:512])

ker.mod = classiKernel(classes = BeetleFly[train.rows, "target"], 
                       fdata = BeetleFly[train.rows, 1:512],
                       h = 10)

# Chunk 5
pred.nn = predict(nn.mod, newdata = BeetleFly[!train.rows, 1:512],
                  predict.type = "prob")
pred.ker = predict(ker.mod, newdata = BeetleFly[!train.rows, 1:512],
                   predict.type = "prob")

# Chunk 6
2 * mean((pred.nn[,1] - as.numeric(BeetleFly[!train.rows, "target"] == "1")) ^ 2)
2 * mean((pred.ker[,1] - as.numeric(BeetleFly[!train.rows, "target"] == "1")) ^ 2)

