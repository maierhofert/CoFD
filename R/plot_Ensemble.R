# Show model weights in LCE and RFE
library("mlr")
library("dplyr")
library("ggplot2")

# load BeetleFly example data
data("BeetleFly", package = "classiFunc")
BeetleFly = BeetleFly[,-ncol(BeetleFly)]
# # export BeetleFly data to mlr data format
fdata = makeFunctionalData(BeetleFly, exclude.cols = c("target"))

# create mlr task from data
task = makeClassifTask(data = fdata, target = "target")
# task = subsetTask(task, c(F, T))
set.seed(1234)

# define base learners
b.lrn1 = makeLearner("classif.classiFunc.knn",
                     id = "L2",
                     par.vals = list(metric = "L2", knn = 3), 
                     predict.type = "prob")

library("dtw")
b.lrn2 = makeLearner("classif.classiFunc.knn", 
                     id = "dtw",
                     par.vals = list(metric = "dtw", knn = 3), 
                     predict.type = "prob")

b.lrn3 = makeLearner("classif.classiFunc.knn", 
                     id = "globMax",
                     par.vals = list(metric = "globMax", knn = 3), 
                     predict.type = "prob")

b.lrn4 = makeLearner("classif.classiFunc.knn", 
                     id = "random",
                     par.vals = list(metric = "custom.metric", 
                                     knn = 3,
                                     custom.metric = function(x, y) runif(1)), 
                     predict.type = "prob")

# create LCE
# use 10 fold CV (default is leave-one-out-CV) for faster run time.
LCE.lrn = makeStackedLearner(
  base.learners = list(b.lrn1, b.lrn2, b.lrn3, b.lrn4), 
  # base.learners = list(b.lrn1, b.lrn3), 
  predict.type = "prob", 
  resampling = makeResampleDesc("CV", iters = 10L),
  method = "classif.bs.optimal")

# create TE (tree ensemble)
TE.lrn = makeStackedLearner(
  base.learners = list(b.lrn1, b.lrn2, b.lrn3, b.lrn4), 
  # base.learners = list(b.lrn1, b.lrn3), 
  predict.type = "prob",
  resampling = makeResampleDesc("CV", iters = 10L),
  method = "stack.cv",
  super.learner = "classif.ctree")

# create RFE
RFE.lrn = makeStackedLearner(
  base.learners = list(b.lrn1, b.lrn2, b.lrn3, b.lrn4), 
  # base.learners = list(b.lrn1, b.lrn3), 
  predict.type = "prob",
  resampling = makeResampleDesc("CV", iters = 10L),
  method = "stack.cv",
  super.learner = "classif.randomForest")

# train models on the training data
set.seed(1)
LCE.m = train(LCE.lrn, task = task)
TE.m = train(TE.lrn, task = task)
RFE.m = train(RFE.lrn, task = task)

