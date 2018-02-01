# this is the R Code that will be used in Section 4 of the paper

################################################################################
### Section 4, usage through mlr-interface
# Section 4.1 Replication of Basic Functionality
# Chunk 1

# install mlr package once
# install.packages("mlr")
# use the version on github if my pull request is not yet merged
devtools::install_github("maierhofert/mlr", ref = "classiFunc")

# load mlr package in every new R-session
library("mlr")

# create learners (= model description in mlr)
lrn.nn = makeLearner(cl = "classif.classiFunc.knn", 
                     predict.type = "prob")
lrn.ker = makeLearner(cl = "classif.classiFunc.kernel", 
                      h = 10,
                      predict.type = "prob")


# Chunk 2

# load BeetleFly example data
data("BeetleFly", package = "classiFunc")
BeetleFly = BeetleFly[,-ncol(BeetleFly)]
# # export DTI data to mlr data format
fdata = makeFunctionalData(BeetleFly, exclude.cols = c("target"))

# create mlr task from data
task = makeClassifTask(data = fdata, target = "target")

# use same train/test split as in  Section 3
set.seed(1)
train.rows = sample(c(TRUE, FALSE), size = getTaskSize(task), 
                    replace = TRUE, prob = c(0.5, 0.5))

# create separate tasks for test/train split
task.train = subsetTask(task, subset = train.rows)
task.test = subsetTask(task, subset = !train.rows)


# chunk 3

# create models (train learners on training data)
mod.nn = train(learner = lrn.nn, task = task.train)
mod.ker = train(learner = lrn.ker, task = task.train)


# chunk 4

# use trained models to predict test data
pred.nn = predict(mod.nn, task = task.test)
pred.ker = predict(mod.ker, task = task.test)


# chunk 5

# confusion matrix for nn estimator
table(pred = getPredictionResponse(pred.nn),
      true = getTaskTargets(task.test))
# confusion matrix for kernel estimator
table(pred = getPredictionResponse(pred.ker), 
      true = getTaskTargets(task.test))

measureMMCE(getTaskTargets(task.test), getPredictionResponse(pred.nn))
measureMMCE(getTaskTargets(task.test), getPredictionResponse(pred.ker))

# get predicted probabilities
getPredictionProbabilities(pred.ker)

################################################################################
# Section 4.2 Automated Hyperparameter Tuning
# Chunk 1

# create classiKernel learner for classification of functional data
lrn.ker = makeLearner("classif.classiFunc.kernel", 
                      predict.type = "prob")

# create parameter set
parSet.h = makeParamSet(
  makeNumericParam(id = "h", lower = 0, upper = 2, 
                   trafo = function(x) 10 ^ x))

# control for tuning hyper parameters
# use higher resolution in application
ctrl = makeTuneControlGrid(resolution = 10L)

# control for resampling, use 5 fold CV
rdesc = makeResampleDesc("CV", iters = 5)

# create tuned learner
set.seed(1)
lrn.bandwidth.tuned = makeTuneWrapper(learner = lrn.ker, 
                                      resampling = rdesc,
                                      measures = mmce,
                                      # measures = multiclass.brier,
                                      # measures = brier,
                                      par.set = parSet.h,
                                      control = ctrl)

# train model on training data task
m.kern.tuned = train(lrn.bandwidth.tuned, task.train)


# Chunk 2

# predict test data set
pred.kern.tuned = predict(m.kern.tuned, task = task.test)
# confusion matrix for kernel estimator
table(pred = getPredictionResponse(pred.kern.tuned), 
      true = getTaskTargets(task.test))
measureMMCE(getTaskTargets(task.test), getPredictionResponse(pred.kern.tuned))


# Chunk 3
# get predicted probabilities
getPredictionProbabilities(pred.kern.tuned)


################################################################################
# Section 4.3 Creating Customized Ensembles


# Chunk 1

# create base learners
b.lrn1 = makeLearner("classif.classiFunc.knn",
                     id = "L2",
                     par.vals = list(metric = "L2"), 
                     predict.type = "prob")

library("dtw")
b.lrn2 = makeLearner("classif.classiFunc.knn", 
                     id = "dtw",
                     par.vals = list(metric = "dtw"), 
                     predict.type = "prob")

b.lrn3 = makeLearner("classif.classiFunc.knn", 
                      id = "globMin",
                      par.vals = list(metric = "globMin"), 
                      predict.type = "prob")

b.lrn4 = makeLearner("classif.classiFunc.knn", 
                     id = "random",
                     par.vals = list(metric = "custom.metric", 
                                     custom.metric = function(x, y) runif(1)), 
                     predict.type = "prob")
# Chunk 2

# create LCE
# set resampling to 10 fold CV (default is LOO-CV) for faster run time.
LCE.lrn = makeStackedLearner(
  base.learners = list(b.lrn1, b.lrn2, b.lrn3, b.lrn4), 
  predict.type = "prob", 
  resampling = makeResampleDesc("CV", iters = 10L),
  method = "classif.bs.optimal")

# create RFE
RFE.lrn = makeStackedLearner(
  base.learners = list(b.lrn1, b.lrn2, b.lrn3, b.lrn4), 
  super.learner = "classif.randomForest",
  predict.type = "prob",
  method = "stack.cv", 
  resampling = makeResampleDesc("CV", iters = 10L))


# Chunk 3

# train models on the training data
set.seed(1)
LCE.m = train(LCE.lrn, task = task.train)
RFE.m = train(RFE.lrn, task = task.train)


# Chunk 4

# predict test data set
LCE.pred = predict(LCE.m, task = task.test)
RFE.pred = predict(RFE.m, task = task.test)

# confusion matrix for LCE
table(pred = getPredictionResponse(LCE.pred), 
      true = getTaskTargets(task.test))
# confusion matrix for RFE
table(pred = getPredictionResponse(RFE.pred), 
      true = getTaskTargets(task.test))

# ##############################
# # miscellaneous
# # compute mean misclassification error (lower better)
# measureMMCE(getTaskTargets(task.test), getPredictionResponse(LCE.pred))
# measureMMCE(getTaskTargets(task.test), getPredictionResponse(RFE.pred))
# 
# # compute accuracy (higher better)
# measureACC(getTaskTargets(task.test), getPredictionResponse(LCE.pred))
# measureACC(getTaskTargets(task.test), getPredictionResponse(RFE.pred))
# 
# # compute brier score (lower better)
# measureBrier(getPredictionProbabilities(LCE.pred),
#              getTaskTargets(task.test), negative = "0", positive = "1")
# measureBrier(getPredictionProbabilities(RFE.pred),
#              getTaskTargets(task.test), negative = "0", positive = "1")
# # getPredictionProbabilities(LCE.pred)
# # getPredictionProbabilities(RFE.pred)


# plots
library("randomForest")
rf = RFE.m$learner.model$super.model$learner.model
varImpPlot(rf)


library("ggplot2")
weight = LCE.m$learner.model$weights
base.learners = BBmisc::extractSubList(LCE.m$learner.model$base.models, "learner",
                                       simplify = FALSE)
base.learners.id = sapply(base.learners, getLearnerId)
plot.data = data.frame(id = base.learners.id, weight = weight)
# barplot with the nnensemble weights
weight.plot <- ggplot(data = plot.data, aes(x = id, y = weight)) +
  geom_bar(stat = "identity") +
  xlab("base model") +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 0.95,
                                   vjust = 0.5))
weight.plot

