# this is the R Code that will be used in Section 4 of the paper

################################################################################
### Section 4, usage through mlr-interface
# Section 4.1 Replication of Basic Functionality
# chunk 1
# install.packages("mlr") # use the version on github if my pull request is not yet merged
devtools::install_github("maierhofert/mlr", ref = "classiFunc")
library("mlr")

# chunk 2
lrn.nn = makeLearner(cl = "classif.classiFunc.knn", 
                     predict.type = "prob")
lrn.ker = makeLearner(cl = "classif.classiFunc.kernel", 
                      h = 15,
                      predict.type = "prob")

# chunk 3
data("BeetleFly", package = "classiFunc")
BeetleFly = BeetleFly[,-ncol(BeetleFly)]
fdata = makeFunctionalData(BeetleFly, exclude.cols = c("target"))
task = makeClassifTask(data = fdata, target = "target")

# chunk 4
task.train = subsetTask(task, subset = train.rows)
task.test = subsetTask(task, subset = !train.rows)


# chunk 5
mod.nn = train(learner = lrn.nn, task = task.train)
mod.ker = train(learner = lrn.ker, task = task.train)

# chunk 6
pred.nn = predict(mod.nn, task = task.test)
pred.ker = predict(mod.ker, task = task.test)


# chunk 7
measureMulticlassBrier(getPredictionProbabilities(pred.nn, c("1", "2")),
                       getTaskTargets(task.test))

measureMulticlassBrier(getPredictionProbabilities(pred.ker, c("1", "2")),
                       getTaskTargets(task.test))


################################################################################
# Section 4.2 Automated Hyperparameter Tuning
# chunk 1
par.h = makeNumericParam(id = "h", lower = 0, upper = 2, 
                         trafo = function(x) 10 ^ x)
parSet.h = makeParamSet(par.h)

# chunk 2
set.seed(1)
lrn.tuned = makeTuneWrapper(learner = lrn.ker,
                            resampling = makeResampleDesc("CV", iters = 5),
                            measures = brier,
                            par.set = parSet.h,
                            control = makeTuneControlGrid())

# chunk 3
m.tuned = train(lrn.tuned, task.train)

# chunk 4
pred.tuned = predict(m.tuned, task = task.test)
measureMulticlassBrier(getPredictionProbabilities(pred.tuned, c("1", "2")),
                       getTaskTargets(task.test))


################################################################################
# Section 4.3 Creating Customized Ensembles

# chunk 1
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
                     id = "globMax",
                     par.vals = list(metric = "globMax"), 
                     predict.type = "prob")
b.lrn4 = makeLearner("classif.classiFunc.knn", 
                     id = "random",
                     par.vals = list(metric = "custom.metric", 
                                     custom.metric = function(x, y) runif(1)), 
                     predict.type = "prob")

# chunk 2

LCE.lrn = makeStackedLearner(
  base.learners = list(b.lrn1, b.lrn2, b.lrn3, b.lrn4), 
  predict.type = "prob", 
  resampling = makeResampleDesc("CV", iters = 10L),
  method = "classif.bs.optimal")

RFE.lrn = makeStackedLearner(
  base.learners = list(b.lrn1, b.lrn2, b.lrn3, b.lrn4), 
  predict.type = "prob",
  resampling = makeResampleDesc("CV", iters = 10L),
  method = "stack.cv",
  super.learner = "classif.randomForest")


# chunk 3
set.seed(1)
LCE.m = train(LCE.lrn, task = task.train)
RFE.m = train(RFE.lrn, task = task.train)


# chunk 4
LCE.pred = predict(LCE.m, task = task.test)
RFE.pred = predict(RFE.m, task = task.test)

measureMulticlassBrier(getPredictionProbabilities(LCE.pred, c("1", "2")),
                       getTaskTargets(task.test))
measureMulticlassBrier(getPredictionProbabilities(RFE.pred, c("1", "2")),
                       getTaskTargets(task.test))
