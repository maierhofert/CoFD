library("classiFunc")
library("dtw")

# see all choices
choices = metricChoices()

# personally, I think that these are the most important semimetrics
choices = c("L2", "L1", "supremum", "Lp", "dtw", "shortEuclidean", "mean", "relAreas", 
            "jump", "globMax", "globMin", "points", "custom.metric", 
            "amplitudeDistance", "phaseDistance", 
            "dtwPath",      
            "rucrdtw", "rucred") # just faster versions of dtw and L2
# Here is the documentation on them
# most important semimetrics and their additional parameters are described here
?classiFunc::computeDistMat


# the ranges for the hyper parameter values are listed in my mlr branch called classiFunc 
# in the file /R/RLearner_classif_classiFunc.knn.R
makeRLearner.classif.classiFunc.knn = function() {
  makeRLearnerClassif(
    cl = "classif.classiFunc.knn",
    package = "classiFunc",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "knn",
                              lower = 1L,
                              # upper = expression(0.5 * nrow(getTaskData(task))),
                              default = 1L),
      # FIXME: is it ok to hand over the values with this function
      # from the original package?
      makeDiscreteLearnerParam(id = "metric", default = "Euclidean",
                               values = classiFunc::metricChoices()),
      makeIntegerLearnerParam(id = "nderiv", default = 0L, lower = 0L),
      makeLogicalLearnerParam(id = "derived", default = FALSE, tunable = FALSE),
      makeDiscreteLearnerParam(id = "deriv.method", default = "base.diff",
                               values = c("base.diff", "fda.deriv.fd")),
      makeFunctionLearnerParam(id = "custom.metric",
                               default = function(x, y, ...) {
                                 return(sqrt(sum( (x - y) ^ 2)))
                               },
                               requires = quote(metric == "custom.metric"),
                               tunable = FALSE),
      # additional arguments to computeDistMat
      makeNumericLearnerParam(id = "dmin", default = 0,
                              lower = 0, upper = 1),
      makeNumericLearnerParam(id = "dmax", default = 1,
                              lower = 0, upper = 1),
      makeNumericLearnerParam(id = "dmin1", default = 0,
                              lower = 0, upper = 1),
      makeNumericLearnerParam(id = "dmax1", default = 1,
                              lower = 0, upper = 1),
      makeNumericLearnerParam(id = "dmin2", default = 0,
                              lower = 0, upper = 1),
      makeNumericLearnerParam(id = "dmax2", default = 1,
                              lower = 0, upper = 1),
      makeNumericLearnerParam(id = "t1", default = 0,
                              lower = 0, upper = 1),
      makeNumericLearnerParam(id = "t2", default = 1,
                              lower = 0, upper = 1),
      makeNumericVectorLearnerParam(id = ".poi", # default = expression(seq(0, 1, ncol(task))),
                                    lower = 0, upper = 1),
      # additional arguments to metrics in computeDistMat
      makeNumericLearnerParam(id = "p", default = 2),
      # TODO additional arguments to Data2fd
      # TODO additional arguments to custom metric
      keys = c("task"),
      forbidden = expression(dmin >= dmax,
                             dmin1 >= dmax1 |
                               # dmax1 > dmin2 |
                               dmin2 >= dmax2,
                             knn %% 2 == 0)
    ),
    # par.vals = list(metric = "Euclidean", knn = 1L),
    properties = c("twoclass", "multiclass", "prob", "single.functional"),
    name = "classiFunc.knn",
    short.name = "classiFunc.knn",
    note = ""
  )
}
