# this file creates the models/learners for the benchmark experiments
library("mlr")
# reference models classifiers
k1nd0_eucl = makeLearner(cl = "classif.classiFunc.knn",
                         id = paste0("knn", 1, "nderiv", 0,
                                     "_eucl"),
                         metric = "Euclidean",
                         predict.type = "prob",
                         par.vals = list(knn = 1, nderiv = 0))
k1nd0_eucl$short.name = "Eucl: k 1; nderiv 0"

# create a dtw learner
library("dtw")
k1nd0_dtw = makeLearner(cl = "classif.classiFunc.knn",
                        id = paste0("knn", 1, "nderiv", 0,
                                    "_dtw"),
                        metric = "dtw",
                        predict.type = "prob",
                        par.vals = list(knn = 1, nderiv = 0))
k1nd0_dtw$short.name = "dtw: k 1; nderiv 0"


lrn.kernel = makeLearner("classif.classiFunc.kernel", predict.type = "prob")
# create parameter set
parSet.bandwidth = makeParamSet(
  makeNumericParam(id = "h", lower = -1, upper = 4, 
                   trafo = function(x) 10^x)
)
# control for tuning hyper parameters
# Use higher resolution in application
ctrl = makeTuneControlGrid(resolution = 20L)
# tuned learners
lrn.bandwidth.tuned = makeTuneWrapper(learner = lrn.kernel, 
                                      resampling = makeResampleDesc("CV", iters = 5),
                                      measures = mmce,
                                      par.set = parSet.bandwidth,
                                      control = ctrl)
lrn.kernel.tuned = lrn.bandwidth.tuned
lrn.kernel.tuned$short.name = "Eucl-Kernel: h CV-opt"



# ######################################################################
# define other semimetrics
# create the phase and amplitude distance learners
k1nd0_phase = makeLearner(cl = "classif.classiFunc.knn",
                          id = paste0("knn", 1, "nderiv", 0,
                                      "_phase"),
                          metric = "phaseDistance",
                          predict.type = "prob",
                          par.vals = list(knn = 1, nderiv = 0))
k1nd0_phase$short.name = "phase: k 1; nderiv 0"
k1nd0_amplitude = makeLearner(cl = "classif.classiFunc.knn",
                              id = paste0("knn", 1, "nderiv", 0,
                                          "_amplitude"),
                              metric = "amplitudeDistance",
                              predict.type = "prob",
                              par.vals = list(knn = 1, nderiv = 0))
k1nd0_amplitude$short.name = "amplitude: k 1; nderiv 0"

# global min and global max
k1nd0_globMax = makeLearner(cl = "classif.classiFunc.knn",
                            id = paste0("knn", 1, "nderiv", 0,
                                        "_globMax"),
                            metric = "globMax",
                            predict.type = "prob",
                            par.vals = list(knn = 1, nderiv = 0))
k1nd0_globMax$short.name = "globMax: k 1; nderiv 0"

k1nd0_globMin = makeLearner(cl = "classif.classiFunc.knn",
                            id = paste0("knn", 1, "nderiv", 0,
                                        "_globMin"),
                            metric = "globMin",
                            predict.type = "prob",
                            par.vals = list(knn = 1, nderiv = 0))
k1nd0_globMin$short.name = "globMin: k 1; nderiv 0"

k1nd0_Manhattan = makeLearner(cl = "classif.classiFunc.knn",
                              id = paste0("knn", 1, "nderiv", 0,
                                          "_manhattan"),
                              metric = "Manhattan",
                              predict.type = "prob",
                              par.vals = list(knn = 1, nderiv = 0))
k1nd0_Manhattan$short.name = "Manhattan: k 1; nderiv 0"

###############################################################################
# define range of knn and nderiv
knn = c(1L, 5L, 9L, 13L)
nderiv = c(0L, 1L, 2L)

# function to create a list of all hyperparameter combinations
createHyperParVals = function(knn, nderiv) {
  hyperpar.vals = list()
  for (i in 1:length(knn)) {
    for (j in 1:length(nderiv)) {
      hyperpar.vals[[(i - 1)*length(nderiv) + j]] = list(knn = knn[i], nderiv = nderiv[j])
    }
  }
  return(hyperpar.vals)
}

knn_eucl_lrns = lapply(createHyperParVals(knn, nderiv[1]), function(par.set) {
  makeLearner(cl = "classif.classiFunc.knn",
              id = paste0("knn", par.set$knn, "nderiv", par.set$nderiv,
                          "_eucl"),
              metric = "Euclidean",
              predict.type = "prob",
              par.vals = par.set)
})

nderiv_eucl_lrns = lapply(createHyperParVals(knn[1], nderiv), function(par.set) {
  makeLearner(cl = "classif.classiFunc.knn",
              id = paste0("knn", par.set$knn, "nderiv", par.set$nderiv,
                          "_eucl"),
              metric = "Euclidean",
              predict.type = "prob",
              par.vals = par.set)
})

nderivKnn_eucl_lrns = lapply(createHyperParVals(knn, nderiv), function(par.set) {
  makeLearner(cl = "classif.classiFunc.knn",
              id = paste0("knn", par.set$knn, "nderiv", par.set$nderiv,
                          "_eucl"),
              metric = "Euclidean",
              predict.type = "prob",
              par.vals = par.set)
})

nderivKnn_manhattan_lrns = lapply(createHyperParVals(knn, nderiv), function(par.set) {
  makeLearner(cl = "classif.classiFunc.knn",
              id = paste0("knn", par.set$knn, "nderiv", par.set$nderiv,
                          "_manhattan"),
              metric = "Manhattan",
              predict.type = "prob",
              par.vals = par.set)
})

nderivKnn_globMax_lrns = lapply(createHyperParVals(knn, nderiv), function(par.set) {
  makeLearner(cl = "classif.classiFunc.knn",
              id = paste0("knn", par.set$knn, "nderiv", par.set$nderiv,
                          "_globMax"),
              metric = "globMax",
              predict.type = "prob",
              par.vals = par.set)
})

nderivKnn_globMin_lrns = lapply(createHyperParVals(knn, nderiv), function(par.set) {
  makeLearner(cl = "classif.classiFunc.knn",
              id = paste0("knn", par.set$knn, "nderiv", par.set$nderiv,
                          "_globMin"),
              metric = "globMin",
              predict.type = "prob",
              par.vals = par.set)
})


# Ensemble learners
# with knne Fuchs etal 2016
# knn_eucl_ensemble = makeStackedLearner(id = "LCE:knn_eucl",
#                                        base.learners = knn_eucl_lrns,
#                                        predict.type = "prob",
#                                        resampling = makeResampleDesc("CV", iters = 10L),
#                                        method = "classif.bs.optimal")
# knn_eucl_ensemble$short.name = "LCE: Eucl; k 1, 5, 9, 13; nderiv 0"
#   
# nderiv_eucl_ensemble = makeStackedLearner(id = "LCE:nderiv_eucl",
#                                           base.learners = nderiv_eucl_lrns,
#                                           predict.type = "prob",
#                                           resampling = makeResampleDesc("CV", iters = 10L),
#                                           method = "classif.bs.optimal")
# nderiv_eucl_ensemble$short.name = "LCE: Eucl; k 1; nderiv 0, 1, 2"
# 
# semimet_ensemble = makeStackedLearner(id = "LCE:semimet",
#                                        base.learners = list(k1nd0_eucl,
#                                                          k1nd0_Manhattan,
#                                                          k1nd0_globMax,
#                                                          k1nd0_globMin),
#                                        predict.type = "prob",
#                                        resampling = makeResampleDesc("CV", iters = 10L),
#                                        method = "classif.bs.optimal")
# semimet_ensemble$short.name = "LCE: semimet; k 1; nderiv 0"


LCE = makeStackedLearner(id = "LCE",
                         base.learners = c(nderivKnn_eucl_lrns,
                                           nderivKnn_manhattan_lrns,
                                           nderivKnn_globMax_lrns,
                                           nderivKnn_globMin_lrns),
                         predict.type = "prob",
                         resampling = makeResampleDesc("CV", iters = 3L),
                         method = "classif.bs.optimal")
LCE$short.name = "LCE"


# with random forest ensemble

# random forest ensemble for different semimetrics
# rf_feat_ensemble = makeStackedLearner(id = "rf_feat_semimet_ensemble",
#                                            base.learners = c(nderivKnn_eucl_lrns,
#                                                              nderivKnn_manhattan_lrns,
#                                                              nderivKnn_globMax_lrns,
#                                                              nderivKnn_globMin_lrns), 
#                                            super.learner = "classif.randomForest",
#                                            predict.type = "prob",
#                                            use.feat = TRUE,
#                                            # resampling = makeResampleDesc("CV", iters = 3L),
#                                            method = "stack.cv")
# rf_feat_ensemble$short.name = "rf-ens: k 1, 5, 9, 13; nderiv 0, 1, 2; use feat"

RFE = makeStackedLearner(id = "RFE",
                         base.learners = c(nderivKnn_eucl_lrns,
                                           nderivKnn_manhattan_lrns,
                                           nderivKnn_globMax_lrns,
                                           nderivKnn_globMin_lrns), 
                         super.learner = "classif.randomForest",
                         predict.type = "prob",
                         use.feat = FALSE,
                         # resampling = makeResampleDesc("CV", iters = 3L),
                         method = "stack.cv")
RFE$short.name = "RFE"



#  learners with a lot of random noisy base learners
random_knn_100 = list()
for (i in 1:100) {
  random_knn_100[[i]] = makeLearner(cl = "classif.classiFunc.knn",
                                    id = paste0("noisy_learner", i),
                                    metric = "custom.metric",
                                    custom.metric = function(x, y) {
                                      runif(1)
                                    },
                                    predict.type = "prob")
}

RFE_noisy = makeStackedLearner(id = "RFE_noisy",
                               base.learners = c(nderivKnn_eucl_lrns,
                                                 nderivKnn_manhattan_lrns,
                                                 nderivKnn_globMax_lrns,
                                                 nderivKnn_globMin_lrns,
                                                 random_knn_100), 
                               super.learner = "classif.randomForest",
                               predict.type = "prob",
                               use.feat = FALSE,
                               # resampling = makeResampleDesc("CV", iters = 3L),
                               method = "stack.cv")
RFE_noisy$short.name = "noisy RFE"

LCE_noisy = makeStackedLearner(id = "LCE_noisy",
                               base.learners = c(nderivKnn_eucl_lrns,
                                                 nderivKnn_manhattan_lrns,
                                                 nderivKnn_globMax_lrns,
                                                 nderivKnn_globMin_lrns,
                                                 random_knn_100),
                               predict.type = "prob",
                               resampling = makeResampleDesc("CV", iters = 3L),
                               method = "classif.bs.optimal")
LCE_noisy$short.name = "noisy LCE"



# list of learners to be compared together
reference_lrns = list(k1nd0_eucl, lrn.kernel.tuned, k1nd0_dtw)
ensemble_lrns = list(LCE, LCE_noisy,
                     RFE, RFE_noisy)


# # train learners
# # This is not needed for benchmarking
# mod1 = train(learner = LCE, task = tsks[[1]])
# mod2 = train(learner = LCE, task = tsks[[2]])
# fda.pred = predict(mod1, task = tsks[[1]])
# fda.pred2 = predict(mod2, task = tsks[[2]])


# ##############################################################################
# create CV opt base learners
# hyper parameters for optimal nderiv/knn/semimetric
hp.ctrl = makeTuneControlGrid()
# tune wrapped learner
hp.res = makeResampleDesc("CV", iters = 5)

# parameter values
knn.vals = c(1, 5, 9, 13)
knn.pars = makeDiscreteLearnerParam("knn", knn.vals)
nderiv.vals = c(0, 1, 2)
nderiv.pars = makeDiscreteLearnerParam("nderiv", nderiv.vals)
semimet.vals = c("Euclidean", "Manhattan", "globMax", "globMin")
semimet.pars = makeDiscreteLearnerParam("metric", semimet.vals)


# parameter sets
parSet.knn = makeParamSet(knn.pars)
parSet.nderiv = makeParamSet(nderiv.pars)
parSet.semimet = makeParamSet(semimet.pars)
parSet.knnNderivSemimet = makeParamSet(knn.pars, nderiv.pars, semimet.pars)

# base learners with optimal nderiv/knn/semimetric
knnOptNderivOptSemimetOpt = makeLearner(cl = "classif.classiFunc.knn",
                                        id = "knnOptNderivOptSemimetOpt",
                                        predict.type = "prob")
knnOptNderivOptSemimetOpt = makeTuneWrapper(learner = knnOptNderivOptSemimetOpt, 
                                            resampling = hp.res,
                                            measures = multiclass.brier,
                                            par.set = parSet.knnNderivSemimet,
                                            control = hp.ctrl)
knnOptNderivOptSemimetOpt$short.name = "Opt base learner"


# # run test
# mod = train(knn1NderivOpt_eucl, tsks[[1]])
# mod$learner.model$opt.result

lrns = c(reference_lrns, list(knnOptNderivOptSemimetOpt), ensemble_lrns)


