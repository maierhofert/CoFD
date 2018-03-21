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



# # TE
# # classification tree
# tree = TE.m$learner.model$super.model
# # pdf("Grafiken/tree_strucplot.pdf", 
# #     width = pdf.width, height = pdf.height)
# plot(tree$learner.model,
#      inner_panel = node_inner(tree$learner.model,
#                               abbreviate = FALSE,            # short variable names
#                               pval = FALSE,                 # no p-values
#                               id = FALSE),
#      terminal_panel = node_terminal(tree$learner.model, 
#                                     abbreviate = TRUE,
#                                     digits = 1,                   # few digits on numbers
#                                     fill = c("white"),            # make box white not grey
#                                     id = FALSE))
# dev.off()
# 
# 
# # tree.plot.raw = plotLearnerPrediction(tree$learner, mod.task, features = c("glucose", "BMI"))
# # 
# # tree.plot = tree.plot.raw + 
# #   ggtitle("") +
# #   scale_fill_manual(values = c("firebrick3", "steelblue3")) +
# #   ylab("body mass index") + 
# #   xlab("plasma glucose") +
# #   mytheme
# # tree.plot
# # ggsave("Grafiken/tree_plot.pdf", tree.plot,
# #        height = pdf.height, width = pdf.width)

# mod = RFE.m
# mod$learner.model$super.model
# varImpPlot(mod$learner.model$super.model$learner.model)
# 
# base.lrn = c("L2", "globMax")
# # TE.m$learner.model$base.models
# # plotEnsemblePrediction = function(mod, base.lrn) {
# # create fake data for all combinations
# 
# # TODO use CV or check out how it works internally
# base.preds = lapply(mod$learner.model$base.models, predict, task = task)
# base.preds = lapply(base.preds, function(x) x$data)
# colnames(base.preds[[1]]) = paste0("L2_", colnames(base.preds[[1]]))
# colnames(base.preds[[2]]) = paste0("dtw_", colnames(base.preds[[2]]))
# colnames(base.preds[[3]]) = paste0("globMax_", colnames(base.preds[[3]]))
# colnames(base.preds[[4]]) = paste0("random_", colnames(base.preds[[4]]))
# 
# dat = cbind(base.preds[[1]], base.preds[[2]], base.preds[[3]], base.preds[[4]])
# 
# 
# # add ensemble prediction
# ens.pred.df = predict(mod, task = task)$data
# dat = cbind(dat, ens.pred.df)
# 
# 
# # aggregate over all data with same prbabilites in base learners
# L2_dtw_dat = dat %>%
#   group_by(L2_prob.1, dtw_prob.1) %>%
#   summarize(prob.1 = mean(prob.1))
# ggplot(L2_dtw_dat, aes(x = L2_prob.1, y = dtw_prob.1, fill = prob.1)) +
#   geom_raster(interpolate = F) +
#   scale_fill_gradient2("P(Beetle)\n", midpoint = 0.5, limits = c(0, 1),
#                        high = "dodgerblue", low = "red")
# 
# L2_random_dat = dat %>%
#   group_by(L2_prob.1, random_prob.1) %>%
#   summarize(prob.1 = mean(prob.1))
# ggplot(L2_random_dat, aes(x = L2_prob.1, y = random_prob.1, fill = prob.1)) +
#   geom_raster(interpolate = F) +
#   scale_fill_gradient2("P(Beetle)\n", midpoint = 0.5, limits = c(0, 1),
#                        high = "dodgerblue", low = "red")
# 
# dtw_random_dat = dat %>%
#   group_by(dtw_prob.1, random_prob.1) %>%
#   summarize(prob.1 = mean(prob.1))
# ggplot(dtw_random_dat, aes(x = dtw_prob.1, y = random_prob.1, fill = prob.1)) +
#   geom_raster(interpolate = F) +
#   scale_fill_gradient2("P(Beetle)\n", midpoint = 0.5, limits = c(0, 1),
#                        high = "dodgerblue", low = "red")
# 
# dtw_globMax_dat = dat %>%
#   group_by(dtw_prob.1, globMax_prob.1) %>%
#   summarize(prob.1 = mean(prob.1))
# ggplot(dtw_globMax_dat, aes(x = dtw_prob.1, y = globMax_prob.1, fill = prob.1)) +
#   geom_raster(interpolate = F) +
#   scale_fill_gradient2("P(Beetle)\n", midpoint = 0.5, limits = c(0, 1),
#                        high = "dodgerblue", low = "red")
# 
# L2_globMax_dat = dat %>%
#   group_by(L2_prob.1, globMax_prob.1) %>%
#   summarize(prob.1 = mean(prob.1))
# ggplot(L2_globMax_dat, aes(x = L2_prob.1, y = globMax_prob.1, fill = prob.1)) +
#   geom_raster(interpolate = F) +
#   scale_fill_gradient2("P(Beetle)\n", midpoint = 0.5, limits = c(0, 1),
#                        high = "dodgerblue", low = "red")
# 
# 
# 
# # # }
# # # RFE
# # # plot learner prediction
# # rf.lrn = RFE.m$learner.model$super.model
# # 
# # rf.plot.raw = plotLearnerPrediction(rf.lrn, mod.task, features = c("glucose", "BMI"))
# # rf.plot = rf.plot.raw + 
# #   ggtitle("") +
# #   scale_fill_manual(values = c("firebrick3", "steelblue3")) +
# #   ylab("body mass index") + 
# #   xlab("plasma glucose") +
# #   mytheme
# # rf.plot
# # # ggsave("Grafiken/rf_plot.pdf", rf.plot,
# # #        height = pdf.height, width = pdf.width)
