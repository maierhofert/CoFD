# this file crates a nearest neighbor ensemble and a random forest ensemble
# for the growth study data and plots the weights of the base models
library("classiFunc")
library("ggplot2")
library("mlr")
library("foreign")
library("dtw")
# generate plots
mytheme = theme_bw(25)

# # read in learners
# source("R/create_base_learners.R")

knn = c(1, 5, 9)
nderiv = c(0, 1)
semimet = c("L2", "supremum", "DTW")
for (this.knn in 1:length(knn)) {
  for (this.nderiv in 1:length(nderiv)) {
    for (this.semimet in 1:length(semimet)) {
      assign(paste0("knn", knn[this.knn], 
                    "nderiv", nderiv[this.nderiv], 
                    "_", semimet[this.semimet]),
             makeLearner(cl = "classif.classiFunc.knn",
                         id = paste0("knn", knn[this.knn], 
                                     "nderiv", nderiv[this.nderiv], 
                                     "_", semimet[this.semimet]),
                         metric = semimet[this.semimet],
                         predict.type = "prob",
                         par.vals = list(knn = knn[this.knn], 
                                         nderiv = nderiv[this.nderiv],
                                         metric = semimet[this.semimet])))
    }
  }
}

base.learners = list(knn1nderiv0_L2, knn5nderiv0_L2,
                     knn1nderiv0_supremum, knn5nderiv0_supremum,
                     knn1nderiv0_DTW, knn5nderiv0_DTW,
                     
                     knn1nderiv1_L2, knn5nderiv1_L2,
                     knn1nderiv1_supremum, knn5nderiv1_supremum,
                     knn1nderiv1_DTW, knn5nderiv1_DTW)
base.learner.labels = c("L2: k = 1, a = 0", "L2: k = 5, a = 0",
                        "supremum: k = 1, a = 0", "supremum: k = 5, a = 0",
                        "dtw: k = 1, a = 0", "dtw: k = 5, a = 0",

                        "L2: k = 1, a = 1", "L2: k = 5, a = 1",
                        "supremum: k = 1, a = 1", "supremum: k = 5, a = 1",
                        "dtw: k = 1, a = 1", "dtw: k = 5, a = 1")

# Ensemble learners
# with knne Fuchs etal 2016
lce = makeStackedLearner(id = "LCE",
                         base.learners = base.learners,
                         predict.type = "prob",
                         resampling = makeResampleDesc("CV", iters = 10L),
                         method = "classif.bs.optimal")
lce$short.name = "LCE"

rfe = makeStackedLearner(id = "RFE_nofeat",
                         base.learners = base.learners, 
                         super.learner = "classif.randomForest",
                         predict.type = "prob",
                         use.feat = FALSE,
                         method = "stack.cv")
rfe$short.name = "RFE"

# source("R/growth_study_data.R")
# tsk = makeFDAClassifTask(data = growth_wide[,2:ncol(growth_wide)],
#                          id = "growth_study",
#                          fd.features = list(ff = 2:(ncol(growth_wide) - 1)),
#                          target = "sex")

data("Growth", package = "classiFunc")
fdata = makeFunctionalData(Growth, exclude.cols = c("ID", "sex"))
tsk = makeClassifTask(data = fdata[,-1], target = "sex")


# train the models
set.seed(123)
lce_mod = train(learner = lce, task = tsk)
rfe_mod = train(learner = rfe, task = tsk)

# save models
saveRDS(lce_mod, paste0("lce_mod.RDS"))
saveRDS(rfe_mod, paste0("rfe_mod.RDS"))
# read in the models
lce_mod = readRDS("lce_mod.RDS")
rfe_mod = readRDS("rfe_mod.RDS")

# #############
# generate data for the nn_ensemble plot
weight = lce_mod$learner.model$weights
base.learners = BBmisc::extractSubList(lce_mod$learner.model$base.models, "learner",
                                       simplify = FALSE)
base.learners.id = sapply(base.learners, getLearnerId)
plot.data = data.frame(id = base.learners.id, weight = weight)


# barplot with the nnensemble weights
weight.plot <- ggplot(data = plot.data, aes(x = id, y = weight)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(breaks = base.learners.id,
                   labels = base.learner.labels) +
  xlab("base model") +
  mytheme +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 0.95,
                                   vjust = 0.5))
weight.plot
ggsave(paste0("Grafiken/weightplot_lce.pdf"), weight.plot, 
       width = 12, height = 10)


# generate data for the rf_ensemble plot
library("randomForest")
randomForest_mod = rfe_mod$learner.model$super.model$learner.model
feat_imp = importance(randomForest_mod)
plot.data = data.frame(id = rownames(feat_imp), var_imp = as.vector(feat_imp))
varImpPlot(randomForest_mod)
# barplot with the nnensemble weights
weight.plot <- ggplot(data = plot.data, aes(x = id, y = var_imp)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(breaks = base.learners.id,
                   labels = base.learner.labels) +
  xlab("base model") +
  ylab("variable importance") +
  mytheme +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 0.95,
                                   vjust = 0.5))
weight.plot
ggsave(paste0("Grafiken/weightplot_rfe.pdf"), weight.plot, 
       width = 12, height = 10)
