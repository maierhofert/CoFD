# this file crates a nearest neighbor ensemble and a random forest ensemble
# for the growth study data and plots the weights of the base models
library("ggplot2")
# generate plots
mytheme = theme_bw(25)

# load the ensembles into the workspace
source("R/plot_Ensemble_PartialDependence_LCE.R")

# #############
# generate data for the nn_ensemble plot
# weight = LCE.m$learner.model$weights
# base.learners = BBmisc::extractSubList(LCE.m$learner.model$base.models, "learner",
#                                        simplify = FALSE)
# base.learners.id = sapply(base.learners, getLearnerId)
# base.learner.labels = base.learners.id
# plot.data = data.frame(id = base.learners.id, weight = weight)


plot.data = data.frame(id = names(bls), weight = Cs)

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
ggsave(paste0("Plots/weightplot_lce.pdf"), weight.plot, 
       width = 12, height = 10)


source("R/plot_Ensemble_PartialDependence_RFE.R")

# generate data for the rf_ensemble plot
library("randomForest")
# randomForest_mod = RFE.m$learner.model$super.model$learner.model
# feat_imp = importance(randomForest_mod)
# plot.data = data.frame(id = rownames(feat_imp), var_imp = as.vector(feat_imp))
# varImpPlot(randomForest_mod)

feat_imp = importance(RFE.m$learner.model$super.model$learner.model)
plot.data = data.frame(id = rownames(feat_imp), var_imp = as.vector(feat_imp))
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
ggsave(paste0("Plots/weightplot_rfe.pdf"), weight.plot, 
       width = 12, height = 10)
