source("R/plot_Ensemble.R")
learner = TE.lrn
task = task


# create partial dependence plots for TE

####
# copy & paste from mlr
# stackCV = function(learner, task) {
td = getTaskDesc(task)
type = ifelse(td$type == "regr", "regr",
              ifelse(length(td$class.levels) == 2L, "classif", "multiclassif"))
bls = learner$base.learners
use.feat = learner$use.feat
# cross-validate all base learners and get a prob vector for the whole dataset for each learner
base.models = probs = vector("list", length(bls))

set.seed(1)
rin = makeResampleInstance(learner$resampling, task = task)
for (i in seq_along(bls)) {
  bl = bls[[i]]
  r = resample(bl, task, rin, show.info = FALSE)
  probs[[i]] = mlr:::getResponse(r$pred, full.matrix = FALSE)
  # also fit all base models again on the complete original data set
  base.models[[i]] = train(bl, task)
}
names(probs) = names(bls)

if (type == "regr" || type == "classif") {
  probs = as.data.frame(probs)
} else {
  probs = as.data.frame(lapply(probs, function(X) X)) #X[, -ncol(X)]))
}

# add true target column IN CORRECT ORDER
tn = getTaskTargetNames(task)
test.inds = unlist(rin$test.inds)

pred.train = as.list(probs[order(test.inds), , drop = FALSE])

probs[,tn] = getTaskTargets(task)[test.inds]

# now fit the super learner for predicted_probs --> target
probs = probs[order(test.inds), , drop = FALSE]
# if (use.feat) {
#   # add data with normal features IN CORRECT ORDER
#   feat = getTaskData(task)#[test.inds, ]
#   feat = feat[, !colnames(feat) %in% tn, drop = FALSE]
#   pred.data = cbind(probs, feat)
#   super.task = makeSuperLearnerTask(learner, data = pred.data, target = tn)
# } else {
super.task = mlr:::makeSuperLearnerTask(learner, data = probs, target = tn)
# }
super.model = train(learner$super.learner, super.task)
# list(method = "stack.cv", base.models = base.models,
#      super.model = super.model, pred.train = pred.train)
# }


################################################################################
# partial dependence
name = "TE"

# dtw globMax
pd_dat_dtw_globMax = generatePartialDependenceData(obj = super.model, input = super.task, 
                                                   features = c("dtw", "globMax"), interaction = TRUE)
plotPartialDependence(pd_dat_dtw_globMax, geom = "tile") +
  scale_fill_gradient2("P(Beetle)\n", midpoint = 0.5, limits = c(0, 1),
                       high = "dodgerblue", low = "red") +
  scale_x_continuous(breaks = c(0, 0.33, 0.66, 1)) +
  scale_y_continuous(breaks = c(0, 0.33, 0.66, 1)) +
  facet_null()
ggsave(paste0("Plots/EnsemblePartialDepence/PD_", name, "_ens_dtw_globMax.pdf"), 
       width = 5, height = 4)

# ##############################################################################
# classification tree
# tree.lrn = makeLearner("classif.ctree", predict.type = "prob")
# tree = train(learner = "classif.ctree", task = mod.task)
library("party")
pdf("Plots/EnsemblePartialDepence/tree_strucplot.pdf", 
    width = 5, height = 4)
plot(super.model$learner.model,
     inner_panel = node_inner(super.model$learner.model,
                              abbreviate = FALSE,            # short variable names
                              pval = FALSE,                 # no p-values
                              id = FALSE),
     terminal_panel = node_terminal(super.model$learner.model, 
                                    abbreviate = TRUE,
                                    digits = 1,                   # few digits on numbers
                                    fill = c("white"),            # make box white not grey
                                    id = FALSE))
dev.off()

