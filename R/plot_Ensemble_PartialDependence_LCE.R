###
source("R/plot_Ensemble.R")
learner = LCE.lrn
task = task


# create partial dependence plots for RFE

####
# copy & paste from maierhofert/mlr classiFunc StackedLearner.R
# fixed major bug
# classif.bs.optimal = function(learner, task) {
bls = learner$base.learners
base.models = probs = vector("list", length(bls))

set.seed(1)
# adjusted code from stackCV
rin = makeResampleInstance(learner$resampling, task = task)
for (i in seq_along(bls)) {
  bl = bls[[i]]
  r = resample(bl, task, rin, show.info = FALSE)
  probs[[i]] = mlr:::getResponse(r$pred, full.matrix = TRUE)
  # also fit all base models again on the complete original data set
  base.models[[i]] = train(bl, task)
}
names(probs) = names(bls)
probsOG = probs
# probs = probsOG

# name of target column
tn = getTaskTargetNames(task)
# order of resampled observations
test.inds = unlist(rin$test.inds)

# convert from list to data frame
probs = as.data.frame(probs)

# reorder data frame to original order
probs = probs[order(test.inds), , drop = FALSE]

# rename columns to be the same for across all classes
nclasses = length(getTaskClassLevels(task))
colnames(probs) = rep(names(bls), each = nclasses)

# reorder data frame to be like P in Fuchs etal. (2015)
probsList = list()
for (i in 1:nclasses) {
  probsList[[i]] = probs[(seq_along(colnames(probs)) - 1) %% nclasses == (i - 1)]
}
# convert list into a data frame
P <- do.call("rbind", probsList)

# convert to matrix
P = as.matrix(P)

# code the response
z = as.vector(model.matrix( ~ . -1, data = data.frame(getTaskTargets(task))))

# From Fuchs Online Appendix
A = P * (-1)
B = z * (-1)

H = rep(0, length = ncol(A))
G = diag(1, nrow = ncol(A))

E = rep(1, ncol(A))
F = 1

####################################
# taken from the limSolve::lsei function
# the lsei function seemed to not be stable
# the solve.QP seems to be more stable
dvec = crossprod(A, B)
Dmat = crossprod(A, A)
diag(Dmat) = diag(Dmat) + 1e-08
Amat = t(rbind(E, G))
bvec = c(F, H)

# E and F are onedimensional
Neq = 1

sol = quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq = Neq)
Cs = round(sol$solution, digits = 4)
names(Cs) = names(bls)
################################

# return the weight vector Cs from the algorithm
list(method = "classif.bs.optimal", base.models = base.models, super.model = NULL,
     pred.train = probs, weights = Cs)
# }



################################################################################
# partial dependence


P = P[1:40,]

# create partial dependence data

# cols = c("L2", "dtw")
# weights = Cs
getPDdataLCE = function(P, weights, cols) {
  # create data frame with unique combinations of al cols
  uniqueList = list()
  for (i in cols) {
    uniqueList[[i]] = unique(P[,i])
  }
  ICE_data = expand.grid(uniqueList)
  # add other (constant) columns
  other_cols = colnames(P)[!colnames(P) %in% cols]
  ICE_data[,other_cols] = colMeans(P[, other_cols]) %>%
    rep(each = nrow(ICE_data))
  ICE_data$ensemble = apply(ICE_data, 1, function(x) {
    sum(x[names(weights)] * weights)
  })
  return(ICE_data)
}
# TODO fix bug in other_cols probability

# partial depencee plot
PDplot = function(this.pd_dat, this.cols) {
  ggplot(this.pd_dat)  +
    geom_tile(aes(x = eval(parse(text = this.cols[1])), 
                  y = eval(parse(text = this.cols[2])),
                  fill = ensemble)) +
    scale_fill_gradient2("P(Beetle)\n", midpoint = 0.5, limits = c(0, 1),
                         high = "dodgerblue", low = "red") +
    scale_x_continuous(breaks = c(0, 0.33, 0.66, 1)) +
    scale_y_continuous(breaks = c(0, 0.33, 0.66, 1)) +
    xlab(this.cols[1]) +
    ylab(this.cols[2]) +
    facet_null()
}

# ##############################################################################
# plot partial dependence plots
name = "LCE"
# L2 dtw
this.cols = c("L2", "dtw")
this.pd_dat = getPDdataLCE(P = P, weights = Cs, cols = this.cols)
PDplot(this.pd_dat, this.cols)
ggsave(paste0("Plots/EnsemblePartialDepence/PD_", name, "_ens_", this.cols[1], "_", this.cols[2], ".pdf"), 
       width = 5, height = 4)
 
# L2 random
this.cols = c("L2", "random")
this.pd_dat = getPDdataLCE(P = P, weights = Cs, cols = this.cols)
PDplot(this.pd_dat, this.cols)
ggsave(paste0("Plots/EnsemblePartialDepence/PD_", name, "_ens_", this.cols[1], "_", this.cols[2], ".pdf"), 
       width = 5, height = 4)

# L2 globMax
this.cols = c("L2", "globMax")
this.pd_dat = getPDdataLCE(P = P, weights = Cs, cols = this.cols)
PDplot(this.pd_dat, this.cols)
ggsave(paste0("Plots/EnsemblePartialDepence/PD_", name, "_ens_", this.cols[1], "_", this.cols[2], ".pdf"), 
       width = 5, height = 4)

# dtw random
this.cols = c("dtw", "random")
this.pd_dat = getPDdataLCE(P = P, weights = Cs, cols = this.cols)
PDplot(this.pd_dat, this.cols)
ggsave(paste0("Plots/EnsemblePartialDepence/PD_", name, "_ens_", this.cols[1], "_", this.cols[2], ".pdf"), 
       width = 5, height = 4)

# dtw globMax
this.cols = c("dtw", "globMax")
this.pd_dat = getPDdataLCE(P = P, weights = Cs, cols = this.cols)
PDplot(this.pd_dat, this.cols)
ggsave(paste0("Plots/EnsemblePartialDepence/PD_", name, "_ens_", this.cols[1], "_", this.cols[2], ".pdf"), 
       width = 5, height = 4)

# random globMax
this.cols = c("globMax", "random")
this.pd_dat = getPDdataLCE(P = P, weights = Cs, cols = this.cols)
PDplot(this.pd_dat, this.cols)
ggsave(paste0("Plots/EnsemblePartialDepence/PD_", name, "_ens_", this.cols[1], "_", this.cols[2], ".pdf"), 
       width = 5, height = 4)

