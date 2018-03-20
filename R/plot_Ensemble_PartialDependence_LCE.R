###
learner = LCE.lrn
task = task


# create partial dependence plots for RFE

####
# copy & paste from maierhofert/mlr classiFunc StackedLearner.R
# fixed major bug
# classif.bs.optimal = function(learner, task) {
bls = learner$base.learners
base.models = probs = vector("list", length(bls))

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
P = bind_rows(probsList)

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
################################

# return the weight vector Cs from the algorithm
list(method = "classif.bs.optimal", base.models = base.models, super.model = NULL,
     pred.train = probs, weights = Cs)
# }



################################################################################
# partial dependence
name = "RFE"

# create partial dependence data
# L2 dtw
pd_dat_L2_dtw = generatePartialDependenceData(obj = super.model, input = super.task, 
                                              features = c("L2", "dtw"), interaction = TRUE)
plotPartialDependence(pd_dat_L2_dtw, geom = "tile") +
  scale_fill_gradient2("P(Beetle)\n", midpoint = 0.5, limits = c(0, 1),
                       high = "dodgerblue", low = "red") +
  facet_null()
ggsave(paste0("Plots/PD_", name, "_ens_L2_dtw.pdf"), width = 6, height = 5)

# L2 random
pd_dat_L2_random = generatePartialDependenceData(obj = super.model, input = super.task, 
                                                 features = c("L2", "random"), interaction = TRUE)
plotPartialDependence(pd_dat_L2_random, geom = "tile") +
  scale_fill_gradient2("P(Beetle)\n", midpoint = 0.5, limits = c(0, 1),
                       high = "dodgerblue", low = "red") +
  facet_null()
ggsave(paste0("Plots/PD_", name, "_ens_L2_random.pdf"), width = 6, height = 5)

# L2 globMax
pd_dat_L2_globMax = generatePartialDependenceData(obj = super.model, input = super.task, 
                                                  features = c("L2", "globMax"), interaction = TRUE)
plotPartialDependence(pd_dat_L2_globMax, geom = "tile") +
  scale_fill_gradient2("P(Beetle)\n", midpoint = 0.5, limits = c(0, 1),
                       high = "dodgerblue", low = "red") +
  facet_null()
ggsave(paste0("Plots/PD_", name, "_ens_L2_globMax.pdf"), width = 6, height = 5)

# dtw random
pd_dat_random_dtw = generatePartialDependenceData(obj = super.model, input = super.task, 
                                                  features = c("random", "dtw"), interaction = TRUE)
plotPartialDependence(pd_dat_random_dtw, geom = "tile") +
  scale_fill_gradient2("P(Beetle)\n", midpoint = 0.5, limits = c(0, 1),
                       high = "dodgerblue", low = "red") +
  facet_null()
ggsave(paste0("Plots/PD_", name, "_ens_random_dtw.pdf"), width = 6, height = 5)

# dtw globMax
pd_dat_dtw_globMax = generatePartialDependenceData(obj = super.model, input = super.task, 
                                                   features = c("dtw", "globMax"), interaction = TRUE)
plotPartialDependence(pd_dat_dtw_globMax, geom = "tile") +
  scale_fill_gradient2("P(Beetle)\n", midpoint = 0.5, limits = c(0, 1),
                       high = "dodgerblue", low = "red") +
  facet_null()
ggsave(paste0("Plots/PD_", name, "_ens_dtw_globMax.pdf"), width = 6, height = 5)

# random globMax
pd_dat_random_globMax = generatePartialDependenceData(obj = super.model, input = super.task, 
                                                      features = c("random", "globMax"), interaction = TRUE)
plotPartialDependence(pd_dat_random_globMax, geom = "tile") +
  scale_fill_gradient2("P(Beetle)\n", midpoint = 0.5, limits = c(0, 1),
                       high = "dodgerblue", low = "red") +
  facet_null()
ggsave(paste0("Plots/PD_", name, "_ens_random_globMax.pdf"), width = 6, height = 5)

