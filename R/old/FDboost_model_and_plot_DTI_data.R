# plot DTI data

library("ggplot2")
library("dplyr")

# load example data
data("DTI", package = "refund")


# # subsample DTI to exclude revisits
# this is not really necessary but doesn't hurt
DTI = DTI[!duplicated(DTI$ID),]
DTI$case = factor(DTI$case)

# plot data
matplot(t(DTI$rcst), type = "l", col = DTI$case)
matplot(t(DTI$cca), type = "l", col = DTI$case, ylim = c(0.2, 0.8))


# ##############################################################################
# create an FDboost model for the DTI data

library(FDboost)
# use same train/test split as in  Section 3
set.seed(123456)
IDs = unique(DTI$ID)
# vector encoding if observation is part of test or training data
train.rows = DTI$ID %in% sample(IDs, size = 0.8 * length(IDs))
# train.rows = DTI$ID %in% IDs[c(1:33, 43:75)]

# how uneven are case/control sampled in training and test data
table(DTI[train.rows, "case"]) / table(DTI[!train.rows, "case"])

# there are missing values in rcst[,1:12]
sum(is.na(DTI$rcst))
summary(is.na(DTI$rcst))
# take out colums with missing values
DTI$rcst = DTI$rcst[,-(1:12)]

# cca contains missing values in  cca_67 and cca_68
sum(is.na(DTI$cca))
summary(is.na(DTI$cca))
# fill in the 2 missing values
library("zoo")
DTI$cca = t(apply(DTI$cca, 1, function(x) {
  zoo::na.approx(x)
}))

# get data in right format
DTI_test = DTI[!train.rows,]
DTI_train = DTI[train.rows,]

DTI_list = lapply(DTI, function(x) x)
DTI_test_list = lapply(DTI_test, function(x) x)
DTI_train_list = lapply(DTI_train, function(x) x)

DTI_list$s1 = 1:ncol(DTI$rcst)
DTI_test_list$s1 = 1:ncol(DTI$rcst)
DTI_train_list$s1 = 1:ncol(DTI$rcst)

DTI_list$s2 = 1:ncol(DTI$cca)
DTI_test_list$s2 = 1:ncol(DTI$cca)
DTI_train_list$s2 = 1:ncol(DTI$cca)

# fit model
sof = FDboost(case ~ bsignal(rcst, s = s1) +
              bsignal(cca, s = s2),
              data = DTI_train_list, 
              timeformula = NULL, 
              family = Binomial())
plot(sof)



# confusion matrix for sof estimator
pred.sof = predict(sof, newdata = DTI_test_list, type = "class")
table(pred = pred.sof,
      true = DTI_test_list$case)

