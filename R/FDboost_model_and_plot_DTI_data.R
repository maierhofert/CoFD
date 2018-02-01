

apply(is.na(DTI_original), 2, sum)


# plot DTI data

library("ggplot2")
library("dplyr")

# load example data
data("DTI", package = "classiFunc")


# subsample DTI for equal case control size
DTI = DTI[!duplicated(DTI$ID),]
DTI = DTI[1:84,]


matplot(t(DTI$rcst)[,1:80], type = "l", col = DTI$case[1:80])

matplot(t(DTI$rcst), type = "l", col = DTI$case, ylim = c(0.2, 0.8))



# ##############################################################################
# create an FDboost model for the DTI data

library(FDboost)
# use same train/test split as in  Section 3
set.seed(12345)
IDs = unique(DTI$ID)
# vector encoding if observation is part of test or training data
train.rows = DTI$ID %in% sample(IDs, size = 0.8 * length(IDs))
# train.rows = DTI$ID %in% IDs[c(1:33, 43:75)]

table(DTI[train.rows, "case"]) / table(DTI[!train.rows, "case"])


DTI_test = DTI[!train.rows,]
DTI_train = DTI[train.rows,]

DTI_list = lapply(DTI, function(x) x)
DTI_test_list = lapply(DTI_test, function(x) x)
DTI_train_list = lapply(DTI_train, function(x) x)

DTI_list$s = 1:ncol(DTI$rcst)
DTI_test_list$s = 1:ncol(DTI$rcst)
DTI_train_list$s = 1:ncol(DTI$rcst)



sof = FDboost(case ~ # bsignal(cca, s = s) +
              bsignal(rcst, s = s),
              data = DTI_train_list, 
              timeformula = NULL, 
              family = Binomial())

plot(sof)

pred.sof = predict(sof, newdata = DTI_test_list, type = "class")

# confusion matrix for sof estimator
# this is way better than my classification models do
# no matter how the subsampling is done
table(pred = pred.sof,
      true = DTI_test_list$case)

