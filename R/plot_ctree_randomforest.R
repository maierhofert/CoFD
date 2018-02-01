# this file creates figures for classification trees and random forest
library("mlr")
library("ggplot2")
library("party")
mytheme = theme_classic(20)
pdf.height = 5
pdf.width = 7

# task
task = pid.task
tskdata = getTaskData(task)
subset = tskdata$glucose > 20 & tskdata$mass > 10 & c(TRUE, FALSE)
mod.task = subsetTask(task, subset, c("glucose", "mass"))
names(mod.task$env$data)[2] = "BMI"

# classification tree
tree.lrn = makeLearner("classif.ctree", predict.type = "prob")
tree = train(learner = "classif.ctree", task = mod.task)
pdf("Grafiken/tree_strucplot.pdf", 
    width = pdf.width, height = pdf.height)
plot(tree$learner.model,
     inner_panel = node_inner(tree$learner.model,
                            abbreviate = FALSE,            # short variable names
                            pval = FALSE,                 # no p-values
                            id = FALSE),
     terminal_panel = node_terminal(tree$learner.model, 
                                  abbreviate = TRUE,
                                  digits = 1,                   # few digits on numbers
                                  fill = c("white"),            # make box white not grey
                                  id = FALSE))
dev.off()

# random forest
rf.lrn = makeLearner("classif.randomForest", predict.type = "prob")
rf = train(learner = rf.lrn, task = mod.task)
# randomForest::varImpPlot(rf$learner.model)


# plot learner prediction
tree.plot.raw = plotLearnerPrediction(tree.lrn, mod.task, features = c("glucose", "BMI"))
tree.plot = tree.plot.raw + 
  ggtitle("") +
  scale_fill_manual(values = c("firebrick3", "steelblue3")) +
  ylab("body mass index") + 
  xlab("plasma glucose") +
  mytheme
tree.plot
ggsave("Grafiken/tree_plot.pdf", tree.plot,
       height = pdf.height, width = pdf.width)

rf.plot.raw = plotLearnerPrediction(rf.lrn, mod.task, features = c("glucose", "BMI"))
rf.plot = rf.plot.raw + 
  ggtitle("") +
  scale_fill_manual(values = c("firebrick3", "steelblue3")) +
  ylab("body mass index") + 
  xlab("plasma glucose") +
  mytheme
rf.plot
ggsave("Grafiken/rf_plot.pdf", rf.plot,
       height = pdf.height, width = pdf.width)
