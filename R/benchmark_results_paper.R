# this file looks into the results of the benchmark analysis
# for the UCR TSC data
library("mlr")
library("ggplot2")
library("dplyr")
library("stringi")
library("forcats")

mytheme = theme_bw(20)

# # tasks and resulting benchmark experiment
# tsks = tsks[df$nobs...obslen <= 100000 & !small_tasks]


##########################################################################################
# Set up data and colors -----------------------------------------------------------------

# read in most current benchmark
bmr = readRDS("Benchmark_results/2019-11-25bmr_paper.RDS")
name = "bmr_paper"

# check for msising values
perf = getBMRPerformances(bmr, as.df = TRUE)
perf %>% summary()
unique(perf[is.na(perf$multiclass.brier),"task.id"])

task.ids = getBMRTaskIds(bmr)

# pretty colors for learners
lrns.colors = c("grey20", "grey60",
                "darkorange3",
                "navy",
                # "orange1", "goldenrod",
                #
                # "navy", "royalblue2",
                "red1", "red4",
                # "coral", "coral3",
                # "deeppink4", "deeppink1",
                #
                "chartreuse1", "chartreuse4"
)

lrns.ids = getBMRLearnerIds(bmr)
order.lrns = 1:length(getBMRLearnerIds(bmr))

labels = c("1nn.eucl", "1nn.dtw", "knn.tuned.eucl", "kernel.tuned.eucl",
           "LCE", "LCE.noisy", "RFE", "RFE.noisy")

# helper function to remove a layer
remove_geom <- function(ggplot2_object, geom_type) {
  # Delete layers that match the requested type.
  layers <- lapply(ggplot2_object$layers, function(x) {
    if (class(x$geom)[1] == geom_type) {
      NULL
    } else {
      x
    }
  })
  # Delete the unwanted layers.
  layers <- layers[!sapply(layers, is.null)]
  ggplot2_object$layers <- layers
  ggplot2_object
}

##########################################################################################
# Table 2: Benchmark Results -------------------------------------------------------------
df2 = getBMRAggrPerformances(bmr, as.df = TRUE)
df2$learner.short = stri_replace_first_regex(df2$learner.id, "classif.classiFunc.", "")
df2$learner.short = stri_replace_first_regex(df2$learner.short, ":\\{.*\\}", "")
df2$learner.short = stri_replace_first_regex(df2$learner.short, ":knn", "")
df2$learner.short = stri_replace_first_regex(df2$learner.short, "_nderiv:0", "")
df2$learner.short = stri_replace(df2$learner.short, "knn.tuned", "knn.tuned.eucl")
df2$learner.short = stri_replace(df2$learner.short, "kernel.tuned", "kernel.tuned.eucl")

df2 %>%
  group_by(task.id) %>%
  mutate(
    rnk_brier = dense_rank(multiclass.brier.test.mean),
    rnk_acc   = dense_rank(desc(acc.test.mean))
  ) %>%
  group_by(learner.short) %>%
  summarize(
    mean_brier = mean(multiclass.brier.test.mean),
    median_brier = median(multiclass.brier.test.mean),
    mean_rnk_brier = mean(rnk_brier),
    mean_accuracy = mean(acc.test.mean),
    median_accuracy = median(acc.test.mean),
    mean_rnk_acc = mean(rnk_acc)
  ) %>%
  write.csv("table_brier_acc.csv")


df3 = df2 %>%
  group_by(task.id) %>%
  mutate(
    rnk_brier = dense_rank(multiclass.brier.test.mean),
    rnk_acc   = dense_rank(desc(acc.test.mean))
  ) %>%
  group_by(learner.short) %>%
  summarize(
    mean_brier = mean(multiclass.brier.test.mean),
    median_brier = median(multiclass.brier.test.mean),
    mean_rnk_brier = mean(rnk_brier),
    mean_accuracy = mean(acc.test.mean),
    median_accuracy = median(acc.test.mean),
    mean_rnk_acc = mean(rnk_acc)
  ) %>%
  rename(learner = learner.short)

write.csv(df3, "table_brier_acc.csv")


library(knitr)
library(kableExtra)


make_bold = function(x, min = TRUE, digits = 3) {
  if (is.numeric(x)) {
    x = round(x, digits)
    if (min) {
      x = cell_spec(x, "latex", bold = ifelse(x == round(min(x), digits), TRUE, FALSE))
    } else {
      x = cell_spec(x, "latex", bold = ifelse(x == round(max(x), digits), TRUE, FALSE))
    }
  }
  return(x)
}

df3 %>%
  mutate(
    mean_brier = make_bold(mean_brier),
    median_brier = make_bold(median_brier),
    mean_rnk_brier = make_bold(mean_rnk_brier),
    mean_accuracy = make_bold(mean_accuracy, FALSE),
    median_accuracy = make_bold(median_accuracy, FALSE),
    mean_rnk_acc = make_bold(mean_rnk_acc)
  ) %>%
  knitr::kable(format = "latex", digits = 3, escape = F)


##########################################################################################
# Figure 8: Bar Plot ---------------------------------------------------------------------
p.bars = plotBMRRanksAsBarChart(bmr, pretty.names = F) +
  scale_fill_manual(values = lrns.colors,
                    labels = labels,
                    name = "Model") +
  scale_x_discrete(breaks = 1:17,
                   labels = c(1, "", 3, "", 5, "", 7, "", 9, "",
                              11, "", 13, "", 15, "", 17)) +
  ylab("Count") +
  xlab("Rank") +
  mytheme
p.bars
ggsave(paste0("Plots/benchmark/", name, "_bars.pdf"), p.bars,
       width = 13, height = 7)


##########################################################################################
# Figure 7: Critical Differences ---------------------------------------------------------
# Friedman Test
friedmanTestBMR(bmr, measure = multiclass.brier)

# Nemenyi Post-Hoc Test
friedmanPostHocTestBMR(bmr, measure = multiclass.brier)

# critical difference diagram
g = generateCritDifferencesData(bmr, measure = multiclass.brier,
                                p.value = 0.05, test = "nemenyi")

# Careful, this requires manual work
p.cd = plotCritDifferences(g, pretty.names = T) %>%
  remove_geom("GeomText") +
  geom_text(aes_string("xend", "yend", color = "learner.id", hjust = "right"),
            label = labels, vjust = -1) +
  # manually insert position of cd label
  annotate("text", label = stringi::stri_paste("Critical Difference =",
                                      round(g$cd.info$cd, 2), sep = " "),
           x = 4.5, y = 4 + 0.05) +
  scale_color_manual(values = lrns.colors,
                     labels = labels,
                     name = "learner") +
  theme(text = element_text(size = 10),
        plot.margin = unit(c(2, 1, 0.5, 0.5), "lines"))
p.cd

ggsave(paste0("Plots/benchmark/", name, "_cd.pdf"), p.cd,
       width = 0.8*13, height = 0.8*9)


##########################################################################################
# Figure 9: Boxplots ---------------------------------------------------------------------

pfull =
  ggplot(df2, aes(x = learner.short, y = multiclass.brier.test.mean, fill = learner.short)) +
  geom_boxplot() +
  theme_bw() +
  ylab("Multiclass Brier Score") +
  xlab("Learner") +
  labs(fill = "Learner") +
  theme(legend.position = "none", axis.text.x = element_blank())
ggsave("Plots/benchmark/boxplot_brier.pdf", plot = pfull, height = 4, width = 4, scale = 1)

pfull2.1 =
  ggplot(df2, aes(x = learner.short, y = acc.test.mean, fill = learner.short)) +
  geom_boxplot() +
  theme_bw() +
  ylab("Accuracy") +
  xlab("Learner") +
  labs(fill = "Learner")
pfull2 = pfull2.1 + theme(legend.position = "none", axis.text.x = element_blank())
ggsave("Plots/benchmark/boxplot_acc.pdf", plot = pfull2, height = 4, width = 4, scale = 1)

library(ggpubr)
(pfull2.1 + theme(legend.position = "bottom")) %>%
  get_legend() %>%
  as_ggplot() %>%
  ggsave("Plots/benchmark/boxplot_legend.pdf", plot = ., height = 1, width = 4, scale = 2)

# --- End plots from paper ---







##########################################################################################
# Other Plots that have not made it in the paper------------------------------------------

# visualize benchmark results
plotBMRBoxplots(bmr, measure = timeboth, pretty.names = FALSE,
                facet.wrap.ncol = 2)
p.box = plotBMRBoxplots(bmr, measure = multiclass.brier, pretty.names = F,
                        facet.wrap.ncol = 2L) +
  geom_boxplot(aes(fill = learner.id)) +
  scale_fill_manual(values = lrns.colors,
                    labels = labels,
                    name = "") +
  ylab("Brier score") +
  mytheme +
  guides(fill = F) +
  theme(text = element_text(size = 15),
        plot.margin = unit(c(1, 5, 0.5, 0.5), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = -60, hjust = 0),
        legend.position = "bottom")
p.box
ggsave(paste0("Plots/benchmark/", name, "_boxplot.pdf"), p.box,
       width = 13, height = 55, limitsize = FALSE)



# # data frame containing results
# getBMRAggrPerformances(bmr, as.df = TRUE)
p.dots = plotBMRSummary(bmr, trafo = "rank", pretty.names = F,
                        jitter = 0.05, pointsize = 10L) +
  # guides(col = guide_legend(ncol = 2, override.aes = aes(size = 4))) +
  scale_x_continuous(breaks = 1:17, minor_breaks = 1:15) +
  scale_color_manual(values = lrns.colors,
                     labels = labels,
                     name = "")  +
  xlab("Rank of Brier score") +
  mytheme +
  theme(legend.position = "bottom",
        plot.margin = unit(c(1, 5, 0.5, 0.5), "lines"))
p.dots

ggsave(paste0("Plots/benchmark/", name, "_dots.pdf"), p.dots,
       width = 13, height = 20)


# # fix short names and learner ids
# str(bmr$learners$LCE_noisy, 1)
#
# # Distinguish between dtw and euclidean
# bmr$learners$KNN_eucl$name = "classiFunc.knn.eucl"
# bmr$learners$KNN_eucl$short.name = "classiFunc.knn.eucl"
#
# bmr$learners$KNN_dtw$name = "classiFunc.knn.dtw"
# bmr$learners$KNN_dtw$short.name = "classiFunc.knn.dtw"
#
# # distinguish between noisy and regular learner
# bmr$learners$LCE_noisy$id = "noisy LCE:knn:{1,5,9,13}_metric:{Euclidean,globMax,globMin,Manhattan,rucrdtw}_nderiv:{0,1,2}"
# bmr$learners$RFE_noisy$id = "noisy RFE:knn:{1,5,9,13}_metric:{Euclidean,globMax,globMin,Manhattan,rucrdtw}_nderiv:{0,1,2}"


# lrns.ids = c("knn1nderiv0_eucl", "fdaclassif.classiKernel.tuned",
#              "knn1nderiv0_dtw",
#              "knn1nderiv0_amplitude", "knn1nderiv0_phase",
#              #
#              "knn_eucl_ensemble", "knnOptNderiv0_eucl.tuned",
#              "nderiv_eucl_ensemble", "knn1NderivOpt_eucl.tuned",
#              "semimet_ensemble", "knn1Nderiv0_semimetOpt.tuned",
#              "nderivKnnSemimet_ensemble", "knnOptNderivOptSemimetOpt.tuned",
#              #
#              "rf_nofeat_semimet_ensemble", "rf_feat_semimet_ensemble",
#              #
#              "noisy_eucl_ensemble", "rf_noisy_ensemble")
#
# order.lrns = c(1:3, 16:17, c(4, 11, 5, 10, 6, 12, 7, 13),
#                9:8, 15:14)
# # helper function to remove a layer
# remove_geom <- function(ggplot2_object, geom_type) {
#   # Delete layers that match the requested type.
#   layers <- lapply(ggplot2_object$layers, function(x) {
#     if (class(x$geom)[1] == geom_type) {
#       NULL
#     } else {
#       x
#     }
#   })
#   # Delete the unwanted layers.
#   layers <- layers[!sapply(layers, is.null)]
#   ggplot2_object$layers <- layers
#   ggplot2_object
# }

# # new helper function to extract bmr objects containing only a subset of the learners
# subsetBMR = function(bmr, learner.ids) {
#   bmr_new = bmr
#   bmr_new$results = mlr:::getBMRObjects(bmr, learner.ids = learner.ids,
#                                         fun = function(x) {x})
#   bmr_new$learners = bmr$learners[learner.ids]
#   return(bmr_new)
# }
# bmr_subs = subsetBMR(bmr, c("knn1nderiv0_eucl", "classif.classiFunc.kernel.tuned",
#                             "knn1nderiv0_dtw", "knnOptNderivOptSemimetOpt.tuned",
#                             "LCE", "LCE_noisy", "RFE", "RFE_noisy"))


# # create critical difference diagrams for useful subsets of the learners
# # critical difference diagram
# learner.ids = c("knn1nderiv0_eucl",
#                 "fdaclassif.classiKernel.tuned")
#
# # new helper function to extract bmr objects containing only a subset of the learners
# subsetBMR = function(bmr, learner.ids) {
#   bmr_new = bmr
#   bmr_new$results = mlr:::getBMRObjects(bmr, learner.ids = learner.ids,
#                                         fun = function(x) {x})
#   bmr_new$learners = bmr$learners[learner.ids]
#   return(bmr_new)
# }
#
# # is it better to ensemble or to choose using CV
# ens_better = c("knn_eucl_ensemble",
#                "nderiv_eucl_ensemble", "semimet_ensemble",
#                "nderivKnnSemimet_ensemble",
#                "knn1NderivOpt_eucl.tuned",
#                "knnOptNderiv0_eucl.tuned", "knn1Nderiv0_semimetOpt.tuned",
#                "knnOptNderivOptSemimetOpt.tuned")
#
# bmr_ens_better = subsetBMR(bmr, ens_better)
#
# g_ens_better = generateCritDifferencesData(bmr_ens_better, measure = multiclass.brier,
#                                            p.value = 0.05, test = "nemenyi")
# p.cd_ens_better = plotCritDifferences(g_ens_better, pretty.names = TRUE) +
#   scale_color_manual(values = lrns.colors[which(lrns.ids %in% ens_better)],
#                      limits = lrns.ids[which(lrns.ids %in% ens_better)],
#                      name = "learner")
# # theme(text = element_text(size = 10),
# #      plot.margin = unit(c(2, 1, 0.5, 0.5), "lines"))
# p.cd_ens_better
#
# ggsave(paste0("Plots/benchmark/", name, "_ensemble_better_cd.pdf"),
#        plot = p.cd_ens_better, width = 0.8*13, height = 0.8*9)
#
#
# # is it better to use the rf or the nn ensemble
# rf_better = c("knn1nderiv0_eucl", "fdaclassif.classiKernel.tuned",
#               "knn1nderiv0_dtw",
#               "nderivKnnSemimet_ensemble", "knnOptNderivOptSemimetOpt.tuned",
#               "rf_feat_semimet_ensemble", "rf_nofeat_semimet_ensemble",
#               "rf_noisy_ensemble", "noisy_eucl_ensemble")
#
# bmr_rf_better = subsetBMR(bmr, rf_better)
#
# g_rf_better = generateCritDifferencesData(bmr_rf_better, measure = multiclass.brier,
#                                           p.value = 0.05, test = "nemenyi")
# p.cd_rf_better = plotCritDifferences(g_rf_better, pretty.names = TRUE) +
#   scale_color_manual(values = lrns.colors[which(lrns.ids %in% rf_better)],
#                      limits = lrns.ids[which(lrns.ids %in% rf_better)],
#                      name = "learner")
# # theme(text = element_text(size = 10),
# #       plot.margin = unit(c(2, 1, 0.5, 0.5), "lines"))
# p.cd_rf_better
#
# ggsave(paste0("Plots/benchmark/", name, "_rf_better_cd.pdf"),
#        plot = p.cd_rf_better, width = 0.8*13, height = 0.8*9)
#
# ######################
# ref_mod = c("knn1nderiv0_eucl", "fdaclassif.classiKernel.tuned",
#             "knn1nderiv0_dtw",
#             "knn1nderiv0_amplitude",
#             "knn1nderiv0_phase",
#             "knn_eucl_ensemble",
#             "nderiv_eucl_ensemble", "semimet_ensemble",
#             "rf_nofeat_semimet_ensemble",
#             "rf_feat_semimet_ensemble")
# bmr_ref_mod = subsetBMR(bmr, ref_mod)
#
# g_ref_mod = generateCritDifferencesData(bmr_ref_mod, measure = multiclass.brier,
#                                         p.value = 0.05, test = "nemenyi")
# p.cd_ref_mod = plotCritDifferences(g_ref_mod, pretty.names = TRUE) +
#   scale_color_manual(values = lrns.colors[which(lrns.ids %in% ref_mod)],
#                      limits = lrns.ids[which(lrns.ids %in% ref_mod)],
#                      name = "learner")
# p.cd_ref_mod
#
# ggsave(paste0("Plots/benchmark/", name, "_ref_mod_cd.pdf"),
#        plot = p.cd_ref_mod, width = 0.8*13, height = 0.8*9)
