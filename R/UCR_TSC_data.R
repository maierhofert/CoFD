# this file reads in the UCR TSC data and creates mlr taks for them to be 
# used in the benchmark experiment

# Path to this R-File
data_path = "Data/TSC Problems"

# get names of all data sets
data_names = list.dirs(data_path, full.names = FALSE, recursive = FALSE)
data_names = data_names[!data_names %in% c("", "Data Descriptions", 
                                           "ElectricDeviceOn", "ECGMeditation", 
                                           "EpilepsyX", "EthanolLevel", 
                                           "HeartbeatBIDMC", "OliveOil",
                                           "Yoga",
                                           "StarLightCurves",
                                           "WormsTwoClass")]

# absolute paths to the data sets
data_paths = paste0(data_path, "/", data_names, "/", data_names, ".arff")
names(data_paths) = data_names

# read in the data sets
library("foreign")

# data_list = lapply(data_paths, read.arff)
data_list = list()

for (i in 1:length(data_paths)) {
  data_list[[i]] = read.arff(data_paths[i])
}
names(data_list) = data_names

# # create the data sets for classiFunc package
# Phoneme = read.arff(data_paths[65])
# Phoneme = Phoneme[1:100, c(TRUE, rep(FALSE, 15))]
# devtools::use_data(Phoneme)
# ArrowHead = read.arff(data_paths[4])
# ArrowHead = ArrowHead[1:100, c(FALSE, FALSE, TRUE)]
# devtools::use_data(ArrowHead)
# Yoga = read.arff(data_paths[96])
# dim(Yoga)
# save(Yoga, file = "data/Yoga.RData")
# devtools::use_data(Yoga)

# Create artificial name colum
for (i in 1:length(data_list)) {
  data_list[[i]]["name"] = factor(names(data_list)[i])
}

# create FDA tasks
library("mlr")
mlr_data_list = list()
for (i in 1:length(data_list)) {
  # dat = data_list[[i]]
  mlr_data_list[[i]] = makeFunctionalData(data = data_list[[i]],
                                          exclude.cols = c("target", "name"))
}


tsks = list()
for (i in 1:length(data_list)) {
  dat = mlr_data_list[[i]]
  tsks[[i]] = makeClassifTask(data = dat[, c("target", "fd1")],
                              id = toString(dat$name[1]),
                              target = "target")
}

# ##############################################################################
# compute summary statistics for data sets
tsks
library("dplyr")
library("data.table")
# # adjusting for maximum entropy is not common
max_ent = function(x) {
  - x * (1/x * log(1/x))
}
plot(max_ent(1:200))

UCR_TSC = data.table(Id = sapply(tsks, getTaskId),
                     NClasses = sapply(tsks, function(x) x %>%
                                         getTaskClassLevels %>%
                                         length),
                     NObs = sapply(tsks, getTaskSize),
                     ObsLen = sapply(tsks, function(x) ncol(getTaskData(x)) - 1),
                     Entropy = sapply(tsks, function(x) getTaskData(x)[,"target"] %>%
                                        table %>%
                                        entropy %>%
                                        round(digits = 2))
)
UCR_TSC = mutate(UCR_TSC, adjEntropy = round(Entropy / max_ent(NClasses), digits = 2))
UCR_TSC = mutate(UCR_TSC, CompCost = round(NObs * ObsLen / 1000, digits = 0))
UCR_TSC
summary(UCR_TSC)
# ##############################################################################
# select feasible tasks
name = nobs = obslen = rep(NA, length(tsks))
for (i in 1:length(tsks)) {
  name[i] = getTaskId(tsks[[i]])
  nobs[i] = getTaskSize(tsks[[i]])
  obslen[i] =  ncol(getTaskData(tsks[[i]]))
}
df = data.frame(name, nobs, obslen, nobs*obslen)
# hist(df$nobs...obslen, breaks = 100)
# summary(df$nobs...obslen)
# quantile(df$nobs...obslen, 0.4)
# df_red = df[df$nobs...obslen <= 50000,]
# nrow(df_red)

tsk_names = lapply(tsks, getTaskId)
small_tasks = sapply(1:length(tsks), function(i) (any(table(getTaskTargets(tsks[[i]])) < 10)))
# # Benchmark_results/2017-
# go up to 100000 later
# tsks = tsks[df$nobs...obslen <= 50000 & !small_tasks]
tsks = tsks[df$nobs...obslen <= 100000 & !small_tasks]
# delete unneeded large objects
rm(data_list)
