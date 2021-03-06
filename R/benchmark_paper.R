# Benchmark the learners on the UCR TSC data
# resampling description
# resampling description
# on local pc run a smaller benchmark
on_server = (.Platform$OS.type != "windows")
if (on_server) {
  res = makeResampleDesc(method = "CV", predict = "test",
                         stratify = TRUE,
                         iters = 10)
} else {
  res = makeResampleDesc(method = "CV", predict = "test",
                         stratify = TRUE,
                         iters = 10L)
  # lrns = lrns[c(1, 6)]
  # tsks = tsks[1:3]
}

# resampling instances
set.seed(1234)
res_instances = lapply(tsks, makeResampleInstance, desc = res)

#################################################################
# start benchmarking
rm(bmr)

library("parallelMap")

# benchmark in parallel
if (on_server) {
  parallelStartSocket(cpus = 10, level = "mlr.resample") # level = "mlr.resample"
  parallelLibrary("dtw", level = "mlr.resample")
} else {
  parallelStartSocket(cpus = 2, level = "mlr.resample")
  parallelLibrary("dtw", level = "mlr.resample")
}

# set a seed for reproducibility
parallel::clusterSetRNGStream(iseed = 42)

bmr = benchmark(learners = lrns,
                tasks = tsks, # [[1]],
                resamplings = res_instances,
                models = FALSE,
                keep.pred = FALSE,
                measures = list(multiclass.brier, mmce,
                                timetrain, timepredict, timeboth))
bmr

parallelStop()
