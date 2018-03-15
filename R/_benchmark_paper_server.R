# this file can be used to run the benchmarking on a server
# for the UCR TSC data
begin = Sys.time()
source("R/UCR_TSC_data.R")
source("R/create_base_learners_paper.R")
source("R/benchmark_paper.R")
end = Sys.time()
difftime(end, begin)
# save benchmarking result
saveRDS(bmr, paste0("Benchmark_results/", Sys.Date(), "bmr_paper.RDS"))
saveRDS(bmr, "Benchmark_results/bmr.RDS")
