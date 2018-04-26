# CoFD
Classification of Functional Data

Overview of repository
* Benchmark_results: Folder containing the .bmr files created by the benchmark experiments
* Data: Folder containing the data from the UCR TSC repository 
* Literatur: Miscellaneous papers that are related to the paper. Not a complete list of all cited papers
* Plots: all plots generated and downloaded from the internet for the paper
  * BeetleFly: Original images of beetles and flies, extracted outlines for one beetle and one fly
  * EnsemblePartialDependence: partial dependence plots for the RFE and the LCE for all base learner combinations
  * benchmark: plots for results of benchmark experiments
* R: R code
  * old: Unused R code. Can be deleted before publicising
  * Section3, Section4: R code chunks that are in paper
  * UCR_TSC_data: read in UCR TSC data, transform it to mlr tasks, select feasible data sets
  * benchmark_paper_server: Run this file on the server to execute the benchmark experiment
  * benchmark_paper: Code for setting up benchmark
  * benchmark_ressults_paper: Plot benchmark results, see /Plots/benchmark/
  * create_learners_paper: Create base learners, ensembles, and reference learners for benchmark
  * main_semimetrics: Overview of most important semimetrics currently implemented in classiFunc, their hyper parameters, and their possible range.
  * plot_Ensemble: Create LCE and RFE for subsequent plotting
  * plot_EnsemblePartDep_TE, plot_EnsemblePartDep_TE, plot_EnsemblePartDep_TE: create partial dependence plots for how the ensembles (tree, LCE, RFE) depend on their base models, see /Plots/EnsemblePartialDependence
  * plot_beetlefly: Create plots for transition of image to outline to 1d function of BeetleFly data
  * plot_classiFunc_users: Plot number of downloads of mlr and classiFunc per day
  * plot_kernelFunctions: Plot all available kernel functions
  * plot_model_weights: Create bar plots for the LCE (model weights) and RFE (variable importance)
* CoFD.Rproj: R project. Open this and the working directory and all paths are correctly set for all R files in /R

