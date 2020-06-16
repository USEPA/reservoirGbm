source("scripts/masterLibraryVm.R") # load libraries


source("scripts/runGBM.v4.R") # load gbm function


source("scripts/kMeansSampling.R") # representative testing/training


source("scripts/loadData.R") # loads meanVariance.c.lake.lu.agg.RData

source("scripts/defineRespCov.R") # assumes allCovar, nationalCovar, localDataGbm, and natDatGbm are in environment

source("scripts/evalVIPplots.R") # generates list of gbms. 
                                 # cList1, 850 models (50 replicates of 17 models)
                                 # cList.1e, 17 best based on os and is MSE
#Sys.time() - time.start # 13.8 hours on 64.0 GB RAM and a 2.10 GHz processor