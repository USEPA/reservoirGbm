## PREVIOUS SIMULATIONS WERE USED TO IDENTIFY ACCEPTABLE VALUES FOR BF, SHR,
## CV FOLDS, AND TRAIN PROPORTION.  SIMULATIONS ALSO SHOWED THAT osMSE VALUES
## WERE APPROXIMATELY LOG-NORMALLY DISTRIBUTED ACROSS REPLICATE RUNS HERE WE ONLY 
## ACCEPT GBMs THAT HAVE OPTIMAL TREES > 1000 TREES, BUT < maxTREES.  OF THOSE THAT
## MEET THESE CRITERIA, WE WILL CHOOSE THE ONE THAT HAS THE LOWEST IN AND OUT OF
## SAMPLE MSE.

## THIS APPROACH WILL BE USED TO SELECT A SINGLE gbm FOR EACH RESPONSE ~ COVARIATE MODEL.  
## THERE IS SOME CONCERN, HOWEVER, THAT THIS SINGLE MODEL MAY NOT BE 'REPRESENTATIVE' 
## OF ALL THE POTENTIAL MODELS THAT MEET THE CRITERIA DESCRIBED ABOVE.  IN THE FOLLOWING 
## CODE WE WILL GENERATE 50 'REPLICATE' MODELS USING THE CRITERIA ABOVE.  WE WILL THEN
## EXAMINE THE VARIABLE IMPORTANCE RANKINGS TO SEE IF THEY ARE LARGELY CONSISTENT, OR HUGELY
## VARIABLE.  FINALLY WE WILL SELECT THE FINAL MODEL FOR EACH RESP~COV.

## THIS SCRIPT EXECUTED ON VM, TOO SLOW ON EZTECH SYSTEM.  LIST OBJECT CONTAINING ALL MODELS
## (cList) AND 629 'GOOD' MODELS (cList.2) IS TOO LARGE TO LOAD INTO EZTECH SYSTEM.  cList.3
## ONLY CONTAINS 17 'FINAL' MODELS AND CAN BE LOADED INTO EZTECH SYSTEM.

# RUN MODELS------------------------
# Set up some parameters for simulations
nRun <- 50 # number of simulations for each resp ~ cov combination
# Define  all combinations of response and predictors
resp <- c("ch4.trate.mg.h_Estimate", "ch4.drate.mg.m2.h_Estimate", "ch4.erate.mg.h_Estimate",
          "co2.trate.mg.h_Estimate", "co2.drate.mg.m2.h_Estimate", "co2.erate.mg.h_Estimate",
          "ebMlHrM2_Estimate")

covList <- c("All", "Nat") # All is national + local

obs <- c("Local", "Nat") # local in EPA 32, Nat is EPA + lit

# Matrix of above, filter out unused scenarios
respCovList <- expand.grid(resp, covList, obs, stringsAsFactors = FALSE) %>%
  rename(cov = Var2, resp = Var1, obs = Var3) %>%
  filter(obs != "Nat" | cov != "All") %>% # Remove Nat obs w/All cov set
  filter(!grepl(pattern = "co2", x = resp) | # Remove Nat CO2 resp, no data
           obs != "Nat") %>%
  filter(obs != "Nat" | # volumetric ebullition not available at national scale
           !grepl(pattern = "eb", x = resp)) %>%
  slice(rep(1:n(), nRun)) %>%
  arrange(resp, cov)

# Use for loop for simulations
cList1 <- list() # empty list to hold results
for(i in 1:nrow(respCovList)) { # 
#for (i in 31:35){
  message(i) # print progress indicator
  run.name <- paste(respCovList$resp[i],
                    respCovList$cov[i],
                    respCovList$obs[i])
  tmpGBM <- runGBM.v4(resp = respCovList$resp[i], 
                      covarType = respCovList$cov[i], 
                      obs = respCovList$obs[i],
                      nTrees = 20000,
                      seed = 2222+i, bf = 0.9, # different seed each run
                      shr = 0.0005, trainProp = 0.8)
  tmpGBM[["run.name"]] <- run.name
  cList1[[i]] <- tmpGBM # assign object to list
}

# SELECT MODEL WITH BEST COMBINED osMSE AND isMSE.----------
# Select models that have both low is and os MSE.
# Define minimum osMSE for each model
mseDist <- lapply(cList1, function(x) {
  x$mse[1:2, 2] # extract MSE, fine for choosing best model, but will use standardized RMSE for reporting, comparing across models.
}) %>%
  do.call(rbind,.) %>% # to vector
  as.data.frame() %>% # to df
  rename(isMSE = V1, osMSE = V2) %>%
  mutate(run.name = with(respCovList, paste(resp, cov, obs, sep = " "))) %>%
  group_by(run.name) %>%
  mutate(isRank = rank(isMSE),
         osRank = rank(osMSE),
         rank = isRank + osRank,
         best.i = ifelse(rank == min(rank),
                         TRUE,
                         FALSE)) %>%
  group_by(run.name, rank) %>%
  mutate(tieBreak = abs(rank/2 - osRank) + abs(rank/2 - isRank),
         bestModel.i = ifelse(best.i == TRUE & tieBreak == min(tieBreak),
                              TRUE,
                              FALSE)) 
  # arrange(run.name, rank)  # DO NOT ARRANGE!!! MESSES UP INDEXING BELOW

# Filter cList to select best model based on best combined osMSE and isMSE performance
# Confirm mseDist and cList are in same order
cListRunName <- lapply(cList1, function(x) x$run.name) %>% # extract run.names
  do.call(rbind, .) %>% # coerce to matrix
  as.vector()
sum(cListRunName == mseDist$run.name) # should be 850


cList.1e <- cList1[mseDist$bestModel.i] # subset based on logical.  
length(cList.1e);length(cList1) # 850 -> 17, 17 best models chosen

# Write to disk and push to repo?
 save(cList.1e, file = "output/cList.1e.RData") # write to disk for use on EZTech laptop

