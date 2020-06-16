# SCRIPT FOR DEFINING IMPORTANT VARIABLES USED IN GBM RUNS
# CANDIDATE VARIABLED WERE SCREENED FOR COLLINEARITY IN variableCorrelation.R

# RESPONSE VARIABLES--------
# List of response variables
respList <- c("ebMlHrM2_Estimate",
              "ch4.trate.mg.h_Estimate",
              "ch4.drate.mg.m2.h_Estimate",
              "ch4.erate.mg.h_Estimate",
              "co2.trate.mg.h_Estimate",
              "co2.drate.mg.m2.h_Estimate",
              "co2.erate.mg.h_Estimate")


# ALL PREDICTOR VARIABLES-------------
# List of covariates to include for gbm model using local observations and
# all covariates.
# First vector contains optimal set of covariates.
allCovar <- c(
  # Local chemistry
  "chla_Estimate", "tp_Estimate", "tn_Estimate", "dissolved.ch4_Estimate", 
  "dissolved.co2_Estimate", "toc_Estimate",
  
  # local chemistry + bathymetry
  "hypoxic.frac", "hypol.frac", 
  
  # National morphometry, 
  # calculated w/Pegasus/Dynamac/UC data
  # si # correlation with res size
  # mean.depth.m  # correlates strongly with max depth and prop.less.3m
  # "res.fetch.m" correlates with area and perimeter (0.85, 0.65)
  # "res.perimeter.m" correlates with area, circ, and watershed area (.87, -0.85,.62)
  "max.depth.m", "depth.ratio", "dynamic.ratio", "prop.less.3m",
  "reservoir.area.m2", 
  # "circ", correlates w/ size and gives a strange result  
  "rda", 
  
  # National watershed
  # "nhdPctAg2006Slp20Ws" # correlates w/agSlp10
  # "watershed.area.m2",  # From Pegasus/Dynamac/UC.  correlates with RDA 0.64
  "percent.agg.ag",  # From Pegasus/Dynamac/UC
  "nhdPctAg2006Slp10Ws", # ag on high slopes
  # "nhdFertWs", # synthetic N fertilizer (kg N/ha/yr) 0.96 correlation w/%ag
  # "nhdManureWs", # manure application (kg N/ha/yr), gives strange result
  # nhdNWs, # correlates with %N >=0.93 where N is Fert + Manure
  "nhdRunoffWs", # mean runoff (mm)
  
  # Kffactor: relative index of susceptibility of bare, cultivated soil to particle detachment and transport by rainfall.
  # "nhdAgKffactWs", # Mean Kffactor on agricultural land (NLCD 2006).  >0.9 correlation w/percent ag and Fert   
  "nhdKffactWs", # Mean Kffactor within watershed
  
  "nhdTmean8110Ws", # 30-year normal mean temperature (C°) 
  "nhdOmWs" # Mean organic matter content (% by weight) of soils 
)


# 'NATIONAL' SUBSET OF PREDICTOR VARIABLES---------------------------
# Correlations among this set of predictors was screened for all observations
# and just local observations.  Correlations structure was very similar among
# two sets of observations.  Set of predictors defined below will work for models
# with just local observations AND models with all observations.
nationalCovar <- c(
  
  # National morphometry, 
  # calculated w/Pegasus/Dynamac/UC data
  # si # correlation with res size
  # mean.depth.m  # correlates strongly with max depth and prop.less.3m
  # "res.fetch.m" correlates with area and perimeter (0.85, 0.65)
  # "res.perimeter.m" correlates with area, circ, and watershed area (.87, -0.85,.62)
  "max.depth.m", "depth.ratio", "dynamic.ratio", "prop.less.3m",
  "reservoir.area.m2", 
  # "circ", correlates w/ size, gives weird result  
  "rda", 
  
  # National watershed
  # "nhdPctAg2006Slp20Ws" # correlates w/agSlp10
  # "watershed.area.m2",  # From Pegasus/Dynamac/UC.  correlates with RDA 0.64
  "percent.agg.ag",  # From Pegasus/Dynamac/UC
  "nhdPctAg2006Slp10Ws", # ag on high slopes
  # "nhdFertWs", # synthetic N fertilizer (kg N/ha/yr) 0.96 correlation w/%ag
  # "nhdManureWs", # manure application (kg N/ha/yr), gives strange result
  # nhdNWs, # correlates with %N >=0.93 where N is Fert + Manure
  "nhdRunoffWs", # mean runoff (mm)
  
  # Kffactor: relative index of susceptibility of bare, cultivated soil to particle detachment and transport by rainfall.
  # "nhdAgKffactWs", # Mean Kffactor on agricultural land (NLCD 2006).  >0.9 correlation w/percent ag and Fert   
  "nhdKffactWs", # Mean Kffactor within watershed
  
  "nhdTmean8110Ws", # 30-year normal mean temperature (C°) 
  "nhdOmWs" # Mean organic matter content (% by weight) of soils 
)

# DATA--------

natDataGbm <- meanVariance.c.lake.lu.agg

localDataGbm <- filter(meanVariance.c.lake.lu.agg, citation == "EPA")

