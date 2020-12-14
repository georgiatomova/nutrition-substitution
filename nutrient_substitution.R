###-----------------------------------------------------------------------
# Adjustment with Nutrients
###-----------------------------------------------------------------------

###-----------------------------------------------------------------------
# This code simulates two datasets based on pre-defined data structures -
# with and without the presence of proxy confounding by common causes of diet.

# Exposure: non-milk extrinsic sugars (NMES)
# Outcome: Body weight (WT)

# Each nutrient is described as 'healthy' or 'unhealthy' based on the
# relative size of the (rescaled) causal effect in comparison to the exposure.
# A series of models are run in which either only the 'healthy' or 'unhealthy'
# nutrients are adjusted in addition to total energy intake, using either the 
# standard model or the all-components model.

###-----------------------------------------------------------------------

library(progress); library(devtools); library(dagitty)

# Define data structures for simulation 

# No confounding
DAG <- dagitty("dag {
                
                NMES  -> WT   [beta = 0.25]
                STR   -> WT   [beta = 0.33]
                FBR   -> WT   [beta = -0.02]
                UF    -> WT   [beta = 0.24]
                PRO   -> WT   [beta = 0.15]
                SF    -> WT   [beta = 0.175]
                ALC   -> WT   [beta = 0.09]
                WTR   -> WT   [beta = 0.00]
                
                U     -> NMES [beta = 0]
                U     -> STR  [beta = 0]
                U     -> FBR  [beta = 0]
                U     -> SF   [beta = 0]
                U     -> UF   [beta = 0]
                U     -> PRO  [beta = 0]
                U     -> ALC  [beta = 0]
                U     -> WTR  [beta = 0]
                
                }") 

# Confounding
DAGU <- dagitty("dag {
                
                NMES  -> WT   [beta = 0.25]
                STR   -> WT   [beta = 0.33]
                FBR   -> WT   [beta = -0.02]
                UF    -> WT   [beta = 0.24]
                PRO   -> WT   [beta = 0.15]
                SF    -> WT   [beta = 0.175]
                ALC   -> WT   [beta = 0.09]
                WTR   -> WT   [beta = 0.00]
                
                U     -> NMES [beta = 0.5]
                U     -> STR   [beta = 0.25]
                U     -> FBR  [beta = -0.5]
                U     -> SF   [beta = 0.5]
                U     -> UF   [beta = 0.25]
                U     -> PRO  [beta = 0.25]
                U     -> ALC  [beta = 0.5]
                U     -> WTR  [beta = 0.00]
                
                }") 


set.seed(96)

# Set up data frames to store means
NutrientMeans <- data.frame(mod1=numeric(0), 
                              mod1U=numeric(0),
                              mod2=numeric(0), 
                              mod2U=numeric(0), 
                              mod3=numeric(0), 
                              mod3U=numeric(0),
                              modA1=numeric(0),
                              modA1U=numeric(0),
                              modA2=numeric(0),
                              modA2U=numeric(0),
                              modA3=numeric(0),
                              modA3U=numeric(0))

# Conduct simulations 
Nsims <- 100000
Nobs  <- 1000
pb <- progress_bar$new(total = Nsims, format = ":bar :percent eta: :eta")

for (i in 1:Nsims) {
  
  SimData  <- simulateSEM(DAG,  N=Nobs)
  SimUData <- simulateSEM(DAGU, N=Nobs)
  
  # Rescale variables based on plausible mean and SD values
  
  SimData$NMES   <- SimData$NMES*125+250  
  
  SimData$STR    <- SimData$STR*250+500  
  SimData$FBR    <- SimData$FBR*50+100  
  SimData$UF     <- SimData$UF*200+400   
  SimData$PRO    <- SimData$PRO*150+300  
  SimData$SF     <- SimData$SF*125+275  
  SimData$ALC    <- SimData$ALC*50+175
  SimData$WTR    <- SimData$WTR*500+2000
  SimData$WT     <- SimData$WT*25+80
  
  SimUData$NMES  <- SimUData$NMES*125+250
  
  SimUData$STR   <- SimUData$STR*250+500  
  SimUData$FBR   <- SimUData$FBR*50+100  
  SimUData$UF    <- SimUData$UF*200+400  
  SimUData$PRO   <- SimUData$PRO*150+300 
  SimUData$SF    <- SimUData$SF*125+275
  SimUData$ALC   <- SimUData$ALC*50+175
  SimUData$WTR    <- SimUData$WTR*1000+2000
  SimUData$WT    <- SimUData$WT*25+80  
  
  # Calculate total and residual energy intake
  
  SimData$TotalEnergy      <- SimData$NMES + SimData$STR + SimData$FBR + SimData$UF + SimData$PRO +  SimData$SF + SimData$ALC
  SimData$ResidualEnergy   <- SimData$STR + SimData$FBR + SimData$UF + SimData$PRO + SimData$SF + SimData$ALC
  SimUData$TotalEnergy     <- SimUData$NMES + SimUData$STR + SimUData$FBR + SimUData$UF + SimUData$PRO + SimUData$SF + SimUData$ALC
  SimUData$ResidualEnergy  <- SimUData$STR + SimUData$FBR + SimUData$UF + SimUData$PRO + SimUData$SF + SimUData$ALC
  
# Run models and store mean effects
  
### 1. The average relative causal effect (a.k.a. The standard model)
  mod1   <- lm(WT ~ NMES + TotalEnergy, data=SimData)
  mod1U  <- lm(WT ~ NMES + TotalEnergy, data=SimUData)
  
  mean1  <- mod1$coefficients[2]*100 
  mean1U <- mod1U$coefficients[2]*100
  
### 2. Average relative causal effect of NMES instead of  more 'healthy' variables
  mod2  <- lm(WT ~ NMES + TotalEnergy + STR + SF + ALC, data=SimData)
  mod2U <- lm(WT ~ NMES + TotalEnergy + STR + SF + ALC, data=SimUData)
  
  mean2  <- mod2$coefficients[2]*100 
  mean2U <- mod2U$coefficients[2]*100

### 3. Average relative causal effect of NMES instead of less 'healthy' variables
  mod3  <- lm(WT ~ NMES + TotalEnergy + FBR + UF + PRO, data=SimData)
  mod3U <- lm(WT ~ NMES + TotalEnergy + FBR + UF + PRO, data=SimUData)
  
  mean3  <- mod3$coefficients[2]*100
  mean3U <- mod3U$coefficients[2]*100

### Alternative 'all-components' model
  
### A1. The average relative causal effect
  modA1  <- lm(WT ~ NMES + STR + FBR + UF + PRO + SF + ALC, data=SimData)
  modA1U <- lm(WT ~ NMES + STR + FBR + UF + PRO + SF + ALC, data=SimUData)
  
  #Calculate weights for mean:
  wi <- c(mean(SimData$STR)/mean(SimData$ResidualEnergy),
               mean(SimData$FBR)/mean(SimData$ResidualEnergy),
               mean(SimData$UF)/mean(SimData$ResidualEnergy),
               mean(SimData$PRO)/mean(SimData$ResidualEnergy),
               mean(SimData$SF)/mean(SimData$ResidualEnergy),
               mean(SimData$ALC)/mean(SimData$ResidualEnergy))
  
  meanA1  <- modA1$coefficients[2]*100-
      (wi[1]*modA1$coefficients[3]*100+
       wi[2]*modA1$coefficients[4]*100+
       wi[3]*modA1$coefficients[5]*100+
       wi[4]*modA1$coefficients[6]*100+
       wi[5]*modA1$coefficients[7]*100+
       wi[6]*modA1$coefficients[8]*100)
  
  meanA1U  <- modA1U$coefficients[2]*100-
      (wi[1]*modA1U$coefficients[3]*100+
       wi[2]*modA1U$coefficients[4]*100+
       wi[3]*modA1U$coefficients[5]*100+
       wi[4]*modA1U$coefficients[6]*100+
       wi[5]*modA1U$coefficients[7]*100+
       wi[6]*modA1U$coefficients[8]*100)

### A2. Average relative causal effect of NMES instead of  more 'healthy' variables 
  
  modA2  <- modA1
  modA2U <- modA1U
  
  # Calculate weights for means:
  wj <- c(mean(SimData$FBR)/mean(SimData$STR+SimData$FBR+SimData$UF+SimData$PRO),
             mean(SimData$UF)/mean(SimData$STR+SimData$FBR+SimData$UF+SimData$PRO),
             mean(SimData$PRO)/mean(SimData$STR+SimData$FBR+SimData$UF+SimData$PRO))
  
  meanA2  <- modA2$coefficients[2]*100 - 
      (wj[1]*modA2$coefficients[4]*100+
       wj[2]*modA2$coefficients[5]*100+
       wj[3]*modA2$coefficients[6]*100)
  
  meanA2U <- modA2U$coefficients[2]*100 -
      (wj[1]*modA2U$coefficients[4]*100+
       wj[2]*modA2U$coefficients[5]*100+
       wj[3]*modA2U$coefficients[6]*100)
  
### A3. Average relative causal effect of NMES instead of  more 'healthy' variables
  
  modA3  <- modA1
  modA3U <- modA1U
  
  # Calculate weights for means:
  wk <- c(mean(SimData$STR)/mean(SimData$STR+SimData$FBR+SimData$UF+SimData$PRO),
          mean(SimData$SF)/mean(SimData$NMES+SimData$SF+SimData$ALC),
          mean(SimData$ALC)/mean(SimData$NMES+SimData$SF+SimData$ALC))
  
  meanA3  <- modA3$coefficients[2]*100-
      (wk[1]*modA3$coefficients[3]*100+
       wk[2]*modA3$coefficients[7]*100+
       wk[3]*modA3$coefficients[8]*100)
  
  meanA3U <- modA3U$coefficients[2]*100-
      (wk[1]*modA3U$coefficients[3]*100+
       wk[1]*modA3U$coefficients[7]*100+
       wk[2]*modA3U$coefficients[8]*100)
  

  #Save all means into a dataframe:
  NutrientMeans[nrow(NutrientMeans)+1,] <- c(mean1, mean1U, mean2, mean2U, mean3, mean3U, meanA1, meanA1U, meanA2, meanA2U, meanA3, meanA3U)

  rm(mod1, mod1U, mod2, mod2U, mod3, mod3U, modA1, modA1U, modA2, modA2U, modA3, modA3U,
     mean1, mean1U, mean2, mean2U, mean3, mean3U, meanA1, meanA1U, meanA2, meanA2U, meanA3, meanA3U, 
     wi, wj, wk)
  
  #Display simulation progress
  pb$tick()
  
} 


### Save mean and centiles for key coefficients

SummaryNutrientMeans <- data.frame(model=character(0), 
                           lower=numeric(0), 
                           point=numeric(0), 
                           upper=numeric(0))

modelname <- list("Mod1", "Mod1U", "Mod2", "Mod2U", "Mod2", "Mod3U",
                  "ModA1", "ModA1U", "ModA2", "ModA2U", "ModA3", "ModA3U")

for (j in 1:12) {
  
  centiles  <- c(round(quantile(NutrientMeans[,j], 0.025), digits=2), round(quantile(NutrientMeans[,j], 0.5), digits=2), round(quantile(NutrientMeans[,j], 0.975),digits=2))
  SummaryNutrientMeans[nrow(SummaryNutrientMeans)+1,]  <- c(modelname[j], unname(as.list(centiles)))
  rm(centiles)
}

View(SummaryNutrientMeans)
#write.csv(SummaryNutrientMeans,"SummaryNutrientMeans.csv") 


