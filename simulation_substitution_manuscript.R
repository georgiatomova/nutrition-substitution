rm(list=ls()) 

# load packages
library(progress); library(devtools); library(dagitty)

# set seed
set.seed(100)

# define data structure for simulation
DAG <- dagitty("dag {
                sugars    -> fpg   [beta = 0.25]
                carbs     -> fpg   [beta = 0.33]
                fiber     -> fpg   [beta = -0.02]
                protein   -> fpg   [beta = 0.15]
                unsat_fat -> fpg   [beta = 0.24]
                sat_fat   -> fpg   [beta = 0.175]
                alcohol   -> fpg   [beta = 0.09]
                water     -> fpg   [beta = 0.00]
                }") 

# set up empty dataframes to store coefficients
estimand1 <- data.frame(mod1_1=numeric(0),
                      mod1_2=numeric(0),
                      mod1_3=numeric(0))

estimand2 <- data.frame(mod2_1=numeric(0),
                        mod2_2=numeric(0),
                        mod2_3=numeric(0))

estimand3 <- data.frame(mod3_1=numeric(0),
                        mod3_2=numeric(0),
                        mod3_3=numeric(0))

# simulate 1000 observations
n_obs=1000
n_sims=100000

# create simulation progress bar
pb <- progress_bar$new(total = n_sims, format = ":bar :percent eta: :eta")

# run simulation
for (i in 1:n_sims) {
  
  sim_data  <- simulateSEM(DAG, N=n_obs)
  
  # rescale variables based on plausible mean and SD values
  
  sim_data$sugars    <- sim_data$sugars*125+250  
  sim_data$carbs     <- sim_data$carbs*250+500  
  sim_data$fiber     <- sim_data$fiber*50+100  
  sim_data$protein   <- sim_data$protein*150+300  
  sim_data$unsat_fat <- sim_data$unsat_fat*200+400   
  sim_data$sat_fat   <- sim_data$sat_fat*125+275  
  sim_data$alcohol   <- sim_data$alcohol*50+175
  sim_data$fpg       <- sim_data$fpg*25+80
  
  # calculate total and remaining energy intake
  
  sim_data$total_energy         <- sim_data$sugars + sim_data$carbs + sim_data$fiber + sim_data$protein + sim_data$unsat_fat +  sim_data$sat_fat + sim_data$alcohol
  sim_data$remaining_energy     <- sim_data$carbs + sim_data$fiber + sim_data$protein + sim_data$unsat_fat +  sim_data$sat_fat + sim_data$alcohol
  sim_data$remaining_energy_b   <- sim_data$carbs + sim_data$protein + sim_data$unsat_fat +  sim_data$sat_fat + sim_data$alcohol
  sim_data$carbs_protein        <- sim_data$carbs + sim_data$protein

  # individual total causal effects of each nutrient using the all-components model
  nutrients   <- lm(fpg ~ sugars + carbs + protein + sat_fat + unsat_fat + fiber + alcohol, data=sim_data)
  
  sugars    <- nutrients$coefficients[2]*100
  carbs     <- nutrients$coefficients[3]*100
  protein   <- nutrients$coefficients[4]*100
  sat_fat   <- nutrients$coefficients[5]*100
  unsat_fat <- nutrients$coefficients[6]*100
  fiber     <- nutrients$coefficients[7]*100
  alcohol   <- nutrients$coefficients[8]*100
  
  ### 1. the relative causal effect of sugars instead of fiber ###
  
  # 1.1. the leave-one-out model 
  mod1_1       <- lm(fpg ~ sugars + total_energy + carbs + protein + unsat_fat + sat_fat + alcohol, data=sim_data)
  mod1_1_coef  <- mod1_1$coefficients[2]*100
  
  # 1.2. simple energy partition model
  mod1_2       <- lm(fpg ~ sugars + fiber + remaining_energy_b, data=sim_data)
  mod1_2_coef  <- mod1_2$coefficients[2]*100 - mod1_2$coefficients[3]*100
  
  # 1.3. comprehensive energy partition model / all-components model
  mod1_3       <- lm(fpg ~ sugars + fiber + carbs + protein + unsat_fat + sat_fat + alcohol, data=sim_data)
  mod1_3_coef  <- mod1_3$coefficients[2]*100 - mod1_3$coefficients[3]*100
  
  ### 2. the relative causal effect of sugars instead of carbs and protein ###
  
  # 2.1. the leave-one-out model 
  mod2_1       <- lm(fpg ~ sugars + total_energy + fiber + unsat_fat + sat_fat + alcohol, data=sim_data)
  mod2_1_coef  <- mod2_1$coefficients[2]*100
  
  # 2.2. simple energy partition model
  mod2_2       <- lm(fpg ~ sugars + carbs_protein + fiber + unsat_fat + sat_fat + alcohol, data=sim_data)
  mod2_2_coef  <- mod2_2$coefficients[2]*100 - mod2_2$coefficients[3]*100
  
  # 2.3. comprehensive energy partition model / all-components model
  wi_mod2_3 <- c(mean(sim_data$carbs)/mean(sim_data$carbs_protein),
               mean(sim_data$protein)/mean(sim_data$carbs_protein))
  
  mod2_3_coef <- mod1_3$coefficients[2]*100-
    (wi_mod2_3[1]*mod1_3$coefficients[4]*100+
     wi_mod2_3[2]*mod1_3$coefficients[5]*100)
  
  ### 3. the average relative causal effect of sugars instead of everything else ###
  
  # 3.1. the leave-one-out model 
  mod3_1       <- lm(fpg ~ sugars + total_energy, data=sim_data)
  mod3_1_coef  <- mod3_1$coefficients[2]*100
  
  # 3.2. simple energy partition model
  mod3_2       <- lm(fpg ~ sugars + remaining_energy, data=sim_data)
  mod3_2_coef  <- mod3_2$coefficients[2]*100 - mod3_2$coefficients[3]*100
  
  # 3.3. comprehensive energy partition model / all-components model
  wi_mod3_3 <- c(mean(sim_data$fiber)/mean(sim_data$remaining_energy),
                 mean(sim_data$carbs)/mean(sim_data$remaining_energy),
                 mean(sim_data$protein)/mean(sim_data$remaining_energy),
                 mean(sim_data$unsat_fat)/mean(sim_data$remaining_energy),
                 mean(sim_data$sat_fat)/mean(sim_data$remaining_energy),
                 mean(sim_data$alcohol)/mean(sim_data$remaining_energy))
  
  mod3_3_coef  <- mod1_3$coefficients[2]*100-
    (wi_mod3_3[1]*mod1_3$coefficients[3]*100+
       wi_mod3_3[2]*mod1_3$coefficients[4]*100+
       wi_mod3_3[3]*mod1_3$coefficients[5]*100+
       wi_mod3_3[4]*mod1_3$coefficients[6]*100+
       wi_mod3_3[5]*mod1_3$coefficients[7]*100+
       wi_mod3_3[6]*mod1_3$coefficients[8]*100)
  
  # save coefficients:
  estimand1[nrow(estimand1)+1,] <- c(mod1_1_coef, mod1_2_coef, mod1_3_coef)
  estimand2[nrow(estimand2)+1,] <- c(mod2_1_coef, mod2_2_coef, mod2_3_coef)
  estimand3[nrow(estimand3)+1,] <- c(mod3_1_coef, mod3_2_coef, mod3_3_coef)

  #rm(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod1_coef, mod2_coef, mod3_coef, mod4_coef, mod5_coef, mod6_coef, mod7_coef) 
  
  # Display simulation progress
  pb$tick()
  
} 

# build a data frame with all final results
model_names_average_relative_effect <- list("Leave-one-out", "Partition", "All-components")
model_names_relative_effects <- list("Leave-one-out", "Energy Partition", "All-components")

final_results <- data.frame(model=character(0),
                            lower=numeric(0), 
                            point=numeric(0), 
                            upper=numeric(0))


final_results[nrow(final_results)+1,] <- c("JOINT EFFECT OF INCREASING SUGARS AND DECREASING FIBER", "", "", "")

for (j in 1:3) {
  
  centiles                             <- c(round(quantile(estimand1[,j], 0.025), digits=2), round(quantile(estimand1[,j], 0.5), digits=2), round(quantile(estimand1[,j], 0.975),digits=2))
  final_results[nrow(final_results)+1,]  <- c(model_names_average_relative_effect[j], unname(as.list(centiles)))
  rm(centiles)
} 

final_results[nrow(final_results)+1,] <- c("JOINT EFFECT OF INCREASING SUGARS AND DECREASING CARBS & PROTEIN", "", "", "")

for (j in 1:3) {
  
  centiles                             <- c(round(quantile(estimand2[,j], 0.025), digits=2), round(quantile(estimand2[,j], 0.5), digits=2), round(quantile(estimand2[,j], 0.975),digits=2))
  final_results[nrow(final_results)+1,]  <- c(model_names_relative_effects[j], unname(as.list(centiles)))
  rm(centiles)
} 


final_results[nrow(final_results)+1,] <- c("AVERAGE RELATIVE CAUSAL EFFECT", "", "", "")

for (j in 1:3) {
  
  centiles                             <- c(round(quantile(estimand3[,j], 0.025), digits=2), round(quantile(estimand3[,j], 0.5), digits=2), round(quantile(estimand3[,j], 0.975),digits=2))
  final_results[nrow(final_results)+1,]  <- c(model_names_relative_effects[j], unname(as.list(centiles)))
  rm(centiles)
} 

View(final_results)
write.csv(final_results, "results_manuscript.csv")

