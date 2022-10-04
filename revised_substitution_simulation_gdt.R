rm(list=ls()) 

# load packages
packages <- c("progress", "devtools", "dplyr", "dagitty", "matrixStats")
lapply(packages, require, character.only = TRUE)

# set seed
set.seed(99)

# set up emtpy dataframes to store simulation results later on 

macronutrients <- data.frame(sugars=numeric(0),
                             carbs=numeric(0),
                             protein=numeric(0),
                             sat_fat=numeric(0),
                             unsat_fat=numeric(0),
                             fiber=numeric(0),
                             alcohol=numeric(0))

#calc_foods_cals <- data.frame(calc_cereal_cals=numeric(0),
                              #calc_dairy_cals=numeric(0),
                              #calc_meat_cals=numeric(0),
                              #calc_fish_cals=numeric(0),
                              #calc_fruit_veg_cals=numeric(0),
                              #calc_nuts_cals=numeric(0),
                              #calc_non_alc_bev_cals=numeric(0),
                              #calc_alc_bev_cals=numeric(0),
                              #calc_misc_cals=numeric(0))

foods_cals_coefs <- data.frame(cereal_cals=numeric(0),
                         dairy_cals=numeric(0),
                         meat_cals=numeric(0),
                         fish_cals=numeric(0),
                         fruit_veg_cals=numeric(0),
                         nuts_cals=numeric(0),
                         non_alc_bev_cals=numeric(0),
                         alc_bev_cals=numeric(0),
                         misc_cals=numeric(0))

#calc_foods <- data.frame(calc_cereal=numeric(0),
                              #calc_dairy=numeric(0),
                              #calc_meat=numeric(0),
                              #calc_fish=numeric(0),
                              #calc_fruit_veg=numeric(0),
                              #calc_nuts=numeric(0),
                              #calc_non_alc_bev=numeric(0),
                              #calc_alc_bev=numeric(0),
                              #calc_misc=numeric(0))

foods_coefs <- data.frame(cereal=numeric(0),
                    dairy=numeric(0),
                    meat=numeric(0),
                    fish=numeric(0),
                    fruit_veg=numeric(0),
                    nuts=numeric(0),
                    non_alc_bev=numeric(0),
                    alc_bev=numeric(0),
                    misc=numeric(0))

calculated <- data.frame(calc_1=numeric(0),
                         calc_2=numeric(0),
                         calc_3=numeric(0),
                         calc_4a=numeric(0),
                         calc_5a=numeric(0),
                         calc_6a=numeric(0),
                         calc_4b=numeric(0),
                         calc_5b=numeric(0),
                         calc_6b=numeric(0))

estimand1 <- data.frame(mod1_1_coef=numeric(0),
                        mod1_2_coef=numeric(0))

estimand2 <- data.frame(mod2_1_coef=numeric(0),
                        mod2_2_coef=numeric(0))

estimand3 <- data.frame(mod3_1_coef=numeric(0),
                        mod3_2_coef=numeric(0),
                        mod3_3_coef=numeric(0))

estimand4A <- data.frame(mod4a_1_coef=numeric(0),
                         mod4a_2_coef=numeric(0))

estimand5A <- data.frame(mod5a_1_coef=numeric(0),
                         mod5a_2_coef=numeric(0))

estimand6A <- data.frame(mod6a_1_coef=numeric(0),
                         mod6a_2_coef=numeric(0),
                         mod6a_3_coef=numeric(0))

estimand4B <- data.frame(mod4b_1_coef=numeric(0),
                         mod4b_2_coef=numeric(0),
                         mod4b_3_coef=numeric(0))

estimand5B <- data.frame(mod5b_1_coef=numeric(0),
                         mod5b_2_coef=numeric(0),
                         mod5b_3_coef=numeric(0))

estimand6B <- data.frame(mod6b_1_coef=numeric(0),
                         mod6b_2_coef=numeric(0),
                         mod6b_4_coef=numeric(0),
                         mod6b_3_coef=numeric(0))




# target macronutrient effects:

effect_pro        <- 1.8 
effect_sat_fat    <- 3.4
effect_unsat_fat  <- 2.8
effect_carbs      <- 3.9
effect_sugars     <- 5.0
effect_fiber      <- 0.7
effect_alcohol    <- 4.5

# define data structure for simulation
DAG <- dagitty("dag {
                
cereal	->	cereal_pro	[beta=0.8]
cereal	->	cereal_sat_fat	[beta=0.8]
cereal	->	cereal_unsat_fat	[beta=0.8]
cereal	->	cereal_carbs	[beta=0.8]
cereal	->	cereal_sugars	[beta=0.8]
cereal	->	cereal_fiber	[beta=0.8]
cereal	->	cereal_water	[beta=0.8]

dairy	->	dairy_pro	[beta=0.8]
dairy	->	dairy_sat_fat	[beta=0.8]
dairy	->	dairy_unsat_fat	[beta=0.8]
dairy	->	dairy_carbs	[beta=0.8]
dairy	->	dairy_sugars	[beta=0.8]
dairy	->	dairy_fiber	[beta=0.8]
dairy	->	dairy_water	[beta=0.8]

meat	->	meat_pro	[beta=0.8]
meat	->	meat_sat_fat	[beta=0.8]
meat	->	meat_unsat_fat	[beta=0.8]
meat	->	meat_carbs	[beta=0.8]
meat	->	meat_sugars	[beta=0.8]
meat	->	meat_fiber	[beta=0.8]
meat	->	meat_water	[beta=0.8]

fish	->	fish_pro	[beta=0.8]
fish	->	fish_sat_fat	[beta=0.8]
fish	->	fish_unsat_fat	[beta=0.8]
fish	->	fish_carbs	[beta=0.8]
fish	->	fish_fiber	[beta=0.8]
fish	->	fish_water	[beta=0.8]

fruit_veg	->	fruit_veg_pro	[beta=0.8]
fruit_veg	->	fruit_veg_sat_fat	[beta=0.8]
fruit_veg	->	fruit_veg_unsat_fat	[beta=0.8]
fruit_veg	->	fruit_veg_carbs	[beta=0.8]
fruit_veg	->	fruit_veg_sugars	[beta=0.8]
fruit_veg	->	fruit_veg_fiber	[beta=0.8]
fruit_veg	->	fruit_veg_water	[beta=0.8]

nuts	->	nuts_pro	[beta=0.8]
nuts	->	nuts_sat_fat	[beta=0.8]
nuts	->	nuts_unsat_fat	[beta=0.8]
nuts	->	nuts_water	[beta=0.8]

alc_bev	->	alc_bev_carbs	[beta=0.8]
alc_bev	->	alc_bev_sugars	[beta=0.8]
alc_bev	->	alc_bev_alcohol	[beta=0.8]
alc_bev	->	alc_bev_water	[beta=0.8]

non_alc_bev	->	non_alc_bev_pro	[beta=0.8]
non_alc_bev	->	non_alc_bev_carbs	[beta=0.8]
non_alc_bev	->	non_alc_bev_sugars	[beta=0.8]
non_alc_bev	->	non_alc_bev_water	[beta=0.8]

misc	->	misc_pro	[beta=0.8]
misc	->	misc_sat_fat	[beta=0.8]
misc	->	misc_unsat_fat	[beta=0.8]
misc	->	misc_carbs	[beta=0.8]
misc	->	misc_sugars	[beta=0.8]
misc	->	misc_fiber	[beta=0.8]
misc	->	misc_water	[beta=0.8]

cereal_pro	->	fpg	[beta=0.036201]
cereal_sat_fat	->	fpg	[beta=0.05593]
cereal_unsat_fat	->	fpg	[beta=0.102491]
cereal_carbs	->	fpg	[beta=0.329108]
cereal_sugars	->	fpg	[beta=0.122434]
cereal_fiber	->	fpg	[beta=0.003523]
			
dairy_pro	->	fpg	[beta=0.031481]
dairy_sat_fat	->	fpg	[beta=0.059575]
dairy_unsat_fat	->	fpg	[beta=0.079386]
dairy_carbs	->	fpg	[beta=0.114168]
dairy_sugars	->	fpg	[beta=0.063206]
dairy_fiber	->	fpg	[beta=0.000587]
			
meat_pro	->	fpg	[beta=0.045164]
meat_sat_fat	->	fpg	[beta=0.060671]
meat_unsat_fat	->	fpg	[beta=0.108322]
meat_carbs	->	fpg	[beta=0.123753]
meat_sugars	->	fpg	[beta=0.052082]
meat_fiber	->	fpg	[beta=0.001977]
			
fish_pro	->	fpg	[beta=0.020496]
fish_sat_fat	->	fpg	[beta=0.021998]
fish_unsat_fat	->	fpg	[beta=0.049483]
fish_carbs	->	fpg	[beta=0.051094]
fish_fiber	->	fpg	[beta=0.000842]
			
fruit_veg_pro	->	fpg	[beta=0.022856]
fruit_veg_sat_fat	->	fpg	[beta=0.033014]
fruit_veg_unsat_fat	->	fpg	[beta=0.082242]
fruit_veg_carbs	->	fpg	[beta=0.219149]
fruit_veg_sugars	->	fpg	[beta=0.044944]
fruit_veg_fiber	->	fpg	[beta=0.003412]
			
nuts_pro	->	fpg	[beta=0.007917]
nuts_sat_fat	->	fpg	[beta=0.018299]
nuts_unsat_fat	->	fpg	[beta=0.043752]
			
alc_bev_carbs	->	fpg	[beta=0.086644]
alc_bev_sugars	->	fpg	[beta=0.075697]
alc_bev_alcohol	->	fpg	[beta=0.09]
			
non_alc_bev_pro	->	fpg	[beta=0.00782]
non_alc_bev_carbs	->	fpg	[beta=0.13208]
non_alc_bev_sugars	->	fpg	[beta=0.119447]
			
misc_pro	->	fpg	[beta=0.017546]
misc_sat_fat	->	fpg	[beta=0.052103]
misc_unsat_fat	->	fpg	[beta=0.102515]
misc_carbs	->	fpg	[beta=0.170949]
misc_sugars	->	fpg	[beta=0.137058]
misc_fiber	->	fpg	[beta=0.001532]
			
cereal_water	->	fpg	[beta=0]
dairy_water	->	fpg	[beta=0]
meat_water	->	fpg	[beta=0]
fish_water	->	fpg	[beta=0]
fruit_veg_water	->	fpg	[beta=0]
nuts_water	->	fpg	[beta=0]
alc_bev_water	->	fpg	[beta=0]
non_alc_bev_water	->	fpg	[beta=0]
misc_water	->	fpg	[beta=0]

               }") 

 # simulate 1000 observations 100,000 times
 n_obs=1000
 n_sims=100000
 
 # create simulation progress bar
 pb <- progress_bar$new(total = n_sims, format = ":bar :percent eta: :eta")
 
 # run simulation
 for (i in 1:n_sims) {
  
  sim_data  <- simulateSEM(DAG, N=n_obs)
  
  # rescale variables to match (proportions of) macronutrient mean and SD values as observed in the NDNS 

  sim_data$cereal_pro	<-	sim_data$cereal_pro	*	50.279544	+	68.79
  sim_data$cereal_sat_fat	<-	sim_data$cereal_sat_fat	*	41.125175	+	41.76
  sim_data$cereal_unsat_fat	<-	sim_data$cereal_unsat_fat	*	91.509562	+	83.74
  sim_data$cereal_carbs	<-	sim_data$cereal_carbs	*	210.966401	+	403.69
  sim_data$cereal_sugars	<-	sim_data$cereal_sugars	*	61.216828	+	59.96
  sim_data$cereal_fiber	<-	sim_data$cereal_fiber	*	12.582528	+	19.79
  
  sim_data$dairy_pro	<-	sim_data$dairy_pro	*	43.723392	+	52.02
  sim_data$dairy_sat_fat	<-	sim_data$dairy_sat_fat	*	43.805137	+	47.38
  sim_data$dairy_unsat_fat	<-	sim_data$dairy_unsat_fat	*	70.880181	+	50.24
  sim_data$dairy_carbs	<-	sim_data$dairy_carbs	*	73.184322	+	48.58
  sim_data$dairy_sugars	<-	sim_data$dairy_sugars	*	31.603006	+	15.98
  sim_data$dairy_fiber	<-	sim_data$dairy_fiber	*	2.097618	+	0.55
  
  sim_data$meat_pro	<-	sim_data$meat_pro	*	62.728164	+	107.07
  sim_data$meat_sat_fat	<-	sim_data$meat_sat_fat	*	44.611321	+	49.14
  sim_data$meat_unsat_fat	<-	sim_data$meat_unsat_fat	*	96.716079	+	93.54
  sim_data$meat_carbs	<-	sim_data$meat_carbs	*	79.328872	+	57.08
  sim_data$meat_sugars	<-	sim_data$meat_sugars	*	26.040833	+	10.85
  sim_data$meat_fiber	<-	sim_data$meat_fiber	*	7.059745	+	6.23
  
  sim_data$fish_pro	<-	sim_data$fish_pro	*	28.466428	+	22.05
  sim_data$fish_sat_fat	<-	sim_data$fish_sat_fat	*	16.174981	+	6.46
  sim_data$fish_unsat_fat	<-	sim_data$fish_unsat_fat	*	44.181444	+	19.52
  sim_data$fish_carbs	<-	sim_data$fish_carbs	*	32.752595	+	9.73
  sim_data$fish_fiber	<-	sim_data$fish_fiber	*	3.006659	+	1.13
  
  sim_data$fruit_veg_pro	<-	sim_data$fruit_veg_pro	*	31.744055	+	27.42
  sim_data$fruit_veg_sat_fat	<-	sim_data$fruit_veg_sat_fat	*	24.274987	+	14.55
  sim_data$fruit_veg_unsat_fat	<-	sim_data$fruit_veg_unsat_fat	*	73.430239	+	53.92
  sim_data$fruit_veg_carbs	<-	sim_data$fruit_veg_carbs	*	140.480426	+	179.00
  sim_data$fruit_veg_sugars	<-	sim_data$fruit_veg_sugars	*	22.472205	+	8.08
  sim_data$fruit_veg_fiber	<-	sim_data$fruit_veg_fiber	*	12.185237	+	18.56
  
  sim_data$nuts_pro	<-	sim_data$nuts_pro	*	10.995795	+	3.29
  sim_data$nuts_sat_fat	<-	sim_data$nuts_sat_fat	*	13.454925	+	4.47
  sim_data$nuts_unsat_fat	<-	sim_data$nuts_unsat_fat	*	39.064050	+	15.26
  
  sim_data$alc_bev_carbs	<-	sim_data$alc_bev_carbs	*	55.540931	+	27.98
  sim_data$alc_bev_sugars	<-	sim_data$alc_bev_sugars	*	37.848382	+	22.92
  sim_data$alc_bev_alcohol	<-	sim_data$alc_bev_alcohol	*	50.000000	+	100.00
  
  sim_data$non_alc_bev_pro	<-	sim_data$non_alc_bev_pro	*	10.861284	+	3.21
  sim_data$non_alc_bev_carbs	<-	sim_data$non_alc_bev_carbs	*	84.666729	+	65.02
  sim_data$non_alc_bev_sugars	<-	sim_data$non_alc_bev_sugars	*	59.723320	+	57.07
  
  sim_data$misc_pro	<-	sim_data$misc_pro	*	24.369653	+	16.16
  sim_data$misc_sat_fat	<-	sim_data$misc_sat_fat	*	38.310834	+	36.24
  sim_data$misc_unsat_fat	<-	sim_data$misc_unsat_fat	*	91.531415	+	83.78
  sim_data$misc_carbs	<-	sim_data$misc_carbs	*	109.582982	+	108.92
  sim_data$misc_sugars	<-	sim_data$misc_sugars	*	68.529191	+	75.14
  sim_data$misc_fiber	<-	sim_data$misc_fiber	*	5.469918	+	3.74
  
  sim_data$fpg	<-	sim_data$fpg	*	25	+	80.00

  # create summary energy intake variables and macronutrients
  
  sim_data$protein              <-  sim_data$cereal_pro       + sim_data$dairy_pro       + sim_data$meat_pro       + sim_data$fish_pro        + sim_data$fruit_veg_pro       + sim_data$nuts_pro                                      + sim_data$non_alc_bev_pro    + sim_data$misc_pro
  sim_data$sat_fat              <-  sim_data$cereal_sat_fat   + sim_data$dairy_sat_fat   + sim_data$meat_sat_fat   + sim_data$fish_sat_fat    + sim_data$fruit_veg_sat_fat   + sim_data$nuts_sat_fat                                                                + sim_data$misc_sat_fat
  sim_data$unsat_fat            <-  sim_data$cereal_unsat_fat + sim_data$dairy_unsat_fat + sim_data$meat_unsat_fat + sim_data$fish_unsat_fat  + sim_data$fruit_veg_unsat_fat + sim_data$nuts_unsat_fat                                                              + sim_data$misc_unsat_fat
  sim_data$carbs                <-  sim_data$cereal_carbs     + sim_data$dairy_carbs     + sim_data$meat_carbs     + sim_data$fish_carbs      + sim_data$fruit_veg_carbs                                + sim_data$alc_bev_carbs      + sim_data$non_alc_bev_carbs  + sim_data$misc_carbs
  sim_data$sugars               <-  sim_data$cereal_sugars    + sim_data$dairy_sugars    + sim_data$meat_sugars                               + sim_data$fruit_veg_sugars                               + sim_data$alc_bev_sugars     + sim_data$non_alc_bev_sugars + sim_data$misc_sugars
  sim_data$fiber                <-  sim_data$cereal_fiber     + sim_data$dairy_fiber     + sim_data$meat_fiber     + sim_data$fish_fiber      + sim_data$fruit_veg_fiber                                                                                            + sim_data$misc_fiber
  sim_data$alcohol              <-  sim_data$alc_bev_alcohol
  
  macro_cals_means              <-  colMeans(sim_data[,c("protein", "sat_fat", "unsat_fat", "carbs", "sugars", "fiber", "alcohol")]); macro_cals_means
  macro_cals_sds                <-  colSds(as.matrix(sim_data), cols=c(which(colnames(sim_data)=="protein"), which(colnames(sim_data)=="sat_fat"), which(colnames(sim_data)=="unsat_fat"), which(colnames(sim_data)=="carbs"), which(colnames(sim_data)=="sugars"), which(colnames(sim_data)=="fiber"), which(colnames(sim_data)=="alcohol")))
  names(macro_cals_sds)         <- c("protein", "sat_fat", "unsat_fat", "carbs", "sugars", "fiber", "alcohol"); macro_cals_sds
  
  # check means and variances of summary groups match sum of parts
  
  near(mean(sim_data$protein)    - mean(sim_data$cereal_pro) - mean(sim_data$dairy_pro) - mean(sim_data$meat_pro) - mean(sim_data$fish_pro) - mean(sim_data$fruit_veg_pro) - mean(sim_data$nuts_pro) - mean(sim_data$non_alc_bev_pro) - mean(sim_data$misc_pro), 0)
  near(mean(sim_data$sat_fat)    - mean(sim_data$cereal_sat_fat) - mean(sim_data$dairy_sat_fat) - mean(sim_data$meat_sat_fat) - mean(sim_data$fish_sat_fat) - mean(sim_data$fruit_veg_sat_fat) - mean(sim_data$nuts_sat_fat) - mean(sim_data$misc_sat_fat),0)
  near(mean(sim_data$unsat_fat)  - mean(sim_data$cereal_unsat_fat) - mean(sim_data$dairy_unsat_fat) - mean(sim_data$meat_unsat_fat) - mean(sim_data$fish_unsat_fat) - mean(sim_data$fruit_veg_unsat_fat) - mean(sim_data$nuts_unsat_fat) - mean(sim_data$misc_unsat_fat),0)
  near(mean(sim_data$carbs)      - mean(sim_data$cereal_carbs) - mean(sim_data$dairy_carbs) - mean(sim_data$meat_carbs) - mean(sim_data$fish_carbs) - mean(sim_data$fruit_veg_carbs) - mean(sim_data$alc_bev_carbs) - mean(sim_data$non_alc_bev_carbs) - mean(sim_data$misc_carbs),0)
  near(mean(sim_data$sugars)     - mean(sim_data$cereal_sugars) - mean(sim_data$dairy_sugars) - mean(sim_data$meat_sugars) - mean(sim_data$fruit_veg_sugars) - mean(sim_data$alc_bev_sugars) - mean(sim_data$non_alc_bev_sugars) - mean(sim_data$misc_sugars),0)
  near(mean(sim_data$fiber)      - mean(sim_data$cereal_fiber) - mean(sim_data$dairy_fiber) - mean(sim_data$meat_fiber) - mean(sim_data$fish_fiber) - mean(sim_data$fruit_veg_fiber) - mean(sim_data$misc_fiber),0)
  near(mean(sim_data$alcohol)    - mean(sim_data$alc_bev_alcohol),0)
  
  near(var(sim_data$protein)     - var(sim_data$cereal_pro) - var(sim_data$dairy_pro) - var(sim_data$meat_pro) - var(sim_data$fish_pro) - var(sim_data$fruit_veg_pro) - var(sim_data$nuts_pro) - var(sim_data$non_alc_bev_pro) - var(sim_data$misc_pro),0)
  near(var(sim_data$sat_fat)     - var(sim_data$cereal_sat_fat) - var(sim_data$dairy_sat_fat) - var(sim_data$meat_sat_fat) - var(sim_data$fish_sat_fat) - var(sim_data$fruit_veg_sat_fat) - var(sim_data$nuts_sat_fat) - var(sim_data$misc_sat_fat),0)
  near(var(sim_data$unsat_fat)   - var(sim_data$cereal_unsat_fat) - var(sim_data$dairy_unsat_fat) - var(sim_data$meat_unsat_fat) - var(sim_data$fish_unsat_fat) - var(sim_data$fruit_veg_unsat_fat) - var(sim_data$nuts_unsat_fat) - var(sim_data$misc_unsat_fat),0)
  near(var(sim_data$carbs)       - var(sim_data$cereal_carbs) - var(sim_data$dairy_carbs) - var(sim_data$meat_carbs) - var(sim_data$fish_carbs) - var(sim_data$fruit_veg_carbs) - var(sim_data$alc_bev_carbs) - var(sim_data$non_alc_bev_carbs) - var(sim_data$misc_carbs),0)
  near(var(sim_data$sugars)      - var(sim_data$cereal_sugars) - var(sim_data$dairy_sugars) - var(sim_data$meat_sugars) - var(sim_data$fruit_veg_sugars) - var(sim_data$alc_bev_sugars) - var(sim_data$non_alc_bev_sugars) - var(sim_data$misc_sugars),0)
  near(var(sim_data$fiber)       - var(sim_data$cereal_fiber) - var(sim_data$dairy_fiber) - var(sim_data$meat_fiber) - var(sim_data$fish_fiber) - var(sim_data$fruit_veg_fiber) - var(sim_data$misc_fiber),0)
  near(var(sim_data$alcohol)     - var(sim_data$alc_bev_alcohol),0)
  
  # calculate total energy
  
  sim_data$total_energy            <- sim_data$protein + sim_data$sat_fat + sim_data$unsat_fat + sim_data$carbs + sim_data$sugars +  sim_data$fiber + sim_data$alcohol

  # create dry food group variables (calories)
  
  sim_data$cereal_cals              <- (sim_data$cereal_pro        + sim_data$cereal_sat_fat     + sim_data$cereal_unsat_fat    + sim_data$cereal_carbs     + sim_data$cereal_sugars    + sim_data$cereal_fiber)
  sim_data$dairy_cals               <- (sim_data$dairy_pro         + sim_data$dairy_sat_fat      + sim_data$dairy_unsat_fat     + sim_data$dairy_carbs      + sim_data$dairy_sugars     + sim_data$dairy_fiber)
  sim_data$meat_cals                <- (sim_data$meat_pro          + sim_data$meat_sat_fat       + sim_data$meat_unsat_fat      + sim_data$meat_carbs       + sim_data$meat_sugars      + sim_data$meat_fiber) 
  sim_data$fish_cals                <- (sim_data$fish_pro          + sim_data$fish_sat_fat       + sim_data$fish_unsat_fat      + sim_data$fish_carbs       + sim_data$fish_fiber) 
  sim_data$fruit_veg_cals           <- (sim_data$fruit_veg_pro     + sim_data$fruit_veg_sat_fat  + sim_data$fruit_veg_unsat_fat + sim_data$fruit_veg_carbs  + sim_data$fruit_veg_sugars + sim_data$fruit_veg_fiber) 
  sim_data$nuts_cals                <- (sim_data$nuts_pro          + sim_data$nuts_sat_fat       + sim_data$nuts_unsat_fat)
  sim_data$alc_bev_cals             <- (sim_data$alc_bev_carbs     + sim_data$alc_bev_sugars     + sim_data$alc_bev_alcohol) 
  sim_data$non_alc_bev_cals         <- (sim_data$non_alc_bev_pro   + sim_data$non_alc_bev_carbs  + sim_data$non_alc_bev_sugars) 
  sim_data$misc_cals                <- (sim_data$misc_pro          + sim_data$misc_sat_fat       + sim_data$misc_unsat_fat      + sim_data$misc_carbs       + sim_data$misc_sugars      + sim_data$misc_fiber) 
  
  food_cals_means                   <- colMeans(sim_data[,c("cereal_cals", "dairy_cals", "meat_cals", "fish_cals", "fruit_veg_cals", "nuts_cals", "alc_bev_cals", "non_alc_bev_cals", "misc_cals")]); food_cals_means
  
  # create dry food group variables (grams)
  
  sim_data$cereal_dry              <- (sim_data$cereal_pro/4        + sim_data$cereal_sat_fat/9     + sim_data$cereal_unsat_fat/9    + sim_data$cereal_carbs/4     + sim_data$cereal_sugars/4    + sim_data$cereal_fiber/2)
  sim_data$dairy_dry               <- (sim_data$dairy_pro/4         + sim_data$dairy_sat_fat/9      + sim_data$dairy_unsat_fat/9     + sim_data$dairy_carbs/4      + sim_data$dairy_sugars/4     + sim_data$dairy_fiber/2)
  sim_data$meat_dry                <- (sim_data$meat_pro/4          + sim_data$meat_sat_fat/9       + sim_data$meat_unsat_fat/9      + sim_data$meat_carbs/4       + sim_data$meat_sugars/4      + sim_data$meat_fiber/2) 
  sim_data$fish_dry                <- (sim_data$fish_pro/4          + sim_data$fish_sat_fat/9       + sim_data$fish_unsat_fat/9      + sim_data$fish_carbs/4       + sim_data$fish_fiber) 
  sim_data$fruit_veg_dry           <- (sim_data$fruit_veg_pro/4     + sim_data$fruit_veg_sat_fat/9  + sim_data$fruit_veg_unsat_fat/9 + sim_data$fruit_veg_carbs/4  + sim_data$fruit_veg_sugars/4 + sim_data$fruit_veg_fiber/2) 
  sim_data$nuts_dry                <- (sim_data$nuts_pro/4          + sim_data$nuts_sat_fat/9       + sim_data$nuts_unsat_fat/9)
  sim_data$alc_bev_dry             <- (sim_data$alc_bev_carbs/4     + sim_data$alc_bev_sugars/4     + sim_data$alc_bev_alcohol/7) 
  sim_data$non_alc_bev_dry         <- (sim_data$non_alc_bev_pro/4   + sim_data$non_alc_bev_carbs/4  + sim_data$non_alc_bev_sugars/4) 
  sim_data$misc_dry                <- (sim_data$misc_pro/4          + sim_data$misc_sat_fat/9       + sim_data$misc_unsat_fat/9      + sim_data$misc_carbs/4       + sim_data$misc_sugars/4      + sim_data$misc_fiber/2) 
  
  food_dry_means                    <- colMeans(sim_data[,c("cereal_dry", "dairy_dry", "meat_dry", "fish_dry", "fruit_veg_dry", "nuts_dry", "alc_bev_dry", "non_alc_bev_dry", "misc_dry")]); food_dry_means
  food_dry_sds                      <- colSds(as.matrix(sim_data), cols=c(which(colnames(sim_data)=="cereal_dry"), which(colnames(sim_data)=="dairy_dry"), which(colnames(sim_data)=="meat_dry"), which(colnames(sim_data)=="fish_dry"), which(colnames(sim_data)=="fruit_veg_dry"), which(colnames(sim_data)=="nuts_dry"), which(colnames(sim_data)=="alc_bev_dry"), which(colnames(sim_data)=="non_alc_bev_dry"), which(colnames(sim_data)=="misc_dry")))
  names(food_dry_sds)               <- c("cereal", "dairy", "meat", "fish", "fruit_veg", "nuts", "alc_bev", "nonalc_bev", "misc"); food_dry_sds
  food_dry_cov                      <- food_dry_sds/food_dry_means
  
  sim_data$total_mass_dry           <- sim_data$cereal_dry + sim_data$dairy_dry + sim_data$meat_dry + sim_data$fish_dry + sim_data$fruit_veg_dry + sim_data$nuts_dry + sim_data$alc_bev_dry +  sim_data$non_alc_bev_dry + sim_data$misc_dry

  
  # create wet food group variables (grams)
  
  # start by converting simulated water variables into grams of water:
  # define vector of target means: 
  
  target_mass = c(209.4, 256.1, 113.2, 38.4, 249.0, 17.5, 366.0, 1006.8, 76.2)
  names(target_mass) = c("cereal", "dairy", "meat", "fish", "fruit_veg", "nuts", "alc_bev", "non_alc_bev", "misc"); target_mass
  
  # determine missing mass of water
  water_mass =   target_mass - food_dry_means; water_mass
  water_proportion = water_mass/target_mass
  
  # estimate SDs by assuming the same coefficient of variation as for the dry mass
  water_sds  =   water_mass*food_dry_cov; water_sds

  sim_data$cereal_water        <- sim_data$cereal_water*water_sds["cereal"]+water_mass["cereal"]
  sim_data$dairy_water         <- sim_data$dairy_water*water_sds["dairy"]+water_mass["dairy"]
  sim_data$meat_water          <- sim_data$meat_water*water_sds["meat"]+water_mass["meat"] 
  sim_data$fish_water          <- sim_data$fish_water*water_sds["fish"]+water_mass["fish"] 
  sim_data$fruit_veg_water     <- sim_data$fruit_veg_water*water_sds["fruit_veg"]+water_mass["fruit_veg"] 
  sim_data$nuts_water          <- sim_data$nuts_water*water_sds["nuts"]+water_mass["nuts"]
  sim_data$alc_bev_water       <- sim_data$alc_bev_water*water_sds["alc_bev"]+water_mass["alc_bev"] 
  sim_data$non_alc_bev_water   <- sim_data$non_alc_bev_water*water_sds["non_alc_bev"]+water_mass["non_alc_bev"] 
  sim_data$misc_water          <- sim_data$misc_water*water_sds["misc"]+water_mass["misc"]
  sim_data$water               <- sim_data$cereal_water + sim_data$dairy_water + sim_data$meat_water + sim_data$fish_water + sim_data$fruit_veg_water + sim_data$nuts_water + sim_data$alc_bev_water + sim_data$non_alc_bev_water + sim_data$misc_water
  
  food_water_means              <- colMeans(sim_data[,c("cereal_water", "dairy_water", "meat_water", "fish_water", "fruit_veg_water", "nuts_water", "alc_bev_water", "non_alc_bev_water", "misc_water")]); food_water_means
  
  
  # now add dry food to water content:
  
  sim_data$cereal              <- sim_data$cereal_dry      + sim_data$cereal_water
  sim_data$dairy               <- sim_data$dairy_dry       + sim_data$dairy_water
  sim_data$meat                <- sim_data$meat_dry        + sim_data$meat_water 
  sim_data$fish                <- sim_data$fish_dry        + sim_data$fish_water 
  sim_data$fruit_veg           <- sim_data$fruit_veg_dry   + sim_data$fruit_veg_water
  sim_data$nuts                <- sim_data$nuts_dry        + sim_data$nuts_water
  sim_data$alc_bev             <- sim_data$alc_bev_dry     + sim_data$alc_bev_water 
  sim_data$non_alc_bev         <- sim_data$non_alc_bev_dry + sim_data$non_alc_bev_water
  sim_data$misc                <- sim_data$misc_dry        + sim_data$misc_water
  
  food_means                   <- colMeans(sim_data[,c("cereal", "dairy", "meat", "fish", "fruit_veg", "nuts", "alc_bev", "non_alc_bev", "misc")]); food_means
  food_sds                     <- colSds(as.matrix(sim_data), cols=c(which(colnames(sim_data)=="cereal"), which(colnames(sim_data)=="dairy"), which(colnames(sim_data)=="meat"), which(colnames(sim_data)=="fish"), which(colnames(sim_data)=="fruit_veg"), which(colnames(sim_data)=="nuts"), which(colnames(sim_data)=="alc_bev"), which(colnames(sim_data)=="non_alc_bev"), which(colnames(sim_data)=="misc")))
  names(food_sds)              <- c("cereal", "dairy", "meat", "fish", "fruit_veg", "nuts", "alc_bev", "nonalc_bev", "misc"); food_sds

  sim_data$total_mass          <- sim_data$cereal + sim_data$dairy + sim_data$meat + sim_data$fish + sim_data$fruit_veg + sim_data$nuts + sim_data$alc_bev +  sim_data$non_alc_bev + sim_data$misc

  
  #### Models with Macronutrient variables ####
  
  # first, determine  individual total causal effects of each nutrient (in cals and grams) using the all-components model
  nutrients   <- lm(fpg ~ sugars + carbs + protein + sat_fat + unsat_fat + fiber + alcohol, data=sim_data)
  
  sugars    <- nutrients$coefficients[2]*100; sugars
  carbs     <- nutrients$coefficients[3]*100; carbs
  protein   <- nutrients$coefficients[4]*100; protein
  sat_fat   <- nutrients$coefficients[5]*100; sat_fat
  unsat_fat <- nutrients$coefficients[6]*100; unsat_fat
  fiber     <- nutrients$coefficients[7]*100; fiber
  alcohol   <- nutrients$coefficients[8]*100; alcohol
  
  ### 1. the relative causal effect of SUGAR instead of PROTEIN ###
  
  # Calculate weighted average directly:
  calc_1 = effect_sugars - effect_pro; calc_1
  
  # 1.1. all-components model / energy partition model
  mod1_1       <- lm(fpg ~ sugars + protein + carbs + sat_fat + unsat_fat + fiber + alcohol, data=sim_data)
  mod1_1_coef  <- mod1_1$coefficients[2]*100 - mod1_1$coefficients[3]*100; mod1_1_coef
  
  # 1.2. the leave-one-out model 
  mod1_2       <- lm(fpg ~ sugars + total_energy + carbs + unsat_fat + sat_fat + fiber + alcohol, data=sim_data)
  mod1_2_coef  <- mod1_2$coefficients[2]*100; mod1_2_coef
  
  ### 2. the (naive) relative causal effects of SUGAR instead of PROTEIN, FAT, and FIBER  ###
  
  # Calculate weighted average directly: 
  
  wi_2 <-      c( mean(sim_data$protein)   / mean(sim_data$protein + sim_data$sat_fat + sim_data$unsat_fat + sim_data$fiber) ,
                  mean(sim_data$sat_fat)   / mean(sim_data$protein + sim_data$sat_fat + sim_data$unsat_fat + sim_data$fiber) ,
                  mean(sim_data$unsat_fat) / mean(sim_data$protein + sim_data$sat_fat + sim_data$unsat_fat + sim_data$fiber) ,
                  mean(sim_data$fiber)     / mean(sim_data$protein + sim_data$sat_fat + sim_data$unsat_fat + sim_data$fiber) )
  
  names(wi_2) <- c("protein", "sat_fat", "unsat_fat", "fiber"); wi_2
  
  
  calc_2 = effect_sugars - (wi_2[1]*effect_pro + 
                                  wi_2[2]*effect_sat_fat + 
                                  wi_2[3]*effect_unsat_fat + 
                                  wi_2[4]*effect_fiber); calc_2
  
  # 2.1. the inadvertent substitution model 
  mod2_1       <- lm(fpg ~ sugars + total_energy + carbs + alcohol, data=sim_data)
  mod2_1_coef  <- mod2_1$coefficients[2]*100; mod2_1_coef # this effect is confounded by the absence of the other dietary components and biased because the comparison is a composite variable
  
  # 2.2. all-components model
  
  mod2_2_coef  <- mod1_1$coefficients[2]*100-
                  wi_2[1]*mod1_1$coefficients[3]*100 -
                  wi_2[2]*mod1_1$coefficients[5]*100 -
                  wi_2[3]*mod1_1$coefficients[6]*100 -
                  wi_2[4]*mod1_1$coefficients[7]*100; mod2_2_coef
  
  # 2.3 there is no analogous  version of the energy partition model
  
  ### 3. the average relative causal effect of sugars instead of everything else ###
  
 # Calculate weighted average directly:
  
  wi_3      <- c( mean(sim_data$protein)   / mean(sim_data$protein + sim_data$carbs + sim_data$sat_fat + sim_data$unsat_fat + sim_data$fiber + sim_data$alcohol) ,
                  mean(sim_data$carbs)     / mean(sim_data$protein + sim_data$carbs + sim_data$sat_fat + sim_data$unsat_fat + sim_data$fiber + sim_data$alcohol) ,
                  mean(sim_data$sat_fat)   / mean(sim_data$protein + sim_data$carbs + sim_data$sat_fat + sim_data$unsat_fat + sim_data$fiber + sim_data$alcohol) ,
                  mean(sim_data$unsat_fat) / mean(sim_data$protein + sim_data$carbs + sim_data$sat_fat + sim_data$unsat_fat + sim_data$fiber + sim_data$alcohol) ,
                  mean(sim_data$fiber)     / mean(sim_data$protein + sim_data$carbs + sim_data$sat_fat + sim_data$unsat_fat + sim_data$fiber + sim_data$alcohol) ,
                  mean(sim_data$alcohol)   / mean(sim_data$protein + sim_data$carbs + sim_data$sat_fat + sim_data$unsat_fat + sim_data$fiber + sim_data$alcohol) )
  
  names(wi_3) <- c("protein", "carbs", "sat_fat", "unsat_fat", "fiber", "alcohol"); wi_3
  
  calc_3  <-  effect_sugars - (wi_3[1]*effect_pro + 
                                     wi_3[2]*effect_carbs + 
                                     wi_3[3]*effect_sat_fat + 
                                     wi_3[4]*effect_unsat_fat +
                                     wi_3[5]*effect_fiber + 
                                     wi_3[6]*effect_alcohol); calc_3
  
  # 3.1. All-components model
  
  mod3_1_coef  <- mod1_1$coefficients[2]*100-
                  wi_3[1]*mod1_1$coefficients[3]*100-
                  wi_3[2]*mod1_1$coefficients[4]*100-
                  wi_3[3]*mod1_1$coefficients[5]*100-
                  wi_3[4]*mod1_1$coefficients[6]*100-
                  wi_3[5]*mod1_1$coefficients[7]*100-
                  wi_3[6]*mod1_1$coefficients[8]*100; mod3_1_coef
  
  # 3.2. the leave-'one'-out model 
  mod3_2       <- lm(fpg ~ sugars + total_energy, data=sim_data)
  mod3_2_coef  <- mod3_2$coefficients[2]*100; mod3_2_coef 
  
  # 3.3. the simple energy partition model 
  sim_data$remaining_sugar <- sim_data$total_energy - sim_data$sugars
  
  mod3_3       <- lm(fpg ~ sugars + remaining_sugar, data=sim_data)
  mod3_3_coef  <- mod3_3$coefficients[2]*100 - mod3_3$coefficients[3]*100; mod3_3_coef 
  
  
  #### Models with Food Group variables (in calories) ####
  
  # First, directly calculate total causal effects of each food group (in cals) using weighted average function:
  
  calc_cereal_cals <- 
  (  (mean(sim_data$cereal_pro)/mean(sim_data$cereal_cals))*effect_pro + 
     (mean(sim_data$cereal_sat_fat)/mean(sim_data$cereal_cals))*effect_sat_fat + 
     (mean(sim_data$cereal_unsat_fat)/mean(sim_data$cereal_cals))*effect_unsat_fat +
     (mean(sim_data$cereal_carbs)/mean(sim_data$cereal_cals))*effect_carbs +
     (mean(sim_data$cereal_sugars)/mean(sim_data$cereal_cals))*effect_sugars +
     (mean(sim_data$cereal_fiber)/mean(sim_data$cereal_cals))*effect_fiber ); calc_cereal_cals
       
  calc_dairy_cals <- 
    (  (mean(sim_data$dairy_pro)/mean(sim_data$dairy_cals))*effect_pro + 
         (mean(sim_data$dairy_sat_fat)/mean(sim_data$dairy_cals))*effect_sat_fat + 
         (mean(sim_data$dairy_unsat_fat)/mean(sim_data$dairy_cals))*effect_unsat_fat +
         (mean(sim_data$dairy_carbs)/mean(sim_data$dairy_cals))*effect_carbs +
         (mean(sim_data$dairy_sugars)/mean(sim_data$dairy_cals))*effect_sugars +
         (mean(sim_data$dairy_fiber)/mean(sim_data$dairy_cals))*effect_fiber ); calc_dairy_cals

  calc_meat_cals <- 
    (  (mean(sim_data$meat_pro)/mean(sim_data$meat_cals))*effect_pro + 
         (mean(sim_data$meat_sat_fat)/mean(sim_data$meat_cals))*effect_sat_fat + 
         (mean(sim_data$meat_unsat_fat)/mean(sim_data$meat_cals))*effect_unsat_fat +
         (mean(sim_data$meat_carbs)/mean(sim_data$meat_cals))*effect_carbs +
         (mean(sim_data$meat_sugars)/mean(sim_data$meat_cals))*effect_sugars +
         (mean(sim_data$meat_fiber)/mean(sim_data$meat_cals))*effect_fiber ); calc_meat_cals
  
  calc_fish_cals <- 
    (  (mean(sim_data$fish_pro)/mean(sim_data$fish_cals))*effect_pro + 
         (mean(sim_data$fish_sat_fat)/mean(sim_data$fish_cals))*effect_sat_fat + 
         (mean(sim_data$fish_unsat_fat)/mean(sim_data$fish_cals))*effect_unsat_fat +
         (mean(sim_data$fish_carbs)/mean(sim_data$fish_cals))*effect_carbs +
         (mean(sim_data$fish_fiber)/mean(sim_data$fish_cals))*effect_fiber ); calc_fish_cals
  
  calc_fruit_veg_cals <- 
    (  (mean(sim_data$fruit_veg_pro)/mean(sim_data$fruit_veg_cals))*effect_pro + 
         (mean(sim_data$fruit_veg_sat_fat)/mean(sim_data$fruit_veg_cals))*effect_sat_fat + 
         (mean(sim_data$fruit_veg_unsat_fat)/mean(sim_data$fruit_veg_cals))*effect_unsat_fat +
         (mean(sim_data$fruit_veg_carbs)/mean(sim_data$fruit_veg_cals))*effect_carbs +
         (mean(sim_data$fruit_veg_sugars)/mean(sim_data$fruit_veg_cals))*effect_sugars +
         (mean(sim_data$fruit_veg_fiber)/mean(sim_data$fruit_veg_cals))*effect_fiber ); calc_fruit_veg_cals
  
  calc_nuts_cals <- 
    (  (mean(sim_data$nuts_pro)/mean(sim_data$nuts_cals))*effect_pro + 
         (mean(sim_data$nuts_sat_fat)/mean(sim_data$nuts_cals))*effect_sat_fat + 
         (mean(sim_data$nuts_unsat_fat)/mean(sim_data$nuts_cals))*effect_unsat_fat ); calc_nuts_cals
  
  calc_alc_bev_cals <- 
    (  (mean(sim_data$alc_bev_carbs)/mean(sim_data$alc_bev_cals))*effect_carbs +
         (mean(sim_data$alc_bev_sugars)/mean(sim_data$alc_bev_cals))*effect_sugars +
         (mean(sim_data$alc_bev_alcohol)/mean(sim_data$alc_bev_cals))*effect_alcohol ); calc_alc_bev_cals
  
  calc_non_alc_bev_cals <- 
    (  (mean(sim_data$non_alc_bev_pro)/mean(sim_data$non_alc_bev_cals))*effect_pro + 
         (mean(sim_data$non_alc_bev_carbs)/mean(sim_data$non_alc_bev_cals))*effect_carbs +
         (mean(sim_data$non_alc_bev_sugars)/mean(sim_data$non_alc_bev_cals))*effect_sugars ); calc_non_alc_bev_cals
  
  calc_misc_cals <- 
    (  (mean(sim_data$misc_pro)/mean(sim_data$misc_cals))*effect_pro + 
         (mean(sim_data$misc_sat_fat)/mean(sim_data$misc_cals))*effect_sat_fat + 
         (mean(sim_data$misc_unsat_fat)/mean(sim_data$misc_cals))*effect_unsat_fat +
         (mean(sim_data$misc_carbs)/mean(sim_data$misc_cals))*effect_carbs +
         (mean(sim_data$misc_sugars)/mean(sim_data$misc_cals))*effect_sugars +
         (mean(sim_data$misc_fiber)/mean(sim_data$misc_cals))*effect_fiber ); calc_misc_cals
  
  # Next, estimate  total causal effects of each food group (in calories) using all-components models:
  
  foods_cals   <- lm(fpg ~ cereal_cals + dairy_cals + meat_cals + fish_cals + fruit_veg_cals + nuts_cals + alc_bev_cals + non_alc_bev_cals + misc_cals, data=sim_data)
  
  cereal_cals       <- foods_cals$coefficients[2]*100; cereal_cals
  dairy_cals        <- foods_cals$coefficients[3]*100; dairy_cals
  meat_cals         <- foods_cals$coefficients[4]*100; meat_cals
  fish_cals         <- foods_cals$coefficients[5]*100; fish_cals
  fruit_veg_cals    <- foods_cals$coefficients[6]*100; fruit_veg_cals
  nuts_cals         <- foods_cals$coefficients[7]*100; nuts_cals
  alc_bev_cals      <- foods_cals$coefficients[8]*100; alc_bev_cals
  non_alc_bev_cals  <- foods_cals$coefficients[9]*100; non_alc_bev_cals
  misc_cals         <- foods_cals$coefficients[10]*100; misc_cals
  

   ### Now run substitution models ### 
  
  ### 4A. the relative causal effect of MEAT instead of FISH - IN CALORIES
  
  # Calculated directly:
  calc_4a <-  calc_meat_cals - calc_fish_cals; calc_4a
  
  # 4A.1. all-components model / energy partition model (ALSO USED BY 5A.2 and 6A.1)
  mod4a_1      <- lm(fpg ~ cereal_cals + dairy_cals + meat_cals + fish_cals + fruit_veg_cals + nuts_cals + alc_bev_cals + non_alc_bev_cals + misc_cals, data=sim_data)
  mod4a_1_coef  <- mod4a_1$coefficients[4]*100 - mod4a_1$coefficients[5]*100; mod4a_1_coef
  
  # 4A.2. the leave-one-out model 
  mod4a_2       <- lm(fpg ~ cereal_cals + dairy_cals + meat_cals + fruit_veg_cals + nuts_cals + alc_bev_cals + non_alc_bev_cals + misc_cals + total_energy, data=sim_data)
  mod4a_2_coef  <- mod4a_2$coefficients[4]*100; mod4a_2_coef
  
  
  ### 5. The (naive) relative causal effect of MEAT instead of CEREALS, DAIRY, FISH, NUTS, MISC  [i.e. NOT fruit_Veg, alc_bev, non_alc_bevs] - IN CALORIES
  
  # Calculated directly:
  
  wi_5a     <- c( mean(sim_data$cereal_cals)/ mean(sim_data$cereal_cals + sim_data$dairy_cals + sim_data$fish_cals + sim_data$nuts_cals + sim_data$misc_cals) ,
                  mean(sim_data$dairy_cals) / mean(sim_data$cereal_cals + sim_data$dairy_cals + sim_data$fish_cals + sim_data$nuts_cals + sim_data$misc_cals) ,
                  mean(sim_data$fish_cals)  / mean(sim_data$cereal_cals + sim_data$dairy_cals + sim_data$fish_cals + sim_data$nuts_cals + sim_data$misc_cals) ,
                  mean(sim_data$nuts_cals)  / mean(sim_data$cereal_cals + sim_data$dairy_cals + sim_data$fish_cals + sim_data$nuts_cals + sim_data$misc_cals) ,
                  mean(sim_data$misc_cals)  / mean(sim_data$cereal_cals + sim_data$dairy_cals + sim_data$fish_cals + sim_data$nuts_cals + sim_data$misc_cals) )
  
  names(wi_5a) <- c("cereals", "dairy_cals", "fish_cals", "nuts_cals", "misc_cals"); wi_5a
  
  calc_5a      <-  calc_meat_cals - 
                         wi_5a[1]*calc_cereal_cals -
                         wi_5a[2]*calc_dairy_cals - 
                         wi_5a[3]*calc_fish_cals -
                         wi_5a[4]*calc_nuts_cals - 
                         wi_5a[5]*calc_misc_cals; calc_5a
  
  # 5A.1 the inadvertent substitution model
  mod5a_1       <- lm(fpg ~ meat_cals + fruit_veg_cals + alc_bev_cals + non_alc_bev_cals + total_energy, data=sim_data)
  mod5a_1_coef  <- mod5a_1$coefficients[2]*100; mod5a_1_coef 
  
  # 5A.2. all-components model
  
  mod5a_2_coef <- mod4a_1$coefficients[4]*100 -
                 wi_5a[1]*mod4a_1$coefficients[2]*100 -
                 wi_5a[2]*mod4a_1$coefficients[3]*100 -
                 wi_5a[3]*mod4a_1$coefficients[5]*100 -
                 wi_5a[4]*mod4a_1$coefficients[7]*100 -
                 wi_5a[5]*mod4a_1$coefficients[10]*100; mod5a_2_coef
  
  # 5A.3 there is no equivalent energy partition model
  
  ### 6A. The average relative causal effect of MEAT instead of all other energy sources - IN CALORIES
  
  # Calculated directly:
  
  wi_6a     <- c( mean(sim_data$cereal_cals)      / mean(sim_data$cereal_cals + sim_data$dairy_cals + sim_data$fish_cals + sim_data$fruit_veg_cals + sim_data$nuts_cals + sim_data$alc_bev_cals + sim_data$non_alc_bev_cals + sim_data$misc_cals) ,
                  mean(sim_data$dairy_cals)       / mean(sim_data$cereal_cals + sim_data$dairy_cals + sim_data$fish_cals + sim_data$fruit_veg_cals + sim_data$nuts_cals + sim_data$alc_bev_cals + sim_data$non_alc_bev_cals + sim_data$misc_cals) ,
                  mean(sim_data$fish_cals)        / mean(sim_data$cereal_cals + sim_data$dairy_cals + sim_data$fish_cals + sim_data$fruit_veg_cals + sim_data$nuts_cals + sim_data$alc_bev_cals + sim_data$non_alc_bev_cals + sim_data$misc_cals) ,
                  mean(sim_data$fruit_veg_cals)   / mean(sim_data$cereal_cals + sim_data$dairy_cals + sim_data$fish_cals + sim_data$fruit_veg_cals + sim_data$nuts_cals + sim_data$alc_bev_cals + sim_data$non_alc_bev_cals + sim_data$misc_cals) ,
                  mean(sim_data$nuts_cals)        / mean(sim_data$cereal_cals + sim_data$dairy_cals + sim_data$fish_cals + sim_data$fruit_veg_cals + sim_data$nuts_cals + sim_data$alc_bev_cals + sim_data$non_alc_bev_cals + sim_data$misc_cals) ,
                  mean(sim_data$alc_bev_cals)     / mean(sim_data$cereal_cals + sim_data$dairy_cals + sim_data$fish_cals + sim_data$fruit_veg_cals + sim_data$nuts_cals + sim_data$alc_bev_cals + sim_data$non_alc_bev_cals + sim_data$misc_cals) ,
                  mean(sim_data$non_alc_bev_cals) / mean(sim_data$cereal_cals + sim_data$dairy_cals + sim_data$fish_cals + sim_data$fruit_veg_cals + sim_data$nuts_cals + sim_data$alc_bev_cals + sim_data$non_alc_bev_cals + sim_data$misc_cals) ,
                  mean(sim_data$misc_cals)        / mean(sim_data$cereal_cals + sim_data$dairy_cals + sim_data$fish_cals + sim_data$fruit_veg_cals + sim_data$nuts_cals + sim_data$alc_bev_cals + sim_data$non_alc_bev_cals + sim_data$misc_cals) )
  
  names(wi_6a) <- c("cereal_cals", "dairy_cals", "fish_cals", "fruit_veg_cals", "nuts_cals", "alc_bev_cals", "non_alc_bev_cals", "misc_cals"); wi_6a
  
  calc_6a      <-  calc_meat_cals - 
                         wi_6a[1]*calc_cereal_cals -
                         wi_6a[2]*calc_dairy_cals - 
                         wi_6a[3]*calc_fish_cals -
                         wi_6a[4]*calc_fruit_veg_cals -
                         wi_6a[5]*calc_nuts_cals - 
                         wi_6a[6]*calc_alc_bev_cals -
                         wi_6a[7]*calc_non_alc_bev_cals -
                         wi_6a[8]*calc_misc_cals; calc_6a
  
  # 6A.1. All-components model
  
  mod6a_1_coef  <- mod4a_1$coefficients[4]*100 -
                  wi_6a[1]*mod4a_1$coefficients[2]*100 -
                  wi_6a[2]*mod4a_1$coefficients[3]*100 -
                  wi_6a[3]*mod4a_1$coefficients[5]*100 -
                  wi_6a[4]*mod4a_1$coefficients[6]*100 -
                  wi_6a[5]*mod4a_1$coefficients[7]*100 -
                  wi_6a[6]*mod4a_1$coefficients[8]*100 -
                  wi_6a[7]*mod4a_1$coefficients[9]*100 -
                  wi_6a[8]*mod4a_1$coefficients[10]*100; mod6a_1_coef
  
  # 6A.2. the leave-'one'-out model 
  mod6a_2       <- lm(fpg ~ meat_cals + total_energy, data=sim_data)
  mod6a_2_coef  <- mod6a_2$coefficients[2]*100; mod6a_2_coef #This estimate is confounded by consumption of other unmodelled foods + biased by the 'average' being a composite variable
  
  # 6A.3 Energy partition model
  sim_data$remaining_meat_cals <- sim_data$total_energy - sim_data$meat_cals 
  
  mod6a_3       <- lm(fpg ~ meat_cals + remaining_meat_cals, data=sim_data)
  mod6a_3_coef  <- mod6a_3$coefficients[2]*100 - mod6a_3$coefficients[3]*100; mod6a_3_coef #This estimate is confounded by consumption of other unmodelled foods + biased by the 'average' being a composite variable
  
  
  #### Models with Food Group variables (in grams) ####
  
  # First, directly calculate total causal effects of each food group (in grams) using weighted average function:
  
  calc_cereal <- 
    (  (mean(sim_data$cereal_pro)/mean(sim_data$cereal_cals))*effect_pro*4 + 
         (mean(sim_data$cereal_sat_fat)/mean(sim_data$cereal_cals))*effect_sat_fat*9 + 
         (mean(sim_data$cereal_unsat_fat)/mean(sim_data$cereal_cals))*effect_unsat_fat*9 +
         (mean(sim_data$cereal_carbs)/mean(sim_data$cereal_cals))*effect_carbs*4 +
         (mean(sim_data$cereal_sugars)/mean(sim_data$cereal_cals))*effect_sugars*4 +
         (mean(sim_data$cereal_fiber)/mean(sim_data$cereal_cals))*effect_fiber*2)*(1-water_proportion["cereal"]); calc_cereal
  
  calc_dairy <- 
    (  (mean(sim_data$dairy_pro)/mean(sim_data$dairy_cals))*effect_pro*4 + 
         (mean(sim_data$dairy_sat_fat)/mean(sim_data$dairy_cals))*effect_sat_fat*9 + 
         (mean(sim_data$dairy_unsat_fat)/mean(sim_data$dairy_cals))*effect_unsat_fat*9 +
         (mean(sim_data$dairy_carbs)/mean(sim_data$dairy_cals))*effect_carbs*4 +
         (mean(sim_data$dairy_sugars)/mean(sim_data$dairy_cals))*effect_sugars*4 +
         (mean(sim_data$dairy_fiber)/mean(sim_data$dairy_cals))*effect_fiber*2)*(1-water_proportion["dairy"]); calc_dairy
  
  calc_meat <- 
    (  (mean(sim_data$meat_pro)/mean(sim_data$meat_cals))*effect_pro*4 + 
         (mean(sim_data$meat_sat_fat)/mean(sim_data$meat_cals))*effect_sat_fat*9 + 
         (mean(sim_data$meat_unsat_fat)/mean(sim_data$meat_cals))*effect_unsat_fat*9 +
         (mean(sim_data$meat_carbs)/mean(sim_data$meat_cals))*effect_carbs*4 +
         (mean(sim_data$meat_sugars)/mean(sim_data$meat_cals))*effect_sugars*4 +
         (mean(sim_data$meat_fiber)/mean(sim_data$meat_cals))*effect_fiber*2)*(1-water_proportion["meat"]); calc_meat
  
  calc_fish <- 
    (  (mean(sim_data$fish_pro)/mean(sim_data$fish_cals))*effect_pro*4 + 
         (mean(sim_data$fish_sat_fat)/mean(sim_data$fish_cals))*effect_sat_fat*9 + 
         (mean(sim_data$fish_unsat_fat)/mean(sim_data$fish_cals))*effect_unsat_fat*9 +
         (mean(sim_data$fish_carbs)/mean(sim_data$fish_cals))*effect_carbs*4 +
         (mean(sim_data$fish_fiber)/mean(sim_data$fish_cals))*effect_fiber*2)*(1-water_proportion["fish"]); calc_fish
  
  calc_fruit_veg <- 
    (  (mean(sim_data$fruit_veg_pro)/mean(sim_data$fruit_veg_cals))*effect_pro*4 + 
         (mean(sim_data$fruit_veg_sat_fat)/mean(sim_data$fruit_veg_cals))*effect_sat_fat*9 + 
         (mean(sim_data$fruit_veg_unsat_fat)/mean(sim_data$fruit_veg_cals))*effect_unsat_fat*9 +
         (mean(sim_data$fruit_veg_carbs)/mean(sim_data$fruit_veg_cals))*effect_carbs*4 +
         (mean(sim_data$fruit_veg_sugars)/mean(sim_data$fruit_veg_cals))*effect_sugars*4 +
         (mean(sim_data$fruit_veg_fiber)/mean(sim_data$fruit_veg_cals))*effect_fiber*2)*(1-water_proportion["fruit_veg"]); calc_fruit_veg
  
  calc_nuts <- 
    (  (mean(sim_data$nuts_pro)/mean(sim_data$nuts_cals))*effect_pro*4 + 
         (mean(sim_data$nuts_sat_fat)/mean(sim_data$nuts_cals))*effect_sat_fat*9 + 
         (mean(sim_data$nuts_unsat_fat)/mean(sim_data$nuts_cals))*effect_unsat_fat*9)*(1-water_proportion["nuts"]); calc_nuts
  
  calc_alc_bev <- 
    (  (mean(sim_data$alc_bev_carbs)/mean(sim_data$alc_bev_cals))*effect_carbs*4 +
         (mean(sim_data$alc_bev_sugars)/mean(sim_data$alc_bev_cals))*effect_sugars*4 +
         (mean(sim_data$alc_bev_alcohol)/mean(sim_data$alc_bev_cals))*effect_alcohol*7)*(1-water_proportion["alc_bev"]); calc_alc_bev
  
  calc_non_alc_bev <- 
    (  (mean(sim_data$non_alc_bev_pro)/mean(sim_data$non_alc_bev_cals))*effect_pro*4 + 
         (mean(sim_data$non_alc_bev_carbs)/mean(sim_data$non_alc_bev_cals))*effect_carbs*4 +
         (mean(sim_data$non_alc_bev_sugars)/mean(sim_data$non_alc_bev_cals))*effect_sugars*4)*(1-water_proportion["non_alc_bev"]); calc_non_alc_bev
  
  calc_misc <- 
    (  (mean(sim_data$misc_pro)/mean(sim_data$misc_cals))*effect_pro*4 + 
         (mean(sim_data$misc_sat_fat)/mean(sim_data$misc_cals))*effect_sat_fat*9 + 
         (mean(sim_data$misc_unsat_fat)/mean(sim_data$misc_cals))*effect_unsat_fat*9 +
         (mean(sim_data$misc_carbs)/mean(sim_data$misc_cals))*effect_carbs*4 +
         (mean(sim_data$misc_sugars)/mean(sim_data$misc_cals))*effect_sugars*4 +
         (mean(sim_data$misc_fiber)/mean(sim_data$misc_cals))*effect_fiber*2)*(1-water_proportion["misc"]); calc_misc
  
  # Next, estimate  total causal effects of each food group (in grams) using all-components models:
  
  foods   <- lm(fpg ~ cereal + dairy + meat + fish + fruit_veg + nuts + alc_bev + non_alc_bev + misc, data=sim_data)
  
  cereal       <- foods$coefficients[2]*100; cereal
  dairy        <- foods$coefficients[3]*100; dairy
  meat         <- foods$coefficients[4]*100; meat
  fish         <- foods$coefficients[5]*100; fish
  fruit_veg    <- foods$coefficients[6]*100; fruit_veg
  nuts         <- foods$coefficients[7]*100; nuts
  alc_bev      <- foods$coefficients[8]*100; alc_bev
  non_alc_bev  <- foods$coefficients[9]*100; non_alc_bev
  misc         <- foods$coefficients[10]*100; misc
  
  # Now run the substitution models:
  
  ### 4B. the relative causal effect of MEAT instead of FISH - IN GRAMS
  
  # Calculated directly:
  calc_4b <-  calc_meat - calc_fruit_veg; calc_4b
  
  # 4B.1. all-components model / energy substitution model (ALSO USED BY 5B.3 and 6B.1)
  mod4b_1       <- lm(fpg ~ cereal + dairy + meat + fish + fruit_veg + nuts + alc_bev + non_alc_bev + misc, data=sim_data)
  mod4b_1_coef  <- mod4b_1$coefficients[4]*100 - mod4b_1$coefficients[5]*100; mod4b_1_coef
  
  # 4B.2. the leave-one-out model - adjusting for total MASS
  mod4b_2       <- lm(fpg ~ cereal + dairy + meat + fruit_veg + nuts + alc_bev + non_alc_bev + misc + total_mass, data=sim_data)
  mod4b_2_coef  <- mod4b_2$coefficients[4]*100; mod4b_2_coef
  
  # 4B.3 - the cross-unit model - adjusting for total ENERGY
  mod4b_3       <- lm(fpg ~ cereal + dairy + meat + fruit_veg + nuts + alc_bev + non_alc_bev + misc + total_energy, data=sim_data)
  mod4b_3_coef  <- mod4b_3$coefficients[4]*100; mod4b_3_coef
  
  ### 5B The (naive) relative causal effect of MEAT instead of CEREAL, DAIRY, FISH, NUTS, MISC  [i.e. NOT fruit_Veg, alc_bev, non_alc_bevs] - IN GRAMS
  
  # Calculated directly:
  
  wi_5b     <- c( mean(sim_data$cereal)/ mean(sim_data$cereal + sim_data$dairy + sim_data$fish + sim_data$nuts + sim_data$misc) ,
                  mean(sim_data$dairy) / mean(sim_data$cereal + sim_data$dairy + sim_data$fish + sim_data$nuts + sim_data$misc) ,
                  mean(sim_data$fish)  / mean(sim_data$cereal + sim_data$dairy + sim_data$fish + sim_data$nuts + sim_data$misc) ,
                  mean(sim_data$nuts)  / mean(sim_data$cereal + sim_data$dairy + sim_data$fish + sim_data$nuts + sim_data$misc) ,
                  mean(sim_data$misc)  / mean(sim_data$cereal + sim_data$dairy + sim_data$fish + sim_data$nuts + sim_data$misc) )
  
  names(wi_5b) <- c("cereal", "dairy", "fish", "nuts", "misc"); wi_5b
  
  calc_5b      <-  calc_meat - 
                         wi_5b[1]*calc_cereal -
                         wi_5b[2]*calc_dairy - 
                         wi_5b[3]*calc_fish -
                         wi_5b[4]*calc_nuts - 
                         wi_5b[5]*calc_misc; calc_5b
  
  # 5B.1 the inadvertent substitution model - TOTAL MASS
  mod5b_1       <- lm(fpg ~ meat + fruit_veg + alc_bev + non_alc_bev + total_mass, data=sim_data)
  mod5b_1_coef  <- mod5b_1$coefficients[2]*100; mod5b_1_coef 
  
  # 5B.2 the inadvertent substitution model - TOTAL ENERGY
  mod5b_2       <- lm(fpg ~ meat + fruit_veg + alc_bev + non_alc_bev+ total_energy, data=sim_data)
  mod5b_2_coef  <- mod5b_2$coefficients[2]*100; mod5b_2_coef 
  
  # 5B.3. all-components model 
  
  mod5b_3_coef <- mod4b_1$coefficients[2]*100 -
                  wi_5b[1]*mod4b_1$coefficients[3]*100 -
                  wi_5b[2]*mod4b_1$coefficients[4]*100 -
                  wi_5b[3]*mod4b_1$coefficients[5]*100 -
                  wi_5b[4]*mod4b_1$coefficients[7]*100 -
                  wi_5b[5]*mod4b_1$coefficients[10]*100; mod5b_3_coef
  
  ### 6B. The average relative causal effect of MEAT instead of all other energy sources - IN GRAMS
  
  # Calculated directly:
  
  wi_6b     <- c( mean(sim_data$cereal)      / mean(sim_data$cereal + sim_data$dairy + sim_data$fish + sim_data$fruit_veg + sim_data$nuts + sim_data$alc_bev + sim_data$non_alc_bev + sim_data$misc) ,
                  mean(sim_data$dairy)       / mean(sim_data$cereal + sim_data$dairy + sim_data$fish + sim_data$fruit_veg + sim_data$nuts + sim_data$alc_bev + sim_data$non_alc_bev + sim_data$misc) ,
                  mean(sim_data$fish)        / mean(sim_data$cereal + sim_data$dairy + sim_data$fish + sim_data$fruit_veg + sim_data$nuts + sim_data$alc_bev + sim_data$non_alc_bev + sim_data$misc) ,
                  mean(sim_data$fruit_veg)   / mean(sim_data$cereal + sim_data$dairy + sim_data$fish + sim_data$fruit_veg + sim_data$nuts + sim_data$alc_bev + sim_data$non_alc_bev + sim_data$misc) ,
                  mean(sim_data$nuts)        / mean(sim_data$cereal + sim_data$dairy + sim_data$fish + sim_data$fruit_veg + sim_data$nuts + sim_data$alc_bev + sim_data$non_alc_bev + sim_data$misc) ,
                  mean(sim_data$alc_bev)     / mean(sim_data$cereal + sim_data$dairy + sim_data$fish + sim_data$fruit_veg + sim_data$nuts + sim_data$alc_bev + sim_data$non_alc_bev + sim_data$misc) ,
                  mean(sim_data$non_alc_bev) / mean(sim_data$cereal + sim_data$dairy + sim_data$fish + sim_data$fruit_veg + sim_data$nuts + sim_data$alc_bev + sim_data$non_alc_bev + sim_data$misc) ,
                  mean(sim_data$misc)        / mean(sim_data$cereal + sim_data$dairy + sim_data$fish + sim_data$fruit_veg + sim_data$nuts + sim_data$alc_bev + sim_data$non_alc_bev + sim_data$misc) )
  
  names(wi_6b) <- c("cereal", "dairy", "fish", "fruit_veg", "nuts", "alc_bev", "non_alc_bev", "misc"); wi_6b
  
  calc_6b      <-  calc_meat - 
    wi_6b[1]*calc_cereal -
    wi_6b[2]*calc_dairy - 
    wi_6b[3]*calc_fish -
    wi_6b[4]*calc_fruit_veg -
    wi_6b[5]*calc_nuts - 
    wi_6b[6]*calc_alc_bev -
    wi_6b[7]*calc_non_alc_bev -
    wi_6b[8]*calc_misc; calc_6b
  
  # 6b.1. All-components model
  
  mod6b_1_coef  <- mod4b_1$coefficients[4]*100 -
    wi_6b[1]*mod4b_1$coefficients[2]*100 -
    wi_6b[2]*mod4b_1$coefficients[3]*100 -
    wi_6b[3]*mod4b_1$coefficients[5]*100 -
    wi_6b[4]*mod4b_1$coefficients[6]*100 -
    wi_6b[5]*mod4b_1$coefficients[7]*100 -
    wi_6b[6]*mod4b_1$coefficients[8]*100 -
    wi_6b[7]*mod4b_1$coefficients[9]*100 -
    wi_6b[8]*mod4b_1$coefficients[10]*100; mod6b_1_coef
     
     # 6B.2. the leave-'one'-out model (total mass)
     mod6b_2       <- lm(fpg ~ meat + total_mass, data=sim_data)
     mod6b_2_coef  <- mod6b_2$coefficients[2]*100; mod6b_2_coef #This estimate is confounded by consumption of other unmodelled foods + biased by the 'average' being a composite variable
     
     # 6B.3 Energy partition model
     sim_data$remaining_meat <- sim_data$total_mass - sim_data$meat 
     
     mod6b_3       <- lm(fpg ~ meat + remaining_meat, data=sim_data)
     mod6b_3_coef  <- mod6b_3$coefficients[2]*100-mod6b_3$coefficients[3]*100; mod6b_3_coef #This estimate is confounded by consumption of other unmodelled foods + biased by the 'average' being a composite variable
     
     # 6B.4. the leave-'one'-out model (total energy)
     mod6b_4      <- lm(fpg ~ meat + total_energy, data=sim_data)
     mod6b_4_coef  <- mod6b_4$coefficients[2]*100; mod6b_4_coef 

     # save coefficients:
     macronutrients[nrow(macronutrients)+1,] <- c(sugars, carbs, protein, sat_fat, unsat_fat, fiber, alcohol)
     #calc_foods_cals <- calc_foods_cals[nrow(calc_foods_cals)+1,] <- c(calc_cereal_cals, calc_dairy_cals, calc_meat_cals, calc_fish_cals, calc_fruit_veg_cals, calc_nuts_cals, calc_non_alc_bev_cals, calc_alc_bev_cals, calc_misc_cals)
     foods_cals_coefs[nrow(foods_cals_coefs)+1,] <- c(cereal_cals, dairy_cals, meat_cals, fish_cals, fruit_veg_cals, nuts_cals, alc_bev_cals, non_alc_bev_cals, misc_cals)
     #calc_foods <- calc_foods[nrow(calc_foods)+1,] <- c(calc_cereal, calc_dairy, calc_meat, calc_fish, calc_fruit_veg, calc_nuts, calc_non_alc_bev, calc_alc_bev, calc_misc)    
     foods_coefs[nrow(foods_coefs)+1,] <- c(cereal, dairy, meat, fish, fruit_veg, nuts, alc_bev, non_alc_bev, misc)
     calculated[nrow(calculated)+1,] <- c(calc_1, calc_2, calc_3, calc_4a, calc_5a, calc_6a, calc_4b, calc_5b, calc_6b)
     
     
     estimand1[nrow(estimand1)+1,] <- c(mod1_1_coef, mod1_2_coef)
     estimand2[nrow(estimand2)+1,] <- c(mod2_1_coef, mod2_2_coef)
     estimand3[nrow(estimand3)+1,] <- c(mod3_1_coef, mod3_2_coef, mod3_3_coef)
     estimand4A[nrow(estimand4A)+1,] <- c(mod4a_1_coef, mod4a_2_coef)
     estimand5A[nrow(estimand5A)+1,] <- c(mod5a_1_coef, mod5a_2_coef)
     estimand6A[nrow(estimand6A)+1,] <- c(mod6a_1_coef, mod6a_2_coef, mod6a_3_coef)
     estimand4B[nrow(estimand4B)+1,] <- c(mod4b_1_coef, mod4b_2_coef, mod4b_3_coef)
     estimand5B[nrow(estimand5B)+1,] <- c(mod5b_1_coef, mod5b_2_coef, mod5b_3_coef)
     estimand6B[nrow(estimand6B)+1,] <- c(mod6b_1_coef, mod6b_2_coef, mod6b_4_coef, mod6b_3_coef)
     
    
     
  #rm() 
  
  # Display simulation progress
  pb$tick()
  
} 

# build a data frame with all final results
estimand1_names <- list("All-components/Energy partition", "Leave-one-out")
estimand2_names <- list("Inadvertant 'leave-one-out' model", "All-components model")
estimand3_names <- list("All-components model", "Leave-one-out", "Simple energy partition")
estimand4A_names <- list("All-components/Energy partition", "Leave-one-out")
estimand5A_names <- list("Inadvertant 'leave-one-out' model", "All-components model")
estimand6A_names <- list("All-components model", "Leave-one-out", "Simple energy partition")
estimand4B_names <- list("All-components/Energy partition", "Leave-one-out (total mass)", "Leave-one-out (total energy")
estimand5B_names <- list("Inadvertant 'leave-one-out' model (total mass)","Inadvertant 'leave-one-out' model (total energy)" , "All-components model")
estimand6B_names <- list("All-components model", "Leave-one-out (total mass)", "Leave-one-out (total energy)", "Simple energy partition")

final_results <- data.frame(model=character(0),
                            lower=numeric(0), 
                            point=numeric(0), 
                            upper=numeric(0))


final_results[nrow(final_results)+1,] <- c("Relative causal effect of SUGAR instead of PROTEIN", "", "", "")

for (j in 1:2) {
  
  centiles                             <- c(round(quantile(estimand1[,j], 0.025), digits=2), round(quantile(estimand1[,j], 0.5), digits=2), round(quantile(estimand1[,j], 0.975),digits=2))
  final_results[nrow(final_results)+1,]  <- c(estimand1_names[j], unname(as.list(centiles)))
  rm(centiles)
} 

final_results[nrow(final_results)+1,] <- c("Naïve causal effect of SUGAR instead of PROTEIN, FAT, and FIBER", "", "", "")

for (k in 1:2) {
  
  centiles                             <- c(round(quantile(estimand2[,k], 0.025), digits=2), round(quantile(estimand2[,k], 0.5), digits=2), round(quantile(estimand2[,k], 0.975),digits=2))
  final_results[nrow(final_results)+1,]  <- c(estimand2_names[k], unname(as.list(centiles)))
  rm(centiles)
} 

final_results[nrow(final_results)+1,] <- c("Average relative causal effect of SUGAR", "", "", "")

for (l in 1:3) {
  
  centiles                             <- c(round(quantile(estimand3[,l], 0.025), digits=2), round(quantile(estimand3[,l], 0.5), digits=2), round(quantile(estimand3[,l], 0.975),digits=2))
  final_results[nrow(final_results)+1,]  <- c(estimand3_names[l], unname(as.list(centiles)))
  rm(centiles)
} 

final_results[nrow(final_results)+1,] <- c("Relative causal effect of MEAT instead of FISH (cals)", "", "", "")

for (m in 1:2) {
  
  centiles                             <- c(round(quantile(estimand4A[,m], 0.025), digits=2), round(quantile(estimand4A[,m], 0.5), digits=2), round(quantile(estimand4A[,m], 0.975),digits=2))
  final_results[nrow(final_results)+1,]  <- c(estimand4A_names[m], unname(as.list(centiles)))
  rm(centiles)
} 

final_results[nrow(final_results)+1,] <- c("Naïve causal effect of MEAT instead of CEREAL, DAIRY, FISH, NUTS, and MISC (cals)", "", "", "")

for (n in 1:2) {
  
  centiles                             <- c(round(quantile(estimand5A[,n], 0.025), digits=2), round(quantile(estimand5A[,n], 0.5), digits=2), round(quantile(estimand5A[,n], 0.975),digits=2))
  final_results[nrow(final_results)+1,]  <- c(estimand5A_names[n], unname(as.list(centiles)))
  rm(centiles)
} 

final_results[nrow(final_results)+1,] <- c("Average relative causal effect of MEAT (cals)", "", "", "")

for (o in 1:3) {
  
  centiles                             <- c(round(quantile(estimand6A[,o], 0.025), digits=2), round(quantile(estimand6A[,o], 0.5), digits=2), round(quantile(estimand6A[,o], 0.975),digits=2))
  final_results[nrow(final_results)+1,]  <- c(estimand6A_names[o], unname(as.list(centiles)))
  rm(centiles)
} 

final_results[nrow(final_results)+1,] <- c("Relative causal effect of MEAT instead of FISH (grams)", "", "", "")

for (p in 1:3) {
  
  centiles                             <- c(round(quantile(estimand4B[,p], 0.025), digits=2), round(quantile(estimand4B[,p], 0.5), digits=2), round(quantile(estimand4B[,p], 0.975),digits=2))
  final_results[nrow(final_results)+1,]  <- c(estimand4B_names[p], unname(as.list(centiles)))
  rm(centiles)
} 

final_results[nrow(final_results)+1,] <- c("Naïve causal effect of MEAT instead of CEREAL, DAIRY, FISH, NUTS, and MISC (grams)", "", "", "")

for (q in 1:3) {
  
  centiles                             <- c(round(quantile(estimand5B[,q], 0.025), digits=2), round(quantile(estimand5B[,q], 0.5), digits=2), round(quantile(estimand5B[,q], 0.975),digits=2))
  final_results[nrow(final_results)+1,]  <- c(estimand5B_names[q], unname(as.list(centiles)))
  rm(centiles)
} 

final_results[nrow(final_results)+1,] <- c("Average relative causal effect of MEAT (grams)", "", "", "")

for (r in 1:4) {
  
  centiles                             <- c(round(quantile(estimand6B[,r], 0.025), digits=2), round(quantile(estimand6B[,r], 0.5), digits=2), round(quantile(estimand6B[,r], 0.975),digits=2))
  final_results[nrow(final_results)+1,]  <- c(estimand6B_names[r], unname(as.list(centiles)))
  rm(centiles)
} 

View(final_results)
write.csv(final_results, "results_revised_manuscript_gdt.csv")

