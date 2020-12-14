###-----------------------------------------------------------------------
# Adjustment with Food Groups
###-----------------------------------------------------------------------

### To run this script and familiarise yourself with the simulation, please run nutrient_substituion.R first.
 
###-----------------------------------------------------------------------
# This code uses simulated data to create composite 'food' variables. 
# Data from the NDNS year 7-8 is used to obtain mean intake of each food group per day.

# The simulated nutrient variables are then used to create the food variables based on the
# NDNS mean intake and based on the percentage contribution of each nutrient to each food group,
# as seen in the NDNS_yr_7_to_8_statistics.xlsx

# The food variables are assigned as either 'healthy' or 'unhealthy' purely for illustrative purposes.
# Adjustment is then made for either healthy or unhealthy variables, using the standard model.
###-----------------------------------------------------------------------

library(progress); library(devtools); library(dagitty)

# Import NDNS wave 7-8
NDNS <- read.delim("ndns_rp_yr7-8a_foodleveldietarydata.tab", header = TRUE, sep = "\t",quote = "")

# View food groups
table(NDNS$MainFoodGroupDesc)

### List of food groups based on the NDNS table of food groups, combined where relevant:

# 1) CEREAL. Cereals and cereal products
# 2) DAIRY.  Milk and milk products, Eggs and egg dishes
# 3) MEAT.   Meat and meat products
# 4) FISH.   Fish and fish dishes
# 5) FVP.    Vegetables and potatoes, Fruit
# 6) NUT.    Nuts and seeds
# 7) ALCO.   Alcoholic beverages
# 8) NONALC. Non-alcoholic beverages
# 9) MISC.   Sugar, preserves and confectionery, Savoury snacks, Fat spreads, Miscellaneous

CEREAL <- subset(NDNS, MainFoodGroupDesc=="BROWN GRANARY AND WHEATGERM BREAD" | MainFoodGroupDesc=="BISCUITS"    | MainFoodGroupDesc=="BUNS CAKES PASTRIES & FRUIT PIES" |
                   MainFoodGroupDesc=="HIGH FIBRE BREAKFAST CEREALS"      | MainFoodGroupDesc=="OTHER BREAD" | MainFoodGroupDesc=="OTHER BREAKFAST CEREALS" |
                   MainFoodGroupDesc=="PASTA RICE AND OTHER CEREALS"      | MainFoodGroupDesc=="PUDDINGS"    | MainFoodGroupDesc=="WHITE BREAD" | 
                   MainFoodGroupDesc=="WHOLEMEAL BREAD")

DAIRY  <- subset(NDNS, MainFoodGroupDesc=="1% Fat Milk"   | MainFoodGroupDesc=="CHEESE"               | MainFoodGroupDesc=="EGGS AND EGG DISHES" |
                   MainFoodGroupDesc=="ICE CREAM"     | MainFoodGroupDesc=="OTHER MILK AND CREAM" | MainFoodGroupDesc=="SEMI SKIMMED MILK" | 
                   MainFoodGroupDesc=="SKIMMED MILK"  | MainFoodGroupDesc=="WHOLE MILK"           | MainFoodGroupDesc=="YOGURT FROMAGE FRAIS AND DAIRY DESSERTS") 

MEAT   <- subset(NDNS, MainFoodGroupDesc=="BACON AND HAM"             | MainFoodGroupDesc=="BEEF VEAL AND DISHES"   | MainFoodGroupDesc=="BURGERS AND KEBABS" |
                   MainFoodGroupDesc=="CHICKEN AND TURKEY DISHES" | MainFoodGroupDesc=="COATED CHICKEN"         | MainFoodGroupDesc=="LAMB AND DISHES" |
                   MainFoodGroupDesc=="LIVER AND DISHES"          | MainFoodGroupDesc=="MEAT PIES AND PASTRIES" | MainFoodGroupDesc=="OTHER MEAT AND MEAT PRODUCTS" |
                   MainFoodGroupDesc=="PORK AND DISHES"           | MainFoodGroupDesc=="SAUSAGES")

FISH   <- subset(NDNS, MainFoodGroupDesc=="OILY FISH" | MainFoodGroupDesc== "OTHER WHITE FISH SHELLFISH & FISH DISHES" | MainFoodGroupDesc=="WHITE FISH COATED OR FRIED" )

FVP    <- subset(NDNS, MainFoodGroupDesc=="CHIPS FRIED & ROAST POTATOES AND POTATO PRODUCTS" | MainFoodGroupDesc=="FRUIT"               | MainFoodGroupDesc=="OTHER POTATOES POTATO SALDS & DISHES" |
                   MainFoodGroupDesc=="SALAD AND OTHER RAW VEGETABLES"                   | MainFoodGroupDesc=="VEGETABLES NOT RAW")

NUT    <- subset(NDNS, MainFoodGroupDesc=="NUTS AND SEEDS")

ALCO   <- subset(NDNS, MainFoodGroupDesc=="BEER LAGER CIDER & PERRY" | MainFoodGroupDesc=="SPIRITS AND LIQUEURS" | MainFoodGroupDesc=="WINE")

NONALC <- subset(NDNS, MainFoodGroupDesc=="FRUIT JUICE"                 | MainFoodGroupDesc=="SMOOTHIES 100% FRUIT AND/OR JUICE" | MainFoodGroupDesc=="SOFT DRINKS LOW CALORIES" |
                   MainFoodGroupDesc=="SOFT DRINKS NOT LOW CALORIE" | MainFoodGroupDesc=="TEA COFFEE AND WATER")

MISC   <- subset(NDNS,  MainFoodGroupDesc=="ARTIFICIAL SWEETENERS"                | MainFoodGroupDesc=="BUTTER"                    | MainFoodGroupDesc=="CHOCOLATE CONFECTIONERY" | 
                   MainFoodGroupDesc=="COMMERCIAL TODDLERS FOODS AND DRINKS" | MainFoodGroupDesc=="CRISPS AND SAVOURY SNACKS" | MainFoodGroupDesc=="DIETARY SUPPLEMENTS" |
                   MainFoodGroupDesc=="LOW FAT SPREAD"                       | MainFoodGroupDesc=="MISCELLANEOUS"             | MainFoodGroupDesc=="OTHER MARGARINE FATS AND OILS" | 
                   MainFoodGroupDesc=="PUFA MARGARINE & OILS"                | MainFoodGroupDesc=="REDUCED FAT SPREAD"        | MainFoodGroupDesc=="SUGAR CONFECTIONERY" | 
                   MainFoodGroupDesc=="SUGAR PRESERVES AND SWEET SPREADS")

# Save all group names
GroupNames <- c("CEREAL", "DAIRY", "MEAT", "FISH", "FVP", "NUT", "ALCO", "NONALC", "MISC")

# Create a list of all food groups
Groups <- list(CEREAL, DAIRY, MEAT, FISH, FVP, NUT, ALCO, NONALC, MISC)

# Calculate the mean grams consumed for each food group
Grams <- lapply(Groups, function(y) {
  y <- aggregate(y$TotalGrams, by=list(seriali=y$seriali), FUN=sum);
  colnames(y)[colnames(y)=="x"] <- "PP";
  y$G <- y$PP/4;
  return(y)
}) 
names(Grams) <- paste0(GroupNames)
list2env(Grams, envir = .GlobalEnv)
MeanGrams <- unlist(lapply(Grams, function(x) {mean(x$G)})); MeanGrams

# Percentage contribution of food groups are not available in NDNS tables for unsaturated fat and starch
# Create a function to calculate this based on average intakes and contributions of other fats and carbs

# Based on average NDNS intakes: 
# total fat - 69.5g, saturated fat - 25.1g     => average unsaturated fat intake - 44.4g
# total carb - 224g, NMES - 57.1g, fibre - 19g => average starch intake - 147.9g

# The functions use the percentage contributions of other fats and carbhydrates to obtain those not directly available:

# Unsaturated Fat
UF_calc <- function(TF,SF) {
  ((TF*69.5-SF*25.1)/44.4)
} 

# Starch
STR_calc <- function(TC,NMES,FBR) {
  ((TC*224-NMES*57.1-FBR*19)/147.9)
}

# Create vectors for unsaturated fat and starch percentage contributions of each food group
UF <- c(UF_calc(0.21,0.21), UF_calc(0.16,0.24), UF_calc(0.24,0.24), UF_calc(0.04,0.03), UF_calc(0.11,0.07), UF_calc(0.03,0.02),
        UF_calc(0,0),       UF_calc(0,0),       UF_calc(0.20,0.18))
names(UF) <- paste0(GroupNames); UF

STR <- c(STR_calc(0.46,0.24,0.38), STR_calc(0.05,0.06,0.01), STR_calc(0.06,0.04,0.12), STR_calc(0.01,0,0.02),    STR_calc(0.2,0.03,0.38), 
         STR_calc(0,0,0),          STR_calc(0.03,0.09,0),    STR_calc(0.07,0.21,0),    STR_calc(0.12,0.31,0.07))
names(STR) <- paste0(GroupNames); STR


# Create food groups based on NDNS contributions of nutrients to each (using calculated contributions for U Fat and Starch)

SimData$CEREAL    <- SimData$PRO*0.23/4 + SimData$SF*0.21/9 + SimData$UF*UF["CEREAL"]/9 + SimData$STR*STR["CEREAL"]/4 + SimData$NMES*0.24/4 + SimData$FBR*0.38/4 + SimData$ALC*0.00/7
SimData$DAIRY     <- SimData$PRO*0.17/4 + SimData$SF*0.24/9 + SimData$UF*UF["DAIRY"]/9  + SimData$STR*STR["DAIRY"]/4  + SimData$NMES*0.06/4 + SimData$FBR*0.01/4 + SimData$ALC*0.00/7
SimData$MEAT      <- SimData$PRO*0.37/4 + SimData$SF*0.24/9 + SimData$UF*UF["MEAT"]/9   + SimData$STR*STR["MEAT"]/4   + SimData$NMES*0.04/4 + SimData$FBR*0.12/4 + SimData$ALC*0.00/7
SimData$FISH      <- SimData$PRO*0.07/4 + SimData$SF*0.03/9 + SimData$UF*UF["FISH"]/9   + SimData$STR*STR["FISH"]/4   + SimData$NMES*0.00/4 + SimData$FBR*0.02/4 + SimData$ALC*0.00/7
SimData$FVP       <- SimData$PRO*0.09/4 + SimData$SF*0.07/9 + SimData$UF*UF["FVP"]/9    + SimData$STR*STR["FVP"]/4    + SimData$NMES*0.03/4 + SimData$FBR*0.38/4 + SimData$ALC*0.00/7
SimData$NUT       <- SimData$PRO*0.01/4 + SimData$SF*0.02/9 + SimData$UF*UF["NUT"]/9    + SimData$STR*STR["NUT"]/4    + SimData$NMES*0.00/4 + SimData$FBR*0.00/4 + SimData$ALC*0.00/7
SimData$ALCO      <- SimData$PRO*0.01/4 + SimData$SF*0.00/9 + SimData$UF*UF["ALCO"]/9   + SimData$STR*STR["ALCO"]/4   + SimData$NMES*0.09/4 + SimData$FBR*0.00/4 + SimData$ALC*1.00/7
SimData$NONALC    <- SimData$PRO*0.01/4 + SimData$SF*0.00/9 + SimData$UF*UF["NONALC"]/9 + SimData$STR*STR["NONALC"]/4 + SimData$NMES*0.21/4 + SimData$FBR*0.00/4 + SimData$ALC*0.00/7
SimData$MISC      <- SimData$PRO*0.05/4 + SimData$SF*0.18/9 + SimData$UF*UF["MISC"]/9   + SimData$STR*STR["MISC"]/4   + SimData$NMES*0.31/4 + SimData$FBR*0.07/4 + SimData$ALC*0.00/7

SimUData$CEREAL   <- SimUData$PRO*0.23/4 + SimUData$SF*0.21/9 + SimUData$UF*UF["CEREAL"]/9 + SimUData$STR*STR["CEREAL"]/4 + SimUData$NMES*0.24/4 + SimUData$FBR*0.38/4 + SimUData$ALC*0.00/7
SimUData$DAIRY    <- SimUData$PRO*0.17/4 + SimUData$SF*0.24/9 + SimUData$UF*UF["DAIRY"]/9  + SimUData$STR*STR["DAIRY"]/4  + SimUData$NMES*0.06/4 + SimUData$FBR*0.01/4 + SimUData$ALC*0.00/7
SimUData$MEAT     <- SimUData$PRO*0.37/4 + SimUData$SF*0.24/9 + SimUData$UF*UF["MEAT"]/9   + SimUData$STR*STR["MEAT"]/4   + SimUData$NMES*0.04/4 + SimUData$FBR*0.12/4 + SimUData$ALC*0.00/7
SimUData$FISH     <- SimUData$PRO*0.07/4 + SimUData$SF*0.03/9 + SimUData$UF*UF["FISH"]/9   + SimUData$STR*STR["FISH"]/4   + SimUData$NMES*0.00/4 + SimUData$FBR*0.02/4 + SimUData$ALC*0.00/7
SimUData$FVP      <- SimUData$PRO*0.09/4 + SimUData$SF*0.07/9 + SimUData$UF*UF["FVP"]/9    + SimUData$STR*STR["FVP"]/4    + SimUData$NMES*0.03/4 + SimUData$FBR*0.38/4 + SimUData$ALC*0.00/7
SimUData$NUT      <- SimUData$PRO*0.01/4 + SimUData$SF*0.02/9 + SimUData$UF*UF["NUT"]/9    + SimUData$STR*STR["NUT"]/4    + SimUData$NMES*0.00/4 + SimUData$FBR*0.00/4 + SimUData$ALC*0.00/7
SimUData$ALCO      <- SimUData$PRO*0.01/4 + SimUData$SF*0.00/9 + SimUData$UF*UF["ALCO"]/9  + SimUData$STR*STR["ALCO"]/4   + SimUData$NMES*0.09/4 + SimUData$FBR*0.00/4 + SimUData$ALC*1.00/7
SimUData$NONALC   <- SimUData$PRO*0.01/4 + SimUData$SF*0.00/9 + SimUData$UF*UF["NONALC"]/9 + SimUData$STR*STR["NONALC"]/4 + SimUData$NMES*0.21/4 + SimUData$FBR*0.00/4 + SimUData$ALC*0.00/7
SimUData$MISC     <- SimUData$PRO*0.05/4 + SimUData$SF*0.18/9 + SimUData$UF*UF["MISC"]/9   + SimUData$STR*STR["MISC"]/4   + SimUData$NMES*0.31/4 + SimUData$FBR*0.07/4 + SimUData$ALC*0.00/7

# Note these are 'dry' weights because nutrients don't include water
# Calculate grams of water based on total weight and dry food weight 
# and then the proportion of water required
Means  <- sapply(SimData[,c("CEREAL","DAIRY","MEAT","FISH","FVP","NUT","ALCO","NONALC","MISC")],  mean)
MeansU <- sapply(SimUData[,c("CEREAL","DAIRY","MEAT","FISH","FVP","NUT","ALCO","NONALC","MISC")], mean)
WaterGrams  <- MeanGrams - Means
WaterGramsU <- MeanGrams - MeansU
WaterProportions <- WaterGrams/mean(SimData$WTR); WaterProportions
WaterProportionsU <- WaterGramsU/mean(SimUData$WTR); WaterProportionsU

# Add water grams to each food group to obtain actual consumption of groups in grams
for (k in GroupNames) {
  SimData[[k]] <- SimData[[k]] + SimData$WTR*WaterProportions[k];
  SimUData[[k]] <- SimUData[[k]] + SimUData$WTR*WaterProportionsU[k]
}

# Check if food group weight with added water now equals the reported food group mean weight (ignore negligible differences)
for (l in GroupNames) {
  if(MeanGrams[l]==mean(SimData[[l]])) {
    print(paste(l,"YES"))
  } else {
    print(paste(l,"NO - they differ by", abs(MeanGrams[l]-mean(SimData[[l]]))))
  }
}

for (m in GroupNames) {
  if(MeanGrams[m]==mean(SimUData[[m]])) {
    print(paste(m,"YES"))
  } else {
    print(paste(m,"NO - they differ by", abs(MeanGrams[m]-mean(SimUData[[m]]))))
  }
}

# Set up dataframes to store means
GroupMeans <- data.frame(modG1=numeric(0), 
                         modG1U=numeric(0),
                         modG2=numeric(0), 
                         modG2U=numeric(0))

# Rescale the NMES exposure to grams for consistency with other variables
SimData$NMESg <- SimData$NMES/4
SimUData$NMESg <- SimUData$NMES/4

# Run models
Nsims <- 100000
pb <- progress_bar$new(total = Nsims, format = ":bar :percent eta: :eta")
for (n in 1:Nsims) {
  
  ### 1. Average relative causal effect of NMES instead of  more 'healthy' foods
  
  modG1  <- lm(WT ~ NMESg + TotalEnergy + MEAT + ALCO + NONALC + MISC, data=SimData)
  modG1U <- lm(WT ~ NMESg + TotalEnergy + MEAT + ALCO + NONALC + MISC, data=SimUData)
  
  meanG1  <- modG1$coefficients[2]*100 
  meanG1U <- modG1U$coefficients[2]*100
  
  ### 2. Average relative causal effect of NMES instead of  less 'healthy' foods
  
  modG2   <- lm(WT ~ NMESg + TotalEnergy + CEREAL + DAIRY + FISH + FVP + NUT, data=SimData)
  modG2U  <- lm(WT ~ NMESg + TotalEnergy + CEREAL + DAIRY + FISH + FVP + NUT, data=SimUData)
  
  meanG2  <- modG2$coefficients[2]*100 
  meanG2U <- modG2U$coefficients[2]*100
  
  # Save all means into a dataframe:
  GroupMeans[nrow(GroupMeans)+1,] <- c(meanG1, meanG1U, meanG2, meanG2U)
  
  #rm(modG1, modG1U, modG2, modG2U, meanG1, meanG1U, meanG2, meanG2U)
  
  # Display simulation progress
  pb$tick()
  
  
} 

### Save means and centiles for key coefficients

SummaryGroupMeans <- data.frame(model=character(0), 
                                lower=numeric(0), 
                                point=numeric(0), 
                                upper=numeric(0))

modelname <- list("ModG1", "ModG1U", "ModG2", "ModG2U")

for (o in 1:4) {
  
  centiles                             <- c(round(quantile(GroupMeans[,o], 0.025), digits=2), round(quantile(GroupMeans[,o], 0.5), digits=2), round(quantile(GroupMeans[,o], 0.975),digits=2))
  SummaryGroupMeans[nrow(SummaryGroupMeans)+1,]  <- c(modelname[o], unname(as.list(centiles)))
  rm(centiles)
}

View(SummaryGroupMeans) 
#write.csv(SummaryGroupMeans,"SummaryGroupMeans.csv") 