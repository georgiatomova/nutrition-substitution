###----------------------------
# Both nutrient_substitution.R and food_substitution.R need to be run before running this script
###----------------------------

library(psych); library(car)

# When all groups are included, some estimates are inestimable
modX1 <- lm(WT ~ TotalEnergy + CEREAL + DAIRY + MEAT + FISH + FVP + NUT + ALCO + NONALC + MISC, data=SimData); modX1$coefficients*100; summary(modX1)
# The coefficient estimate of the exposure therefore depends on the order of the variables in the model, because the last few will not be estimated and taken into account.

# Run this model multiple times, each time dropping a different variable and/or rearranging order, to see this:
modX2 <- lm(WT ~ NMES + TotalEnergy + CEREAL + DAIRY + MEAT + FISH + FVP + NUT + ALCO + NONALC + MISC, data=SimData); modX2$coefficients[2]*100; summary(modX2)

# When a full summary is printed, it is obvious that some coefficients are inestimable, and these are always the last 3.
# Therefore, the exposure coefficient depends on the order in which variables are included in the model, more importantly - which last 3
# coefficients are not estimable

# Explore correlation
cor.plot(SimData) # cor between many pairs of food groups >0.9 (while cor among nutrients seems okay)

## Variance Inflation Factor
# food group models used in the simulations:
vif(modG1) # severe
vif(modG2) # severe

# We initially explored multicollinearity as an issue, because in some models the exposure coefficient was negative, which was not
# otherwise possible, based on the data we have simulated.

# If we play around with the model, we can always get a negative coefficient for the exposure if we choose food groups that are very highly correlated.
# For example, FVP, DAIRY, NUT and NONALC are all highly correlated (>0.09), exposure coefficient is negative and vif is very high
modX3 <- lm(formula = WT ~ NMESg + TotalEnergy + FVP + DAIRY + NUT + NONALC, data = SimData); summary(modX3); vif(modX3) 


