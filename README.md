# Analyzing and interpreting substitutions in nutrition research

This project explores the analyses and interpretation of nutrient or food substitutions in nutritional research. It uses simulations to investigate the causal estimands corresponding to specific substitutions, and quantifies the performance of two types of models in reducing confounding bias when substitutions are modelled.

## Table of Contents
* Data
* Setup

## Data
All data used for the analyses were simulated based on mean, standard deviation, and percentage contribution values similar to those in #NDNS_yr_7_to_8_statistics.xlsx.   
For the food substitution analyses only, real data was used to extract specific means required for the simulation. [NDNS Year 7-8](https://www.gov.uk/government/collections/national-diet-and-nutrition-survey) data can be downloaded from the [UK Data Service](https://www.ukdataservice.ac.uk). 

## Setup
All simulations were conducted using R 4.0.3.  
The file nutrient_substitution.R contains the data simulation and main analyses, and should be run first before proceeding to the other scripts.  
The file food_substitution.R contains secondary analyses using food groups.    
The file multicollinearity.R explores multicollinearity in some of the models contained in food_substitution.R  

Order in which files should be run:  
* 1) nutrition_substitution.R  
* 2) food_substitution.R (or food_substitution_nodata.R)  
* 3) multicollinearity.R
