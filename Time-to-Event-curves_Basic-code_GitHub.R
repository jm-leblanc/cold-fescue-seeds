### Time-to-Event Curves for Germination Data 

rm(list = ls())

library(dplyr)
library(tidyr)
library(drcte)

### Load in and clean up data ####################################################

FH_long <-read.csv("F-hallii-rows_Long-format.csv")
FC_long <-read.csv("F-camp-rows_Long-format.csv")

head(FH_long)
head(FC_long)

# Convert columns to factors so the model can properly incorporate them
FH_long <- FH_long %>%
  mutate(across(1:6, factor))
  
FC_long <- FC_long %>%
  mutate(across(1:6, factor))
  
# Remove problematic FC groups (i.e. groups which had 0 germination and cause the model to short out)
FC_long <- FC_long %>%
  filter(!(Temperature == "DF" & Length == "1M" & Moisture == "PO") &
           !(Temperature == "FR" & Length == "5M" & Moisture == "PR") &
           !(Temperature == "MF" & Length == "3M" & Moisture == "PR") &
           !(Temperature == "DF" & Length == "3M" & Moisture == "PO")) 
  
## Combine the two species into one dataframe so I can compare their germination directly 
combined_data <- rbind(FH_long, FC_long)


### Fit TtE curves ###############################################################

## Combined data with both species =============================================##

# Fit the time to event model to the full dataset with both species
combined.TtE <- drmte(count ~ timeBef + timeAf,
                fct = loglogistic(),
                data = combined_data,
                curveid = Species) # Split curves according to species

# Run a likelihood ratio test to determine if differences between species is significant
compCDF(combined.TtE)

# Plot the germination curves for visual comparison
plot(combined.TtE, log = "",
     xlab = "Time (days)",
     ylab = "Cumulative probability of germination",
     legendPos = c(9.5, 0.68),
     col = c("#49BFBA", "#CA376C"), pch = c(16, 17))
title(main = "2024", font.main = 2)

## F. hallii data ==============================================================##
  
# Fit the 3-way interaction model
FH.TtE <- drmte(count ~ timeBef + timeAf,
                fct = loglogistic(),
                data = FH_long,
                curveid = Temperature:Length:Moisture)

## Code below is unnecessary but kept for comparion; the single factor and main factor models 
## do not fit the germination data as well as the full 3-factor model 

# # Two-factor models
# FH.TtE.TL <- drmte(count ~ timeBef + timeAf,
#                    fct = loglogistic(),
#                    data = FH_long,
#                    curveid = Temperature:Length)
# 
# FH.TtE.TM <- drmte(count ~ timeBef + timeAf,
#                    fct = loglogistic(),
#                    data = FH_long,
#                    curveid = Temperature:Moisture)
# 
# FH.TtE.LM <- drmte(count ~ timeBef + timeAf,
#                    fct = loglogistic(),
#                    data = FH_long,
#                    curveid = Length:Moisture)
#             
# # Single factor models        
# FH.TtE.Temp <- drmte(count ~ timeBef + timeAf,
#                      fct = loglogistic(),
#                      data = FH_long,
#                      curveid = Temperature)
# 
# FH.TtE.Length <- drmte(count ~ timeBef + timeAf,
#                        fct = loglogistic(),
#                        data = FH_long,
#                        curveid = Length)
# 
# FH.TtE.Moisture <- drmte(count ~ timeBef + timeAf,
#                          fct = loglogistic(),
#                          data = FH_long,
#                          curveid = Moisture)

# Compare all models using compCDF
# Comparing the 3-way model with each of the 2-way models
# compCDF(FH.TtE)       # Compare 3-way model to null hypothesis
# compCDF(FH.TtE.TL)    # Compare 3-way model to 2-way Temperature:Length
# compCDF(FH.TtE.TM)    # Compare 3-way model to 2-way Temperature:Moisture
# compCDF(FH.TtE.LM)    # Compare 3-way model to 2-way Length:Moisture
# 
# # Compare the 3-way model with the main effects models
# compCDF(FH.TtE.Temp)       # Compare 3-way model to main effect of Temperature
# compCDF(FH.TtE.Length)     # Compare 3-way model to main effect of Length
# compCDF(FH.TtE.Moisture) 

## F. campestris data ==========================================================##

# Fit time-to-event model for F. campestris seeds
FC.TtE <- drmte(count ~ timeBef + timeAf,
                fct = loglogistic(),
                data = FC_long,
                curveid = Temperature:Length:Moisture,
                separate = TRUE) # Fit each curve separately to avoid errors in the model


### Examine model output and isolate parameters ##################################

## F. hallii data ==============================================================##

summary(FH.TtE, robust = T, units = Dish.Number) # Cluster robust method accounts for grouping of seeds within petri dishes
compCDF(FH.TtE) # Likelihood ratio tests
compParm(FH.TtE, "d") # compParm compares the significance of parameter d between pairs of treatment groups

# Write this into a table ------------------------------------------------------#

FH_table <- summary(FH.TtE, robust = T, units = Dish.Number)$coefficients

write.csv(FH_table, "FH_TtE_Parameter-d_Raw.csv", row.names = TRUE)


## F. campestris data ==========================================================##

summary(FC.TtE) # We can't do the robust SE estimation here, because the curves are fit separately
# summary(FC.TtE, robust = TRUE, units = Dish.Number)
compCDF(FC.TtE) # CompCDF also doesn't work here

# Write this into a table ------------------------------------------------------#

FC_table <- summary(FC.TtE)$coefficients
write.csv(FC_table, "FC_TtE_Parameter-d_Raw.csv", row.names = TRUE)