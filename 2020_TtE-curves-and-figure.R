### 2020 Rough Fescue Germination Experiment Analysis with drcte

## Load in required libraries
library(tidyverse)
library(drcte)

## Load in dataset
germdata <- read.csv("Cleaned_2020-germ-per-day_Wide.csv", header = TRUE, sep = ",", check.names = FALSE)
germdata

# germdata is a 2020 germination experiment looking at two species of rough fescue grass, Festuca hallii (FH) and Festuca campestris (FC)
# I planted 60 pots per species with 10 seeds each, for a total of 120 pots and 1,200 seeds
# I monitored the emergence of the seeds in each pot for 27 days
# I am aiming to see if there is a difference in the germination rate/patterns between the two species

## Convert the data from wide to long format for analysis
germlong <- melt_te(germdata, count_cols = 3:25, treat_cols = Species, #treat_cols is the experimental treatments, in this case, species
                    monitimes = 1:23, n.subjects = rep(10,120)) # n.subjects rep = 10 seeds per pot across 120 pots
germlong

### Fitting the data to a parametric TtE model
germ.mod <- drmte(count ~ timeBef + timeAf, fct = loglogistic(),
                     data = germlong, 
                     curveid = Species)

summary(germ.mod, robust = T, units = Units) # summarizing by Units provides us with more accurate SE values

## Plotting the parametric TtE model
plot(germ.mod, log = "", 
     xlim = c(0, 25),
     ylim = c(0,0.7),
     xlab = "Time (days)", 
     ylab = "Cumulative probability of germination",
     legendPos = c(6.5, 0.6),
     col = c("#E66100", "#5D3A9B"), pch = c(15, 18))
title(main = "2020")

# Looks pretty good, the fit of the curves isn't perfect, but this is probably because we were tracking emergence, not germination. The seeds very abruptly began emerging around day 7, when in reality they were probably germinating gradually for a few days before that)

## Testing signficance of the parametric TtE model using Likelihood Ratio Test
compCDF(germ.mod, type = "permutation", units = germlong$Units)

# Significance of the LRT tells us that this model, which separates the curves by species, is significantly different than a null model where the curves are combined


