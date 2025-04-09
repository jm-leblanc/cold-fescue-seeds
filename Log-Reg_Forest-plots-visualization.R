###### Visualizing Logistic Regression Data###### 

rm(list = ls())

install.packages("devtools")
devtools::install_github("NightingaleHealth/ggforestplot")

# Load required packages
library(dplyr)
library(emmeans)
library(broom)
library(ggforestplot)
library(ggforce)

## Information for creating forest plots came from this tutorial: https://nightingalehealth.github.io/ggforestplot/articles/ggforestplot.html

### Fitting models for comparison to control conditions #############################################

## Load in and clean data =========================================================================##

# Simplified germination data for use in logistic regression
FC.combo.data <- read.csv("FC-cleaned-counts_For-Log-Reg.csv") 
FH.combo.data <- read.csv("FH-cleaned-counts_For-Log-Reg.csv") 

head(FC.combo.data)
head(FH.combo.data)

# Convert appropriate columns to factors for analysis
FC.combo.data <- FC.combo.data %>%
  mutate(across(c(1:5, 8), as.factor))

FH.combo.data <- FH.combo.data %>%
  mutate(across(c(1:5, 8), as.factor))

# Set the reference level as our control group
FH.combo.data$Combo <- relevel(FH.combo.data$Combo, ref = "C.RT.NS")
FC.combo.data$Combo <- relevel(FC.combo.data$Combo, ref = "C.RT.NS")

## F. hallii model =============================================================##

#Fit the model: germination ~ Combo
FH.combo.LR <- glm(cbind(Total.germinated, Starting.viable.count - Total.germinated) ~ Combo, 
                   family = "binomial", 
                   data = FH.combo.data)

# Check the model
summary(FH.combo.LR)

# Convert model output to dataframe --------------------------------------------#

FH.combo.df <- tidy(FH.combo.LR) 
#> note: DON'T specify exp = TRUE here; we want the logit values, because
#> ggforestplot has functionality to automatically calculate log odds for us

FH.combo.df$term <- gsub("^Combo", "", FH.combo.df$term)
# Merge the model output with the original dataset, so we have all factor levels
# alongside the coefficients and p-values

# Merge the coefficients and p-values with the factor level data
FH.merged <- FH.combo.df %>%
  left_join(FH.combo.data %>%
              dplyr::select(Combo, Species, Length, Temperature, Moisture), 
            by = c("term" = "Combo")) %>%
  distinct(term, .keep_all = TRUE)

# Filter out groups we don't want in our plots

FH.filtered <- FH.merged %>%
  filter(!term %in% c("5M.RT.PR", "3M.RT.PR", "1M.RT.PR", "(Intercept)"))

FH.filtered$Moisture <- factor(FH.filtered$Moisture, 
                             levels = c("NS", "PR", "PO"), 
                             labels = c("No Soak", "Pre-Soak", "Post-Soak"))

FH.filtered$Temperature <- factor(FH.filtered$Temperature, 
                                levels = c("FR", "MF", "DF", "RT"), 
                                labels = c("Fridge", "Mild Freeze", "Deep Freeze", "Room Temperature control, soaked 24h before germinating"))

FH.filtered$Length <- factor(FH.filtered$Length, 
                           levels = c("1M", "3M", "5M", "C"), 
                           labels = c("1 Month", "3 Months", "5 Months", "5 Months"))

# Draw forest plots ------------------------------------------------------------#

FH.forest.plot <- forestplot(
  df = FH.filtered,
  estimate = estimate,
  se = std.error,
  name = Length,
  logodds = TRUE,
  pvalue = p.value,  
  psignif = 0.05,    
  xlab = "Odds ratios for probability of germination (95% CI)\n given seed storage treatment factor combination",
  title = "Festuca hallii",
  colour = Moisture,
  shape = Moisture,
  size = 7
) +
  ggforce::facet_col(
    facets = ~ Temperature,  
    scales = "free_y",  
    space = "free"
) + 
  ggplot2::scale_shape_manual(
    values = c(21L, 23L, 22L),  
    labels = c("No Soak", "Pre-Soak", "Post-Soak")
) +
  ggplot2::scale_colour_manual(
    values = c("#EDAE49", "#00798C", "#D1495B"), 
    labels = c("No Soak", "Pre-Soak", "Post-Soak") 
  ) +
  labs(
    colour = "Hydration", shape = "Hydration"
  ) +
  theme(
    plot.title = element_text(size = rel(1.1), face = "bold.italic"),
    plot.margin = margin(20, 20, 20, 20),
    legend.title = element_text(face = "bold")
  )

FH.forest.plot

## F. campestris model =============================================================##

# Fit the model: germination ~ Combo
FC.combo.LR <- glm(cbind(Total.germinated, Starting.viable.count - Total.germinated) ~ Combo, 
                   family = "binomial", 
                   data = FC.combo.data)

# Check the model
summary(FC.combo.LR)

# Convert model output to dataframe --------------------------------------------#

FC.combo.df <- tidy(FC.combo.LR) 
#> note: DON'T specify exp = TRUE here; we want the logit values

FC.combo.df$term <- gsub("^Combo", "", FC.combo.df$term)

# Merge the model output with the original dataset, so we have all factor levels
# alongside the coefficients and p-values
FC.merged <- FC.combo.df %>%
  left_join(FC.combo.data %>%
              dplyr::select(Combo, Species, Length, Temperature, Moisture), 
            by = c("term" = "Combo")) %>%
  distinct(term, .keep_all = TRUE)

# Filter out groups we don't want in our plots
FC.filtered <- FC.merged %>%
  filter(!term %in% c("5M.RT.PR", "3M.RT.PR", "1M.RT.PR", 
                      "(Intercept)", "1M.DF.PO", "3M.MF.PR", "5M.FR.PR"))

# Factor levels and labels
FC.filtered$Moisture <- factor(FC.filtered$Moisture, 
                               levels = c("NS", "PR", "PO"), 
                               labels = c("No Soak", "Pre-Soak", "Post-Soak"))

FC.filtered$Temperature <- factor(FC.filtered$Temperature, 
                                  levels = c("FR", "MF", "DF", "RT"), 
                                  labels = c("Fridge", "Mild Freeze", "Deep Freeze", "Room Temperature control, soaked 24h before germinating"))

FC.filtered$Length <- factor(FC.filtered$Length, 
                             levels = c("1M", "3M", "5M", "C"), 
                             labels = c("1 Month", "3 Months", "5 Months", "5 Months"))

# Draw forest plot ------------------------------------------------------------#

FC.forest.plot <- forestplot(
  df = FC.filtered,
  estimate = estimate,
  se = std.error,
  name = Length,
  logodds = TRUE,
  pvalue = p.value,
  psignif = 0.05,    
  xlab = "Odds ratios for probability of germination (95% CI)\n given seed storage treatment factor combination",
  title = "Festuca campestris", 
  colour = Moisture,
  shape = Moisture,
  size = 7
) +
  ggforce::facet_col(
    facets = ~ Temperature,  
    scales = "free_y",  
    space = "free"
  ) + 
  ggplot2::scale_shape_manual(
    values = c(21L, 23L, 22L),  
    labels = c("No Soak", "Pre-Soak", "Post-Soak")
  ) +
  ggplot2::scale_colour_manual(
    values = c("#EDAE49", "#00798C", "#D1495B"),  
    labels = c("No Soak", "Pre-Soak", "Post-Soak")  
  ) +
  scale_x_continuous(labels = scales::label_number()
  ) +
  labs(colour = "Hydration", shape = "Hydration"
       )+
  theme(
    plot.title = element_text(size = rel(1.1), face = "bold.italic"),
    plot.margin = margin(20, 20, 20, 20),
    legend.title = element_text(face = "bold")
  )

FC.forest.plot

### Print plots
ggsave(filename = "F-camp_Forest-plot_High-Res.png",
       plot = FC.forest.plot,  
       width = 6.35,
       height = 6.35,
       dpi = 300,  
       units = "in",
       bg = "white")

ggsave(filename = "F-hallii_Forest-plot_High-Res.png",
       plot = FH.forest.plot,  
       width = 6.35,
       height = 6.35,
       dpi = 300,  
       units = "in",
       bg = "white")