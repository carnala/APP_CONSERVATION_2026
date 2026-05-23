# Load packages
library(dplyr)
library(ggeffects)
library(ggplot2)
library(glmmTMB)
library(performance)
library(readr)
library(readxl)
library(tidyverse)


# Load data
data <- read_csv("C:/Users/verme/OneDrive/Documents/tout.csv")
# View data
View(data)
colnames(data)

# modify data where needed
data$Zone        <- as.factor(data$Zone)
data$Deadwood    <- as.factor(data$Deadwood)
data$Stone_piles <- as.factor(data$Stone_piles)

# Building Generalized Linear Model (GLM)

# Response variable: carabid abundance (Mfull)
# Explanatory variable: environmental variable of interest


# Vizualisation of data ditribution
mean(data$Mfull)
var(data$Mfull) 
hist(data$Mfull,
     col = "#A4C3A2",
     main= "Distribution of all carabids' morphospecies",
     xlab = "Number of morphospecies",
     ylab= "Frequency") 


# variance far larger than mean - cannot be Poisson -> data is negative binomially distributed


# A simple nbinom2 GLM is used for each explanatory variable
# No random effect is included due to small sample size (n=8)
# Models estimate an average relationship between carabid abundance and each habitat variable.


# --- Univariate models ---
# Site characteristics
MfullM1  <- glmmTMB(Mfull ~ Zone,                data = data, family = nbinom2)
MfullM2  <- glmmTMB(Mfull ~ River_distance,       data = data, family = nbinom2)
MfullM3  <- glmmTMB(Mfull ~ Elevation_from_river, data = data, family = nbinom2)

# Bare ground cover
MfullM4  <- glmmTMB(Mfull ~ Bare_ground_cover,    data = data, family = nbinom2)
MfullM5  <- glmmTMB(Mfull ~ Sand_Bare_ground,     data = data, family = nbinom2)
MfullM6  <- glmmTMB(Mfull ~ Gravel,               data = data, family = nbinom2)
MfullM7  <- glmmTMB(Mfull ~ Stone,                data = data, family = nbinom2)
MfullM8  <- glmmTMB(Mfull ~ Soil,                 data = data, family = nbinom2)
MfullM9  <- glmmTMB(Mfull ~ DOM,                  data = data, family = nbinom2)
MfullM10 <- glmmTMB(Mfull ~ Deadwood,             data = data, family = nbinom2)
MfullM11 <- glmmTMB(Mfull ~ Stone_piles,          data = data, family = nbinom2)

# Vegetation cover
MfullM12 <- glmmTMB(Mfull ~ Vegetation_cover,     data = data, family = nbinom2)
MfullM13 <- glmmTMB(Mfull ~ Moss,                 data = data, family = nbinom2)
MfullM14 <- glmmTMB(Mfull ~ Herb,                 data = data, family = nbinom2)
MfullM15 <- glmmTMB(Mfull ~ Woody,                data = data, family = nbinom2)
MfullM16 <- glmmTMB(Mfull ~ Flower,               data = data, family = nbinom2)

# Vegetation height
MfullM17 <- glmmTMB(Mfull ~ Veg_heigth_0_10,      data = data, family = nbinom2)
MfullM18 <- glmmTMB(Mfull ~ Veg_heigth_10_30,     data = data, family = nbinom2)
MfullM19 <- glmmTMB(Mfull ~ Veg_heigth_30_60,     data = data, family = nbinom2)
MfullM20 <- glmmTMB(Mfull ~ Veg_heigth_60,        data = data, family = nbinom2)

# Other habitat variables
MfullM21 <- glmmTMB(Mfull ~ Water_cover,          data = data, family = nbinom2)
MfullM22 <- glmmTMB(Mfull ~ Slop_Flat,            data = data, family = nbinom2)
MfullM23 <- glmmTMB(Mfull ~ Slop_Stoped,          data = data, family = nbinom2)
MfullM24 <- glmmTMB(Mfull ~ data$`Slop_ Steep`,        data = data, family = nbinom2)

# Percentages
MfullM25 <- glmmTMB(Mfull ~ sand,                 data = data, family = nbinom2)
MfullM26 <- glmmTMB(Mfull ~ sandp,                data = data, family = nbinom2)
MfullM27 <- glmmTMB(Mfull ~ gravelp,              data = data, family = nbinom2)
MfullM28 <- glmmTMB(Mfull ~ stonep,               data = data, family = nbinom2)
MfullM29 <- glmmTMB(Mfull ~ soilp,                data = data, family = nbinom2)
MfullM30 <- glmmTMB(Mfull ~ domp,                 data = data, family = nbinom2)
MfullM31 <- glmmTMB(Mfull ~ mossp,                data = data, family = nbinom2)
MfullM32 <- glmmTMB(Mfull ~ herbp,                data = data, family = nbinom2)
MfullM33 <- glmmTMB(Mfull ~ woodyp,               data = data, family = nbinom2)
MfullM34 <- glmmTMB(Mfull ~ flowerp,              data = data, family = nbinom2)

# --- View summaries ---
# Look for model outputs with explanaory vaiables with p < 0.05 (*) or p < 0.1 (.) as potentially interesting
# ignore non-significant models (ns)

summary(MfullM1)   # Zone ns (ns)
summary(MfullM2)   # River_distance (ns)
summary(MfullM3)   # Elevation_from_river (ns)
summary(MfullM4)   # Bare_ground_cover (ns)
summary(MfullM5)   # Sand_Bare_ground (ns)
summary(MfullM6)   # Gravel (ns)
summary(MfullM7)   # Stone (ns)
summary(MfullM8)   # Soil (ns)
summary(MfullM9)   # DOM (ns)
summary(MfullM10)  # Deadwood (ns)
summary(MfullM11)  # Stone_piles (ns)
summary(MfullM12)  # Vegetation_cover (ns)
summary(MfullM13)  # Moss (ns)
summary(MfullM14)  # Herb (ns)
summary(MfullM15)  # Woody (ns)
summary(MfullM16)  # Flower (ns)
summary(MfullM17)  # Veg_heigth_0_10 (ns)
summary(MfullM18)  # Veg_heigth_10_30 (ns)
summary(MfullM19)  # Veg_heigth_30_60 (ns)
summary(MfullM20)  # Veg_heigth_60 (ns)
summary(MfullM21)  # Water_cover (ns)
summary(MfullM22)  # Slop_Flat (ns)
summary(MfullM23)  # Slop_Stoped (ns)
summary(MfullM24)  # Slop_ Steep (ns)
summary(MfullM25)  # sand (ns)
summary(MfullM26)  # sandp (ns)
summary(MfullM27)  # gravelp (ns)
summary(MfullM28)  # stonep (ns)
summary(MfullM29)  # soilp (ns)
summary(MfullM30)  # domp (ns)
summary(MfullM31)  # mossp (ns)
summary(MfullM32)  # herbp (ns)
summary(MfullM33)  # woodyp (ns)
summary(MfullM34)  # flowerp (ns)

# none significant, increasing dataset size by merging last year's data
# Load the merged dataset of the two years

data_all <- read_excel("C:/Users/verme/OneDrive/Documents/data_all.xlsx")

#data ajustement
data_all$deadwoodbin<-(data_all$Deadwood>0)*1# deadwood 2025 data are binary
data_all$deadwoodbin <- as.numeric(as.character(data_all$deadwoodbin))

# --- Univariate models ---
# Site  and year characteristics
MallM1 <- glmmTMB(nb_individ ~ site, data = data_all, family = nbinom2)
MallM2 <- glmmTMB(nb_individ ~ year, data = data_all, family = nbinom2)

# Bare ground cover
MallM3 <- glmmTMB(nb_individ ~ BG_Tot, data = data_all, family = nbinom2)
MallM4 <- glmmTMB(nb_individ ~ BG_Sand, data = data_all, family = nbinom2)
MallM5 <- glmmTMB(nb_individ ~ BG_Gravel, data = data_all, family = nbinom2)
MallM6 <- glmmTMB(nb_individ ~ BG_Cobble, data = data_all, family = nbinom2)
MallM7 <- glmmTMB(nb_individ ~ BG_Stone, data = data_all, family = nbinom2)
MallM8 <- glmmTMB(nb_individ ~ BG_Soil, data = data_all, family = nbinom2)
MallM9 <- glmmTMB(nb_individ ~ BG_Dead_Org_Matt, data = data_all, family = nbinom2)

# Vegetation cover
MallM10 <- glmmTMB(nb_individ ~ Veg_Tot, data = data_all, family = nbinom2)
MallM11 <- glmmTMB(nb_individ ~ Veg_Moss, data = data_all, family = nbinom2)
MallM12 <- glmmTMB(nb_individ ~ Veg_Herb, data = data_all, family = nbinom2)
MallM13 <- glmmTMB(nb_individ ~ Veg_Woody, data = data_all, family = nbinom2)

# Water distance
MallM14 <- glmmTMB(nb_individ ~ Water_Tot, data = data_all, family = nbinom2)

# Vegetation height
MallM15 <- glmmTMB(nb_individ ~ VegHeight_10, data = data_all, family = nbinom2)
MallM16 <- glmmTMB(nb_individ ~ VegHeight_10_30, data = data_all, family = nbinom2)
MallM17 <- glmmTMB(nb_individ ~ VegHeight_30_60, data = data_all, family = nbinom2)
MallM18 <- glmmTMB(nb_individ ~ VegHeight_60, data = data_all, family = nbinom2)


# Slope
MallM19 <- glmmTMB(nb_individ ~ Flat_30, data = data_all, family = nbinom2)
MallM20 <- glmmTMB(nb_individ ~ Sloped_30_60, data = data_all, family = nbinom2)
MallM21 <- glmmTMB(nb_individ ~ Steep_60, data = data_all, family = nbinom2)

# Small structures
MallM22b <- glmmTMB(nb_individ ~ deadwoodbin, data = data_all, family = nbinom2)
MallM23 <- glmmTMB(nb_individ ~ Stonepiles, data = data_all, family = nbinom2)

# --- View summaries ---
# Look for model outputs with explanaory vaiables with p < 0.05 (*) or p < 0.1 (.) as potentially interesting
# ignore non-significant models (ns)

summary(MallM1)   # site (control vs focal)
summary(MallM2)   # year (2025 vs 2026)
summary(MallM3)   # BG_Tot
summary(MallM4)   # BG_Sand
summary(MallM5)   # BG_Gravel
summary(MallM6)   # BG_Cobble
summary(MallM7)   # BG_Stone
summary(MallM8)   # BG_Soil (0.0918 .)
summary(MallM9)   # BG_Dead_Org_Matt
summary(MallM22b) # Deadwood binary
summary(MallM23)  # Stonepiles
summary(MallM10)  # Veg_Tot
summary(MallM11)  # Veg_Moss
summary(MallM12)  # Veg_Herb
summary(MallM13)  # Veg_Woody
summary(MallM14)  # Water_Tot (0.0168 *)
summary(MallM15)  # VegHeight_10
summary(MallM16)  # VegHeight_10_30
summary(MallM17)  # VegHeight_30_60
summary(MallM18)  # VegHeight_60
summary(MallM19)  # Flat_30 (0.048249 * )
summary(MallM20)  # Sloped_30_60 (0.0583 .)
summary(MallM21)  # Steep_60

# Signifcant (p < 0.05): Water_tot, Flat_30
# Trend (p < 0.1): BG_Soil, Sloped_30_60
