## Opening libraries
library(gdm)
library(dplyr)
library(magrittr)

setwd("~/Desktop/Conservation_Genomics/4_Landscape_Genomics/EO")

## Loading pairwise Fst dataset
EO.Fst.imp <- read.csv("~/Desktop/Conservation_Genomics/3_Population_Genomics/EO/EO_PairwiseFst_imp.csv")

# Get spatial coordinates and environmental variables
env <- read.csv("~/Desktop/Conservation_Genomics/2_Environmental_data/EO_env_2.csv")[, c(1,5:9)] %>% set_colnames(., c('Site','Easting','Northing','TWI','SAP','SpH')) #Northing(y, Latitude); Easting(x, Longitude)

# Convert Character Site Names to Numeric IDs 
site_map <- setNames(seq_len(nrow(EO.Fst.imp)), EO.Fst.imp$Site)
EO.Fst.imp$Site <- site_map[EO.Fst.imp$Site]
env$Site <- site_map[env$Site]

## Convert to gdm format
gdm.input <- formatsitepair(bioData = EO.Fst.imp, 
                            bioFormat = 3, 
                            siteColumn = "Site", 
                            XColumn = "Easting", 
                            YColumn = "Northing", 
                            predData = env)

## Fit GDM model
EO.gdm.fit<-gdm(gdm.input,geo=T)

summary(EO.gdm.fit) 

#Percentage of null deviance explained
EO.gdm.fit$explained 

#Quantify model significance and variable importance
EO.gdm.importance <- gdm.varImp(gdm.input, 
                                geo = T, 
                                nPerm = 1000, 
                                predSelect = FALSE,
                                parallel = FALSE) 

print(EO.gdm.importance)

# Relative percentage of predictor importance
(EO.gdm.importance$`Predictor Importance` * 100) / sum(EO.gdm.importance$`Predictor Importance`)

# Plot variable contribution
pdf("EO_GDM.pdf")
plot(EO.gdm.fit)
dev.off()

#For custom plotting
save(EO.gdm.fit, file="EO.gdm.fit.RData")

