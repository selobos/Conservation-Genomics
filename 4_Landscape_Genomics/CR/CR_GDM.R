## Opening libraries
library(gdm)
library(dplyr)
library(magrittr)

setwd("~/Desktop/Conservation_Genomics/4_Landscape_Genomics/CR")

## Loading pairwise Fst dataset
CR.Fst.imp <- read.csv("~/Desktop/Conservation_Genomics/3_Population_Genomics/CR/CR_PairwiseFst_imp.csv")

# Get spatial coordinates and environmental variables
env <- read.csv("~/Desktop/Conservation_Genomics/2_Environmental_data/CR_env_2.csv")[, c(1,5:9)] %>% set_colnames(., c('Site','Easting','Northing','TWI','SAP','SpH')) #Northing(y, Latitude); Easting(x, Longitude)

# Convert Character Site Names to Numeric IDs 
site_map <- setNames(seq_len(nrow(CR.Fst.imp)), CR.Fst.imp$Site)
CR.Fst.imp$Site <- site_map[CR.Fst.imp$Site]
env$Site <- site_map[env$Site]

## Convert to gdm format
gdm.input <- formatsitepair(bioData = CR.Fst.imp, 
                            bioFormat = 3, 
                            siteColumn = "Site", 
                            XColumn = "Easting", 
                            YColumn = "Northing", 
                            predData = env)

## Fit GDM model
CR.gdm.fit<-gdm(gdm.input,geo=T)

summary(CR.gdm.fit) 

#Percentage of null deviance explained
CR.gdm.fit$explained 

#Quantify model significance and variable importance
CR.gdm.importance <- gdm.varImp(gdm.input, 
                                geo = T, 
                                nPerm = 1000, 
                                predSelect = FALSE,
                                parallel = FALSE) 

print(CR.gdm.importance)

# Relative percentage of predictor importance
(CR.gdm.importance$`Predictor Importance` * 100) / sum(CR.gdm.importance$`Predictor Importance`)

# Plot variable contribution
pdf("CR_GDM.pdf")
plot(CR.gdm.fit)
dev.off()

#For custom plotting
save(CR.gdm.fit, file="CR.gdm.fit.RData")

