## Opening libraries
library(gdm)
library(dplyr)
library(magrittr)

setwd("~/Desktop/Conservation_Genomics/4_Landscape_Genomics/SC")

## Loading pairwise Fst dataset
SC.Fst.imp <- read.csv("~/Desktop/Conservation_Genomics/3_Population_Genomics/SC/SC_PairwiseFst_imp.csv")

# Get spatial coordinates and environmental variables
env <- read.csv("~/Desktop/Conservation_Genomics/2_Environmental_data/SC_env_2.csv")[, c(1,5:9)] %>% set_colnames(., c('Site','Easting','Northing','TWI','SAP','SpH')) #Northing(y, Latitude); Easting(x, Longitude)

# Convert Character Site Names to Numeric IDs 
site_map <- setNames(seq_len(nrow(SC.Fst.imp)), SC.Fst.imp$Site)
SC.Fst.imp$Site <- site_map[SC.Fst.imp$Site]
env$Site <- site_map[env$Site]

## Convert to gdm format
gdm.input <- formatsitepair(bioData = SC.Fst.imp, 
                            bioFormat = 3, 
                            siteColumn = "Site", 
                            XColumn = "Easting", 
                            YColumn = "Northing", 
                            predData = env)

## Fit GDM model
SC.gdm.fit<-gdm(gdm.input,geo=T)

summary(SC.gdm.fit) 

#Percentage of null deviance explained
SC.gdm.fit$explained 

#Quantify model significance and variable importance
SC.gdm.importance <- gdm.varImp(gdm.input, 
                                geo = T, 
                                nPerm = 1000, 
                                predSelect = FALSE,
                                parallel = FALSE) 

print(SC.gdm.importance)

# Relative percentage of predictor importance
(SC.gdm.importance$`Predictor Importance` * 100) / sum(SC.gdm.importance$`Predictor Importance`)

# Plot variable contribution
pdf("SC_GDM.pdf")
plot(SC.gdm.fit)
dev.off()

#For custom plotting
save(SC.gdm.fit, file="SC.gdm.fit.RData")

