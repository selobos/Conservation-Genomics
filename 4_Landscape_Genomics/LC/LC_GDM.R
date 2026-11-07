## Opening libraries
library(gdm)
library(dplyr)
library(magrittr)

setwd("~/Desktop/Conservation_Genomics/4_Landscape_Genomics/LC")

## Loading pairwise Fst dataset
LC.Fst.imp <- read.csv("~/Desktop/Conservation_Genomics/3_Population_Genomics/LC/LC_PairwiseFst_imp.csv")

# Get spatial coordinates and environmental variables
env <- read.csv("~/Desktop/Conservation_Genomics/2_Environmental_data/LC_env_2.csv")[, c(1,5:9)] %>% set_colnames(., c('Site','Easting','Northing','TWI','SAP','SpH')) #Northing(y, Latitude); Easting(x, Longitude)

# Convert Character Site Names to Numeric IDs 
site_map <- setNames(seq_len(nrow(LC.Fst.imp)), LC.Fst.imp$Site)
LC.Fst.imp$Site <- site_map[LC.Fst.imp$Site]
env$Site <- site_map[env$Site]

## Convert to gdm format
gdm.input <- formatsitepair(bioData = LC.Fst.imp, 
                            bioFormat = 3, 
                            siteColumn = "Site", 
                            XColumn = "Easting", 
                            YColumn = "Northing", 
                            predData = env)

## Fit GDM model
LC.gdm.fit<-gdm(gdm.input,geo=T)

summary(LC.gdm.fit) 

#Percentage of null deviance explained
LC.gdm.fit$explained 

#Quantify model significance and variable importance
LC.gdm.importance <- gdm.varImp(gdm.input, 
                                geo = T, 
                                nPerm = 1000, 
                                predSelect = FALSE,
                                parallel = FALSE) 

print(LC.gdm.importance)

# Relative percentage of predictor importance
(LC.gdm.importance$`Predictor Importance` * 100) / sum(LC.gdm.importance$`Predictor Importance`)

# Plot variable contribution
pdf("LC_GDM.pdf")
plot(LC.gdm.fit)
dev.off()

#For custom plotting
save(LC.gdm.fit, file="LC.gdm.fit.RData")

