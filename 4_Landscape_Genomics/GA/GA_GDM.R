## Opening libraries
library(gdm)
library(dplyr)
library(magrittr)

setwd("~/Desktop/Conservation_Genomics/4_Landscape_Genomics/GA")

## Loading pairwise Fst dataset
GA.Fst.imp <- read.csv("~/Desktop/Conservation_Genomics/3_Population_Genomics/GA/GA_PairwiseFst_imp.csv")

# Get spatial coordinates and environmental variables
env <- read.csv("~/Desktop/Conservation_Genomics/2_Environmental_data/GA_env_2.csv")[, c(1,5:9)] %>% set_colnames(., c('Site','Easting','Northing','TWI','SAP','SpH')) #Northing(y, Latitude); Easting(x, Longitude)

# Convert Character Site Names to Numeric IDs 
site_map <- setNames(seq_len(nrow(GA.Fst.imp)), GA.Fst.imp$Site)
GA.Fst.imp$Site <- site_map[GA.Fst.imp$Site]
env$Site <- site_map[env$Site]

## Convert to gdm format
gdm.input <- formatsitepair(bioData = GA.Fst.imp, 
                            bioFormat = 3, 
                            siteColumn = "Site", 
                            XColumn = "Easting", 
                            YColumn = "Northing", 
                            predData = env)

## Fit GDM model
GA.gdm.fit<-gdm(gdm.input,geo=T)

summary(GA.gdm.fit) 

#Percentage of null deviance explained
GA.gdm.fit$explained 

#Quantify model significance and variable importance
GA.gdm.importance <- gdm.varImp(gdm.input, 
                                geo = T, 
                                nPerm = 1000, 
                                predSelect = FALSE,
                                parallel = FALSE) 

print(GA.gdm.importance)

# Relative percentage of predictor importance
(GA.gdm.importance$`Predictor Importance` * 100) / sum(GA.gdm.importance$`Predictor Importance`)

# Plot variable contribution
pdf("GA_GDM.pdf")
plot(GA.gdm.fit)
dev.off()

#For custom plotting
save(GA.gdm.fit, file="GA.gdm.fit.RData")

