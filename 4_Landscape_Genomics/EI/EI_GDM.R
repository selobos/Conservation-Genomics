## Opening libraries
library(gdm)
library(dplyr)
library(magrittr)

setwd("~/Desktop/Conservation_Genomics/4_Landscape_Genomics/EI")

## Loading pairwise Fst dataset
EI.Fst.imp <- read.csv("~/Desktop/Conservation_Genomics/3_Population_Genomics/EI/EI_PairwiseFst_imp.csv")

# Get spatial coordinates and environmental variables
env <- read.csv("~/Desktop/Conservation_Genomics/2_Environmental_data/EI_env_2.csv")[, c(1,5:9)] %>% set_colnames(., c('Site','Easting','Northing','TWI','SAP','SpH')) #Northing(y, Latitude); Easting(x, Longitude)

# Convert Character Site Names to Numeric IDs 
site_map <- setNames(seq_len(nrow(EI.Fst.imp)), EI.Fst.imp$Site)
EI.Fst.imp$Site <- site_map[EI.Fst.imp$Site]
env$Site <- site_map[env$Site]

## Convert to gdm format
gdm.input <- formatsitepair(bioData = EI.Fst.imp, 
                            bioFormat = 3, 
                            siteColumn = "Site", 
                            XColumn = "Easting", 
                            YColumn = "Northing", 
                            predData = env)

## Fit GDM model
EI.gdm.fit<-gdm(gdm.input,geo=T)

summary(EI.gdm.fit) 

#Percentage of null deviance explained
EI.gdm.fit$explained 

#Quantify model significance and variable importance
EI.gdm.importance <- gdm.varImp(gdm.input, 
                                geo = T, 
                                nPerm = 1000, 
                                predSelect = FALSE,
                                parallel = FALSE) 

print(EI.gdm.importance)

# Relative percentage of predictor importance
(EI.gdm.importance$`Predictor Importance` * 100) / sum(EI.gdm.importance$`Predictor Importance`)

# Plot variable contribution
pdf("EI_GDM.pdf")
plot(EI.gdm.fit)
dev.off()

#For custom plotting
save(EI.gdm.fit, file="EI.gdm.fit.RData")

