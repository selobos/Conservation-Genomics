## Opening libraries
library(gdm)
library(dplyr)
library(magrittr)

setwd("~/Desktop/Conservation_Genomics/4_Landscape_Genomics/XA")

## Loading pairwise Fst dataset
XA.Fst.imp <- read.csv("~/Desktop/Conservation_Genomics/3_Population_Genomics/XA/XA_PairwiseFst_imp.csv")

# Get spatial coordinates and environmental variables
env <- read.csv("~/Desktop/Conservation_Genomics/2_Environmental_data/XA_env_2.csv")[, c(1,5:9)] %>% set_colnames(., c('Site','Easting','Northing','TWI','SAP','SpH')) #Northing(y, Latitude); Easting(x, Longitude)

# Convert Character Site Names to Numeric IDs 
site_map <- setNames(seq_len(nrow(XA.Fst.imp)), XA.Fst.imp$Site)
XA.Fst.imp$Site <- site_map[XA.Fst.imp$Site]
env$Site <- site_map[env$Site]

## Convert to gdm format
gdm.input <- formatsitepair(bioData = XA.Fst.imp, 
                            bioFormat = 3, 
                            siteColumn = "Site", 
                            XColumn = "Easting", 
                            YColumn = "Northing", 
                            predData = env)

## Fit GDM model
XA.gdm.fit<-gdm(gdm.input,geo=T)

summary(XA.gdm.fit) 

#Percentage of null deviance explained
XA.gdm.fit$explained 

#Quantify model significance and variable importance
XA.gdm.importance <- gdm.varImp(gdm.input, 
                                geo = T, 
                                nPerm = 1000, 
                                predSelect = FALSE,
                                parallel = FALSE) 

print(XA.gdm.importance)

# Relative percentage of predictor importance
(XA.gdm.importance$`Predictor Importance` * 100) / sum(XA.gdm.importance$`Predictor Importance`)

# Plot variable contribution
pdf("XA_GDM.pdf")
plot(XA.gdm.fit)
dev.off()

#For custom plotting
save(XA.gdm.fit, file="XA.gdm.fit.RData")

