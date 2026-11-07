library(ggplot2)
library(dplyr)
library(magrittr)
library(gdm)

setwd("~/Desktop/Conservation_Genomics/4_Landscape_Genomics/Spp")

## Banksia----
load("/Users/simon/Desktop/Conservation_Genomics/4_Landscape_Genomics/BM/BM.gdm.fit.RData")

#extract splines
BM.splines<-isplineExtract(BM.gdm.fit)

#example plot splines from variable sstmax
BM.splines.x<-as.data.frame(BM.splines$x)
BM.x.TWI <- BM.splines.x$TWI
BM.x.SAP <- BM.splines.x$SAP
BM.x.SpH <- BM.splines.x$SpH
BM.x.g <- BM.splines.x$Geographic

BM.splines.y<-as.data.frame(BM.splines$y)
BM.y.TWI <- BM.splines.y$TWI
BM.y.SAP <- BM.splines.y$SAP
BM.y.SpH <- BM.splines.y$SpH
BM.y.g <- BM.splines.y$Geographic

## Correa----
load("/Users/simon/Desktop/Conservation_Genomics/4_Landscape_Genomics/CX/CX.gdm.fit.RData")

#extract splines
CX.splines<-isplineExtract(CX.gdm.fit)

#example plot splines from variable sstmax
CX.splines.x<-as.data.frame(CX.splines$x)
CX.x.TWI <- CX.splines.x$TWI
CX.x.SAP <- CX.splines.x$SAP
CX.x.SpH <- CX.splines.x$SpH
CX.x.g <- CX.splines.x$Geographic

CX.splines.y<-as.data.frame(CX.splines$y)
CX.y.TWI <- CX.splines.y$TWI
CX.y.SAP <- CX.splines.y$SAP
CX.y.SpH <- CX.splines.y$SpH
CX.y.g <- CX.splines.y$Geographic

## Grevillea----
load("/Users/simon/Desktop/Conservation_Genomics/4_Landscape_Genomics/GA/GA.gdm.fit.RData")

#extract splines
GA.splines<-isplineExtract(GA.gdm.fit)

#example plot splines from variable sstmax
GA.splines.x<-as.data.frame(GA.splines$x)
GA.x.TWI <- GA.splines.x$TWI
GA.x.SAP <- GA.splines.x$SAP
GA.x.SpH <- GA.splines.x$SpH
GA.x.g <- GA.splines.x$Geographic

GA.splines.y<-as.data.frame(GA.splines$y)
GA.y.TWI <- GA.splines.y$TWI
GA.y.SAP <- GA.splines.y$SAP
GA.y.SpH <- GA.splines.y$SpH
GA.y.g <- GA.splines.y$Geographic

## Stenanthera----
load("/Users/simon/Desktop/Conservation_Genomics/4_Landscape_Genomics/SC/SC.gdm.fit.RData")

#extract splines
SC.splines<-isplineExtract(SC.gdm.fit)

#example plot splines from variable sstmax
SC.splines.x<-as.data.frame(SC.splines$x)
SC.x.TWI <- SC.splines.x$TWI
SC.x.SAP <- SC.splines.x$SAP
SC.x.SpH <- SC.splines.x$SpH
SC.x.g <- SC.splines.x$Geographic

SC.splines.y<-as.data.frame(SC.splines$y)
SC.y.TWI <- SC.splines.y$TWI
SC.y.SAP <- SC.splines.y$SAP
SC.y.SpH <- SC.splines.y$SpH
SC.y.g <- SC.splines.y$Geographic

###Env----
BM.TWI.spline.plot <- as.data.frame(cbind(BM.x.TWI,BM.y.TWI)) %>% set_colnames(., c('x','y')) %>% mutate(Spp = "BM")
BM.SAP.spline.plot <- as.data.frame(cbind(BM.x.SAP,BM.y.SAP)) %>% set_colnames(., c('x','y')) %>% mutate(Spp = "BM")
BM.SpH.spline.plot <- as.data.frame(cbind(BM.x.SpH,BM.y.SpH)) %>% set_colnames(., c('x','y')) %>% mutate(Spp = "BM") 

GA.TWI.spline.plot <- as.data.frame(cbind(GA.x.TWI,GA.y.TWI)) %>% set_colnames(., c('x','y')) %>% mutate(Spp = "GA") 
GA.SAP.spline.plot <- as.data.frame(cbind(GA.x.SAP,GA.y.SAP)) %>% set_colnames(., c('x','y')) %>% mutate(Spp = "GA")
GA.SpH.spline.plot <- as.data.frame(cbind(GA.x.SpH,GA.y.SpH)) %>% set_colnames(., c('x','y')) %>% mutate(Spp = "GA") 

CX.TWI.spline.plot <- as.data.frame(cbind(CX.x.TWI,CX.y.TWI)) %>% set_colnames(., c('x','y')) %>% mutate(Spp = "CX") 
CX.SAP.spline.plot <- as.data.frame(cbind(CX.x.SAP,CX.y.SAP)) %>% set_colnames(., c('x','y')) %>% mutate(Spp = "CX")
CX.SpH.spline.plot <- as.data.frame(cbind(CX.x.SpH,CX.y.SpH)) %>% set_colnames(., c('x','y')) %>% mutate(Spp = "CX") 

SC.TWI.spline.plot <- as.data.frame(cbind(SC.x.TWI,SC.y.TWI)) %>% set_colnames(., c('x','y')) %>% mutate(Spp = "SC") 
SC.SAP.spline.plot <- as.data.frame(cbind(SC.x.SAP,SC.y.SAP)) %>% set_colnames(., c('x','y')) %>% mutate(Spp = "SC")
SC.SpH.spline.plot <- as.data.frame(cbind(SC.x.SpH,SC.y.SpH)) %>% set_colnames(., c('x','y')) %>% mutate(Spp = "SC") 

CR.SAP.spline.plot <- data.frame(x = seq(0, 72.36003, length.out = 200), y = 0, Spp = "CR")

####TWI----
TWI.spline.plot <- rbind(BM.TWI.spline.plot, GA.TWI.spline.plot, CX.TWI.spline.plot, SC.TWI.spline.plot)

TWI.spline.plot %>%
  ggplot(aes(x = x, y = y, color = Spp, linetype = Spp)) +
  geom_line(linewidth = 1.5) +
  #scale_color_manual(values = coluors) +
  scale_linetype_manual(values = c(SC = "dotted", CX = "solid", 
                                   GA = "solid", BM = "aa")) +
  ylab("Cumulative Importance") +
  xlab("TWI") +
  ylim(0,0.06) +
  theme_minimal() +
  theme(legend.title=element_text(size=18, face="plain"),
        legend.position="bottom", legend.box = "horizontal",
        legend.text=element_text(size=10, face="plain"), 
        axis.text.x = element_text(colour="black", 
                                   vjust = 0.5, hjust = 0.5, size=21.5),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"),
        #margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
        axis.text.y = element_text(colour="black",size=21.5,face="plain"),
        axis.title.x = element_text(colour="black",size=25,face="plain"),
        axis.title.y = element_text(colour="black",size=25,face="plain")) +
  guides(fill = guide_colorbar(title = expression(paste("F",italic("ST"))),
                               label.position = "bottom",
                               title.position = "left", title.vjust = 0.8,
                               frame.colour = "black",
                               barwidth = 10,
                               barheight = 1.5,
                               ticks.colour = "grey30")) 

#ggsave(file = "TWI_GDM.spp.pdf", width = 7, height = 7, dpi = 300)
ggsave(file = "TWI_GDM.spp_scaled.pdf", width = 7, height = 7, dpi = 300)

####SAP----
SAP.spline.plot <- rbind(BM.SAP.spline.plot, GA.SAP.spline.plot, CX.SAP.spline.plot, SC.SAP.spline.plot, CR.SAP.spline.plot)

SAP.spline.plot %>%
  ggplot(aes(x = x, y = y, color = Spp, linetype = Spp)) +
  geom_line(linewidth = 1.5) +
  #scale_color_manual(values = coluors) +
  scale_linetype_manual(values = c(SC = "dotted", CX = "twodash", 
                                   GA = "longdash", BM = "aa", CR = "solid")) +
  ylab("Cumulative Importance") +
  xlab("SAP (mg/kg)") +
  ylim(0,0.06) +
  theme_minimal() +
  theme(legend.title=element_text(size=18, face="plain"),
        legend.position="bottom", legend.box = "horizontal",
        legend.text=element_text(size=10, face="plain"), 
        axis.text.x = element_text(colour="black", 
                                   vjust = 0.5, hjust = 0.5, size=21.5),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"),
        #margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
        axis.text.y = element_text(colour="black",size=21.5,face="plain"),
        axis.title.x = element_text(colour="black",size=25,face="plain"),
        axis.title.y = element_text(colour="black",size=25,face="plain")) +
  guides(fill = guide_colorbar(title = expression(paste("F",italic("ST"))),
                               label.position = "bottom",
                               title.position = "left", title.vjust = 0.8,
                               frame.colour = "black",
                               barwidth = 10,
                               barheight = 1.5,
                               ticks.colour = "grey30")) 

#ggsave(file = "SAP_GDM.spp.pdf", width = 7, height = 7, dpi = 300)
ggsave(file = "SAP_GDM.spp_scaled.pdf", width = 7, height = 7, dpi = 300)

####SpH----
SpH.spline.plot <- rbind(BM.SpH.spline.plot, GA.SpH.spline.plot, CX.SpH.spline.plot, SC.SpH.spline.plot)

SpH.spline.plot %>%
  ggplot(aes(x = x, y = y, color = Spp, linetype = Spp)) +
  geom_line(linewidth = 1.5) +
  #scale_color_manual(values = coluors) +
  scale_linetype_manual(values = c(SC = "dotted", CX = "twodash", 
                                   GA = "longdash", BM = "solid")) +
  ylab("Cumulative Importance") +
  xlab("SpH") +
  theme_minimal() +
  ylim(0,0.06) +
  theme(legend.title=element_text(size=18, face="plain"),
        legend.position="bottom", legend.box = "horizontal",
        legend.text=element_text(size=10, face="plain"), 
        axis.text.x = element_text(colour="black", 
                                   vjust = 0.5, hjust = 0.5, size=21.5),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"),
        #margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
        axis.text.y = element_text(colour="black",size=21.5,face="plain"),
        axis.title.x = element_text(colour="black",size=25,face="plain"),
        axis.title.y = element_text(colour="black",size=25,face="plain")) +
  guides(fill = guide_colorbar(title = expression(paste("F",italic("ST"))),
                               label.position = "bottom",
                               title.position = "left", title.vjust = 0.8,
                               frame.colour = "black",
                               barwidth = 10,
                               barheight = 1.5,
                               ticks.colour = "grey30")) 

#ggsave(file = "SpH_GDM.spp.pdf", width = 7, height = 7, dpi = 300)
ggsave(file = "SpH_GDM.spp_scaled.pdf", width = 7, height = 7, dpi = 300)


###Geography----
BM.g.spline.plot <- as.data.frame(cbind(BM.x.g,BM.y.g)) %>% set_colnames(., c('x','y')) %>% mutate(Spp = "BM")
CX.g.spline.plot <- as.data.frame(cbind(CX.x.g,CX.y.g)) %>% set_colnames(., c('x','y')) %>% mutate(Spp = "CX")
GA.g.spline.plot <- as.data.frame(cbind(GA.x.g,GA.y.g)) %>% set_colnames(., c('x','y')) %>% mutate(Spp = "GA")
SC.g.spline.plot <- as.data.frame(cbind(SC.x.g,SC.y.g)) %>% set_colnames(., c('x','y')) %>% mutate(Spp = "SC")

CR.g.spline.plot <- data.frame(x = seq(0, 77080.74, length.out = 200), y = 0, Spp = "CR")
EI.g.spline.plot <- data.frame(x = seq(0, 77080.74, length.out = 200), y = 0, Spp = "EI")
EO.g.spline.plot <- data.frame(x = seq(0, 77080.74, length.out = 200), y = 0, Spp = "EO")
LC.g.spline.plot <- data.frame(x = seq(0, 77080.74, length.out = 200), y = 0, Spp = "LC")
XA.g.spline.plot <- data.frame(x = seq(0, 77080.74, length.out = 200), y = 0, Spp = "XA")

g.spline.plot <- rbind(SC.g.spline.plot, CX.g.spline.plot, CR.g.spline.plot,
                       GA.g.spline.plot, EO.g.spline.plot, LC.g.spline.plot, 
                       XA.g.spline.plot, EI.g.spline.plot, BM.g.spline.plot)

g.spline.plot %>%
  ggplot(aes(x = x, y = y, color = Spp, linetype = Spp)) +
  geom_line(linewidth = 1.5) +
  #scale_color_manual(values = coluors) +
  scale_linetype_manual(values = c(SC = "dotted", CX = "twodash", CR = "solid", 
                                   GA = "longdash", EO = "solid", LC = "solid",
                                   XA = "solid", EI = "solid", BM = "aa")) +
  ylab("Cumulative Importance") +
  xlab("Geography (km)") +
  theme_minimal() +
  ylim(0,0.06) + 
  theme(legend.title=element_text(size=18, face="plain"),
        legend.position="bottom", legend.box = "horizontal",
        legend.text=element_text(size=10, face="plain"), 
        axis.text.x = element_text(colour="black", 
                                   vjust = 0.5, hjust = 0.5, size=21.5),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"),
        #margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
        axis.text.y = element_text(colour="black",size=21.5,face="plain"),
        axis.title.x = element_text(colour="black",size=25,face="plain"),
        axis.title.y = element_text(colour="black",size=25,face="plain")) +
  guides(fill = guide_colorbar(title = expression(paste("F",italic("ST"))),
                               label.position = "bottom",
                               title.position = "left", title.vjust = 0.8,
                               frame.colour = "black",
                               barwidth = 10,
                               barheight = 1.5,
                               ticks.colour = "grey30")) 

#ggsave(file = "Geography_GDM.pdf", width = 7, height = 7, dpi = 300)
ggsave(file = "Geography_GDM_scaled.pdf", width = 7, height = 7, dpi = 300)






