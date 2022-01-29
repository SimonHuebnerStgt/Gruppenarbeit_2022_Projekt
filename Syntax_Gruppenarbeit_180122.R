##############################################
# Gruppenarbeit CSS, USL Gruppe 6
##############################################
# vorgelegt von Alisa Naumann, Ronja Plendl, Simon Hübner

# Datensatz: Mirkozensus 2010
# Explorative Untersuchung Berufsgruppen und Lebensumstände // hierarchische Clusteranalyse (+Hauptkomponentenanalye/PCA?)
# Es geht um die Berufe der Haupteinkommensbezieher eines Haushalts und deren Wohnsituation

library('descr')  
library('ggplot2')
library('stargazer')
library('factoextra')
library('FactoMineR')
library("corrplot")
library('datasets')
library ('dotwhisker')
library('summarytools')


setwd("/Users/alisanaumann/Desktop/Gruppenarbeit_CSS")
getwd()

install.packages('haven')
library(haven)
Mikrozensus <- read_dta('mz2010_cf.dta')


#Subdatensatz mit (eventuell) relevanten Variablen ### Eventuell nochmal anpassen an tatsächlich verwendete Variablen

MZsubCA <-Mikrozensus[,c( "ef1",                                                                       #"ef136", "ef310", "ef312", "ef44", "ef46", "ef131",
                         "ef739", "ef742", "ef745", "ef731", "ef734",
                         "ef707", "ef492", "ef638", "ef663", "ef669", "ef770", "ef667")]

str(MZsubCA)

#### Befragter
#MZsubCA$Beruf                     <- MZsubCA$ef136
#MZsubCA$Einkommen                 <- MZsubCA$ef830 
#MZsubCA$Schulabschluss            <- MZsubCA$ef310
#MZsubCA$hoechsterAbschluss        <- MZsubCA$ef312 
#MZsubCA$Geschlecht                <- MZsubCA$ef44 
#MZsubCA$Alter                     <- MZsubCA$ef46 

#### Haupteinkommensbezieher
MZsubCA$Land                      <- MZsubCA$ef1

MZsubCA$BerufHEB                  <- MZsubCA$ef739
MZsubCA$EinkommenHEB              <- MZsubCA$ef742
MZsubCA$AbschlussHEB              <- MZsubCA$ef745
MZsubCA$GeschlechtHEB             <- MZsubCA$ef731
MZsubCA$StaatsangehörigkeitHEB    <- MZsubCA$ef734

#### Haushalt
MZsubCA$Haushaltseinkommen        <- MZsubCA$ef707 
MZsubCA$Wohnraumgroeße            <- MZsubCA$ef492 
MZsubCA$QuadratmeterMiete         <- MZsubCA$ef638 
MZsubCA$Haushaltsgroeße           <- MZsubCA$ef663 
MZsubCA$AnzahlKinderHH            <- MZsubCA$ef669
MZsubCA$AnzahlKinderInsges        <- MZsubCA$ef770 
MZsubCA$AnzahlEinkommensbezieher  <- MZsubCA$ef667 


# einzelne Variablen betrachten/umcodieren

install.packages('summarytools')
library(summarytools)
library(car)             # umcodieren von Variablen

MZsubCA$Land <- recode(MZsubCA$Land, "11=2")  #West = 1, Ost = 0
freq(MZsubCA$Land)

freq(MZsubCA$BerufHEB)

MZsubCA$EinkommenHEB <- recode(MZsubCA$EinkommenHEB, "50=NA;90=NA;99=NA") #selbstständiger Landwirt, kein Einkommen, ohne Angabe = NA
freq(MZsubCA$EinkommenHEB)

MZsubCA$AbschlussHEB <- recode(MZsubCA$AbschlussHEB, "11=1;21=2;31=3;32=4;33=5;41=6;51=7;52=8;60=9") #ordinale 9-stufige Skala für Abschluss
freq(MZsubCA$AbschlussHEB)


CA <-MZsubCA[,c("Land", "BerufHEB", "EinkommenHEB", "AbschlussHEB", "GeschlechtHEB", "StaatsangehörigkeitHEB",
                   "Haushaltseinkommen", "Wohnraumgroeße", "QuadratmeterMiete", "Haushaltsgroeße",
                   "AnzahlKinderHH", "AnzahlKinderInsges", "AnzahlEinkommensbezieher")]

summary(CA)
names(CA)





# Skalieren

CA1 <- scale(xxxx) #### folgt noch

# Ellbogenkriterium / Silhouette

fviz_nbclust(CA1, hcut, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method")

fviz_nbclust(CA1, hcut, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Distanzmatrix

d1 <- dist(CA1, method = 'euclidean') # check different distances with: 
?dist

head(as.matrix(d1))

# Dendrogramm

hc.compl <- hclust(d1, method="complete")
fviz_dend(hc.compl)






