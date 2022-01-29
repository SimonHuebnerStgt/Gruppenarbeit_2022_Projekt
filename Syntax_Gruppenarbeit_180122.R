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


#Subdatensatz mit relevanten Variablen

MZsubCA <-Mikrozensus[,c( "ef1",                                                                       #"ef136", "ef310", "ef312", "ef44", "ef46", "ef131",
                         "ef739", "ef742", "ef745", "ef731", "ef734",
                         "ef707", "ef492", "ef638", "ef663", "ef669", "ef770", "ef667", "ef491")]

str(MZsubCA)

#MZsubCA$Beruf                     <- MZsubCA$ef136
#MZsubCA$Einkommen                 <- MZsubCA$ef830 
#MZsubCA$Schulabschluss            <- MZsubCA$ef310
#MZsubCA$hoechsterAbschluss        <- MZsubCA$ef312 
#MZsubCA$Geschlecht                <- MZsubCA$ef44 
#MZsubCA$Alter                     <- MZsubCA$ef46 

MZsubCA$Land                      <- MZsubCA$ef1

MZsubCA$BerufHEB                  <- MZsubCA$ef739
MZsubCA$EinkommenHEB              <- MZsubCA$ef742
MZsubCA$AbschlussHEB              <- MZsubCA$ef745
MZsubCA$GeschlechtHEB             <- MZsubCA$ef731
MZsubCA$StaatsangehoerigkeitHEB    <- MZsubCA$ef734

MZsubCA$Haushaltseinkommen        <- MZsubCA$ef707 
MZsubCA$Wohnraumgroeße            <- MZsubCA$ef492 
MZsubCA$QuadratmeterMiete         <- MZsubCA$ef638 
MZsubCA$Haushaltsgroeße           <- MZsubCA$ef663 
MZsubCA$AnzahlKinderHH            <- MZsubCA$ef669
MZsubCA$AnzahlKinderInsges        <- MZsubCA$ef770 
MZsubCA$AnzahlEinkommensbezieher  <- MZsubCA$ef667 
MZsubCA$Wohneigentum              <- MZsubCA$ef491   #Wohnhaft in eigenem Gebäude/eigener Wohnung/HauptmieterIn/UntermieterIn

# einzelne Variablen betrachten/umcodieren

install.packages('summarytools')
library(summarytools)
library(car)             # umcodieren von Variablen

MZsubCA$BerufHEB <- recode(MZsubCA$BerufHEB, "999=NA") #Berufe ohne Angabe = NA
freq(MZsubCA$BerufHEB)

MZsubCA$Land <- recode(MZsubCA$Land, "11=0")  #West = 1, Ost = 0
freq(MZsubCA$Land)

MZsubCA$EinkommenHEB <- recode(MZsubCA$EinkommenHEB, "50=NA;90=NA;99=NA") #selbstständiger Landwirt, kein Einkommen, ohne Angabe = NA
freq(MZsubCA$EinkommenHEB)

MZsubCA$AbschlussHEB <- recode(MZsubCA$AbschlussHEB, "11=1;21=2;31=3;32=4;33=5;41=6;51=7;52=8;60=9") #ordinale 9-stufige Skala für Abschluss
freq(MZsubCA$AbschlussHEB)

MZsubCA$GeschlechtHEB <- recode(MZsubCA$GeschlechtHEB, "2=0")  #Männlich=1, Weiblich=0
freq(MZsubCA$GeschlechtHEB)

MZsubCA$StaatsangehoerigkeitHEB <- recode(MZsubCA$StaatsangehoerigkeitHEB, "2=0")  #Deutsch=1, Andere=0
freq(MZsubCA$StaatsangehoerigkeitHEB)   

MZsubCA$Haushaltseinkommen <- recode(MZsubCA$Haushaltseinkommen, "50=NA;99=NA") #selbstständiger Landwirt, ohne Angabe = NA
freq(MZsubCA$Haushaltseinkommen)

MZsubCA$Wohnraumgroeße <- recode(MZsubCA$Wohnraumgroeße, "999=NA") #Wohnraumgröße ohne Angabe = NA
freq(MZsubCA$Wohnraumgroeße)

freq(MZsubCA$Haushaltsgroeße)
freq(MZsubCA$AnzahlKinderHH)
freq(MZsubCA$AnzahlKinderInsges)
freq(MZsubCA$AnzahlEinkommensbezieher)

freq(MZsubCA$QuadratmeterMiete) ### fällt raus, da viel NA und Personen die keine Miete zahlen - Stattdessen Wohneigentum?

MZsubCA$Wohneigentum <- recode(MZsubCA$Wohneigentum, "1=4;2=3;3=2;4=1") #umcodiert, sodass aufsteigend von Untermieter bis Gebäudeeigentümer
freq(MZsubCA$Wohneigentum)


CA <-MZsubCA[,c("Land", "BerufHEB", "EinkommenHEB", "AbschlussHEB", "GeschlechtHEB", "StaatsangehoerigkeitHEB",
                   "Haushaltseinkommen", "Wohnraumgroeße", "Haushaltsgroeße",
                   "AnzahlKinderHH", "AnzahlKinderInsges", "AnzahlEinkommensbezieher", "Wohneigentum")]

summary(CA)

## Tabelle für CA müsste vorbereitet werden



# Skalieren

CA1 <- scale(CA)

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


