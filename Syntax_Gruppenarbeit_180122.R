##############################################
# Gruppenarbeit CSS, USL Gruppe 6
##############################################
# vorgelegt von Alisa Naumann, Ronja Plendl, Simon Hübner

# Datensatz: Mikrozensus 2010
# Explorative Untersuchung Berufsgruppen und Lebensumstände // hierarchische Clusteranalyse (+Hauptkomponentenanalye/PCA?)
# Es geht um die Berufe der Haupteinkommensbezieher eines Haushalts und deren Wohnsituation

#library('descr')  
#library('ggplot2')
#library('stargazer')
#library('factoextra')
#library('FactoMineR')
#library("corrplot")
#library('datasets')
#library ('dotwhisker')


setwd("F:/Gruppenarbeit Dateien/Gruppenarbeit_2022_Projekt/Data/")
getwd()

install.packages('haven')
library(haven)
Mikrozensus <- read_dta('mz2010_cf.dta')

#Subdatensatz mit (eventuell) relevanten Variablen ### Eventuell nochmal anpassen an tatsächlich verwendete Variablen

MZsubCA <-Mikrozensus[,c("ef1", "ef136", "ef137", "ef830", "ef310", "ef312", "ef44", "ef46", "ef131", "ef739",
                          "ef737", "ef742", "ef743", "ef745", "ef707", "ef492", "ef638", "ef663", "ef669", "ef770", "ef667", "ef736")]

# einzelne Variablen betrachten
install.packages('summarytools')
library(summarytools)

freq(MZsubCA$ef136) 
freq(MZsubCA$ef739) # Clusteranalyse lieber auf Haupteinkommensbezieher beziehen? Eigentlich fast sinnvoller, oder?

###### VARIABLEN #########################################
#LandD <- Mikrozensus$ef1

#### BEFRAGTER
#Beruf <- Mikrozensus$ef136
#Wirtschaftszweig <- Mikrozensus$ef137
#Einkommen <- Mikrozensus$ef830

#hSchulabschluss <- Mikrozensus$ef310
#hAbschluss <- Mikrozensus$ef312
#Geschlecht <- Mikrozensus$ef44

#Alter <- Mikrozensus$ef46
#Arbeitszeit <- Mikrozensus$ef131    # viel NA (52%) - trotzdem reinnehmen?

#### HAUPTEINKOMMENSBEZIEHER
#BerufHEB <- Mikrozensus$ef739
#WirtschaftszweigHEB <- Mikrozensus$ef737
#NettoeinkommenHEB <- Mikrozensus$ef742

#hSchulabschlussHEB <- Mikrozensus$ef743
#hAbschlussHEB <- Mikrozensus$ef745
#GeschlechtHEB <- Mikrozensus$ef731

#AlterHEB <- Mikrozensus$ef         # Gibts nicht?
#GeschlechtHEB <- Mikrozensus$ef    # Gibts nicht?
#ArbeitszeitHEB <- Mikrozensus$ef   # Gibts nicht?

#### HAUSHALT
#Haushaltseinkommen <- Mikrozensus$ef707
#Wohnraumgröße <- Mikrozensus$ef492
#QuadratmeterMiete <- Mikrozensus$ef638   # 57% NA - trotzdem interessant?
#Haushaltsgröße <- Mikrozensus$ef663
#AnzahlKinderHH <- Mikrozensus$ef669
#AnzahlKinderInsges <- Mikrozensus$ef770
#AnzahlEinkommensbezieher <- Mikrozensus$ef667
#Lebensformenkonzept <- Mikrozensus$ef736   # 4 Lebensformkonzepte, vielleicht auch interessant für eine Untersuchung?









