# ok lets go 
x <- c("1","2","3")
# Test test

# Also dann los gehts
# was passiert denn, wenn ich hier editiere - könnt ihr das sehen?
# Y/N?

# Antwort von Ronja/Alisa:
# alternativ Github-Kommentar????????????





# Gruppenarbeit Gruppe 6 
# vorgelegt von Alisa Naumann, Ronja Plendl, Simon Hübner

# Datensatz: Mirkozensus 2010
# Explorative Untersuchung Berufsgruppen und Lebensumstände // hierarchische Clusteranalyse (+Hauptkomponentenanalye/PCA?)


#install.packages('descr')
#install.packages('ggplot2')
#install.packages("stargazer")
#install.packages('factoextra')
#install.packages('FactoMineR')
#install.packages('corrplot')
#install.packages('datasets')
#install.packages('dotwhisker')


library('descr')  
library('ggplot2')
library('stargazer')
library('factoextra')
library('FactoMineR')
library("corrplot")
library('datasets')
library ('dotwhisker')


setwd("F:/Gruppenarbeit Dateien/Gruppenarbeit_2022_Projekt/Data/")
getwd()

install.packages('haven')
library(haven)
Zensus <- read_dta('mz2010_cf.dta')


# Beruf (136) (oder Wirtschaftszweig (137))
# Einkommen (830), bzw. Haushaltseinkommen (707)
# Arbeitszeit (217)
# Wohnraumgröße (492)
# Quadratmeter-Miete (638)
# Haushaltsgröße (663)
# Anzahl Kinder im Haushalt (u18, u27 und ü27?) (669)
# Anzahl Einkommensbezieher (667)
# höchster Bildungsabschluss (540)
# (+ Geschlecht (44), Alter (46))
