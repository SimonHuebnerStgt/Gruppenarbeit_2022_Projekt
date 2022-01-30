#Alisa Aktuell: 

##############################################
# Gruppenarbeit CSS, USL Gruppe 6
##############################################
# vorgelegt von Alisa Naumann, Ronja Plendl, Simon Hübner

# Datensatz: Mikrozensus 2010
# Explorative Untersuchung Berufsgruppen und Lebensumstände // hierarchische Clusteranalyse (+Hauptkomponentenanalye/PCA?)
# Es geht um die Berufe der Haupteinkommensbezieher eines Haushalts und deren Wohn- und Lebenssituation

# Vorüberlegungen und Methodik: 
# Mikrozensus erfasst die beruflichen und privaten Lebensumstände der deutschen Bevölkerung
# Der Datensatz beinhaltet 23374 Fälle und schlüsselt verschiedene Lebensbereiche der Befragten detailliert auf
# Auffallend ist die Erfassung von über 100 verschiedenen Berufen bzw. Berufsgruppen (113?)

# Als Forschungskontext der Untersuchung interessierte uns daher folgendes:
# Welche Jobs bzw. Branchen ähneln sich in der Lebensführung /-umstände der Beschäftigten? 
# Welche Gruppen bzw. soziale Entitäten lassen sich bilden?  


library('descr')  
library('ggplot2')
library('stargazer')
library('factoextra')
library('FactoMineR')
library("corrplot")
library('datasets')
library ('dotwhisker')
library('summarytools')


setwd("E:/Gruppenarbeit Dateien/Gruppenarbeit_2022_Projekt/Data")
getwd()

#install.packages('haven')
library(haven)
Mikrozensus <- read_dta('mz2010_cf.dta')


#Subdatensatz mit relevanten Variablen [ist hier nur zur Übersicht drin oder?]

MZsubCA <-Mikrozensus[,c( "ef1",                                                                       #"ef136", "ef310", "ef312", "ef44", "ef46", "ef131",
                          "ef739", "ef742", "ef745", "ef731", "ef734",
                          "ef707", "ef492", "ef638", "ef663", "ef669", "ef770", "ef667", "ef491")]
variable.names(MZsubCA)
str(MZsubCA)

#### Befragter - aktuell rauslassen, wird aber ggf. nochmal relevant 
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
MZsubCA$StaatsangehoerigkeitHEB   <- MZsubCA$ef734

MZsubCA$Haushaltseinkommen        <- MZsubCA$ef707 
MZsubCA$Wohnraumgroesse            <- MZsubCA$ef492 
MZsubCA$QuadratmeterMiete         <- MZsubCA$ef638 
MZsubCA$Haushaltsgroesse           <- MZsubCA$ef663 
MZsubCA$AnzahlKinderHH            <- MZsubCA$ef669
MZsubCA$AnzahlKinderInsges        <- MZsubCA$ef770 
MZsubCA$AnzahlEinkommensbezieher  <- MZsubCA$ef667 
MZsubCA$Wohneigentum              <- MZsubCA$ef491   #Wohnhaft in eigenem Gebäude/eigener Wohnung/HauptmieterIn/UntermieterIn


############# Deskriptiver Teil ######################

# Für Fragestellung relevante  Variablen betrachten und ggf. umcodieren

# Relevante Packages:
# install.packages('summarytools')
library(summarytools)
# install.packages('car')
library(car)             # umcodieren von Variablen

MZsubCA$BerufHEB <- recode(MZsubCA$BerufHEB, "999=NA") #Berufe ohne Angabe = NA
freq(MZsubCA$BerufHEB)
table(MZsubCA$BerufHEB)

# Wollen wir hier gleich deskriptiv ein paar Sachen raus ziehen?                        ###Ja, stimmt, ich denke auch wir sollten die Berufsgruppen besser noch einmal betrachten
# z.B.: mit 694 Fällen ist der größte Berufsbereich: "Architekten, Ingenieure und verwandte Wissenschaftler"
# [edit: ÄHM was ist denn das für ne umfassende Gruppe?! Die könnten wir uns ggf. mal genauer anschauen]

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

MZsubCA$Wohnraumgroesse <- recode(MZsubCA$Wohnraumgroesse, "999=NA") #Wohnraumgröße ohne Angabe = NA
freq(MZsubCA$Wohnraumgroesse)

freq(MZsubCA$Haushaltsgroesse)
freq(MZsubCA$AnzahlKinderHH)
freq(MZsubCA$AnzahlKinderInsges)
freq(MZsubCA$AnzahlEinkommensbezieher)

freq(MZsubCA$QuadratmeterMiete) ### fällt raus, da viel NA und Personen die keine Miete zahlen - Stattdessen Wohneigentum?
# [gute Ergänzung!]

MZsubCA$Wohneigentum <- recode(MZsubCA$Wohneigentum, "1=4;2=3;3=2;4=1") #umcodiert, sodass aufsteigend von Untermieter bis Gebäudeeigentümer
freq(MZsubCA$Wohneigentum)

# [Alternativ: nur Eigentum vs Miete?]         ###Sehr gut, find ich auch
MZsubCA$WohneigentumJaNein <- recode(MZsubCA$Wohneigentum, "1=1 ; 2=1 ; 3=2 ; 4=2 ; 9=NA") 
# umcodiert, wäre dann1= Eigentum und 2= Miete
freq(MZsubCA$WohneigentumJaNein)


CA <-MZsubCA[,c("Land", "BerufHEB", "EinkommenHEB", "AbschlussHEB", "GeschlechtHEB", "StaatsangehoerigkeitHEB",
                "Haushaltseinkommen", "Wohnraumgroesse", "Haushaltsgroesse",
                "AnzahlKinderHH", "AnzahlKinderInsges", "AnzahlEinkommensbezieher", "WohneigentumJaNein")]

CAnoNA <- na.omit(CA)

#Betrachten der Variable Berufe - Häufigkeiten, ggf. Berufsgruppen entfernen?
freq(CA$BerufHEB)
freq(CAnoNA$BerufHEB)
CAnoNA$BerufHEB          #811 (mineralaufbereitung/keramikherstellung),825 (Maschienenbediener für Papiererzeugnisse),834 (Deckpersonal auf Schiffen) - unter 5
                         #111 (Angehörige gesetzgebender Körperschaften), 348 (Ordensbrüder und Seelsorger), 612 (tierwirtschaftliche Berufe) - unter 10
                         #ansonsten alle Berufe vertreten, oder?


#### Tabelle mit Means der Berufsgruppen je Variable
CAmeans <- aggregate(cbind(Land, EinkommenHEB, AbschlussHEB, GeschlechtHEB, StaatsangehoerigkeitHEB,
                           Haushaltseinkommen, Wohnraumgroesse, Haushaltsgroesse,
                           AnzahlKinderHH, AnzahlKinderInsges, AnzahlEinkommensbezieher, WohneigentumJaNein) ~ BerufHEB, CA, mean)

CAmeans$BerufHEB
freq(CAmeans$BerufHEB)

###nicht die schnellste und sauberste Variante, braucht eventuell einen Gegencheck :)
rownames(CAmeans) <- c( "11","111","114","120","121","122", "123", "130","131","211","212","213","214","221","222","231","232","233","234","235",
                        "241","242","243","244","245","246","247","311","312","313","314","315","321","322","323","332","333","334",
                        "341","342","343","344","345","346","347","348",
                        "411","412","413","414","419","421","422","511","512","513","514","516","521","610","611","612","614","711","712","713","714",
                        "721","722","723","724","731","732","734","741","742","743","744","811","812","814","815","816","821","822","823","825","826","827","828","829",
                        "831","832","833","834","911","913","914","915","921","931","932","933")
                  
rownames(CAmeans)


#CAmeans$BerufHEB <- as.character(CAmeans$BerufHEB)  #hmmm, kann man die Zeilen irgendwie benennen? Wäre cool für die Übersicht/Grafiken


######### Testdatensätze von Simon
#[Test Vergleich weniger Variablen]
#Test1 <-MZsubCA[,c("BerufHEB", "Haushaltseinkommen", "Wohnraumgroeße", "Haushaltsgroeße",
#                "AnzahlKinderHH",  "Wohneigentum")]
#Test2 <-MZsubCA[,c("Haushaltseinkommen", "Wohnraumgroeße", "Haushaltsgroeße",
#                                 "AnzahlKinderHH",  "Wohneigentum")]
#
# [Die letzte Überlegung für mich heute war: Wenn wir analog zu diesem Swiss Datensatz arbeiten wollen,
# müssen wir dann nicht den ganzen Datensatz nach den BerufHEB "gruppieren"? (sind etwas über 100)]


#### Clusteranalyse

library(factoextra)
library(FactoMineR)
library(ggplot2)

# Skalieren

CA1 <- scale(CAmeans[,2:13])


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













#########Sicherheitsspeicher letzter Upload Simon

##############################################
# Gruppenarbeit CSS, USL Gruppe 6
##############################################
# vorgelegt von Alisa Naumann, Ronja Plendl, Simon Hübner

# Datensatz: Mikrozensus 2010
# Explorative Untersuchung Berufsgruppen und Lebensumstände // hierarchische Clusteranalyse (+Hauptkomponentenanalye/PCA?)
# Es geht um die Berufe der Haupteinkommensbezieher eines Haushalts und deren Wohn- und Lebenssituation

# Vorüberlegungen und Methodik: 
# Mikrozensus erfasst die beruflichen und privaten Lebensumstände der deutschen Bevölkerung
# Der Datensatz beinhaltet 23374 Fälle und schlüsselt verschiedene Lebensbereiche der Befragten detailliert auf
# Auffallend ist die Erfassung von über 100 verschiedenen Berufen bzw. Berufsgruppen (113?)

# Als Forschungskontext der Untersuchung interessierte uns daher folgendes:
# Welche Jobs bzw. Branchen ähneln sich in der Lebensführung /-umstände der Beschäftigten? 
# Welche Gruppen bzw. soziale Entitäten lassen sich bilden?  


library('descr')  
library('ggplot2')
library('stargazer')
library('factoextra')
library('FactoMineR')
library("corrplot")
library('datasets')
library ('dotwhisker')
library('summarytools')


setwd("E:/Gruppenarbeit Dateien/Gruppenarbeit_2022_Projekt/Data")
getwd()

#install.packages('haven')
library(haven)
Mikrozensus <- read_dta('mz2010_cf.dta')


#Subdatensatz mit relevanten Variablen [ist hier nur zur Übersicht drin oder?]

MZsubCA <-Mikrozensus[,c( "ef1",                                                                       #"ef136", "ef310", "ef312", "ef44", "ef46", "ef131",
                          "ef739", "ef742", "ef745", "ef731", "ef734",
                          "ef707", "ef492", "ef638", "ef663", "ef669", "ef770", "ef667", "ef491")]
variable.names(MZsubCA)
str(MZsubCA)

#### Befragter - aktuell rauslassen, wird aber ggf. nochmal relevant 
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
MZsubCA$StaatsangehoerigkeitHEB   <- MZsubCA$ef734

MZsubCA$Haushaltseinkommen        <- MZsubCA$ef707 
MZsubCA$Wohnraumgroeße            <- MZsubCA$ef492 
MZsubCA$QuadratmeterMiete         <- MZsubCA$ef638 
MZsubCA$Haushaltsgroeße           <- MZsubCA$ef663 
MZsubCA$AnzahlKinderHH            <- MZsubCA$ef669
MZsubCA$AnzahlKinderInsges        <- MZsubCA$ef770 
MZsubCA$AnzahlEinkommensbezieher  <- MZsubCA$ef667 
MZsubCA$Wohneigentum              <- MZsubCA$ef491   #Wohnhaft in eigenem Gebäude/eigener Wohnung/HauptmieterIn/UntermieterIn


############# Deskriptiver Teil ######################

# Für Fragestellung relevante  Variablen betrachten und ggf. umcodieren

# Relevante Packages:
# install.packages('summarytools')
library(summarytools)
# install.packages('car')
library(car)             # umcodieren von Variablen

MZsubCA$BerufHEB <- recode(MZsubCA$BerufHEB, "999=NA") #Berufe ohne Angabe = NA
freq(MZsubCA$BerufHEB)
table(MZsubCA$BerufHEB)

# Wollen wir hier gleich deskriptiv ein paar Sachen raus ziehen?
# z.B.: mit 694 Fällen ist der größte Berufsbereich: "Architekten, Ingenieure und verwandte Wissenschaftler"
# [edit: ÄHM was ist denn das für ne umfassende Gruppe?! Die könnten wir uns ggf. mal genauer anschauen]

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
# [gute Ergänzung!]

MZsubCA$Wohneigentum <- recode(MZsubCA$Wohneigentum, "1=4;2=3;3=2;4=1") #umcodiert, sodass aufsteigend von Untermieter bis Gebäudeeigentümer
freq(MZsubCA$Wohneigentum)

# [Alternativ: nur Eigentum vs Miete?]
MZsubCA$WohneigentumJaNein <- recode(MZsubCA$Wohneigentum, "1=1 ; 2=1 ; 3=2 ; 4=2 ; 9=NA") 
# umcodiert, wäre dann1= Eigentum und 2= Miete
freq(MZsubCA$WohneigentumJaNein)



CA <-MZsubCA[,c("Land", "BerufHEB", "EinkommenHEB", "AbschlussHEB", "GeschlechtHEB", "StaatsangehoerigkeitHEB",
                "Haushaltseinkommen", "Wohnraumgroeße", "Haushaltsgroeße",
                "AnzahlKinderHH", "AnzahlKinderInsges", "AnzahlEinkommensbezieher", "Wohneigentum")]

#[Test Vergleich weniger Variablen]
#Test1 <-MZsubCA[,c("BerufHEB", "Haushaltseinkommen", "Wohnraumgroeße", "Haushaltsgroeße",
#                "AnzahlKinderHH",  "Wohneigentum")]
#Test2 <-MZsubCA[,c("Haushaltseinkommen", "Wohnraumgroeße", "Haushaltsgroeße",
#                                 "AnzahlKinderHH",  "Wohneigentum")]
#
# [Die letzte Überlegung für mich heute war: Wenn wir analog zu diesem Swiss Datensatz arbeiten wollen,
# müssen wir dann nicht den ganzen Datensatz nach den BerufHEB "gruppieren"? (sind etwas über 100)]



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


### ab hier eigentlich ähnlich wie im Sktipt vorgehen, und ne hübsche Grafik machen, oder?

