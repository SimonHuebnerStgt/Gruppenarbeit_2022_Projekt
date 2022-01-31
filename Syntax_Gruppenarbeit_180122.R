##############################################
# Gruppenarbeit CSS, USL Gruppe 6
##############################################
# vorgelegt von Alisa Naumann, Ronja Plendl, Simon Hübner
# Datensatz: Mikrozensus 2010
# Explorative Untersuchung Berufsgruppen und Lebensumstände anhand einer hierarchischen Clusteranalyse
# Thema: Die Berufe der Haupteinkommensbezieher eines Haushalts und deren Wohn- und Lebenssituation
#
# Vorüberlegungen und Methodik: 
# Mikrozensus erfasst die beruflichen und privaten Lebensumstände der deutschen Bevölkerung.
# Der Datensatz beinhaltet 23374 Fälle und schlüsselt verschiedene Lebensbereiche der Befragten detailliert auf.
# Abgefragt werden unter anderem mehr als 100 verschiedenen Berufe in 10 verschiedenen Berufsgruppen
# nach der Internationalen Standardklassifikation der Berufe (ISCO-88 COM).
# Als Forschungskontext der Untersuchung interessierte uns daher folgendes:
# Welche Jobs bzw. Branchen ähneln sich in der Lebensführung /-umstände der Beschäftigten? 
# Welche Gruppen bzw. soziale Entitäten lassen sich bilden?  


setwd("/Users/alisanaumann/Desktop/Gruppenarbeit_CSS")
getwd()
library('datasets')
library ('dotwhisker')
library('summarytools')
library("haven")

Mikrozensus <- read_dta('mz2010_cf.dta')
#Subdatensatz mit relevanten Variablen für bessere Übersichtlichkeit
                                                                                                       # Angaben zum Befragten
MZsubCA <-Mikrozensus[,c( "ef1",                                                                       #"ef136", "ef310", "ef312", "ef44", "ef46", "ef131",
                          "ef739", "ef742", "ef745", "ef731", "ef734",
                          "ef707", "ef492", "ef638", "ef663", "ef669", "ef770", "ef667", "ef491")]
variable.names(MZsubCA)
str(MZsubCA)
#### Angaben des Befragten - aktuell rauslassen, wird aber ggf. nochmal relevant 
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
MZsubCA$Wohnraumgroesse           <- MZsubCA$ef492 
MZsubCA$QuadratmeterMiete         <- MZsubCA$ef638 
MZsubCA$Haushaltsgroesse          <- MZsubCA$ef663 
MZsubCA$AnzahlKinderHH            <- MZsubCA$ef669
MZsubCA$AnzahlKinderInsges        <- MZsubCA$ef770
MZsubCA$AnzahlEinkommensbezieher  <- MZsubCA$ef667 
MZsubCA$Wohneigentum              <- MZsubCA$ef491   #Wohnhaft in eigenem Gebäude/eigener Wohnung/HauptmieterIn/UntermieterIn

# Relevante Packages:
# install.packages('summarytools')
library(summarytools)
# install.packages('car')
library(car)             # umcodieren von Variablen
MZsubCA$BerufHEB <- recode(MZsubCA$BerufHEB, "999=NA") #Berufe ohne Angabe = NA
freq(MZsubCA$BerufHEB)
table(MZsubCA$BerufHEB) # Berufe werden an späterer Stelle genauer aufbereitet

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
freq(MZsubCA$QuadratmeterMiete) # fällt raus, da viel NA und Personen die keine Miete zahlen - Stattdessen Wohneigentum?
MZsubCA$Wohneigentum <- recode(MZsubCA$Wohneigentum, "1=4;2=3;3=2;4=1") #umcodiert, sodass aufsteigend von Untermieter bis Gebäudeeigentümer
freq(MZsubCA$Wohneigentum)
MZsubCA$WohneigentumJaNein <- recode(MZsubCA$Wohneigentum, "1=1 ; 2=1 ; 3=2 ; 4=2 ; 9=NA") # umcodiert, wäre dann 1= Eigentum und 2= Miete
freq(MZsubCA$WohneigentumJaNein)

CA <-MZsubCA[,c("Land", "BerufHEB", "EinkommenHEB", "AbschlussHEB", "GeschlechtHEB", "StaatsangehoerigkeitHEB",
                "Haushaltseinkommen", "Wohnraumgroesse", "Haushaltsgroesse",
                "AnzahlKinderHH", "AnzahlKinderInsges", "AnzahlEinkommensbezieher", "WohneigentumJaNein")]

CAnoNA <- na.omit(CA) # Üperprüfung der Veränderung nach NA-Bereinigung - von 23374 Fällen verbleiben 14347 Fälle
freq(CA)
freq(CAnoNA)

#Betrachten der Variable Berufe - Häufigkeiten, ggf. Berufsgruppen entfernen?
freq(CA$BerufHEB)
freq(CAnoNA$BerufHEB)
CAnoNA$BerufHEB          #unter 5: 811 (mineralaufbereitung/keramikherstellung),825 (Maschienenbediener für Papiererzeugnisse),834 (Deckpersonal auf Schiffen)
                         #unter 10: 111 (Angehörige gesetzgebender Körperschaften(Politiker)/Verwaltungsangestellte), 348 (Ordensbrüder und Seelsorger), 612 (tierwirtschaftliche Berufe)
                         #nicht vorhanden: 223 (wiss. Krankenpflege & Geburtshilfe), 331 (nicht-wiss. Lehrkräfte Primärbereich),
                         #nicht vorhanden: 613 (Ackerbauern/Tierzüchter), 817 (Bediener Industrieroboter), 912 (Schuhputzer, etc.)

#### Tabelle mit Mittelwerten der Berufsgruppen je Variable, 14347 Fälle von Berufstätigen zu Berufsgruppen aggregiert
CAmeans <- aggregate(cbind(Land, EinkommenHEB, AbschlussHEB, GeschlechtHEB, StaatsangehoerigkeitHEB,
                           Haushaltseinkommen, Wohnraumgroesse, Haushaltsgroesse,
                           AnzahlKinderHH, AnzahlKinderInsges, AnzahlEinkommensbezieher, WohneigentumJaNein) ~ BerufHEB, CA, mean)
CAmeans$BerufHEB
freq(CAmeans$BerufHEB)
#rownames(CAmeans) <- c( "11","111","114","120","121","122", "123", "130","131","211","212","213","214","221","222","231","232","233","234","235",
#                       "241","242","243","244","245","246","247","311","312","313","314","315","321","322","323","332","333","334",
#                       "341","342","343","344","345","346","347","348",
#                       "411","412","413","414","419","421","422","511","512","513","514","516","521","610","611","612","614","711","712","713","714",
#                       "721","722","723","724","731","732","734","741","742","743","744","811","812","814","815","816","821","822","823","825","826","827","828","829",
#                       "831","832","833","834","911","913","914","915","921","931","932","933")

#### Benennung der Berufsgruppen für bessere Übersichtlichkeit/Interpretierbarkeit
rownames(CAmeans) <- c( "Soldaten",                                        #11
                        "Politiker/leitende Verwaltungsbedienstete",       #111
                        "leitende Bedienstete Interessenorg.",             #114
                        "Geschäftsleiter/Bereichsleiter große Org.",       #120
                        "Direktoren/Hauptgeschäftsführer",                 #121
                        "Produktions-/Operationsleiter",                   #122
                        "sonstige Fachbereichsleiter",                     #123
                        "Leiter kleiner Unternehmen o.n.A.",               #130
                        "Leiter kleiner Unternehmen",                      #131
                        "Physiker/Chemiker etc.",                          #211
                        "Mathematiker/Statistiker/etc.",                   #212
                        "Informatiker",                                    #213
                        "Architekten/Ingenieure etc.",                     #214
                        "Biowissenschaftler",                              #221
                        "Mediziner",                                       #222
                        "Uni-/Hochschullehrer",                            #231
                        "Lehrer Sekundarbereich",                          #232
                        "wiss. Lehrer Primär-/Vorschulbereich",            #233
                        "wiss. Sonderschullehrer",                         #234
                        "sonst. wiss. Lehrkräfte",                         #235
                        "Unternehmensberatungs-/Organisationsfachkr.",     #241
                        "Juristen",                                        #242
                        "Archiv-/Bibliotheks-/Informationswiss.",          #243
                        "Sozialwissenschaftler etc.",                      #244
                        "Schriftsteller, bildende/darst. Künstler",        #245
                        "Geistliche/Seelsorger",                           #246
                        "wiss. Verwaltungsfachkr. öffentl. Dienst",        #247
                        "material-/ingenieurtechnische Fachkr",            #311
                        "Datenverarbeitungsfachkräfte",                    #312
                        "Bediener optischer/elektronischer Anlagen",       #313
                        "Schiffs-/Flugzeugführer etc.",                    #314
                        "Sicherheits-/Qualitätskontrolleure",              #315
                        "Biotechniker etc.",                               #321
                        "med. Fachberufe (ohne Krankenpflege)",            #322
                        "Krankenpflege/Geburtshilfefachkr. n.wiss.",       #323
                        "Lehrkräfte Vorschule n.wiss",                     #332
                        "Sonderschullehrkräfte n.wiss.",                   #333
                        "Sonstige Lehrkräfte n.wiss.",                     #334
                        "Finanz-/Verkaufsfachkräfte",                      #341
                        "Vermittler gew. Dienstleist./Handelsmakler",      #342
                        "Verwaltungsfachkräfte",                           #343
                        "Zoll-/Steuer-Fachkräfte etc.",                    #344
                        "Polizeikommissare/Detektive",                     #345
                        "Sozialpflegerische Berufe",                       #346
                        "Künstler-/unterhaltungs-/Sportberufe",            #347
                        "Ordensbrüder/-Schwestern/Seelsorgehelfer",        #348
                        "Sekretärinnen/Machienenschreibkr. etc.",          #411
                        "Rechnungs-/Statistik-/Finanzwesensangest.",       #412
                        "Materialverwaltungs-/Tansportangestellte",        #413
                        "Bibliotheks-/Postangestellte etc.",               #414
                        "sonstige Büroangestellte",                        #419
                        "Kassierer-/Schalterangestellte etc.",             #421
                        "Kundeninformationsangestellte",                   #422
                        "Reisebegleiter etc.",                             #511
                        "Dienstleistungsberuf Hauswirtschaft/Gastro",      #512
                        "Pflegeberufe etc.",                               #513
                        "sonst. personenbez. Dienstleistungsberufe",       #514
                        "Sicherheitsbedienstete",                          #516
                        "Mannequins/Dressmen/Marktverkäufer etc.",         #521
                        "Fachkr. Landwirtschaft/Fischerei",                #610
                        "Gärtner/Ackerbauern",                             #611
                        "Tierwirtschaftliche Berufe etc.",                 #612
                        "Forstarbeitskräfte/Fischer/Jäger etc.",           #614
                        "Bergleute/Sprengmeister/Steinbearbeiter etc.",    #711
                        "Baukonstruktionsberufe etc.",                     #712
                        "Ausbauberufe etc.",                               #713
                        "Maler/Gebäudereiniger etc.",                      #714
                        "Former/Baumetallverformer/Schweißer etc.",        #721       
                        "Grobschmiede/Werkzeugmacher etc.",                #722
                        "Maschinenmechaniker/-schlosser",                  #723
                        "Elektro-/Elektronikmechaniker/-monteure",         #724
                        "Präzisionsarbeiter Metall verw. Werkstoffe",      #731
                        "Töpfer/Glasmacher/Kunsthandwerker etc.",          #732
                        "Druckhandwerker etc.",                            #734
                        "Nahrungsmittelverarbeitungsberufe etc.",          #741
                        "Holzbearbeiter/Möbeltischler etc.",               #742
                        "Textil-/Bekleidungsberufe etc.",                  #743
                        "Fell-/Lederverarbeiter/Schuhmacher",              #744
                        "Anlagenbed. Mineralaufber./Keramik/Glas etc.",    #811
                        "Verfahrensanlagebed. Metallerzeugung/-formung",   #812
                        "Anlagenbed. Holzaufbereitung/Papierherst.",       #814
                        "Bediener chemischer Verfahrensanlagen",           #815
                        "Bediener Energieerzeugungsanlagen etc.",          #816
                        "Maschinenbed. Metall/Mineralerz.",                #821
                        "Maschinenbed. chemische Erzeugnisse",             #822
                        "Maschinenbed. Gummi/Kunststofferzeugn./Holz",     #823
                        "Maschinenbed. Druck-/Buchbinde-/Papiererz.",      #825
                        "Maschinenbed. Textil-/Pelz-/Ledererz.",           #826
                        "Maschinenbed. Nahrungs-/Genussmittelherst.",      #827
                        "Montierer",                                       #828
                        "sonstige Maschinenbediener",                      #829
                        "Lokomotivführer etc.",                            #831
                        "Kraftfahrzeugführer",                             #832
                        "Landmaschinenführer etc.",                        #833
                        "Deckspersonal Schiffe etc.",                      #834
                        "Straßenhändler etc./Müllsammler",                 #911
                        "Haushaltshilfen/Reinigungspersonal etc.",         #913
                        "Hausmeister/Fensterputzer etc.",                  #914
                        "Boten/Träger/Pförtner etc.",                      #915
                        "Hilfsarbeiter Landwirtsch./Fischerei",            #921
                        "Hilfsarbeiter Bergbau/Baugewerbe",                #931
                        "Hilfsarbeiter Fertigung",                         #932
                        "Transport-/Frachtarbeiter")                       #933
rownames(CAmeans)

#### Durchführung der Clusteranalyse
library(factoextra)
library(FactoMineR)
library(ggplot2)

# Skalieren, standardisierte Werte (z-Werte) erzeugen für Vergleichbarkeit
CA1 <- scale(CAmeans[,2:13])

# Ellbogenkriterium / Silhouette
fviz_nbclust(CA1, hcut, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2)+
  labs(subtitle = "Elbow method")
# Kein eindeutiger "Knick" erkennbar - am ehesten noch bei 5 Clustern?

fviz_nbclust(CA1, hcut, method = "silhouette")+
  labs(subtitle = "Silhouette method")
# Maximum eindeutig bei 2 Clustern -> 2 Cluster probieren?

# Distanzmatrix
d1 <- dist(CA1, method = 'euclidean')
head(as.matrix(d1))

# Dendrogramm
HC <- hclust(d1, method="complete")
fviz_dend(HC)
fviz_dend(HC,2)
fviz_dend(HC,3)
fviz_dend(HC,5)


## Ableiten der Lösung für das Cluster
#Lösung 2 Cluster
cluster2 <- cutree(HC, k = 2) 
head(cluster2)
CAmeans$cluster2 <- cluster2
CAmeans$BerufeHEB <- row.names(CAmeans)

#Lösung 3 Cluster
cluster3 <- cutree(HC, k = 3) 
head(cluster3)
CAmeans$cluster3 <- cluster3
CAmeans$BerufeHEB <- row.names(CAmeans)

#Auffällig: 2. Cluster nur sehr geringe Anzahl Fälle/Berufsgruppen

#Lösung 5 Cluster
cluster5 <- cutree(HC, k = 5) 
head(cluster5)
CAmeans$cluster5 <- cluster5
CAmeans$BerufeHEB <- row.names(CAmeans)


#### Cluster visualisieren

#  geom_label leider noch unübersichtlicher als geom_text 
#  geom_label(aes(label= BerufeHEB), size = 2)

# Betrachtung Einkommen und Wohnraumgröße in allen Clustermöglichkeiten
# 2 Cluster:
ggplot(CAmeans, aes(x=EinkommenHEB, y = Wohnraumgroesse, color = factor(cluster2))) +   
  geom_point() + 
  geom_text(aes(label= BerufeHEB), size = 2, hjust = 0, nudge_x = 0.05)
# 3 Cluster:
ggplot(CAmeans, aes(x=EinkommenHEB, y = Wohnraumgroesse, color = factor(cluster3))) +   
  geom_point() +
  geom_text(aes(label= BerufeHEB), size = 2, hjust = 0, nudge_x = 0.05)
# 5 Cluster:
ggplot(CAmeans, aes(x=EinkommenHEB, y = Wohnraumgroesse, color = factor(cluster5))) +   
  geom_point() + 
  geom_text(aes(label= BerufeHEB), size = 2, hjust = 0, nudge_x = 0.05)

# Betrachtung WohneigentumJaNein und Wohnraumgröße in allen Clustermöglichkeiten
# 2 Cluster:
ggplot(CAmeans, aes(x=WohneigentumJaNein, y = Wohnraumgroesse, color = factor(cluster2))) +   
  geom_point() + 
  geom_text(aes(label= BerufeHEB), size = 2, hjust = 0, nudge_x = 0.05)
# 3 Cluster:
ggplot(CAmeans, aes(x=WohneigentumJaNein, y = Wohnraumgroesse, color = factor(cluster3))) +   
  geom_point() +
  geom_text(aes(label= BerufeHEB), size = 2, hjust = 0, nudge_x = 0.05)
# 5 Cluster:
ggplot(CAmeans, aes(x=WohneigentumJaNein, y = Wohnraumgroesse, color = factor(cluster5))) +   
  geom_point() + 
  geom_text(aes(label= BerufeHEB), size = 2, hjust = 0, nudge_x = 0.05)

# Vergleich der Lebensumstände hier deutlich: Wohlhabenderes Cluster verfügt über mehr Wohnfläche (häufiger in Eigentum) und Einkommen

# Betrachtung Geschlecht des Haupteinkommenbeziehers und Anzahl der Kinder im Haushalt
# 2 Cluster:
ggplot(CAmeans, aes(x= GeschlechtHEB , y = AnzahlKinderHH, color = factor(cluster2))) +   
  geom_point() +
  geom_text(aes(label= BerufeHEB), size = 2, hjust = 0, nudge_x = 0.02)

ggplot(CAmeans, aes(x=EinkommenHEB  , y = AnzahlKinderHH, color = factor(cluster2))) +   #zusätzliche Betrachtung von Einkommen und Kindern im Haushalt
  geom_point() 
geom_text(aes(label= BerufeHEB), size = 2, hjust = 0, nudge_x = 0.05)+
  geom_label(aes(label= BerufeHEB), size = 3)

# Cluster 1 hat zudem häufiger "klassisches Familienbild" mit Mann als Haupteinkommensbezieher
# Damit gehen interessanterweise auch häufiger eine höhere Anzahl an Kindern im Haushalt einher

# 3 Cluster:
ggplot(CAmeans, aes(x= GeschlechtHEB , y = AnzahlKinderHH, color = factor(cluster3))) +   
  geom_point() +
  geom_text(aes(label= GeschlechtHEB), size = 2, hjust = 0, nudge_x = 0.02)

# 5 Cluster:
ggplot(CAmeans, aes(x= Haushaltsgroesse , y = AnzahlKinderHH, color = factor(cluster5))) +   
  geom_point() +
  geom_text(aes(label= BerufeHEB), size = 2, hjust = 0, nudge_x = 0.02)



# Vergleich der Clustereigenschaften anhand der 13 Variablen
# Ein Mittelwertvergleich der jeweiligen Cluster und ihrer Ausprägungen der Variablen + Gegenüberstellung mit transpose(t) 

# 2 Cluster: 
Vergleich2 <- c('EinkommenHEB','Haushaltseinkommen', 'AbschlussHEB', 'AnzahlKinderHH','AnzahlKinderInsges', 
                'Wohnraumgroesse', 'Haushaltsgroesse','Land', 'GeschlechtHEB', 'StaatsangehoerigkeitHEB',
                'AnzahlEinkommensbezieher', 'WohneigentumJaNein')
cluster.descr2 <- aggregate(CAmeans[,Vergleich2], by=list(cluster=CAmeans$cluster2), mean)
cluster.descr2
t(cluster.descr2)

# In der Cluster-Gegenüberstellung lassen sich Unterschiede erfassen
# Cluster 1 verfügt über ein etwas höheres `"Wohlstandniveau", wenn man die Lebensumstände betrachtet
# Das zeigt sich in höherem verfügbaren Haushaltseinkommen und mehr Wohnfläche
# Zugleich ist in den zugehörigen Berufsgruppen auch die Kinderanzahl höher als in Cluster 2 
# Nicht zuletzt ist in diesem Cluster das Geschlecht des Haupteinkommensbeziehers häufiger Männlich

# 3 Cluster:
Vergleich3 <- c('EinkommenHEB','Haushaltseinkommen', 'AbschlussHEB', 'AnzahlKinderHH','AnzahlKinderInsges', 
                'Wohnraumgroesse', 'Haushaltsgroesse','Land', 'GeschlechtHEB', 'StaatsangehoerigkeitHEB',
                'AnzahlEinkommensbezieher', 'WohneigentumJaNein')

cluster.descr3 <- aggregate(CAmeans[,Vergleich3], by=list(cluster=CAmeans$cluster3), mean)
cluster.descr3
t(cluster.descr3)
# Cluster 1 höheres Einkommen, Wohnraumgröße

# Cluster 2 kleinstes Cluster (Politiker, Bediener Mineralaufber.)

# Cluster 3 geringeres Einkommen, Wohnraumgröße, etc.

# 5 Cluster:
Vergleich5 <- c('EinkommenHEB','Haushaltseinkommen', 'AbschlussHEB', 'AnzahlKinderHH','AnzahlKinderInsges', 
                'Wohnraumgroesse', 'Haushaltsgroesse','Land', 'GeschlechtHEB', 'StaatsangehoerigkeitHEB',
                'AnzahlEinkommensbezieher', 'WohneigentumJaNein')

cluster.descr5 <- aggregate(CAmeans[,Vergleich5], by=list(cluster=CAmeans$cluster5), mean)
cluster.descr5
t(cluster.descr5)

# Cluster 1 Größte Haushaltsgröße, größte Kinderanzahl, durchschnittliches Einkommen   

# Cluster 2 als kleinstes Cluster (Berufsgruppe "Politiker/leitende verwaltungsbedienstete" (n=9) und "bediener bergbaul./mineralaufbereitungsanlagen; glas- u. keramikherstellungsanlagen" (n=3))
#rein männlich, in Westdeutschland, mit Abstand größte Wohnraumgröße und größtes Einkommen

# Cluster 3 größerer Frauenanteil als bei anderen Clustern

# Cluster 4 erhöhtes Einkommen, höchster Abschluss, leicht erhöhte Wohnraumgröße, erhöhte Anzahl Kinder

# Cluster 5: ebenfalls kleinstes Cluster, nahe an Cluster 3 - geringstes Einkommen, geringste Wohnraumgröße, geringste Bildung, geringste Anzahl an Kinder ###Tierwirtschaftliche Berufe (n=9) und
# Maschinenbediener Druck/Papiererzeugnisse (n=3)
# vor allem junge Menschen/Männer in Ostdeutschland?

# Ausführungen!?
# Hier wäre beispielsweise ein deutlicheres Cluster mit den eher Priviligierten/Wohlhabenden bzw. mit gut bezahlten Berufsgruppen dabei

# Ergänzung R.: k-means

d1.k2 <- kmeans(d1, 
                2, 
                iter.max = 10, 
                nstart = 2)
print(d1.k2)

d1.k3 <- kmeans(d1, 
                3, 
                iter.max = 10, 
                nstart = 2)
print(d1.k3)

# 5 Cluster
d1.k5 <- kmeans(d1, 
                5, 
                iter.max = 10, 
                nstart = 2)
print(d1.k5)

#pca
#ich fang mal an mit drei Clustern wie ich ursprünglich hatte

fviz_cluster(data = d1,
            d1.k3)
CA.pca <- PCA(CA1,
              scale.unit = F,
              ncp = 3, graph = F)
print(CA.pca)
CA.pca$eig

#die drei Cluster erklären 73% Varianz

fviz_cluster(data = d1,
             d1.k5)
CA.pca <- PCA(CA1,
              scale.unit = F,
              ncp = 3, graph = F)
print(CA.pca)
CA.pca$eig

#Der Cluster-Plot validiert unser Konstrukt, da die Cluster kaum Überschneidungen haben.  Es lassen sich so mindestens drei (oder 5?)Cluster unterscheiden. 

#Scree plot
fviz_eig(CA.pca, 
         addlabels = TRUE)

# Output

var <- CA.pca$var
names(var)
head(var$coord)
fviz_pca_var(CA.pca, col.var = "black")  #nicht alle Variablen leserlich
fviz_pca_var(
  CA.pca,
  axes = c(1, 2),
  repel = TRUE,
  col.var = "black",
  fill.var = "white",
  alpha.var = 1,
  col.quanti.sup = "blue",
  col.circle = "grey70",
  select.var = list(name = NULL, cos2 = NULL, contrib = NULL)
  
#wie ändert man Dimensionen
head(var$cos2)
  
  #corrplot
  library("corrplot")
  corrplot(var$cos2, is.corr=FALSE)
  #Dimension 1 clustert besonders nach EinkommenHEB, AbschlussHEB und Haushaltseinkommen.
  #Dimension 2 clustert besonders nach Einkommen, AbschlussHEB und StaatsangehörigkeitHEB
  #Dimension 3 clustert fast ausschließlich nach GeschlechtsHEB
  head(var$contrib)
  corrplot(var$contrib, is.corr=FALSE) 
  #interessant dass eine Dimension gibt die außschließlich mit Geschlecht clustert.
  #mögliche Interpretation: Es gibt einen Zusammenhang zwischen Geschlcht und Berufsgruppen, bei dem in unserer Clusteranalyse untersuchten Variablen keine Störungsvariablen darstellen
  
  #contribution of variables als Barplot
  fviz_contrib(CA.pca, choice = "var", axes = 1, top = 10)
  fviz_contrib(CA.pca, choice = "var", axes = 2, top = 10)
  fviz_contrib(CA.pca, choice = "var", axes = 3, top = 10)
  
  #geordnete Darstellung
  pc.1 <- var$cor[order(-var$cor[, 'Dim.1']), 'Dim.1']
  head(pc.1)
  pc.2 <- var$cor[order(-var$cor[, 'Dim.2']), 'Dim.2']
  head(pc.2)
  pc.3 <- var$cor[order(-var$cor[, 'Dim.3']), 'Dim.3']
  head(pc.3)
  
  #individuelle PCA -> Müll für uns?, brauch weitere Aufbereitung auf jeden Fall
  fviz_pca_ind(CA.pca, col.ind = "cos2", 
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE )
  
  # Interpretation der letzten Grafik/Aktion
