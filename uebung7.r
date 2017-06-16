# Uebungsblatt 7
# Namen: Janina Schoenberger, Benjamin Weigner
# Tutorin: Gergana Stanilova
# Uebung: Mi 12-14 Uhr

# Aufgabe 17
# LDA: linear discriminant analysis
# Voraussage: Metastasenbildung ja/nein = 1/0
# Wahrscheinlichkeit fuer Auftreten von Metastasen p
p <- 0.5
# Mittelwert M0-Gruppe: 0 bzw 2
mu1 <- matrix(c(0,2), nrow=2, ncol=1)
mu1
# Mittelwert M1-Gruppe: 2 bzw -2
mu2 <- matrix(c(2,2), nrow=2, ncol=1)
mu2
# Kovarianzmatrix ueber beide Gruppen hat auf Hauptdiagonalen Eintraege 2, 0.5, auf Nebendiagonalen 0
epsilon = matrix( c(2,0,0,0.5), nrow=2, ncol=2, byrow=TRUE)
epsilon

# A
# Was kann man aus der Kovarianzmatrix ueber die Korrelation zwischen den beiden Genen folgern?
# -> Auf den Nebendiagonalen der Kovarianzmatrix stehen Cov(M0,M1) und Cov(M1,M0)
# -> M0 und M1 haben keinen monotonen Zusammenhang, da die Kovarianz 0 ist d.h. sie korrelieren nicht
# Was kann man aus der Kovarianzmatrix ueber die Form der multivarianten Normalverteilung folgern?
# -> Die Varianz von M0 betraegt 2, die von M1 0.5
# -> Das bedeutet, dass die Normalverteilungskurve von M0 breiter ist, als die von M1

# B
# Entscheidungsformel der LDA im konkreten Beispiel als Funktion a+bx1^+cx2
lambda <- solve(epsilon)%*%(mu1-mu2)
lambda
# -> D = -1*x1 + 0*x2

# C
# Weitere Beobachtung p=(1,0) klassifizieren
# Klassifikationsergebnis basierend auf Entscheidungsformel: 
# D(1,0) = -1
# -------------- Heisst das Metastasen oder keine Metastasen?? -------------------

# D 
# Weitere Informationen benoetigt fuer Durchfuehrung einer QDA: 
# -------- normalverteilung ist doch in A schon ueberprueft -------------------
# Es muesste ueberprueft werden, ob die Daten (annaehernd) normalverteilt sind,
# denn das ist Voraussetzung fuer eine Diskriminanzanalyse
# (Eine gleiche Varianz ist nur bei LDA noetig)


# Aufgabe 18
# Datensatz einer Tumorprobe
# setwd(...)
Patient1 <- read.csv2("Patient1.txt", header=FALSE)
Patient1Label <- read.table("Patient1Label.txt", header=FALSE)
#head(Patient1)
#head(Patient1Label)
# Datensatz umfasst Messungen von 118 Massekanaelen und 440 raeumlichen Positionen
# Betrachtet werden Kaenaele 3 und 7
# Label-Daten = Response-Variable (0 fuer Bindegewebe, 1 fuer Tumorgewebe)

# A
# Massekanaele 3 und 7 und Labels visualisieren
#plot(Patient1[,c(3,7)],col=Patient1Label[,1]) 
# dieser Plot mit allen Werten dauert ewig und ist komplett schwarz
# deshalb eingrenzen der Werte
xy <- head(Patient1[,c(3,7)], n=3)
groups <- head(Patient1Label[,1], n=3)
plot(xy, col=groups)
# Kommentar: Ist eine graphische Visualisieren leicht moeglich?
# Anscheinend nicht, denn im Scatterplot sind zu viele Punkte, als dass man etwas erkennen koennte

# B
# LDA Klassifikator (lda aus MASS Library)
library(MASS)
cl <- lda(Patient1Label[,1]~Patient1[,3]+Patient1[,7])
plot(cl)
# Kommentar: Die erklaerenden Variablen sind kolinear und es wird nur 0 predicted

# C
# Klassifikationsguete mit verschiedenen Massen
# predict benutzen
p <- predict(cl,newdata=Patient1[,c(3,7)])$class

# confusion matrix
#t<- table(p,Patient1[,c(3,7)]) # ?
t <- table(Patient1Label[,1], p)
t

# accuracy
sum(diag(t))/sum(t)*100

# missclassification error
#mean(vorhergesagte_werte != wirkliche werte)
t[1,2]+t[2,1]/(t[1,1]+t[1,2]+t[2,1]+t[2,2]) *100


# D
# QDA Klassifikator (qda aus MASS Library)
# ...
?qda
qda(Patient1Label[,1]~Patient1[,3]+Patient1[,7])

# E
# Klassifikationsguete mit verschiedenen Massen
# ....
# Vergleich zu Guete von lda
#....