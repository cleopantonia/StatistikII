# Uebungsblatt 9
# Namen: Janina Schoenberger, Benjamin Weigner
# Tutorin: Gergana Stanilova
# Uebung: Mi 12-14 Uhr

# Aufgabe 19
# Fuer Wahrscheinlichkeiten die zugehoerigen Odds
p_a = 0.3
p_a/(1-p_a)
p_a = 0.8
p_a/(1-p_a)
p_a = 0.5
p_a/(1-p_a)
# Fuer Odds die zugehoerigen Wahrscheinlichkeiten
r_a = 0.1
r_a/(1+r_a)
r_a = 100
r_a/(1+r_a)
r_a = 5
r_a/(1+r_a)

# Aufgabe 20

# A
# setwd
Unfaelle <- read.table("Unfaelle.txt", header=TRUE)
head(Unfaelle)
attach(Unfaelle)
# Alter&Fahrpraxis in Jahren, Geschlecht m=1 w=2, Unfall n=0 j=1

# B
# Logistisches Regressionsmodell (Modellgleichung) 
# mit abhaengiger Variable Y: Unfall und 
# unabhaengigen Variablen x_1: Geschlecht, x_2(1,2): Beruf, x_3: Alter, x_4: Fahrpraxis
# (Beruf hat 2 Auspraegungen -> 2 Dummy Variablen. x21=Physiker, x22=Zahnarzt)
# Logit(Y=1|X=x_i) = b_0 + b_1*x_1 + b_21*x_21 + b_22*x22 + b_3*x_3 + b_4*x_4

# Geeignete Kodierung fuer Beruf und Geschlecht (as.factor, relevel) -> Referenzkategorie Zahnarzt
Unfaelle$Beruf=as.factor(Unfaelle$Beruf)
Unfaelle=within(Unfaelle,Beruf<-relevel(Beruf, ref="Zahnarzt"))
Unfaelle$Geschlecht = as.factor(Unfaelle$Geschlecht)
sapply(Unfaelle, class)

# C
# Model M mit Intercept fuer die Haupteffekte (glm, family=binomial)
modell <- glm(Unfall~Geschlecht+Alter+Beruf+Fahrpraxis, family="binomial")
summary(modell)

# D
# Welche Berufsgruppe, welches Geschlecht hat hoeheres Unfallrisiko?
# Odds Ratio = Odds(X+1)/Odds(X)

# Odds Ratio zwischen Physikern und Zahnaerzten
# OR(Beruf=Physiker|Beruf=Zahnarzt) = e^(b_21+b_22)/e^(b_21) = e^(b_22)
# b_22 ist Koeffizient von Variable BerufZahnarzt
exp(coefficients(modell)[5])
# OR > 1 -> Odds der ersten Gruppe ist kleiner
# --> Hoeheres Unfallrisiko (Unfall=1) bei Beruf Zahnarzt
# Odds Ratio zwischen Maennern und Frauen
# OR(Geschlecht=1|Geschlecht=2) = e^(b_1)
# b_1 ist Koeffizient von Geschlecht
exp(coefficients(modell)[2])
# OR > 1 -> Odds der ersten Gruppe ist kleiner
# --> Hoeheres Unfallrisiko (Unfall=1) bei Geschlecht Frau

# E
# Kreuztabelle fuer Unfall und Geschlecht
tab <- table(Unfall, Geschlecht)
tab
# Chi-Quadrat Test
chisq.test(tab, correct=FALSE)
# Aus Kreuztabelle Odds Ratio zw Maennern und Frauen
library(vcd)
exp(oddsratio(tab)$coefficients)

# F
# Vergleich Odds Ratio mit dem aus dem logistischen Regressionsmodell
# Warum nicht gleich?
# -> aus E: 2.18, aus D: 3.22
# -> Bei der Kreuztabelle sind im Modell nur Unfall und Geschlecht enthalten
#    waehrend im logisitischen Regressionsmodell "modell" alle Variablen 
#    enthalten sind.
# -> Daraus ergeben sich andere Koeffizienten, mit denen die OR berechnet wird

# G
# Erweitere M zu M_int um Interaktion zwischen Geschlecht und Beruf
modell2 <- glm(Unfall~Geschlecht+Alter+Beruf+Fahrpraxis+Geschlecht*Beruf, family="binomial")
# Likelihood Test fuer M und M_int (Teststatistik X, p-Wert)
library(epiDisplay)
lrtest (modell, modell2)
# H_0 = p(modell)=p(modell2)
# Verteilung: Chi Quadrat Verteilt
# Plot Dichte der Verteilung. Markiere X und Grenzen des Ablehnungsbereichs fuer alpha=0.05
X <- anova(modell,modell2)
summary(X)
x <- X$Deviance[2]
curve( dchisq(x, df=1), col='red', main = "Chi-Square Density Graph",
       from=0,to=60)
# Schwellenwert
qchisq(X$Deviance,df=1)
# Dichte
dchisq(X$Deviance,df=1)
# p-Wert
1-pchisq(X$Deviance,df=1)

# Entscheidung fuer ein Modell: Die Interaktion zwischen Geschlecht und Beruf bringt
# keine neuen Informationen fuer das Modell. Deshalb sollte sich fuer das erste Modell 
# entschieden werden

# H
# Fuer M Wkt und Odds fuer Unfall einer 25-jaehrigen Physikerin mit 7 Jahren Fahrpraxis
nd <- data.frame("Geschlecht" = 2, "Beruf"=as.factor("Physiker"), "Alter"=25, "Fahrpraxis"=7)
p_a <- predict(modell, nd, type="response")
p_a
# -> Wahrscheinlichkeit fuer Unfall fuer die Person = ca. 41%
p_a/(1-p_a)
# -> Odds fuer Unfall = 1:0.7

# I
# Fuer welche Individuen aus dem Datensatz sagt das Modell die hoechste und niedrigste
# unfallwahrscheinlichkeit voraus? Wie hoch sind die Wahrscheinlichkeiten?
Unfaelle <- read.table("Unfaelle.txt", header=TRUE)
p = c()
for (i in 1:(length(Unfaelle))){
  newdata <- data.frame(Unfaelle[i,c(3,4,5,6)])
  p[i] = predict(modell, newdata, type="response")
}
p
# Hoechste Unfallwahrscheinlichkeit:
max(p)
# bei Individuum Nr.
which(p==max(p))
# Niedrigste Unfallwahrscheinlichkeit:
min(p)
# bei Individuum Nr.
which(p==min(p))


detach(Unfaelle)
