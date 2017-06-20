# Uebungsblatt 8
# Namen: Janina Schoenberger, Benjamin Weigner
# Tutorin: Gergana Stanilova
# Uebung: Mi 12-14 Uhr

# Aufgabe 19

# A
# Einfaktorielle ANOVA fuer primer und applic

paint = data.frame(adhf = c(4.0,4.5,4.3,5.6,4.9,5.4,3.8,3.7,
                            4.0,5.4,4.9,5.6,5.8,6.1,6.3,5.5,5.0,5.0),
                   primer = factor(rep(rep(1:3,rep(3,3)),2)),
                   applic = factor(rep(c("D","S"),c(9,9))))
paint

paint.primer <- aov(adhf ~ primer, data=paint)
paint.applic <- aov(adhf ~ applic, data=paint)
summary(paint.primer)
summary(paint.applic)

# Interpretation
# Die einfaktorielle Varianzanalyse (mit ANOVA) gibt an, ob es einen signifikanten
# Unterschied im Mittelwert von verschiedenen Gruppen gegenueber der abhaengigen Variable
# gibt.
# Dieser Unterschied ist signifikant, falls der p-Wert des F-Tests unter 0.05 liegt.
# adhf ist die abhaengige Variable.
# Mit primer als unabhaengiger Variable erhaelt man einen p-Wert von 0.0153. Dieser ist signifikant.
# Mit applic als unabhaengiger Variable erhaelt man einen p-Wert von 0.00204. Dieser ist
# signifikant und zeigt einen deutlicheren Unterschied, als bei primer.


# B
# Grafisch: Interaktion zwischen primer und applic im Bezug auf den Endpunkt adhf
# Aufgrund des Ergebnisses zweifaktorielle ANOVA mit oder ohne Interaktionsterm
# Begruendung fuer mit oder ohne: -------------
# Vergleich zu 19 A
library(ggplot2)
library(gplots)
gboxplot(paint,primer,adhf)
gggline(paint,primer,adhf)
??ggline
interaction.plot(factor(paint$primer),factor(paint$applic),paint$adhf)
########## richtiger plot?? Wie liesst man hieraus interaktionsterm j/n ab??


# C
# Kontrasttest
# install.packages("lsmeans") und ("multcomp")
library(lsmeans)
library(multcomp)
library(afex)
# a) primer 2 vs 1,3 unter applic D
fita <- aov(adhf~primer*applic, data=paint)
ref1 <- lsmeans(fita,"applic")
ref1
head(paint)
c <- c(0,1,-1) ######## wie primer 2 gegen 1 und 3?? in contrast list ist doch nur die spalte
summary(contrast(ref1,c),adjust="none") #-----------------??? geht nicht -> ref oder aov falsch

#############################################
# stackoverflow bsp (aus hinweisen)
data(obk.long)
head(obk.long)
fit <- aov_car(value~treatment*gender + Error(id),data=obk.long, return = "aov")
(ref1 <- lsmeans(fit, c("treatment", "gender")))
c_list <- list(c1 = c(0, -1, 1, 0, 0, 0),
               c2 = c(0, -0.5, 0.5, 0, -0.5, 0.5))
summary(contrast(ref1, c_list), adjust = "holm")
pairs(lsmeans(fit, "treatment"), adjust = "none")
#################################################

# b) applic D vs S fuer alle primer und paarweisen Vergleiche mit Tukey Adjustierung
pairs(lsmeans(fita,"applic"),adjust="tukey")
# Interpretation


# D
# Vergleich Haupteffekt von primer ohne Adjustierung mit Bonferroni und Bonferroni Holm
pairs(lsmeans(fita,"primer"),adjust="none")
## ??