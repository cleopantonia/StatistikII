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
head(paint)

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
if (FALSE){
  # Die Funktionen ggline und ggboxplot haben nicht funktioniert
  library(ggplot2)
  library(gplots)
  ggboxplot(paint,primer,adhf)
  ggline(paint,primer,adhf)
}
interaction.plot(as.factor(paint$primer),as.factor(paint$applic),paint$adhf)
interaction.plot(as.factor(paint$applic),as.factor(paint$primer),paint$adhf)
# Sowohl der Faktor primer, als auch der Faktor applic scheinen einen Einfluss auf 
# die Adhesion Force (paint$adhf) zu haben. 
# 1. Plot: Sowohl bei D, als auch bei S Kurve (D/S von applic) ist Ausschlag bei Primer 2 
# zu erkennen
# -> primer hat Einfluss auf adhf
# 2. Plot: Kurven von allen 3 Primer sind bei S hoeher als bei D
# -> applic hat Einfluss auf adhf
# Deshalb fuehren wir eine zweifaktorielle Anova durch.
fita <- aov(adhf~primer*applic, data=paint)
summary(fita)

# Vergleich zu 19 A: Die zweifaktorielle Anova hat hoehere F-Werte und daher 
# noch niedrigere P-Werte. Das Ergebnis ist also mit noch geringerem P-Wert 
# signifikant, als die einfaktorielle Anova.


# C
# Kontrasttest = paarweise t-Tests (Vorschalttest)
# install.packages("lsmeans") und ("multcomp")
library(lsmeans)
library(multcomp)


if (FALSE){
  # stackoverflow Bsp aus Hinweisen
  data(obk.long)
  head(obk.long)
  fit <- aov_car(value~treatment*gender + Error(id),data=obk.long, return = "aov")
  (ref1 <- lsmeans(fit, c("treatment", "gender")))
  c_list <- list(c1 = c(0, -1, 1, 0, 0, 0),
                 c2 = c(0, -0.5, 0.5, 0, -0.5, 0.5))
  summary(contrast(ref1, c_list), adjust = "holm")
  pairs(lsmeans(fit, "treatment"), adjust = "none")
}

ref1 <- lsmeans(fita,c("applic","primer"))
ref1
# c1: primer 2 gegen 1,3 unter applic D
c1 <- c(-0.25,0,0.5,0,-0.25,0)
# c2: applic D gegen S fuer alle primer
c2 <- c(1/6,-1/6,1/6,-1/6,1/6,-1/6)
c_list <- list(c1,c2)
summary(contrast(ref1,c_list,adjust="none")) ########## welches mit tukey?
pairs(ref1, adjust="none")
pairs(ref1, adjust="tukey")

# Interpretation:
# Tukey kontrolliert alle h=k(k-1)/2 Nullhypothesen und beruecksichtigt die Korrelation
# zwischen den Gruppen. Bei abhaengigen Vergleichen besser als Bonferroni
# Die p-Werte mit Tukey Adjustierung sind deutlich hoeher. Da wir wissen, dass die 
# Variablen korrelieren, macht das Sinn.

# D
# Vergleich Haupteffekt von primer ohne Adjustierung mit Bonferroni und Bonferroni Holm
pairs(lsmeans(fita,"primer"),adjust="none")
pairs(lsmeans(fita,"primer"),adjust="bonferroni")
pairs(lsmeans(fita,"primer"),adjust="holm")
# Es faellt auf, dass die t-Ratios alle gleich sind, waehrend sich die p-Werte
# durch die Adjustierungsmethoden unterscheiden.
# Die p-Werte von Holm und Ohne Adjustierung sind gleich,
# waehrend die Bonferroni Adjustierung hoehre p-Werte vergiebt,
# sodass die Ergebnisse schneller nicht signifikant werden.