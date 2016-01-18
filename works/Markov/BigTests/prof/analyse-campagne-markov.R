data1 <- read.table("markov_bench_books.csv", h=T, sep=";")



# Reorganisation des lignes en les triant par k puis par n
data1 <- data1[order(data1$k, data1$n),]
attach(data1)

##############################################
# On verifie que count1 ne depend pas de k ...
##############################################

plot(k, count1)
# C'est assez moche avec les options par defaut, mais on peut mieux faire
# en jouant avec les options
plot(k, count1, pch=3, xlab="k", ylab="count1 (tri des prefixes)",  cex.lab=0.9, cex.axis=0.8, cex=0.8)

summary(lm(count1 ~ k))
# La pente de la regression lineaire vaut 31.59 et elle n'est pas statistiquement
# differente de 0 (p-value Pr(>|t|) largement superieure a 0.05).
# Le coefficient de determination (R^2) vaut 10^-12, donc quasiment 0.
# On peut l'interpreter comme le pourcentage de variance (de count1) expliqué par
# les variations du paramètre k.
abline(lm(count1 ~ k)) # on voit bien que la pente est quasi nulle


#################
# ... ni de m...
#################

# Verifiez ici que count1 ne depend pas non plus de m
plot(m, count1)
# C'est assez moche avec les options par defaut, mais on peut mieux faire
# en jouant avec les options
plot(m, count1, pch=3, xlab="m", ylab="count1 (tri des prefixes)",  cex.lab=0.9, cex.axis=0.8, cex=0.8)

summary(lm(count1 ~ m))
# La pente de la regression lineaire vaut 45.04 et elle n'est pas statistiquement
# differente de 0 (p-value Pr(>|t|) largement superieure a 0.05).
# Le coefficient de determination (R^2) vaut 10^-28, donc quasiment 0.
# On peut l'interpreter comme le pourcentage de variance (de count1) expliqué par
# les variations du paramètre m.
abline(lm(count1 ~ m))


########################################################################
# ... mais qu'il depend fortement de la taille du fichier d'entree (n)
########################################################################

plot(n, count1, pch=3, xlab="n", ylab="count1 (tri des prefixes)",  cex.lab=0.9, cex.axis=0.8, cex=0.8)
# Faire le fit lineaire de count1 en fonction de n
summary(lm(count1 ~ n))
# Quel R^2 obtenez-vous ?
# R^2 = 0.9994 environ égal à 1
# Ajoutez la droite de regression sur le graphique.
abline(lm(count1 ~ n))

# count1 correspond au nombre de comparaisons faites par le quicksort
# En principe on peut s'attendre a ce que ce nombre soit proportionel
# a n.log_2(n). Regardons si on peut obtenir un meilleur fit si on
# prend n.log_2(n) en x.

log2n <- log(n,2)
z <- n*log2n
plot(z, count1, pch=3, xlab="n.log2(n)", ylab="count1 (tri des prefixes)",  cex.lab=0.9, cex.axis=0.8, cex=0.8)
summary(lm(count1 ~ z + 0)) # le '+ 0' force une ordonnee a l'origine nulle
# R^2 = 1
# On a donc ameliore le fit en prenant en compte notre connaissance de l'algo.
# On pourra faire des predictions plus precises
abline(lm(count1 ~ z + 0))


##################################################################################
# Au lieu de faire 3 regressions lineaires, on peut faire une regression multiple
##################################################################################

lmcount1 <- lm(count1 ~ n + k + m)
summary(lmcount1)
# On voit que seul le parametre n a un coefficient statistiquement different de 0
# (cf les p-values, colonne Pr(>|t|))



# Faites la meme regression multiple pour count2 et count3 : de quels parametres
# dependent-ils ? est-ce attendu etant donne l'algorithme ?
lmcount2 <- lm(count2 ~ n + k + m)
summary(lmcount2)
# Count 2 : Il dépend principalement de n et k. 

lmcount3 <- lm(count3 ~ n + k + m)
summary(lmcount3)
# Count 3 : Il dépend de n, k et m

#####################################
# Analyse plus approfondie de count3
#####################################

# Comme la dependance de count3 a m est trivialement lineaire, elle n'est pas tres interessante,
# donc on va "eliminer" le parametre m en s'interessant a c3 = count3/m.
# On va essayer de faire un graphique qui montre bien comment c depend de n et k.

c3 <- count3/m
plot(n, c3, col=k, pch=3, xlab="Taille du texte original (n)", ylab="Nb d'appels a wordncmp par mot en sortie", cex.lab=0.9, cex.axis=0.8, cex=0.8)

lm2 <- lm(c3[k==2] ~ 0 + n[k==2] )
abline(lm2, col=2)
summary(lm2) # R-squared = 0.8534

lm3 <- lm(c3[k==3] ~ 0 + n[k==3])
abline(lm3, col=3)
summary(lm3) # R-squared = 0.6567

lm4 <- lm(c3[k==4] ~ 0 + n[k==4])
abline(lm4, col=4)
summary(lm4) # R-squared = 0.4577


lm5 <- lm(c3[k==5] ~ 0 + n[k==5])
abline(lm5, col=5)
summary(lm5) # R-squared = 0.4406

lm6 <- lm(c3[k==6] ~ 0 + n[k==6])
abline(lm6, col=6)
summary(lm6) # R-squared = 0.4124

lm7 <- lm(c3[k==7] ~ 0 + n[k==7])
abline(lm7, col=7)
summary(lm7) # R-squared = 0.4236

legend(0, 120, c("k=2", "k=3", "k=4","k=5","k=6", "k=7"), lty="solid", lwd=2, col=2:7, bty="n", cex=0.8)


# Ce serait plus joli d'avoir une echelle log en x.
plot(log(n), c3, col=k, pch=3, xlab="log(taille du texte original)", ylab="Nb d'appels a wordncmp par mot en sortie", cex.lab=0.9, cex.axis=0.8, cex=0.8)

# On ne peut plus utiliser abline pour dessiner le fit lineaire
# car ce n'est plus une droite en echelle semilog. On utilise
# 'lines' avec en y, les fitted.values de l'objet lm.
lines(log(n[k==2]), lm2$fitted.values, col=2)
lines(log(n[k==3]), lm3$fitted.values, col=3)
lines(log(n[k==4]), lm4$fitted.values, col=4)
lines(log(n[k==5]), lm5$fitted.values, col=5)
lines(log(n[k==6]), lm6$fitted.values, col=6)
lines(log(n[k==7]), lm7$fitted.values, col=7)
legend(8.2, 120, c("k=2", "k=3", "k=4","k=5","k=6", "k=7"), lty="solid", lwd=2, col=2:7, bty="n", cex=0.8)


# On est contents, exportons le graphique au format pdf.
# Avec la commande pdf, on ouvre un "device" pdf :
pdf("count3-normalise-vs-n.pdf", w=4, h=4.5)
# On rappelle ensuite les commandes plot, lines et legend
# tout cela va etre trace dans le pdf
plot(log(n), c3, col=k, pch=3, xlab="log(taille du texte original)", ylab="Nb d'appels a wordncmp par mot en sortie", cex.lab=0.9, cex.axis=0.8, cex=0.8)
lines(log(n[k==2]), lm2$fitted.values, col=2)
lines(log(n[k==3]), lm3$fitted.values, col=3)
lines(log(n[k==4]), lm4$fitted.values, col=4)
lines(log(n[k==5]), lm5$fitted.values, col=5)
lines(log(n[k==6]), lm6$fitted.values, col=6)
lines(log(n[k==7]), lm7$fitted.values, col=7)
legend(8.2, 120, c("k=2", "k=3", "k=4","k=5","k=6", "k=7"), lty="solid", lwd=2, col=2:7, bty="n", cex=0.8)
# Pour finaliser le pdf, on utilise dev.off()
dev.off()


###################################################################
# Analyser la correlation du temps CPU avec count1, count2, count3
###################################################################

cputime <- usr + sys

# Faites le graphique du temps CPU en fonction de count1.
plot(cputime, count1, pch=3, xlab="cputime", ylab="count1 (tri des prefixes)",  cex.lab=0.9, cex.axis=0.8, cex=0.8)
# Faire le fit lineaire de count1 en fonction de n
summary(lm(count1 ~ cputime))
# Quel R^2 obtenez-vous ?
# R^2 = 0.9994 environ égal à 1
# Ajoutez la droite de regression sur le graphique.
abline(lm(count1 ~ cputime))
# Meme question avec count2 et count3.
plot(cputime, count2, pch=3, xlab="cputime", ylab="count2",  cex.lab=0.9, cex.axis=0.8, cex=0.8)
# Faire le fit lineaire de count1 en fonction de n
summary(lm(count2 ~ cputime))
# Quel R^2 obtenez-vous ?
# R^2 = 0.9994 environ égal à 1
# Ajoutez la droite de regression sur le graphique.
abline(lm(count2 ~ cputime))
plot(cputime, count3, pch=3, xlab="cputime", ylab="count3",  cex.lab=0.9, cex.axis=0.8, cex=0.8)
# Faire le fit lineaire de count1 en fonction de n
summary(lm(count3 ~ cputime))
# Quel R^2 obtenez-vous ?
# R^2 = 0.9994 environ égal à 1
# Ajoutez la droite de regression sur le graphique.
abline(lm(count3 ~ cputime))
# Lequel des trois semble le mieux correle au temps CPU ?
# Count 2 ?

# Utilisez cor et cor.test pour analyser plus quantitativement cette correlation.
cor(cputime, count1)
cor(cputime, count2)
cor(cputime, count3)
cor.test(cputime, count2)



#######
# Fin
#######

detach(data1)
