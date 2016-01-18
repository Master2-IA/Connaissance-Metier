# On separe la fenetre graphique en 2 car on va 
# comparer deux cas

par(mfrow=c(2,1)) # 2 lignes, 1 colonne


#######################
# Cas ou H0 est vraie 
####################### 

# On va considerer deux populations qui ont exactement
# les memes parametres : les deux sont uniformement 
# distribuees entre 0 et 1. Les deux populations ont
# donc la meme moyenne, en l'occurrence 0.5. 
# Puis nous allons faire une experience : 
# - tirer un petit echantillon dans chaque population
# - calculer la moyenne observee dans l'echantillon 1,
#   puis celle observee dans l'echantillon 2. On 
#   n'obtiendra pas exactement 0.5 ni pour l'une ni pour 
#   l'autre.
# - calculer la difference entre les deux moyennes 
#   d'echantillons. Appelons d cette difference.

tirerEchantillonPop1H0 <- function(n) {
	vraieMoyenne <- 0.5
	plageDeDispersion <- 1.0
	min <- vraieMoyenne - plageDeDispersion/2.0
	max <- vraieMoyenne + plageDeDispersion/2.0
	ech <- runif(n, min, max)
	return(ech)
}

tirerEchantillonPop2H0 <- function(n) {
	vraieMoyenne <- 0.5
	plageDeDispersion <- 1.0
	min <- vraieMoyenne - plageDeDispersion/2.0
	max <- vraieMoyenne + plageDeDispersion/2.0
	ech <- runif(n, min, max)
	return(ech)
}

# Ecrire ici le code permettant de faire une "experience"
# c'est a dire tirer un echantillon de taille 20 dans 
# chaque population et mesurer la difference d observee
# entre les 2 moyennes d'echantillon.
pop1 <- tirerEchantillonPop1H0(20)
pop2 <- tirerEchantillonPop2H0(20)

mean(pop1)
mean(pop2)
abs(mean(pop1) - mean(pop2))
abs(mean(tirerEchantillonPop1H0(20)) - mean(tirerEchantillonPop2H0(20)))

# Si 1000 etudiants font independamment cette experience,
# chaque etudiant aura des echantillons differents et donc
# chacun aura une valeur differente pour d. Si nous 
# mettons toutes ces 1000 valeurs ensemble dans un vecteur
# (appele diffMoyH0), quelle sera la moyenne de ce vecteur ? 
# Ecrire ici le code pour repondre a cette question.

diffMoyH0 = c()

for(i in  1:1000)
{
  diffMoyH0 = c(diffMoyH0, abs(mean(tirerEchantillonPop1H0(20)) - mean(tirerEchantillonPop2H0(20))))
}

mean(diffMoyH0)

# On obtient donc 0.071


# Ecrire le code pour tracer l'histogramme de ces 1000 valeurs.   

plot(diffMoyH0)
abline(h=mean(diffMoyH0), col="red")


#######################
# Cas ou H1 est vraie 
####################### 

# On va voir comment se comporte notre indicateur quand 
# les deux populations n'ont pas la meme moyenne
# (mais ont tout de meme la meme dispersion)
# eg . pop1 comprise entre 0.0 et 1.0
#   et pop2 comprise entre 0.2 et 1.2 
  
tirerEchantillonPop1H1 <- function(n) {
	vraieMoyenne <- 0.5
	plageDeDispersion <- 1.0
	min <- vraieMoyenne - plageDeDispersion/2.0
	max <- vraieMoyenne + plageDeDispersion/2.0
	ech <- runif(n, min, max)
	return(ech)
}

tirerEchantillonPop2H1 <- function(n) {
	vraieMoyenne <- 0.7
	plageDeDispersion <- 1.0
	min <- vraieMoyenne - plageDeDispersion/2.0
	max <- vraieMoyenne + plageDeDispersion/2.0
	ech <- runif(n, min, max)
	return(ech)
}


# Ecrire ici le code pour faire les 1000 experiences 
# comme precedemment, en nommant cette fois le vecteur
# diffMoyH1. Calculez sa moyenne et tracez son histogramme.

diffMoyH1 = c()

for(i in  1:1000)
{
  diffMoyH1 = c(diffMoyH1, abs(mean(tirerEchantillonPop1H1(20)) - mean(tirerEchantillonPop2H1(20))))
}

mean(diffMoyH1)

# On obtient donc 0.2

pdf("x1-x2-2.pdf", w=4, h=4.5)
par(mfrow=c(2,1))
plot(x1)
abline(h=cMean(x1, x2), col="red")
plot(x2)
abline(h=cMean(x1, x2), col="red")
dev.off()

plot(diffMoyH1)
abline(h=mean(diffMoyH1), col="red")

pdf("diffMoy.pdf", w=4, h=4.5)
plot(diffMoyH0)
abline(h=mean(diffMoyH0), col="red")
plot(diffMoyH1)
abline(h=mean(diffMoyH1), col="red")
dev.off()

# Attention, ceci n'est qu'un exemple :
# en realite,  il y a une infinite de "H1" possibles.
# Par exemple, la population 2 pourrait avoir une 
# moyenne de 0.55, ou 0.7, etc. On dit "H1"
# pour indiquer "contraire de H0" (qui, elle, est unique).




################################
# Trancher entre H0 et "pas H0"
################################

# Placons nous maintenant dans le cas ou l'on ait fait 
# une seule experience. On a tire les deux echantillons 
# suivants :
# > x1
# [1] 0.97050525 0.13734516 0.80793033 0.05207726 0.62629180 0.93485856
# [7] 0.58220744 0.65935145 0.76467195 0.73512414 0.45139560 0.93225380
# [13] 0.53790595 0.99845675 0.31035081 0.43082815 0.15475353 0.42647652
# [19] 0.65676067 0.74186048
# > x2
# [1] 0.33565036 0.28830545 0.51556544 0.93223089 0.29192576 0.43505823
# [7] 0.63127002 0.86082799 0.56533392 0.19083212 0.13087779 0.09849703
# [13] 0.98921291 0.91480756 0.78556552 0.33859160 0.88482223 0.76701274
# [19] 0.24190609 0.46251866
#
# Ecrire le code pour evaluer la plausibilite de H0: "les deux 
# echantillons proviennent de deux populations qui ont la meme moyenne".

cMean <- function(v1, v2)
{
  return(abs(mean(v1) - mean(v2)))
}
sameMean <- function(v1, v2)
{
  return(cMean(v1, v2) < 0.1)
}

sameMean(tirerEchantillonPop1H0(20), tirerEchantillonPop2H0(20))
sameMean(tirerEchantillonPop1H1(20), tirerEchantillonPop2H1(20))

x1 <- c(0.97050525, 0.13734516, 0.80793033, 0.05207726, 0.62629180, 0.93485856,
        0.58220744, 0.65935145, 0.76467195, 0.73512414, 0.45139560, 0.93225380,
        0.53790595, 0.99845675, 0.31035081, 0.43082815, 0.15475353, 0.42647652,
        0.65676067, 0.74186048)
x2 <- c(0.33565036, 0.28830545, 0.51556544, 0.93223089, 0.29192576, 0.43505823,
        0.63127002, 0.86082799, 0.56533392, 0.19083212, 0.13087779, 0.09849703,
        0.98921291, 0.91480756, 0.78556552, 0.33859160, 0.88482223, 0.76701274,
        0.24190609, 0.46251866)

pdf("x1-x2-1.pdf", w=4, h=4.5)
par(mfrow=c(2,1))
plot(x1)
abline(h=cMean(x1, x2), col="red")
plot(x2)
abline(h=cMean(x1, x2), col="red")
dev.off()

cMean(x1, x2)
sameMean(x1, x2)
# Oui, ces deux échantillons sont de même moyenne


# Meme question avec les deux echantillons suivants :
# > x1
# [1] 0.41236444 0.28422821 0.15093798 0.05885328 0.25514435 0.63026931
# [7] 0.56325462 0.76304859 0.56523993 0.92535660 0.10898729 0.51579642
# [13] 0.07223967 0.53483839 0.52516575 0.20250815 0.89634680 0.53879059
# [19] 0.58736912 0.53945749
# > x2
# [1] 1.0809923 0.5772333 0.7252340 1.1529082 1.0924642 0.6046166 0.9495800
# [8] 0.3019857 0.7701195 1.0746508 0.3928894 1.0885017 0.5101510 1.1871599
# [15] 0.7953318 0.5711237 0.5505642 0.9854085 0.5105643 0.9601635


x1 <- c(0.41236444, 0.28422821, 0.15093798, 0.05885328, 0.25514435, 0.63026931,
        0.56325462, 0.76304859, 0.56523993, 0.92535660, 0.10898729, 0.51579642,
        0.07223967, 0.53483839, 0.52516575, 0.20250815, 0.89634680, 0.53879059,
        0.58736912, 0.53945749)
x2 <- c(1.0809923, 0.5772333, 0.7252340, 1.1529082, 1.0924642, 0.6046166, 0.9495800,
        0.3019857, 0.7701195, 1.0746508, 0.3928894, 1.0885017, 0.5101510, 1.1871599,
        0.7953318, 0.5711237, 0.5505642, 0.9854085, 0.5105643, 0.9601635)

pdf("x1-x2-2.pdf", w=4, h=4.5)
par(mfrow=c(2,1))
plot(x1)
abline(h=cMean(x1, x2), col="red")
plot(x2)
abline(h=cMean(x1, x2), col="red")
dev.off()

cMean(x1, x2)
sameMean(x1, x2)
# Non, ces deux échantillons ne sont pas de même moyenne
