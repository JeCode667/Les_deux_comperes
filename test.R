load("donneesProjet2A-2024.RData")
attach(donneesProjet)

require(PCAmixdata)
AcP <- PCAmix(donneesProjet, graph=TRUE)
MatXY <- data.frame(Pct.BF,Age,Weight,Height,Neck,Chest,Abdomen,Hip,Thigh,Knee,Ankle,Bicep,Forearm,Wrist)
pairs(MatXY) # C'est illisible mais en gros on voit à peu près que ça confirme ce que dit l'ACP -> Ankle semble correlé avec 

#Un peu d'infos sur Pct.Bf
summary(Pct.BF)

#Modèle ascendant
res <- lm(Pct.BF~1,data=MatXY)
step(res,~Age+Weight+Height+Neck+Chest+Abdomen+Hip+Thigh+Knee+Ankle+Bicep+Forearm+Wrist)
res <- lm(Pct.BF~Age+Weight+Abdomen+Thigh+Bicep+Wrist, data=MatXY)
#Ba il garde pas Ankle 
summary(res)
# Multiple R-squared:  0.7424,	Adjusted R-squared:  0.736 

res <- lm(Pct.BF~1,data=MatXY)

add1(res,~Age+Weight+Height+Neck+Chest+Abdomen+Hip+Thigh+Knee+Ankle+Bicep+Forearm+Wrist)
# On ajoute Abdomen
res <- lm(Pct.BF~Abdomen,data=MatXY)
summary(res)
# Multiple R-squared:  0.6785,	Adjusted R-squared:  0.6772 

add1(res,~Age+Weight+Height+Neck+Chest+Abdomen+Hip+Thigh+Knee+Ankle+Bicep+Forearm+Wrist)
# On ajoute Weight
res <- lm(Pct.BF~Abdomen+Weight,data=MatXY)
summary(res)
# Multiple R-squared:  0.7228,	Adjusted R-squared:  0.7205
#Pct.Bf=-47.44511+0.97651*Abdomen -0.13276*Weight

# A partir de ce point, l'addition de nouveaux éléments n'est pas très utile
shapiro.test(res$residuals)#pvalue = 0.1917> au risque de première espèce, on garde la normalité
sigma <- sd(res$residuals)
plot(res$fitted,res$residuals)# pas de soucis
abline(h=2*sigma,col=2)
abline(h=-2*sigma,col=2)

#Par curiosité on peut continuer d'ajouter des variables
add1(res,~Age+Weight+Height+Neck+Chest+Abdomen+Hip+Thigh+Knee+Ankle+Bicep+Forearm+Wrist)
# On ajoute Wrist
res2 <- lm(Pct.BF~Abdomen+Weight+Wrist,data=MatXY)
summary(res2)
# Multiple R-squared:  0.7335,	Adjusted R-squared:  0.7303 
shapiro.test(res$residuals)# pvalue =5,8%... là les résidus sont quasiment plus normaux... une raison de plus de ne pas rajouter cette variable

add1(res,~Age+Weight+Height+Neck+Chest+Abdomen+Hip+Thigh+Knee+Ankle+Bicep+Forearm+Wrist)
# On ajoute Bicep
res2 <- lm(Pct.BF~Abdomen+Weight+Wrist+Bicep,data=MatXY)
summary(res2)
# Multiple R-squared:  0.7375,	Adjusted R-squared:  0.7332 même pas d'augmentation de R-squared
shapiro.test(res$residuals)# là c'est plus normal.


# Modèle sans outliers
rstudent <- rstudent(res)
outliers <- which(abs(rstudent) > 2)
dataSansOutliers <- MatXY[-outliers, ]
resSO <- lm(Pct.BF~Weight+Abdomen, data= dataSansOutliers)
summary(resSO)## Avec le modèle précédent sans outliers, on a Multiple R-squared:  0.7522,	Adjusted R-squared:  0.7501
#Pct.BF =-47.43103 + 0.98750*Abdomen -0.13808*Weight

shapiro.test(resSO$residuals)# rejet de la normalité mais en même temps on a enlevé des valeurs donc...
sigma <- sd(resSO$residuals)
plot(resSO$fitted,resSO$residuals)# pas de motif apparent...
abline(h=2*sigma,col=2)
abline(h=-2*sigma,col=2)

#On essaye un autre step mais cette fois avec les datas sans outliers
res3 <- lm(Pct.BF~1,dataSansOutliers)
step(res3,~Age+Weight+Height+Neck+Chest+Abdomen+Hip+Thigh+Knee+Ankle+Bicep+Forearm+Wrist)
#On obtient les paramètres :Abdomen + Weight + Wrist + Forearm + Age + Thigh + Ankle 

res3 <-lm(Pct.BF~Abdomen + Weight + Wrist + Forearm + Age + 
            Thigh + Ankle,dataSansOutliers)
summary(res3)#Multiple R-squared:  0.7727,	Adjusted R-squared:  0.7659 
#Pct.Bf =-43.56765 +0.91215*Abdomen -0.13608*Weight - 1.74715*Wrist + 0.33775*Forearm + 0.07364*Age + 0.24381*Thigh + 0.30945*Ankle
#On voit que Ankle n'est probablement pas significatif (pvalue = 12%) et que forerarm possiblement non plus

res4 <-lm(Pct.BF~Abdomen + Weight + Wrist + Forearm + Age + 
            Thigh,dataSansOutliers)
summary(res4)#Multiple R-squared:  0.7704,	Adjusted R-squared:  0.7645, forerarm toujours pas significatif à priori

res5<-lm(Pct.BF~Abdomen + Weight + Wrist + Age + 
                Thigh,dataSansOutliers)
summary(res5)#Multiple R-squared:  0.7672,	Adjusted R-squared:  0.7622 
# Toutes les variables sont "significatives" Mais pour 3 variables de plus que res on gagne que 3 point d'explicabilité... pas ouf
# Pct.BF =-35.59543 + 0.88302*Abdomen -0.10835*Weight -1.41729*Wrist + 0.06689*Age + 0.25869*Thigh

####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
### Tout ce qu'il y a en dessous on jette je pense
#On peut conclure avec res ça suffit


add1(res,~Age+Weight+Height+Neck+Chest+Abdomen+Hip+Thigh+Knee+Ankle+Bicep+Forearm+Wrist)
# On ajoute Age
res <- lm(Pct.BF~Abdomen+Weight+Wrist+Bicep+Age,data=MatXY)
summary(res)
# Multiple R-squared:  0.7415,	Adjusted R-squared:  0.7352 

add1(res,~Age+Weight+Height+Neck+Chest+Abdomen+Hip+Thigh+Knee+Ankle+Bicep+Forearm+Wrist)
# On ajoute Thigh
res <- lm(Pct.BF~Abdomen+Weight+Wrist+Bicep+Age+Thigh,data=MatXY)
summary(res)
# Multiple R-squared:  0.7424,	Adjusted R-squared:  0.736 

add1(res,~Age+Weight+Height+Neck+Chest+Abdomen+Hip+Thigh+Knee+Ankle+Bicep+Forearm+Wrist)
# On ne peut rien ajouter

sigma <- sd(res$residuals)
plot(res$fitted,res$residuals)
abline(h=2*sigma,col=2)
abline(h=-2*sigma,col=2)
#On voit des valeurs un peu loin du reste
shapiro.test(res$residuals)# pvalue = 3,8%<5% donc rejet de H0 hypothèse de normalité -> outliers ?

# Modèle ascendant sans outliers
rstudent <- rstudent(res)
outliers <- which(abs(rstudent) > 2)
dataSansOutliers <- MatXY[-outliers, ]
res <- lm(Pct.BF~Age+Weight+Abdomen+Thigh+Bicep+Wrist, data= dataSansOutliers)
summary(res)
sigma <- sd(res$residuals)
plot(res$fitted,res$residuals)
abline(h=2*sigma,col=2)
abline(h=-2*sigma,col=2)
# je pense jvais plot toutes les données et voir où est-ce que y'a des valeurs trop loin du reste 
boxplot(Pct.BF)
plot(Pct.BF)
title("Pct.Bf")
shapiro.test(Pct.BF)# pvalue > 5% Ok non rejet de l'hypothèse de normalité

#Modèle descendant
res <- lm(Pct.BF~Age+Weight+Height+Neck+Chest+Abdomen+Hip+Thigh+Knee+Ankle+Bicep+Forearm+Wrist,data=MatXY)
step(res)
res <- lm(Pct.BF~Age+Height+Neck+Abdomen+Hip+Thigh+Forearm+Wrist,data=MatXY)
summary(res)
# Multiple R-squared:  0.7469,	Adjusted R-squared:  0.7385
# Le modèle descendant est meilleur

res <- lm(Pct.BF~Age+Weight+Height+Neck+Chest+Abdomen+Hip+Thigh+Knee+Ankle+Bicep+Forearm+Wrist,data=MatXY)

drop1(res)
# On retire Knee
res <- lm(Pct.BF~Age+Weight+Height+Neck+Chest+Abdomen+Hip+Thigh+Ankle+Bicep+Forearm+Wrist,data=MatXY)
summary(res)
# Multiple R-squared:  0.7505,	Adjusted R-squared:  0.7378 

drop1(res)
# On retire Weight
res <- lm(Pct.BF~Age+Height+Neck+Chest+Abdomen+Hip+Thigh+Ankle+Bicep+Forearm+Wrist,data=MatXY)
summary(res)
# Multiple R-squared:  0.7504,	Adjusted R-squared:  0.7388 

drop1(res)
# On retire Ankle
res <- lm(Pct.BF~Age+Height+Neck+Chest+Abdomen+Hip+Thigh+Bicep+Forearm+Wrist,data=MatXY)
summary(res)
# Multiple R-squared:  0.7497,	Adjusted R-squared:  0.7393

drop1(res)
# On retire Bicep
res <- lm(Pct.BF~Age+Height+Neck+Chest+Abdomen+Hip+Thigh+Forearm+Wrist,data=MatXY)
summary(res)
# Multiple R-squared:  0.7486,	Adjusted R-squared:  0.7392 

drop1(res)
# On retire Chest
res <- lm(Pct.BF~Age+Height+Neck+Abdomen+Hip+Thigh+Forearm+Wrist,data=MatXY)
summary(res)
# Multiple R-squared:  0.7469,	Adjusted R-squared:  0.7385

drop1(res)
# On ne peut plus rien retirer

sigma <- sd(res$residuals)
plot(res$fitted,res$residuals)
abline(h=2*sigma,col=2)
abline(h=-2*sigma,col=2)
#On voit des valeurs un peu loin du reste
shapiro.test(res$residuals)# pvalue = 13%>5% non rejet de H0 hypothèse de normalité.

#drop 
res <- lm(Pct.BF~Age+Weight+Height+Neck+Chest+Abdomen+Hip+Thigh+Knee+Ankle+Bicep+Forearm+Wrist,data=MatXY)
drop1(res)

rstudent <- rstudent(res)
outliers <- which(abs(rstudent) > 2)
dataSansOutliers <- MatXY[-outliers, ]
res <- lm(Pct.BF~Ankle, data= dataSansOutliers)
summary(res)
sigma <- sd(res$residuals)
plot(res$fitted,res$residuals)
abline(h=2*sigma,col=2)
abline(h=-2*sigma,col=2)
# je pense jvais plot toutes les données et voir où est-ce que y'a des valeurs trop loin du reste 
boxplot(Pct.BF)
plot(Pct.BF)
title("Pct.Bf")
shapiro.test(Pct.BF)# pvalue > 5% Ok non rejet de l'hypothèse de normalité

#Age
boxplot(Age)
plot(Age)
title("Age")
shapiro.test(Age) #pvalue << 5% chelou -> rejet de l'hypothèse de normalité
hist(Age)
