load("donneesProjet2A-2024.RData")
attach(donneesProjet)

require(PCAmixdata)
AcP <- PCAmix(donneesProjet, graph=TRUE)
MatXY <- data.frame(Pct.BF,Age,Weight,Height,Neck,Chest,Abdomen,Hip,Thigh,Knee,Ankle,Bicep,Forearm,Wrist)
pairs(MatXY) # C'est illisible mais en gros on voit à peu près que ça confirme ce que dit l'ACP -> Ankle semble correlé avec 

#Modèle ascendant
res <- lm(Pct.BF~1,data=MatXY)
step(res,~Age+Weight+Height+Neck+Chest+Abdomen+Hip+Thigh+Knee+Ankle+Bicep+Forearm+Wrist)
res <- lm(Pct.BF~Age+Weight+Abdomen+Thigh+Bicep+Wrist, data=MatXY)
#Ba il garde pas Ankle faut croire bizarre
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
# A partir de ce point, l'addition de nouveaux éléments n'est pas très utile

add1(res,~Age+Weight+Height+Neck+Chest+Abdomen+Hip+Thigh+Knee+Ankle+Bicep+Forearm+Wrist)
# On ajoute Wrist
res <- lm(Pct.BF~Abdomen+Weight+Wrist,data=MatXY)
summary(res)
# Multiple R-squared:  0.7335,	Adjusted R-squared:  0.7303 

add1(res,~Age+Weight+Height+Neck+Chest+Abdomen+Hip+Thigh+Knee+Ankle+Bicep+Forearm+Wrist)
# On ajoute Bicep
res <- lm(Pct.BF~Abdomen+Weight+Wrist+Bicep,data=MatXY)
summary(res)
# Multiple R-squared:  0.7386,	Adjusted R-squared:  0.7333 

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
