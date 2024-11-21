load("donneesProjet2A-2024.RData")
attach(donneesProjet)

require(PCAmixdata)
AcP <- PCAmix(donneesProjet, graph=TRUE)
MatXY <- data.frame(Pct.BF,Age,Weight,Height,Neck,Chest,Abdomen,Hip,Thigh,Knee,Ankle,Bicep,Forearm,Wrist)
pairs(MatXY) # C'est illisible mais en gros on voit à peu près que ça confirme ce que dit l'ACP -> Ankle semble correlé avec 

res <- lm(Pct.BF~1,data=MatXY)
step(res,~Age+Weight+Height+Neck+Chest+Abdomen+Hip+Thigh+Knee+Ankle+Bicep+Forearm+Wrist)
#Ba il garde pas Ankle faut croire bizarre
summary(res)
sigma <- sd(res$residuals)
plot(res$fitted,res$residuals)
abline(h=2*sigma,col=2)
abline(h=-2*sigma,col=2)
#On voit des valeurs un peu loin du reste
shapiro.test(res$residuals)# pvalue = 17%>5% non rejet de H0 hypothèse de normalité.

#Ankle 
plot(Pct.BF,Ankle)
res <- lm(Pct.BF~Ankle)
summary(res)
sigma <- sd(res$residuals)
plot(res$fitted,res$residuals)
abline(h=2*sigma,col=2)
abline(h=-2*sigma,col=2)


rstudent <- rstudent(res)
outliers <- which(abs(rstudent) > 2)
dataSansOutliers <- MatXY[-outliers, ]
res <- lm(Pct.BF~Ankle, data= dataSansOutliers)
summary(res)
sigma <- sd(res$residuals)
plot(res$fitted,res$residuals)
abline(h=2*sigma,col=2)
abline(h=-2*sigma,col=2)
# je pense jvais plot toutes les données et voir où est-ce que y'a des valeurs trop loin du reste parce que là c'est des trucs de fou
boxplot(Pct.BF,title("Pct.BF"))
boxplot(Age,title="Age")
boxplot(Weight,title("Weight"))
