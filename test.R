load("donneesProjet2A-2024.RData")
attach(donneesProjet)

require(PCAmixdata)
AcP <- PCAmix(donneesProjet, graph=TRUE)
 