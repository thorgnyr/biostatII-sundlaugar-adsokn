data <- read.table("gogn-transposed2.csv", header = TRUE, sep = ";")
library(gmodels)

#hist(summary(data$Fjoldi))

#Ætla að nota sundlaugar og Teg gjalda sem skýribreytur og Y sem svarbreytuna sem segir til um hvort 
#aðsókn var meiri (1) eða minni (0)

#Hlutfall liða sem að lækkuðu/hækkuðu
table(data$Y)
prop.table(table(data$Y))

table(data$Sundlaug, data$Y)
prop.table(table(data$Sundlaug, data$Y), 1)

table(data$Teg_gjald, data$Y)
prop.table(table(data$Teg_gjald, data$Y), 1)

table(data$Teg_gjald, data$Sundlaug, data$Y) #Þrívíð tafla, fjöldi hækkana vs. fjöldi lækkana

CrossTable(data$Teg_gjald, data$Y, prop.c = F, prop.chisq = F, prop.t = F, expected = T)
#Sjáum að chisq gildið er hátt, getum því hafnað 0 tilgátunni.

CrossTable(data$Sundlaug, data$Y, prop.c = F, prop.chisq = F, prop.t = F, expected = T)
#Sjáum að chisq gildið er lágt, getum því ekki hafnað 0 tilgátunni á grundvelli sundlauga.
#Þetta er alveg mikilvægt!

#Þarf að gera nýjan dálk sem að gerir grein fyrir muninum fyrir hverja röð. En af hverju?

#Væri áhugavert að gera RxC töflu þar sem við gerum Y samanburð við tiltekna 
#aðra liði, t.d. þar sem Teg_gjald er eitthvað tiltekið

#Myndi það styrkja módelið að bera saman árin á undan?

#Smíða glm (logistic regression model). Hvað sýnir það? Hvaða merkingu hefur vinnan hér á undan?

fit.simple <- glm(Y ~ Teg_gjald, data = data)
summary(fit.simple)
