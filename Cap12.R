# 12. Aplicações Florestais

# Instalar pacotes necessários
#install.packages("data.table")
#install.packages("car")
#install.packages("lmtest")
#install.packages("faraway")

# Carregar pacotes
library(data.table)
library(car)
library(lmtest)
library(faraway)

## 12.1. Análise de Variância (ANOVA)
### 12.1.1. Delineamento Inteiramente Casualizado (DIC)
parica <- fread(file = "Data/parica.txt", stringsAsFactors = T)
print(parica)

# Resumo
summary(parica)

# média de altura em cada tratamento?
media <- tapply(parica$Rep, parica$Trat, mean) 
print(media)

# variância da altura em cada tratamento?
var <- tapply(parica$Rep, parica$Trat, var)
print(var)

bartlett.test(parica$Rep, parica$Trat)  # homogeneidade de variâncias

# Gráfico: Tratamentos versus Alturas das plântulas
par(mar = c(4.5,3,1.5,1), mgp = c(2,1,0), mfrow = c(1,2))

# plot: Rep x Trat
plot(parica$Trat, parica$Rep, type="o", 
     main="Schizolobium parahyba",
     xlab="Tratamentos", ylab="Altura (cm)")

points(media, pch=20, col=2, cex=1.5)      # Adiciona médias/tratamento

# Usando boxplot
boxplot(parica$Rep ~ parica$Trat, main="Schizolobium parahyba",
        xlab="Tratamentos", ylab="Altura (cm)")
points(media, pch=20, col=2, cex=1.5)

# ANOVA - Delineamento Inteiramente Casualizado (DIC)
anova.DIC <- aov(Rep~Trat,data=parica)
anova(anova.DIC)

# Resultados da ANOVA - DIC
par(mfrow=c(2,2))
plot(anova.DIC)       # análise dos resíduos

# Teste de Tukey
par(mfrow=c(1,1))
Tukey <- TukeyHSD(anova.DIC)
plot(Tukey)

# Pressupostos da ANOVA
## Teste de Normalidade - shapiro-wilk
shapiro.test(resid(anova.DIC))

# Normal Q-Q plot
qqnorm (resid(anova.DIC))       # obtendo o papel de probabilidade normal.
qqline(resid(anova.DIC))        # inserindo uma linha auxiliar (linear).

# Homogeneidade de variâncias - Teste de Levene
leveneTest(Rep~Trat,parica)

#----------------------------------------------------------------------------
# 12.2 Regressão Linear
teca <- fread("Data/Tectona.csv", stringsAsFactors = T)

## Gráficos de dispersão
par(mar = c(4.5,3.5,1.5,1), mgp = c(2,1,0), mfrow = c(1,2))

plot(DAP,Volume, type = "p", main=NULL, font.main=NULL, col.main=NULL, 
     xlab="DAP (cm)", ylab=expression(Volume~(m^3)), font.lab=1, col.lab="black",
     font.axis=1, col.axis = "black")

plot(H,Volume, type = "p", main=NULL, font.main=NULL, col.main=NULL, 
     xlab="Altura (m)", ylab=expression(Volume~(m^3)), font.lab=1, col.lab="black",
     font.axis=1, col.axis = "black")

## Ajuste de modelos
Berkhout <- lm(Volume ~ DAP, data=teca)                 # Berkhout
KGehrardt <- lm(Volume ~ I(DAP^2), data=teca)           # Kopezky-Gehrardt
SHall <- lm(log(Volume) ~ log(DAP) + log(H), data=teca) # Shumacher-Hall
print(Berkhout); print(KGehrardt); print(SHall)

### Resumo dos ajustes
summary(Berkhout)
summary(KGehrardt)
summary(SHall)

### ANOVA da Regressão
anova(Berkhout)
anova(KGehrardt)
anova(SHall)

### Reta ajustada - Berkhout
par(mar = c(4.5,3.5,1.5,1), mgp = c(2,1,0), mfrow = c(1,1))

plot(teca$Volume~teca$DAP, main="Berkhout", 
     xlab="DAP (cm)", ylab = expression(Volume~(m^3)))
abline(Berkhout,lty=2, col="red")

# Predições e Resíduos - Berkhout
predict(Berkhout)
residuals(Berkhout)

### Análise de Resíduos
#### Normalidade dos resíduos
shapiro.test(Berkhout$residuals)
shapiro.test(KGehrardt$residuals)
shapiro.test(SHall$residuals)

#### Autocorrelação de resíduos
durbinWatsonTest(Berkhout)
durbinWatsonTest(KGehrardt)
durbinWatsonTest(SHall)

#### Heterocedasticidade dos resíduos
bptest(Berkhout)
bptest(KGehrardt)
bptest(SHall)

#### Gráficos de resíduos
# Berkhout
par(mar = c(4.5,3,1.5,1), mgp = c(2,1,0), mfrow = c(2,2))
plot(Berkhout)

# KGehrardt
par(mar = c(4.5,3,1.5,1), mgp = c(2,1,0), mfrow = c(2,2))
plot(KGehrardt)

# Schumacher-Hall
par(mar = c(4.5,3,1.5,1), mgp = c(2,1,0), mfrow = c(2,2))
plot(SHall)

### Multicolinearidade - Regressão múltipla
vif(SHall)

# Fim-----------