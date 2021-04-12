# 13. Amostragem Aleatória Simples (AAS)

# Instalar pacotes necessários
#install.packages("data.table")
#install.packages("FinCal")

# Carregar pacotes
library(data.table)
library(FinCal)

# Carregando o conjunto de dados
AAS <- fread("Data/AAS.csv", stringsAsFactors = T)
AAS

# Estatísticas básicas para as unidades amostrais
## 1. Média aritmética
mean(AAS$Volume)

## 2. Variância
var(AAS$Volume)

## 3. Desvio Padrão
sd(AAS$Volume)

## 4. Coeficiente de variação
coefficient.variation(sd=sd(AAS$Volume),
                      avg=mean(AAS$Volume))*100

## 5. Intensidade amostral
E <- function(x){
  media = mean(x)
  E = signif(0.1*media, 4)
  return(E)
}

E(AAS$Volume)

## Número de unidades amostrais possíveis na população
N <- function(A,a){
  N <- A/a
  return(N)
}

N <- N(400000,600)
N

## Determinar se a população é finita ou infinita
FC <- function(x,A,a){
  n <- length(x)
  N <- ceiling(A/a)
  f <- n/N
  FC <- 1-f
  
  if(FC >= 0.98){
    cat("A população é Infinita. Portanto, despreze o FC na fórmula da n.\n")
  }else{
    cat("A população é Finita. Portanto, use o FC para corrigir n.\n")
  }
  return(list(f=f,FC=FC))
}

FC <- FC(x=AAS$Volume, A=400000, a = 600)

# Intensidade de amostragem ideal em função da variância
n <- function(x,A,a){
  N <- ceiling(A/a)
  E = 0.1*mean(x)
  t = qt(1-.05/2, df=length(x)-1)
  n <- ceiling((N*t^2*var(x))/(N*E^2 + t^2*var(x)))
  cat(paste("Para atender ao erro estabelecido você deve amostrar", n, 
            "parcelas.\n"))
  
  if(n <= length(x)){
    cat("Esforço amostral satisfatório. O IF é definitivo!")
  }else{
    cat(paste("Retorne a campo e meça mais", abs(length(x)-n), "parcelas."))
  }
}

n(x = AAS$Volume, A = 400000, a = 600)

## 6. Variância da média
var(AAS$Volume)/length(AAS$Volume)*(FC$FC)

## 7. Erro padrão da média
sbarx <- sd(AAS$Volume)/sqrt(length(AAS$Volume))*(sqrt(FC$FC))
sbarx

## 8. Erro de amostragem
# a) Erro de amostragem absoluto
Ea <- qt(1-.05/2, df=length(AAS$Volume)-1)*sbarx
Ea

# b) Erro de amostragem relativo
Er <- Ea/mean(AAS$Volume)*100
Er

## 9. Intervalo de confiança para média
# Limite inferior para média (LI)
LIbarx <- mean(AAS$Volume)-Ea
LIbarx

# Limite superior para média (LS)
LSbarx <- mean(AAS$Volume)+Ea
LSbarx

## 10. Total da população
hatX <- N*mean(AAS$Volume)
hatX

## 11. Intervalo de confiança para o total
# Limite inferior para total da população (LI)
LIhatx <- hatX - N*Ea
LIhatx

# Limite superior para total da população (LS)
LShatx <- hatX + N*Ea
LShatx

# Usando o pacote "forester" (em construção)
# O pacote "forester" ainda não está disponível no CRAN. 
# Mas, as funções existentes podem ser usadas a partir da 
# versão em desenvolvimento disponível no GitHub.

# Instalar a versão em desenvolvimento...
# remotes::install_github("DeivisonSouza/forester")
library(forester)
RS(x = AAS$Volume, A = 400000, a = 600, LE = 0.1)

# Resultados no painel de visualização
RS(x = AAS$Volume, A = 400000, a = 600, 
   LE = 0.1, DT = TRUE)

# Fim-----------