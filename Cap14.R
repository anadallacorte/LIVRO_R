## 14. Análise fitossociológica

### 14.1 Estrutura Horizontal

# Instalar pacotes necessários
#install.packages("data.table")
#install.packages("ggplot2")
#install.packages("plyr")

# Carregar pacotes
library(data.table)
library(ggplot2)
library(plyr)

# Carregando o conjunto de dados
FOM <- fread("Data/Fito.csv", stringsAsFactors = T)
FOM

# Inspeção dos dados
nrow(FOM)
names(FOM)
dim(FOM)

# n = Número total de indivíduos amostrados na j-ésima parcela
FOM[, .(n=.N), by=Parcela][]

# Número de indivíduos amostrados da i-ésima espécie na j-ésima parcela
FOM[, .(ni=.N), by=c("Parcela", "Especie")]

# Uma visualização gráfica
ggplot(FOM[, .(ni=.N), by=c("Parcela", "Especie")], 
       aes(x=Especie, y=ni, fill=Especie)) + 
  geom_bar(stat="identity",position="dodge",width = 1,colour="black")+
  geom_text(aes(label=ni,hjust=-.3, vjust=0.5),
            position=position_dodge(width = 0.7))+
  facet_grid(~ Parcela, labeller=labeller(
    Parcela = Parcela<-as_labeller(
      c(`1`="Parcela 1",`2`="Parcela 2",`3`="Parcela 3"))))+
  coord_flip()+
  geom_text(data=ddply(.data=FOM, .(Parcela), summarize, 
                       n=paste("n =", length(Especie))), 
            aes(x=23, y=7, label=n), colour="black", 
            inherit.aes=FALSE, parse=FALSE)+
  theme_bw()+
  theme(axis.line.x=element_line(size=0.5,colour="black"),
        axis.line.y=element_line(size=0.5,colour="black"),
        axis.line=element_line(size=1,colour="black"),
        strip.text.x=element_text(colour=1,size=12,family="serif",face="bold"),
        strip.background = element_rect(colour="black", fill="snow2"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_rect(color="black"),
        panel.background=element_blank(),
        axis.text.x=element_text(colour="black",size=12,family="serif",angle=0),
        axis.text.y=element_text(colour=1,size=12,family="serif",face="italic"),
        legend.position="none")+
  scale_x_discrete(name="Espécie")+
  scale_y_continuous(name="Número de indivíduos",
                     limits=c(0,8))

# Parâmetros da estrutura horizontal - Araucaria angustifolia

## 1. Densidade absoluta
DAi <- function(x, A){
  ni <- nrow(subset(FOM, Especie=="Araucaria angustifolia"))
  DAi <- ni/A
  return(DAi)
}

DAi(x = FOM$Especie, A = 0.3)

## 2. Densidade relativa
DRi <- function(x, A){
  ni <- nrow(subset(FOM, Especie=="Araucaria angustifolia"))
  DAi <- ni/A
  DTA <- length(x)/A
  DRi <- (DAi/DTA)*100
  return(DRi)
}

DRi(x = FOM$Especie, A = 0.3)

## 3. Dominância absoluta
DoAi <- function(data, A, ...){
  data <- data[Especie=="Araucaria angustifolia"]
  gi <- data[, .(gi=pi*DAP^2/40000)]
  Gi <- sum(gi)
  DoAi <- Gi/A
  return(DoAi)
}

DoAi(data=FOM, A=0.3)

## 4. Dominância relativa
DoRi <- function(data, A, ...){
  Gt <- data[, .(gi=pi*DAP^2/40000)]
  data <- data[Especie=="Araucaria angustifolia"]
  gi <- data[, .(gi=pi*DAP^2/40000)]
  Gi <- sum(gi)
  DoAi <- Gi/A
  DoRi <- (Gi/sum(Gt))*100
  return(DoRi)
}

DoRi(data=FOM, A=0.3)

## 5. Frequência absoluta
FAi <- function(data, ...){
  Ut <- length(unique(data$Parcela))
  Ui <- unique(data, by=c("Especie", "Parcela"))[, .(Ui=.N), by="Especie"]
  Ui <- Ui[Especie=="Araucaria angustifolia", Ui]
  FAi <- (Ui/Ut)*100
  return(FAi)
}

FAi(data=FOM)

## 6. Frequência relativa
FRi <- function(data, ...){
  Ut <- length(unique(FOM$Parcela))
  Ui <- unique(FOM, by=c("Especie", "Parcela"))[, .(Ui=.N), by="Especie"]
  FAi <- Ui[, .(FAi=(Ui/length(unique(FOM$Parcela)))*100)]
  Ui_AA <- Ui[Especie=="Araucaria angustifolia", Ui]
  FAi_AA <- (Ui_AA/Ut)*100
  FRi <- (FAi_AA /sum(FAi))*100
  return(FRi)
}

FRi(data=FOM)

## 7. Valor de cobertura
VCi <- DRi(x = FOM$Especie, A = 0.3) + DoRi(data=FOM, A=0.3)
VCi

## 8. Porcentagem de cobertura
PCi <- (DRi(x = FOM$Especie, A = 0.3) + DoRi(data=FOM, A=0.3))/2
PCi

## 9. Valor de importância
VIi <- DRi(x = FOM$Especie, A = 0.3) + DoRi(data=FOM, A=0.3) + FRi(data=FOM)
VIi

## 10. Porcentagem de importância
PIi <- (DRi(x = FOM$Especie, A = 0.3) + DoRi(data=FOM, A=0.3) + FRi(data=FOM))/3
PIi

# Uma função genérica
EH <- function(species, sample, d, A,...){
  DT <- data.table(species=species,sample=sample,d=d)
  DT <- DT[,`:=`(gi=pi*d^2/40000)]
  Ui <- unique(DT, by=c("species", "sample"))[, .(Ui=.N), by="species"][order(species)]
  ni <- DT[, .(ni=.N, Gi = sum(gi)), by="species"]
  ni <- ni[Ui,on="species"]
  EH <- ni[,DAi := ni/A,
  ][,DRi := (DAi/sum(DAi))*100,
  ][,DoAi := Gi/A,
  ][,DoRi := (DoAi/sum(DoAi))*100,
  ][,VC := DRi + DoRi,
  ][,PC := VC/2,
  ][,FAi := (Ui/length(unique(DT$sample)))*100,
  ][,FRi := (FAi/sum(FAi))*100,
  ][,VIi := DRi + DoRi + FRi,
  ][,PIi := VIi/3][order(-VIi)]
  return(EH)
}

EH <- EH(species=FOM$Especie, sample=FOM$Parcela, 
         d=FOM$DAP, A=0.3)

# Fim-----------