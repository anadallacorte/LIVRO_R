# 11 Teste de hipóteses
## 11.1 Teste t-Student

# A seguir, considere os diâmetros de árvores oriundos de 
# diferentes amostras:
dap1 <- c(30.5,35.3,33.2,40.8,42.3,41.5,36.3,43.2,34.6,38.5)
dap2 <- c(28.2,35.1,33.2,35.6,40.2,37.4,34.2,42.1,30.5,38.4)

## 11.1.1 Teste t para uma média

t.test(dap1, # amostra a ser testada
       mu=35, # hipótese de nulidade
       alternativa = "greater") # teste unilateral à direita

## 11.1.2 Teste t para as médias de duas amostras independentes

t.test(dap1, dap2, # amostras a serem comparadas
       conf.level = 0.99) # nível de significância

## 11.1.3 Teste t para médias de duas amostras dependentes
t.test(dap1, dap2,
       conf.level = 0.99, # nível de significância
       paired = TRUE) # afirma dependência entre as amostras

# Fim-----------
