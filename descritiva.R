load("dados.Rda")
library(ggplot2)
library(ggcorrplot)
library(Hmisc)

# graficos ----------------------------------------------------------------

#Número de comidas para cada observação agrupadas por sexo.
ggplot(dados_col, aes(x=comida,y=qtde)) + geom_boxplot() + labs(title="Relação de quantidade e Alimento",x="Alimento", y = "Quantidade")

ggplot(dados_col, aes(x=sexo,y=qtde)) + geom_boxplot() + facet_wrap(~ comida) + theme(legend.position="none") + labs(title="Relação de quantidade e Alimento por sexo",x="Sexo", y = "Quantidade")


#Correlação das comidas:
correlacao <- cor(dados_col[,c(3:9)])
ggcorrplot(correlacao, hc.order = TRUE, lab = TRUE)

# facetado estaçao
ggplot(dados, aes(x=comida,y=qtde, fill=sexo)) + geom_boxplot() + labs(title="Relação de quantidade e alimento por sexo",x="Alimento", y = "Quantidade")  + facet_wrap(~ estacao) 

# O segundo gráfico com estação pode ser obsevardo abaixo:
ggplot(dados, aes(x=estacao,y=qtde)) + geom_boxplot() + labs(title="Relação de quantidade e estação por alimento",x="Estação", y = "Quantidade")  + facet_wrap(~ comida)


# analises ----------------------------------------------------------------

analise <- describe(dados)

#análises descritivas
aran <- subset(dados, dados$comida == "aranha")
bara <- subset(dados, dados$comida == "barata")
beso <- subset(dados, dados$comida == "besouro")
cupi <- subset(dados, dados$comida == "cupim")
formi <- subset(dados, dados$comida == "formiga")
trac <- subset(dados, dados$comida == "traca")
