library(readxl)
library(dplyr)
library(tidyr)

# organização dos dados ---------------------------------------------------

#dados_c será tabela contendo todas as comidas (1 a 10)
dados_c <- read_excel("comidas.xlsx")
names(dados_c) <- c("id", "sexo", "estacao", paste0("c", 1:10))

# omitir ultimas 4 colunas e renomear de acordo com descrição de comidas 
# do artigo gerando nova tabela: dados
dados <- dados_c[,-(10:13)]
names(dados) <- c("id", "sexo", "estacao", "aranha", "barata", "besouro", 
                  "formiga", "cupim", "traca")

#tidyup tabelas - um tipo de observacao por linha
dados_c <- dados_c %>% gather(key = "comida", value = "qtde", 
                              -c("id", "sexo", "estacao"))
dados <- dados %>% gather(key = "comida", value = "qtde", 
                          -c("id", "sexo", "estacao"))

#id, sexo, estação e comida como variaveis categóricas
dados_c <- dados_c %>% mutate_at(vars(-qtde), as.factor)
dados <- dados %>% mutate_at(vars(-qtde), as.factor)

#colapsando estações
dados_col <- dados %>% group_by(id,sexo,comida) %>% 
  summarise_at("qtde", sum) %>% ungroup()

dados_c_col <- dados_c %>% group_by(id,sexo,comida) %>% 
  summarise_at("qtde", sum) %>% ungroup()

# salvar dados ------------------------------------------------------------

save(dados, dados_c, dados_c_col, dados_col, file='dados.Rda')

