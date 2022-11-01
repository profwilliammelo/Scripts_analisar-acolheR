wd <- "G:\\Meu Drive\\Docência e pesquisa\\R\\Curso Analisar e Acolher - 2022.2\\Bases de dados"

setwd(wd)

library(rio)
library(tidyverse)

# Abrindo base do exemplo do livro Manual de Investigacao em Ciencias Sociais -----


base_presencas <- import(file = "base_presencas_indice_satisfacao_Manual_Investigacao - Página1.csv") 

glimpse(base_presencas)


# Grafico de dispersao ------

grafico1 <-
  base_presencas %>%
  ggplot(aes(x = indice_razoes_presenca, y = taxa_presenca_media)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red", se = F)

grafico1 + theme_classic() + ggtitle("Correlação linear entre x e y") +
  xlab("RAZAO PARA ESTAR EM AULA") + ylab("Taxa de presenca")

correlacao <- cor(base_presencas$indice_razoes_presenca, base_presencas$taxa_presenca_media)

plotly::ggplotly(grafico1)

# Criando faixas de presenca

base_presencas <- base_presencas %>%
  mutate(categoria_presenca = ifelse(taxa_presenca_media > 53.9, "alta", "baixa"))

# Criando outro grafico ----------

grafico2 <-
  base_presencas %>%
  ggplot(aes(x = indice_razoes_presenca, y = taxa_presenca_media)) +
  geom_point() +
  geom_smooth(method = "lm", aes(col = categoria_presenca), se = F)
