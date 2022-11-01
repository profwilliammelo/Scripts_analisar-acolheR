# AULA DE REPOSICAO 17/09

library(tidyverse)
library(weights) # para correlacoes com peso usando PNADc

# VARIAVEL QUANTITATIVA DISCRETA --------------

idade <- c(30, 20, 19, 27, 47, 34)


## Tamanho da amostra

n <- length(idade)

## Media "na mao" ------------

media_idade <- sum(idade)/n

## Media com funcao -----------

mean(idade, na.rm = T)

# Variancia "na mao"

desvios_quadraticos <- NULL

for (i in 1:n){print(i)
  
  desvios_quadraticos[i] <- (idade[i]-media_idade)^2
}

variancia_idade <- sum(desvios_quadraticos)/(n-1)

## Desvio padrao "na mao" --------

desvio_padrao_idade <- sqrt(variancia_idade)

mean(idade)

## Variancia e desvio padrao com funcoes -------

var(idade)
sd(idade)


# FOMOS ATE AQUI NA AULA DE REPOSICAO --------



## Grafico descritivo - frequencia

banco_de_dados <- data.frame(idade) 

banco_de_dados$idade

grafico <- banco_de_dados %>%  
  ggplot(mapping = aes(x = idade)) +
  geom_bar(fill = "green", col = "black") +
  scale_y_continuous(limits = c(0,1), breaks = c(0, 1)) +
  ylab(label = "CONTAGEM") +
  xlab(label = "IDADE") +
  theme_classic() +
  ggtitle("Contagem de pessoas de cada idade",
          subtitle = "Dados fictícios")


grafico_descritivo_idade <-
  
 data.frame(idade) %>%
 ggplot(aes(x = idade)) +
 geom_bar(fill = "blue", col = "black") + 
 scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
 theme_linedraw()
  

# CRIANDO OUTRA VARIAVEL DISCRETA (ESCOLARIDADE - ANOS DE ESTUDO)

anos_estudo <- c(8, 6, 5, 7, 9, 10) 

# Olhando para a correlacao entre idade e anos de estudo ------------------

grafico_idade_anos_estudo <- 
  
  data.frame(idade, anos_estudo) %>%
  ggplot(aes(x = idade, y = anos_estudo)) +
  geom_point(col = "black") +
  geom_smooth(col = "red", method = "lm", se = F) +
  theme_classic() +
  xlab("Idade") +
  ylab("Anos de estudo") +
  ggtitle("Correlação linear entre idade e anos de estudo", subtitle = "Dados fictícios")

cor(anos_estudo, idade)

# criando variavel continua - PNAD CONTINUA 2019 ----------

wd <- "G:\\Meu Drive\\Docência e pesquisa\\R\\Lego I\\Bancos de dados baixados\\PNADS" # coloque aqui o caminho da sua pnad disponibilizada no Notion

setwd(wd)

getwd()

pnadc_2020 <- rio::import("pnadc_2020_t1.fst")

pnadc_2020 <- pnadc_2020 %>% mutate(renda_trabalho = VD4019,
                                    log_renda_trabalho = log(VD4019),
                                    cor = V2010,
                                    anos_estudo = VD3005,
                                    anos_estudo_numerica = as.numeric(VD3005),
                                    peso = V1027) %>%
                             mutate(raca = case_when(cor == "Branca" ~ "branco",
                                                       cor == "Preta" | cor == "Parda" ~ "negro",
                                                       T ~ as.character(NA)))




## Grafico descritivo - histograma e densidade

options(scipen = 999) # desligando notacao cientifica

grafico_renda_hist <-
  
pnadc_2020 %>%
  ggplot(aes(x = renda_trabalho, y = stat(density))) +
  geom_histogram(col = "black", fill = "blue", binwidth = 30) +
  scale_y_continuous(limits = c(0, 0.0015))

grafico_renda_densidade_com_peso <- pnadc_2020 %>%
  ggplot(aes(x = renda_trabalho)) +
  geom_density(col = "black", fill = "blue", aes(weight = peso)) +
  scale_y_continuous(limits = c(0, 0.0015))

grafico_renda_densidade_sem_peso <- pnadc_2020 %>%
  ggplot(aes(x = renda_trabalho)) +
  geom_density(col = "black", fill = "blue") +
  scale_y_continuous(limits = c(0, 0.0015))

ggpubr::ggarrange(grafico_renda_densidade_com_peso, grafico_renda_densidade_sem_peso)


grafico_log_renda_hist_com_peso <-
  pnadc_2020 %>%
  ggplot(aes(x = log(renda_trabalho), y = stat(density), weight = peso)) +
  geom_histogram(col = "black", fill = "blue", binwidth = 0.05) +
  scale_y_continuous(limits = c(0, 1.5))


grafico_log_renda_densidade <-
  pnadc_2020 %>%
  ggplot(aes(x = log(renda_trabalho), weight = peso)) +
  geom_density(col = "black", fill = "blue") +
  scale_y_continuous(limits = c(0, 1.5)) +
  ggtitle("Distribuição de log de renda do trabalho", subtitle = "Valores ponderados / PNAD continua 2020") +
  theme_classic() +
  xlab("Renda do trabalho - em log") +
  ylab("Densidade de frequência")


grafico_boxplot_renda <- 
  pnadc_2020 %>%
  ggplot(aes(y = log(renda_trabalho, weight = peso))) +
  geom_boxplot(col = "black", fill = "blue")

grafico_boxplot_renda_x <- 
  pnadc_2020 %>%
  ggplot(aes(x = log(renda_trabalho), weight = peso)) +
  geom_boxplot(col = "black", fill = "blue")


library(ggpubr)

ggarrange(grafico_log_renda_densidade, grafico_boxplot_renda_x, nrow = 2)



# CRIANDO ANALISES PARA VARIAVEL QUALITATIVA -----

## Exemplo 1: usando banco Iris -------------

library(tidyverse)

banco_iris <- iris

banco_iris <- banco_iris %>% mutate(tamanho_sepala = ifelse(Sepal.Length >= 6, "grande", "pequena"))

library(janitor)

tabela_descricao <- banco_iris %>% tabyl(tamanho_sepala)

tabela_contingencia <-
  banco_iris %>% tabyl(Species, tamanho_sepala) %>%
  adorn_totals() %>%
  adorn_percentages(denominator = "row")

                  
banco_iris %>% tabyl(Species, tamanho_sepala) %>% chisq.test()



## Exemplo 2: usando pnad contínua -----------------


tabela_anos_estudo_raca_com_peso <-
  pnadc_2020 %>%
  filter(!is.na(raca), !is.na(anos_estudo)) %>%
  group_by(raca) %>%
  summarise(percentual_anos_estudo = wpct(anos_estudo, weight = peso)) %>%
  mutate(anos_estudo = names(percentual_anos_estudo),
         anos_estudo = factor(anos_estudo, levels = levels(pnadc_2020$anos_estudo)))

grafico_descritivo_anos_estudo_com_peso <-
  pnadc_2020 %>%
  filter(!is.na(anos_estudo)) %>%
  ggplot(aes(x = anos_estudo, weight = peso)) +
  geom_bar(fill = "blue", col = "black") +
  theme_classic() +
  coord_flip()
  
grafico_descritivo_anos_estudo_com_peso_porcentagem <- 
  tabela_anos_estudo_raca_com_peso %>%
  ggplot(aes(x = anos_estudo, y = percentual_anos_estudo)) +
  geom_col(fill = "blue", col = "black") +
  theme_classic() +
  coord_flip()

ggpubr::ggarrange(grafico_descritivo_anos_estudo_com_peso, 
                  grafico_descritivo_anos_estudo_com_peso_porcentagem, ncol = 1)



# VARIAVEL QUALI X QUANTI -----

grafico_log_renda_densidade_raca_com_peso <-
  pnadc_2020 %>%
  filter(!is.na(raca)) %>%
  ggplot(aes(x = log(renda_trabalho))) +
  geom_density(col = "black", aes(fill = raca, weight = peso), alpha = 0.5) +
  ggtitle("Com peso")

# Highcharter

hchart(
  density(
    x = pnadc_2020$log_renda_trabalho[pnadc_2020$raca == "negro"],
    na.rm = T,
    weights = pnadc_2020$peso[pnadc_2020$raca == "negro"]
  ),
  type = "area",
  color = "brown",
  name = "Negros"
) %>% 
  hc_add_series(
  density(x = pnadc_2020$log_renda_trabalho[pnadc_2020$raca == "branco"], 
          na.rm = T,
          weights = pnadc_2020$peso[pnadc_2020$raca == "branco"]),
  type = "area",
  color = "blue",
  name = "Brancos"
) %>% highcharter::hc_title(text = "Desigualdade racial na renda do trabalho no Brasil")

grafico_log_renda_densidade_raca_sem_peso <-
  pnadc_2020 %>%
  filter(!is.na(raca)) %>%
  ggplot(aes(x = log(renda_trabalho))) +
  geom_density(col = "black", aes(fill = raca), alpha = 0.5) +
  ggtitle("Sem peso")

ggpubr::ggarrange(grafico_log_renda_densidade_raca_com_peso, 
                  grafico_log_renda_densidade_raca_sem_peso, ncol = 1)

tabela_renda_raca <-
  pnadc_2020 %>%
  mutate(`Raça` = raca) %>%
  filter(!is.na(`Raça`)) %>%
  group_by(`Raça`) %>%
  summarise(`Media de renda` = round(weighted.mean(x = renda_trabalho, w = peso, na.rm = T),2)) %>%
  flextable::qflextable()
