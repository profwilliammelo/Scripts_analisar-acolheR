
# Revisando objetos e principais classes de objetos ----------------------------

## Character ---------------

rafael <- "Rafael"

william = 'William'

alunos_curso <- c("Rafael", "William", "Alan", "Evandro", "Pierre", "Grazi")

## Numeric ------------------------

numero <- c(1, 10.2, 20, 50, 85, 61)

numero_character <- as.character(numero)

teste <- c("Rafael", 2, 2, 2, 10, 25, 50) # EM R TODO VETOR SO PODE CARREGAR VALORES DE UM SO TIPO

# Olha o que acontece com as operacoes de objetos de classes diferentes

mean(numero)
mean(numero_character)

# Da pra transformar caracter em numero em R?

as.numeric(alunos_curso)

## Logical ------------------

logico <- c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE)

alunos_curso == "Alan"

vetor_logico_alan <- ifelse(alunos_curso == "Alan", "é Alan", "não é Alan")


## Factor --------------------------------

salario <- c("1 salario", "2 salarios", "3 salarios", "4 salarios", "5 salarios", "6 salarios")

salario <- factor(salario, levels = salario)

as.numeric(salario)

## Data.frame ------------------------------

teste <- c(1, 2, NA, 3, NA, 4)

banco_dados <- data.frame(alunos_curso, numero, salario, logico, teste)


## Como verificar tipo de classes ---------------------

class(alunos_curso)
class(numero)
class(salario)
class(logico)
class(banco_dados)

# Operacoes basicas com R-base e funcoes importantes ---------------

numero + 1

numero*2

((numero*2)^2)/3

mean(numero)

sum(numero)

sqrt(numero)

var(numero)

sqrt(var(numero))



# Subsetting e seleção de vetores em data.frame com R-base ---------------------

numero[3]

class(banco_dados[4])

class(banco_dados[ ,4])

class(banco_dados$logico)

banco_dados$logico[1]

banco_dados$alunos_curso[banco_dados$salario == "3 salarios"]



# Manipulando bases de dados em R --------------------------------

## Pacote dyplr -------------------------

#install.packages("tidyverse") # inclui dyplr, ggplot e outros que sao compativeis com o operador %>%
library(tidyverse)

banco_dados <- banco_dados %>% mutate(teste = ifelse(is.na(teste), 0, teste))

banco_dados %>% filter(salario == "3 salarios") %>% select(alunos_curso, salario)


group_by(genero), summarise flextable


## Pacote janitor ----------------------

tabyl

## Pacote ggplot ---------------------------

# Aqui o mais importante é ebtender que o ggplot é feito por uma camada base
# e varias camadas auxiliares de acordo com o tipo de gráfico e com o estilo que a pessoa quer colocar

ggplot(aes(x =, y = )) = geom_col()





