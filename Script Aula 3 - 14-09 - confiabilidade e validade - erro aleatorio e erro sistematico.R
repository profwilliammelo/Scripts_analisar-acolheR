# Moeda e o tamanho da amostra (reduzindo o erro aleatório) ------------

nomes_presentes_aula <- c("william", "Evandro", "Alan", "Pierre", "Adalberto")

moeda <- c("cara", "coroa")

prop.table(table(sample(x = moeda, size = 10000, replace = T)))  


# Moeda viciada (viesada - com erro sistemático) ----------------

prop.table(table(sample(x = moeda, size = 1000000, prob = c(0.1, 0.9), replace = T)))



