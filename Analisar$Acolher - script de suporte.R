# QUEBRANDO MALDICAO DA PROGRAMACAO --------------------------------------------

OLA_MUNDO <-
  function(nome, sentimento)
    
  {
    # GERANDO FRASE a ser inputada na funcao
    
    frase <-
      paste(
        "Olá mundo! Me chamo",
        nome,
        "e estou me sentindo muito",
        sentimento,
        "por começar a programar em R."
      )
    
    
    # Parte que transforma a frase em app Shiny

    ## Carregando pacotes importantes para produzir o app
    
    #install.packages("shiny")
    
    library(shiny)
    library(shinydashboard)
    
    ## Parte de interface do usuario do app
    
    ui <-
      # definindo pagina
      dashboardPage(
        # definindo cabecalho
        dashboardHeader(title = "Bem vindo(a/e)!!!"),
        # definindo barra lateral
        dashboardSidebar(),
        # definindo corpo do app
        dashboardBody(
          # colocando a frase, objeto criado la emcima, dentro de um box de fundo preto
          box(frase, background = "black"),
          # uma imagemzinha pra abrir um sorriso =)
          img(src = "https://media.giphy.com/media/fAMztTgKyW4g0/giphy.gif")
          
        ))
    
    ## Parte reativa do app, aquela que faz mudar a interface do usuario de acordo
    ## com inputs dados no proprio app --- nao usamos aqui
    
    server <- function(input, output, session) 
      {}
    
    shinyApp(ui, server)
  }

OLA_MUNDO(nome = "Graziele", sentimento = "ansiosa")




# BANCOS DE DADOS --------------------------------------------------------------

dados <- data.frame(x = seq(1, 30, by = 1),
                    y = seq(2, 60, by = 2))

# VISUALIZACAO DE DADOS --------------------------------------------------------

library(ggplot2)
library(dplyr)

correlacao <- cor(dados$x, dados$y)

dados %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(col = "red") +
  geom_smooth(method = "lm", se = F) +
  theme_classic() +
  xlab("variavel independente") +
  ylab("variavel dependente") +
  ggtitle("Relacao entre X e Y") +
  annotate(x = 20, y = 10, geom = "label", label = paste("Correlação =", correlacao))
  
