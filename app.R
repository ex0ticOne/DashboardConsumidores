library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(DT)
library(shinydashboard)

source('carrega_dataset.R')

cabecalho <- dashboardHeader(title = "Dashboard Analítico de Reclamações de Consumidores", 
                             titleWidth = "500")

#Lateral
lateral <- dashboardSidebar(sidebarMenu(uiOutput("selecao_segmento_mercado"),
                                        menuItem("Panorama Geral", tabName = "panorama_geral"),
                                        menuItem("Indicadores", tabName = "indicadores")), 
                            width = "300")

#Corpo
corpo <- dashboardBody(tabItems(tabItem(tabName = "panorama_geral",
                                        fluidRow(box(titlePanel("Esse é o panorama geral do segmento selecionado"),
                                                     valueBoxOutput("media_nota_mercado", width = "6"),
                                                     valueBoxOutput("indice_geral_respostas", width = "6"),
                                                     valueBoxOutput("tempo_medio_respostas_mercado", width = "6"),
                                                     valueBoxOutput("indice_geral_resolucao", width = "6"), width = "100%"))),
                                tabItem(tabName = "indicadores", 
                                        tabBox( 
                                          tabPanel(title = "Resumo Analítico por Empresa do Segmento",
                                                   titlePanel("Esse é o ranking geral por empresa do segmento selecionado"),
                                                   DTOutput("resumo_analitico")),
                                          tabPanel(title = "Perfil Reclamante",
                                                   titlePanel("Quais os perfis de quem está reclamando nesse segmento?"),
                                                   DTOutput("perfil_reclamante")),
                                          tabPanel(title = "Principais Problemas",
                                                   titlePanel("Quais os principais problemas enfrentados pelos clientes?"),
                                                   DTOutput("principais_problemas")),
                                          tabPanel(title = "Meio de Contratação",
                                                   titlePanel("Quais os canais de contratação dos produtos/serviços que geraram reclamações?"),
                                                   DTOutput("meio_contratacao")), width = "100%", height = "100%"))))

ui <- dashboardPage(cabecalho, lateral, corpo, skin = "green", 
                    title = "Dashboard Analítico de Reclamações de Consumidores")

server <- function(input, output) {
  
  output$selecao_segmento_mercado <- renderUI(selectInput("segmento_selecionado", 
                                                          label = "Selecione o segmento de mercado",
                                                          choices = unique(lista_opcoes$segmento_de_mercado)))
  
  
  dataset_segmento <- reactive({
    
    dataset <- dataset_reclamacoes %>%
      filter(segmento_de_mercado %in% input$segmento_selecionado)
    
    return(dataset)
    
  })
  
  criacao_media_nota <- reactive({
    
    media_nota_mercado <- round(mean(dataset_segmento()$nota_do_consumidor, na.rm = TRUE), digits = 2)
    
  })
  
  output$media_nota_mercado <- renderValueBox(valueBox(subtitle = "Média da Nota do Consumidor (de 1 a 5)", 
                                                       value = criacao_media_nota(), color = "blue"))
  
  criacao_indice_geral_respostas <- reactive({
    
    indice_geral_respostas <- round(sum(dataset_segmento()$respondida) / length(dataset_segmento()$respondida) * 100, digits = 2)
    
  })
  
  output$indice_geral_respostas <- renderValueBox(valueBox(subtitle = "Índice Geral de Respostas",
                                                           value = criacao_indice_geral_respostas(), color = "olive"))
  
  criacao_tempo_medio_respostas_mercado <- reactive({
    
    tempo_medio_respostas <- round(mean(dataset_segmento()$tempo_resposta, na.rm = TRUE), digits = 2)
    
  })
  
  output$tempo_medio_respostas_mercado <- renderValueBox(valueBox(subtitle = "Tempo Médio de Respostas em Dias", 
                                                                  value = criacao_tempo_medio_respostas_mercado(), color = "maroon"))
  
  criacao_indice_geral_resolucao <- reactive({
    
    indice_geral_resolucao <- round(sum(dataset_segmento()$resolveu_reclamacao) / length(dataset_segmento()$resolveu_reclamacao) * 100, digits = 2)
    
  })
  
  output$indice_geral_resolucao <- renderValueBox(valueBox(subtitle = "Índice Geral de Resolução de Acordo com o Cliente", 
                                                           value = criacao_indice_geral_resolucao(), color = "orange"))
  
  #Resumo Analítico
  criacao_resumo <- reactive({
    
    resumo_analitico_empresas <- dataset_segmento() %>%
      group_by(nome_fantasia) %>%
      summarise(quantidade_reclamacoes = n(), 
                prazo_media_dias_resposta = round(mean(tempo_resposta, na.rm = TRUE), digits = 2),
                media_nota = round(mean(nota_do_consumidor, na.rm = TRUE), digits = 2),
                indice_respostas = round(sum(respondida, na.rm = TRUE) / quantidade_reclamacoes * 100, digits = 2),
                indice_resolucao = round(sum(resolveu_reclamacao, na.rm = TRUE) / quantidade_reclamacoes * 100, digits = 2)) %>%
      drop_na() %>%
      arrange(desc(quantidade_reclamacoes))
    
    return(resumo_analitico_empresas)
    
  })
  
  criacao_perfil_reclamante <- reactive({
    
    perfil_reclamante <- dataset_segmento() %>%
      count(uf, cidade, faixa_etaria, sexo, name = 'quantidade', sort = TRUE)
    
    return(perfil_reclamante)
    
  })
  
  criacao_principais_problemas <- reactive({
    
    principais_problemas <- dataset_segmento() %>%
      count(problema, name = 'quantidade', sort = TRUE)
    
    return(principais_problemas)
    
  })
  
  criacao_meio_contratacao <- reactive({
    
    meio_contratacao <- dataset_segmento() %>%
      count(como_comprou_contratou, name = 'quantidade', sort = TRUE)
    
    return(meio_contratacao)
    
  })
  
  output$resumo_analitico <- renderDT(criacao_resumo(), 
                                      filter = list(position = 'top'),
                                      options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.13.1/i18n/pt-BR.json')),
                                      colnames = c('Nome da Empresa', 'Quantidade de Reclamações', 'Prazo Médio para Resposta em Dias', 'Média da Nota', 'Índice de Respostas da Empresa', 'Índice de Resolução de acordo com o cliente'))
  
  output$perfil_reclamante <- renderDT(criacao_perfil_reclamante(), 
                                       filter = list(position = 'top'),
                                       options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.13.1/i18n/pt-BR.json')),
                                       colnames = c('UF', 'Cidade', 'Faixa Etária', 'Sexo', 'Quantidade'))
  
  output$principais_problemas <- renderDT(criacao_principais_problemas(), 
                                          filter = list(position = 'top'),
                                             options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.13.1/i18n/pt-BR.json')),
                                             colnames = c('Problema', 'Quantidade'))
  
  output$meio_contratacao <- renderDT(criacao_meio_contratacao(), 
                                      filter = list(position = 'top'),
                                         options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.13.1/i18n/pt-BR.json')), 
                                         colnames = c('Meio de Contratação', 'Quantidade'))
  
}

shinyApp(ui, server)