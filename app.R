library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(DT)
library(shinydashboard)

source('carrega_dataset.R')

cabecalho <- dashboardHeader(title = "Dashboard analítico de reclamações", 
                             titleWidth = "500")

#Lateral
lateral <- dashboardSidebar(sidebarMenu(uiOutput("selecao_segmento_mercado")), 
                            width = "300")

#Corpo
corpo <- dashboardBody(tabBox(tabPanel(title = "Resumo Analítico por Empresa do Segmento",
                                       DTOutput("resumo_analitico")),
                              tabPanel(title = "Perfil Reclamante",
                                       DTOutput("perfil_reclamante")),
                              tabPanel(title = "Principais Problemas",
                                       DTOutput("principais_problemas")),
                              tabPanel(title = "Meio de Contratação",
                                       DTOutput("meio_contratacao")), width = "100%", height = "100%"))

ui <- dashboardPage(cabecalho, lateral, corpo, skin = "green", 
                    title = "Dashboard")

server <- function(input, output) {
  
  output$selecao_segmento_mercado <- renderUI(selectInput("segmento_selecionado", 
                                                          label = "Selecione o segmento de mercado",
                                                          choices = unique(lista_opcoes$segmento_de_mercado)))
  
  
  dataset_segmento <- reactive({
    
    dataset <- dataset_reclamacoes %>%
      filter(segmento_de_mercado %in% input$segmento_selecionado)
    
    return(dataset)
    
  })
  
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
                                      options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.13.1/i18n/pt-BR.json')),
                                      colnames = c('Nome da Empresa', 'Quantidade de Reclamações', 'Prazo Médio para Resposta em Dias', 'Média da Nota', 'Índice de Respostas da Empresa', 'Índice de Resolução de acordo com o cliente'))
  
  output$perfil_reclamante <- renderDT(criacao_perfil_reclamante(), 
                                       options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.13.1/i18n/pt-BR.json')),
                                       colnames = c('UF', 'Cidade', 'Faixa Etária', 'Sexo', 'Quantidade'))
  
  output$principais_problemas <- renderDT(criacao_principais_problemas(), 
                                             options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.13.1/i18n/pt-BR.json')),
                                             colnames = c('Problema', 'Quantidade de Ocorrências'))
  
  output$meio_contratacao <- renderDT(criacao_meio_contratacao(), 
                                         options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.13.1/i18n/pt-BR.json')), 
                                         colnames = c('Meio de Contratação', 'Quantidade'))
  
}

shinyApp(ui, server)