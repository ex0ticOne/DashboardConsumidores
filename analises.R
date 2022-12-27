library(tidyverse)

#dataset_reclamacoes <- read_csv("dataset_reclamacoes.csv")

dataset_segmento <- dataset_reclamacoes %>%
  filter(segmento_de_mercado %in% 'Operadoras de Telecomunicações (Telefonia, Internet, TV por assinatura)')

media_nota_mercado <- round(mean(dataset_segmento$nota_do_consumidor, na.rm = TRUE), digits = 2)
indice_geral_respostas <- round(sum(dataset_segmento$respondida) / length(dataset_segmento$respondida) * 100, digits = 2)

resumo_analitico_empresas <- dataset_segmento %>%
  group_by(nome_fantasia) %>%
  summarise(quantidade_reclamacoes = n(), 
            prazo_media_dias_resposta = round(mean(tempo_resposta, na.rm = TRUE), digits = 2),
            media_nota = round(mean(nota_do_consumidor, na.rm = TRUE), digits = 2),
            indice_respostas = round(sum(respondida, na.rm = TRUE) / quantidade_reclamacoes * 100, digits = 2),
            indice_resolucao = round(sum(resolveu_reclamacao, na.rm = TRUE) / quantidade_reclamacoes * 100, digits = 2)) %>%
  drop_na() %>%
  arrange(desc(quantidade_reclamacoes))

perfil_reclamante <- dataset_segmento %>%
  count(uf, cidade, faixa_etaria, sexo, name = 'quantidade', sort = TRUE)

principais_problemas <- dataset_segmento %>%
  count(problema, name = 'quantidade', sort = TRUE)

meio_contratacao <- dataset_segmento %>%
  count(como_comprou_contratou, name = 'quantidade', sort = TRUE)
