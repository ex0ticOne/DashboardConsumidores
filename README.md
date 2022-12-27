# Dashboard Analítico de Reclamações de Consumidores

Dashboard construído em Shiny para compilar e apresentar insights de reclamações de consumidores por segmento de mercado.

O dataset usado foi extraído do portal consumidor.gov.br e organizado por mim usando o pacote tidyverse, estando disponível no meu Kaggle para download em <https://www.kaggle.com/datasets/ex0ticone/client-complaints-from-brazilian-companies>

O dashboard compila um panorama geral e as seguintes informações sob o formato de rankings por segmento:

-   Resumo Analítico por Empresa, com quantidade de reclamações, prazo médio de resposta da empresa, média da nota atribuída pelo consumidor, índice de respostas da empresa e índice de resolução de acordo com o consumidor;

-   Perfil do Reclamante por UF, cidade, faixa etária e gênero;

-   Principais problemas enfrentados pelo consumidor;

-   Meio de Contratação do produto/serviço causador da reclamação;

As tabelas são pesquisáveis por meio dos campos de busca geral e das colunas, além de serem ordenáveis da forma que o usuário desejar ao clicar no cabeçalho da mesma.

Você pode ver o dashboard funcionando em <https://ricardo-baptista.shinyapps.io/DashboardConsumidores/>

OBS: o dataset usado na versão hospedada foi amostrado em 50% do total (totalizando cerca de 2 milhões de observações), para que fosse possível comportar a aplicação dentro do ambiente de conta gratuita do shinyapps.io

Pacotes:

-   tidyverse

-   shiny, shinydashboard

-   DT (datatables)
