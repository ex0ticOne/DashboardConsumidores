library(vroom)
library(janitor)
library(lubridate)

compilado <- vroom(list.files(path = getwd(), pattern = "reclamacoes", full.names = T))

compilado <- compilado %>% clean_names()

compilado$procurou_empresa[compilado$procurou_empresa == 'N'] <- 0
