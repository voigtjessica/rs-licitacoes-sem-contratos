---
title: "Licitações sem contrato"
author: "Jessica Voigt"
date: "5 de junho de 2020"
output: 
html_document:
    theme: paper 
    toc: true
    toc_depth: 3
    code_folding: hide
    toc_float: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(data.table)
library(dplyr)
library(janitor)
library(kableExtra)

```


### Montando os bancos:

```{r}
# Montando os bancos

CD_TIPO_EVENTO <- c('AED', 'AND','ANO', 'EFC','EFH', 'EFI', 'ENC', 'ESC', 'IME', 'PUB', 'PUE', 'RCL', 'REE', 'REI', 'REO', 'RHA',
        'RHP', 'RPP', 'RPR', 'SDJ', 'SUM', 'SUO')
descricao <- c('Alteração do edital', 'Anulação por determinação judicial','Anulação de ofício', 
               'Encerramento por falta de propostas classificadas', 
               'Encerramento por falta de licitantes habilitados', 'Encerramento por falta de interessados', 
               'Encerramento', 'Esclarecimento', 'Impugnação do edital', 'Publicação', 'Publicação do edital', 
               'Recurso de credenciamento/lances', 'Republicação do edital', 'Reinício', 
               'Revogação de ofício', 'Recurso da habilitação', 'Recurso de habilitação/proposta', 
               'Recurso da proposta/projeto', 'Recurso da proposta', 
               'Suspensão por determinação judicial', 'Suspensão por medida cautelar', 
               'Suspensão de ofício')

eventos_sg <- data.frame(CD_TIPO_EVENTO ,descricao)

contratos <- fread("contrato.csv", encoding = "UTF-8")
licitacao <- fread("licitacao.csv" , encoding = "UTF-8")
eventos_licitacao <- fread("evento_lic.csv", encoding = "UTF-8")

#licitações de mereda:

lic <- licitacao %>%
  mutate(DS_OBJETO_PROCESSED = tolower(iconv(DS_OBJETO, 
                                     from="UTF-8", 
                                     to="ASCII//TRANSLIT"))) %>%
  filter(grepl("^.*(genero.*aliment|aliment.*esc|genero.*agric.*famil|merenda|pnae).*$",
                                    DS_OBJETO_PROCESSED)) %>%
  select(CD_ORGAO, NR_LICITACAO, ANO_LICITACAO, CD_TIPO_MODALIDADE, DT_ADJUDICACAO) %>%
  mutate_at(c('CD_ORGAO', 'NR_LICITACAO', 'ANO_LICITACAO'), as.character)


event <- eventos_licitacao %>%
  select(SQ_EVENTO, CD_ORGAO, NR_LICITACAO, ANO_LICITACAO, CD_TIPO_MODALIDADE, CD_TIPO_EVENTO, DT_EVENTO)  %>%
  mutate_at(c('CD_ORGAO', 'NR_LICITACAO', 'ANO_LICITACAO', 'CD_TIPO_MODALIDADE',
              'CD_TIPO_EVENTO'), as.character) %>%
  mutate(DT_EVENTO = as.Date(DT_EVENTO, format="%Y-%m-%d"))


con <- contratos %>%
  select(CD_ORGAO, NR_LICITACAO, ANO_LICITACAO, CD_TIPO_MODALIDADE, NR_CONTRATO) %>%
  mutate_all(as.character) %>%
  mutate(tem_contrato = 1) 

glimpse(licitacao)
```

### Existe mais de um evento de licitação por licitação?
```{r}
print(paste0("Existem ", nrow(lic), " licitações em 2020"))
```

```{r}

lic %>%
  left_join(event) %>%
  nrow()

```

Existem mais de um evento de licitação por licitação.

```{r}

lic %>%
  left_join(event) %>%
  group_by(CD_ORGAO, NR_LICITACAO, ANO_LICITACAO) %>%
  summarise(eventos = n()) %>%
  arrange(desc(eventos)) %>%
  kable() %>%
  kable_styling() %>%
  scroll_box( height = "200px")

```

Verifiquei no Manual do Leiaute que o campo ```SQ_EVENTO``` é o número de registro do evento e ele é sequencial, ou seja, segue a ordem dos eventos de licitação. Como exitem mais de um evento de licitação no mesmo dia, eu vou usar o ```max(SQ_EVENTO)``` para encontrar o último evento.


```{r}
ult_evento <- lic %>%
  left_join(event) %>%
  group_by(CD_ORGAO, NR_LICITACAO, ANO_LICITACAO, CD_TIPO_MODALIDADE) %>%
  filter(SQ_EVENTO == max(SQ_EVENTO) | is.na(SQ_EVENTO)) %>%
  ungroup() %>%
  left_join(eventos_sg) #colocando a desrição dos eventos
  
paste(nrow(lic), nrow(ult_evento), sep=" lic / / / ultimo evento ")

```

Agora deu certo!
Agora vou responder às perguntas:

### Todas as licitações que não têm contratos, foram canceladas?

```{r}
ult_evento %>%
  left_join(con) %>%
  group_by(tem_contrato) %>%
  summarise(total = n()) %>%
  kable()

```


temos 521 licitações sem contrato. Destas licitações sem contrato, qual é o último evento registrado?

```{r}

ult_evento %>%
  left_join(con) %>%
  filter(is.na(tem_contrato)) %>%
  select(-c(tem_contrato, NR_CONTRATO)) %>%
  group_by(descricao) %>%
  summarise(total_licitacoes = n()) %>%
  kable() %>%
  kable_styling() %>%
  scroll_box( height = "200px")

```

## Existe uma licitação que ainda está aberta mas que tem contrato?

Para saber se a licitação está aberta eu vou verificar se existe ou não data da adjudicação.

```{r}

lic %>%
  mutate(tem_adjudicacao = is.na(DT_ADJUDICACAO)) %>%
  group_by(tem_adjudicacao) %>%
  summarise(total = n())
```

Todas as licitações têm data de adjudicação.
A descrição do campo ```DT_ADJUDICACAO``` é o seguinte:

> Data de Adjudicação: data em que o objeto licitado foi adjudicado ao vencedor do certame. Campo obrigatório quando a fase atual da Licitação for "Adjudicação/Homologação". Não preencher se a modalidade da licitação for CPC(Chamamento Público/Credenciamento). Formato dd/mm/aaaa.

Sendo assim, a outra maneira de verificar se a licitação ainda está aberta seria utilizando o último evento de contrato. No entanto, não está claro, por exemplo, se os eventos "publicação" e "publicação do edital" podem estar associados com licitações com contrato ou ainda "abertas". Não sei ao certo qual seria a outra análise possível.



