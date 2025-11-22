install.packages("tidyverse")
install.packages("readr")

library(tidyverse)

# Lendo o arquivo de texto
texto <- read_lines("cartomante.txt")

# transformar o texto em um único texto
texto_unico <- paste(texto, collapse = "")

#minusculo
texto_unico <- tolower(texto_unico)

#tirar pontuacao 
texto_limpo <- texto_unico %>% 
  str_replace_all("[[:punct:]]", "")

# separar palavras
palavras <- str_split(texto_limpo, "\\s+") [[1]]

# ver quantas palavras tem no texto

length(palavras)


# -----------------------------
# ESTATÍSTICAS BÁSICAS DO TEXTO
# -----------------------------

# 1. Número de linhas
num_linhas <- length(texto)
num_linhas

# 2. Número de palavras
num_palavras <- length(palavras)
num_palavras

# 3. Número total de caracteres
num_caracteres <- sum(nchar(texto_limpo))
num_caracteres

# 4. Tamanho de cada palavra
tamanhos <- nchar(palavras)

# 5. Média e mediana do tamanho das palavras
media_tamanho <- mean(tamanhos)
mediana_tamanho <- median(tamanhos)

media_tamanho
mediana_tamanho


# --------------------------------
# FREQUÊNCIA DAS PALAVRAS NO TEXTO
# --------------------------------

library(dplyr)
library(ggplot2)

# Tabela de frequência
freq <- as.data.frame(table(palavras), stringsAsFactors = FALSE) %>%
  arrange(desc(Freq))

# renomear colunas
names(freq) <- c("palavra", "n")

# ver top 20
head(freq, 20)

# Gráfico
freq %>%
  slice_head(n = 20) %>%
  ggplot(aes(x = reorder(palavra, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Palavra",
    y = "Frequência",
    title = "20 Palavras mais Frequentes em 'A Cartomante'"
  )

### Acabou ficando só com palavras gramaticais, vamos tirá-las!

# Criar lista de stopwords em português
install.packages("stopwords")
library(stopwords)
stop_pt <- stopwords("pt")

# Remover stopwords
palavras_sem_funcionais <- palavras[!palavras %in% stop_pt]

# Remover palavras muito curtas (geralmente funcionais)
palavras_sem_funcionais <- palavras_sem_funcionais[nchar(palavras_sem_funcionais) > 2]

# Remover números e símbolos
palavras_sem_funcionais <- palavras_sem_funcionais[!grepl("^[0-9]+$", palavras_sem_funcionais)]

# Criar tabela de frequência
freq_semantic <- as.data.frame(table(palavras_sem_funcionais), stringsAsFactors = FALSE) %>%
  arrange(desc(Freq))

# renomear colunas
names(freq_semantic) <- c("palavra", "n")

# Mostrar as 20 mais frequentes
head(freq_semantic, 20)

# Gráfico das 20 palavras com maior frequência
freq_semantic %>%
  slice_head(n = 20) %>%
  ggplot(aes(x = reorder(palavra, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Palavra",
    y = "Frequência",
    title = "Top 20 palavras semânticas em 'A Cartomante'"
  )

# Nuvem de palavras
install.packages("wordcloud2")
library(wordcloud2)

top20 <- freq_semantic %>% slice_head(n = 20)

wordcloud2(top20, size = 0.7, color = 'random-light', backgroundColor = "white")


#Treemap (área proporcional à frequência)
install.packages("treemapify")
library(treemapify)

ggplot(top20, aes(area = n, fill = n, label = palavra)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", size = 15) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Top 20 palavras semânticas - Treemap")

