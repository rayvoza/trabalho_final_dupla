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
