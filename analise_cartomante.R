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


#nuvem de palavras2
library(wordcloud2)
library(treemapify)
library(dplyr)
library(ggplot2)

# Selecionar top 20
top20 <- freq_semantic %>% slice_head(n = 20)

# ---- WORDCLOUD ----
# Renomear colunas para o formato exigido pelo wordcloud2
df_wc <- top20 %>%
  rename(word = palavra,
         freq = n)

# Gerar nuvem de palavras
wordcloud2(df_wc, 
           size = 0.7, 
           color = "random-light", 
           backgroundColor = "white")


# ---- TREEMAP ----
ggplot(top20, aes(area = n, fill = n, label = palavra)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", size = 15) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Top 20 palavras semânticas - Treemap")

# ======================================
# CATEGORIZAÇÃO SIMPLES DE PALAVRAS
# ======================================

library(dplyr)
library(stringr)

# Base: freq_semantic (que você já criou acima)
df <- freq_semantic

# ------------------------------
# 1. Categorização por tamanho
# ------------------------------
df <- df %>% 
  mutate(
    classe_tamanho = case_when(
      nchar(palavra) <= 4 ~ "curta",
      nchar(palavra) <= 7 ~ "media",
      TRUE ~ "longa"
    )
  )

# Ver primeiros resultados
head(df)

# ------------------------------
# 2. Categorização ortográfica
# ------------------------------
df <- df %>%
  mutate(
    classe_ortografica = case_when(
      str_detect(palavra, "[áéíóúàãõâêô]") ~ "acentuada",
      str_detect(palavra, "-") ~ "hifenizada",
      str_detect(palavra, "^[0-9]+$") ~ "numerica",
      TRUE ~ "simples"
    )
  )

head(df)

# ------------------------------
# 3. Categorização por frequência
# ------------------------------
df <- df %>%
  mutate(
    classe_freq = case_when(
      n <= 2 ~ "rara",
      n <= 10 ~ "comum",
      TRUE ~ "muito_frequente"
    )
  )

head(df)

# graficos
# frequencia por classe de tamanho 

df %>%
  group_by(classe_tamanho) %>%
  summarise(total = sum(n)) %>%
  ggplot(aes(x = classe_tamanho, y = total, fill = classe_tamanho)) +
  geom_col() +
  labs(title = "Distribuição por tamanho da palavra")

# frequencia por tipo ortografico

df %>%
  group_by(classe_ortografica) %>%
  summarise(total = sum(n)) %>%
  ggplot(aes(x = classe_ortografica, y = total, fill = classe_ortografica)) +
  geom_col() +
  labs(title = "Distribuição por tipo ortográfico")

# frequencia por categoria de frequencia

df %>%
  group_by(classe_freq) %>%
  summarise(total = sum(n)) %>%
  ggplot(aes(x = classe_freq, y = total, fill = classe_freq)) +
  geom_col() +
  labs(title = "Distribuição por frequência")

library(dplyr)
library(stringr)

df <- freq_semantic  # usando sua tabela final

# -----------------------------------
# FUNÇÃO PARA CONTAR SÍLABAS (simples)
# -----------------------------------
conta_silabas <- function(palavra) {
  # extrair grupos de vogais
  grupos <- str_extract_all(palavra, "[aeiouáéíóúâêôãõ]+")[[1]]
  
  if (length(grupos) == 0) {
    return(1)  # palavra sem vogal (raro), considerar 1 sílaba
  } else {
    return(length(grupos))
  }
}

# Aplicar ao banco
df <- df %>%
  mutate(
    silabas = sapply(palavra, conta_silabas),
    classe_silaba = case_when(
      silabas == 1 ~ "monossílaba",
      silabas == 2 ~ "dissílaba",
      silabas == 3 ~ "trissílaba",
      silabas >= 4 ~ "polissílaba"
    )
  )

head(df)

# frequencia por numero de silabas

df %>%
  count(silabas, wt = n) %>%
  ggplot(aes(x = silabas, y = n)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Distribuição das palavras por número de sílabas",
    x = "Número de sílabas",
    y = "Frequência total"
  )

# categorias 

df %>%
  group_by(classe_silaba) %>%
  summarise(total = sum(n)) %>%
  ggplot(aes(x = classe_silaba, y = total, fill = classe_silaba)) +
  geom_col() +
  labs(
    title = "Distribuição por categoria silábica",
    x = "Classe silábica",
    y = "Frequência"
  )


# criar um treemap por categoria de palavras

library(dplyr)
library(treemapify)
library(ggplot2)

# Agrupando por classe silábica
treemap_silabas <- df %>%
  group_by(classe_silaba) %>%
  summarise(freq_total = sum(n)) %>%
  arrange(desc(freq_total))

treemap_silabas

ggplot(treemap_silabas, aes(
  area = freq_total,
  fill = classe_silaba,
  label = classe_silaba
)) +
  geom_treemap() +
  geom_treemap_text(
    colour = "white",
    place = "centre",
    grow = TRUE,
    reflow = TRUE
  ) +
  labs(
    title = "Treemap por Categoria Silábica",
    fill = "Classe Silábica"
  ) +
  scale_fill_brewer(palette = "Blues")


ggplot(df, aes(
  area = n,
  fill = classe_silaba,
  label = palavra,
  subgroup = classe_silaba
)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "white", size = 2) +
  geom_treemap_text(
    place = "centre",
    grow = FALSE,
    reflow = TRUE,
    colour = "white"
  ) +
  labs(
    title = "Treemap de Palavras por Categoria Silábica",
    fill = "Classe Silábica"
  )


## VARIAVEIS NUMERICAS

tamanho <- nchar(df$palavra)
num_vogais <- stringr::str_count(df$palavra, "[aeiouáéíóú]")
num_consoantes <- stringr::str_count(df$palavra, "[bcdfghjklmnpqrstvwxyz]")
letras_repetidas <- stringr::str_count(df$palavra, "([a-z])\\1")
digrafos <- stringr::str_count(df$palavra, "lh|nh|ch|rr|ss")
primeira_letra_num <- match(substr(df$palavra, 1, 1), letters)
ultima_letra_num <- match(substr(df$palavra, nchar(df$palavra), nchar(df$palavra)), letters)
freq_norm <- df$n / sum(df$n)
sem_acento <- stringi::stri_trans_general(df$palavra, "Latin-ASCII")
tamanho_sem_acento <- nchar(sem_acento)

library(dplyr)
library(stringr)
library(stringi)

df <- df %>%
  mutate(
    tamanho = nchar(palavra),
    num_vogais = str_count(palavra, "[aeiouáéíóú]"),
    num_consoantes = str_count(palavra, "[bcdfghjklmnpqrstvwxyz]"),
    final_r = as.integer(str_detect(palavra, "r$")),
    letras_repetidas = str_count(palavra, "([a-z])\\1"),
    digrafos = str_count(palavra, "lh|nh|ch|rr|ss"),
    primeira_letra_num = match(substr(palavra, 1, 1), letters),
    ultima_letra_num = match(substr(palavra, nchar(palavra), nchar(palavra)), letters),
    freq_norm = if ("n" %in% names(df)) n / sum(n) else NA,
    tamanho_sem_acento = nchar(stri_trans_general(palavra, "Latin-ASCII"))
  )

# GRAFICOS PARA ESSAS VARIAVEIS

# tamanho das palavras
ggplot(df, aes(x = tamanho)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  labs(
    title = "Distribuição do Tamanho das Palavras",
    x = "Número de letras",
    y = "Frequência"
  )

# numero de vogais
ggplot(df, aes(x = num_vogais)) +
  geom_histogram(bins = 10, fill = "darkgreen", color = "white") +
  labs(
    title = "Distribuição de Vogais nas Palavras",
    x = "Quantidade de vogais",
    y = "Frequência"
  )

# consoantes

ggplot(df, aes(x = num_consoantes)) +
  geom_histogram(bins = 10, fill = "firebrick", color = "white") +
  labs(
    title = "Distribuição de Consoantes nas Palavras",
    x = "Quantidade de consoantes",
    y = "Frequência"
  )

# graficos para digrafos

ggplot(df, aes(x = digrafos)) +
  geom_bar(fill = "orchid") +
  labs(
    title = "Frequência de Dígrafos por Palavra",
    x = "Quantidade de dígrafos",
    y = "Número de palavras"
  )

# vogais x consoantes

ggplot(df, aes(x = num_vogais, y = num_consoantes)) +
  geom_point(alpha = 0.5, color = "purple") +
  labs(
    title = "Relação entre Vogais e Consoantes",
    x = "Nº de vogais",
    y = "Nº de consoantes"
  )

# tamanho palavras

ggplot(df, aes(y = tamanho)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Boxplot do Tamanho das Palavras",
    y = "Número de letras"
  )


