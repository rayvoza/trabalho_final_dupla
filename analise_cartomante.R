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
