library(tidyverse)
library(tm)# para el text mining
library(SnowballC) # para quitar las stopwords
library(wordcloud2) #wordcloud con mejores interacciones


# funcion nube genera df con el speech de cada candidato
nube = function(x,iniciales){
  
  x = Corpus(VectorSource(x))# El texto es un speech la data debe subirse como un corpus
  # inspect(vm) ## revisando
  
  ## Cleaning los stopwords, numeros y puntacion (no hay caracteres especiales &/?@)
  x = x %>% 
    tm_map(removeNumbers)%>% # eliminar los minutos
    tm_map(removePunctuation)%>% # eliminar puntuacion o dospuntos
    tm_map(stripWhitespace) # quitar dobles espacios o enter
  
  x = tm_map(x, content_transformer(tolower)) # aplicamos minusculas
  x = tm_map(x, removeWords, stopwords("Spanish")) # quitar stopwords
  x = tm_map(x, removeWords, 
             c("entonces","aquí","el","de","en","que","por","los","las","para","una","con","este")) # quitar mis propias stopwords
  
  dtm = TermDocumentMatrix(x)# crear una taba con la frecuencia de repeticion 
  matriz = as.matrix(dtm) # crear una matriz
  palabras = sort(rowSums(matriz), decreasing = TRUE) # sumo las filas en la matriz
  x_df = data.frame(row.names = NULL, word = names(palabras), freq = palabras, candidato = iniciales) # creo dataframe
  return(x_df)
}

# cargo los speechs de cada uno
vm = readLines("debate/vm_corrupcion.txt", encoding="UTF-8") # extrae el texto con tildes
du = readLines("debate/du_corrupcion.txt", encoding="UTF-8")
gf = readLines("debate/gf_corrupcion.txt", encoding="UTF-8")
kf = readLines("debate/kf_corrupcion.txt", encoding="UTF-8")
yl = readLines("debate/yl_corrupcion.txt", encoding="UTF-8")

# genero df de cada speech
df_yl = nube(yl,"YL")
df_vm = nube(vm,"VM")
df_kf = nube(kf,"KF")
df_du = nube(du,"DU")
df_gf = nube(gf,"GF")


# uno todos los dfs en uno solo 
df  = df_yl %>%
  union_all(df_vm) %>%
  union_all(df_hds) %>%
  union_all(df_rla) %>%
  union_all(df_gf) %>%
  union_all(df_fc) %>%
  union_all(df_ma)

# wordcloud de cada candidato

wordcloud2(data = df_vm[1:2], size = 0.7, color = 'random-dark')
wordcloud2(data = df_gf[1:2], size = 0.7, color = 'random-dark')
wordcloud2(data = df_kf[1:2], size = 0.6, color = 'random-dark')
wordcloud2(data = df_du[1:2], size = 0.7, color = 'random-dark')
wordcloud2(data = df_yl[1:2], size = 0.7, color = 'random-dark')

count(df_yl)
count(df_vm)
count(df_gf)
count(df_kf)
count(df_du)

#ggplot(df[2:3], aes(candidato))+ 
 # geom_bar()
