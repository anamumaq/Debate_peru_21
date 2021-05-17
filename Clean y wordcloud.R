library(tidyverse)
library(tm)# para el text mining
library(SnowballC) # para quitar las stopwords
library(wordcloud2) #wordcloud con mejores interacciones


# funcion nube genera df con el speech de cada candidato
nube = function(x,iniciales){
  
  x = Corpus(VectorSource(vm))# El texto es un speech la data debe subirse como un corpus
  #inspect(vm) ## revisando
  
  ## Cleaning los stopwords, numeros y puntacion (no hay caracteres especiales &/?@)
  x = x %>% 
    tm_map(removeNumbers)%>% # eliminar los minutos
    tm_map(removePunctuation)%>% # eliminar puntuacion o dospuntos
    tm_map(stripWhitespace)%>% # quitar dobles espacios o enter
    tm_map(content_transformer(tolower))%>% # aplicamos minusculas
    tm_map(removeWords, stopwords("Spanish"))%>% # quitar stopwords
    tm_map(removeWords, 
            c("entonces","aquí","el","de","en","que",
              "por","los","las","para","una","con","este", "así","ejemplo", "vamos")) # quitar mis propias stopwords
  
  dtm = TermDocumentMatrix(x)# crear una tabla con la frecuencia de repeticion 
  matriz = as.matrix(dtm) # crear una matriz
  palabras = sort(rowSums(matriz), decreasing = TRUE) # sumo las filas en la matriz
  x_df = data.frame(row.names = NULL, palabra = names(palabras), freq = palabras, candidato = iniciales) # creo dataframe
  return(x_df)
}

# cargo los speechs de cada uno
vm = readLines("debate/vm_pandemia.txt", encoding = "UTF-8") # extrae el texto con tildes
du = readLines("debate/du_pandemia.txt", encoding = "UTF-8")
gf = readLines("debate/gf_pandemia.txt", encoding = "UTF-8")
kf = readLines("debate/kf_pandemia.txt", encoding = "UTF-8")
yl = readLines("debate/yl_pandemia.txt", encoding = "UTF-8")



# genero df de cada speech
df_yl = nube(yl,"YL")
df_vm = nube(vm,"VM")
df_kf = nube(kf,"KF")
df_du = nube(du,"DU")
df_gf = nube(gf,"GF")


# wordcloud de cada candidato

wordcloud2(data = df_vm[1:2], size = 0.5, color = 'random-dark')
wordcloud2(data = df_gf[1:2], size = 0.5, color = 'random-dark')
wordcloud2(data = df_kf[1:2], size = 0.5, color = 'random-dark')
wordcloud2(data = df_du[1:2], size = 0.5, color = 'random-dark')
wordcloud2(data = df_yl[1:2], size = 0.6, color = 'random-dark')

par(mfrow = c(2,2))
wordcloud2(data = df_vm[1:2], size = 0.5, color = 'random-dark')
wordcloud2(data = df_kf[1:2], size = 0.5, color = 'random-dark')

count(df_yl)
count(df_vm)
count(df_gf)
count(df_kf)
count(df_du)

#ggplot(df[2:3], aes(candidato))+ 
 # geom_bar()

df  = df_yl %>%
  union(df_vm) %>%
  union(df_gf) %>%
  union(df_du) %>%
  union(df_kf)


df = df %>%
  group_by(word)%>%
  summarise(frecuencia =sum(freq))


#wordcloud2(data = df[1:2], size = 0.3, color = 'random-dark')
wordcloud(words = df$word, freq = df$frecuencia, 
          min.freq = 2, max.words = 50, 
          random.order = FALSE, 
          colors = brewer.pal(8,"Dark2"))


