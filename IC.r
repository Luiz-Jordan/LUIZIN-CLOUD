pacman::p_load(devtools, rtweet, tm, RColorBrewer, cluster, fpc, httpuv, SnowballC,
               ggplot2, wordcloud, wordcloud2, tidytext,
               stringr, tidyverse, knitr, png, webshot, htmlwidgets,pkgbuild,lexiconPT, dplyr)

DADOS = read.csv2('C:/Users/GUSTAVO/Desktop/Aquivos Para o (RStudio)/amostra_180137786.csv',sep = ',')
#PREPARANDO OS DADOS!!!
DADOS = DADOS[,c(5,8,9,10)]
DADOS[1] = table(DADOS[1])
DADOS[4] = table(DADOS[4])

###### P/Construção das amostra/IC ######
amostras_30 = data.frame()
z_star_95 <- qnorm(0.975)
IC_list_30 = data.frame()

####IC DA NOTAS pt#### de amostra tamanho (30)!!!!!
notapt = DADOS[,2]
notapt = as.numeric(as.character(notapt))
notapt = data.frame(notapt)
IC_list_30 = data.frame()
for (i in 1:50){
  amostras_30 = notapt[sample(nrow(notapt), 30),]
  amostras_30 = data.frame(amostras_30)
  
  linhapt = data.frame(inf = c(mean(amostras_30[,]) - z_star_95 * (sd(amostras_30[,]) / sqrt(30))),
             uper = c(mean(amostras_30[,]) + z_star_95 * (sd(amostras_30[,]) / sqrt(30))))
  
  IC_list_30 = rbind(IC_list_30,linhapt)
  i = i+1
}

# PLOTAR ----->
data = data.frame(id = c(1:50, 1:50),
                  IC_bound = c(IC_list_30$inf, IC_list_30$uper))

install.packages('ggplot2')
ggplot(data = data, aes(x = IC_bound, y = id, 
                           group = id)) +
  geom_point(size = 2) +  # add points at the ends, size = 2
  geom_line() +           # connect with lines
  geom_vline(xintercept = params$mu, color = "darkgray") # draw vertical line

