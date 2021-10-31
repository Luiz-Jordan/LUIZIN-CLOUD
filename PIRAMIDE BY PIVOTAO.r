setwd('C:/Users/GUSTAVO/Desktop/trab-demo')



pacman::p_load(read.dbc,tidyr,tidyverse,reshape,dplyr,stringr,foreign,ggplot2,lubridate,RColorBrewer,LexisPlotR)


library(ggplot2)
install.packages('tidyverse')
library(tidyverse)

####Banco de dados --->


data = read.csv('./Piramide/ano_2010.csv')
data = data[4:15,]

data = colsplit(data,';',c('Age','M','F','T'))
data$Faixa.Etária = as.character(data$Faixa.Etária)
data[2,2] = data[2,2]+data[1,2]
data[2,3] = data[2,3]+data[1,3]

data = data[-c(1),]

data[c(1)] = c('0-4','5-9','10-14','15-19','20-29','30-39','40-49','50-59','60-69','70-79','80+')
data$T=NULL
data$Age = as.factor((data$Age))


#teste --->



data <- data%>%
  pivot_longer(names_to = 'Gender', values_to = 'Population', cols = 2:3) %>%
  mutate(PopPerc=case_when(Gender=='M'~round(Population/sum(Population)*100,2),
                           TRUE~-round(Population/sum(Population)*100,2)),
         signal=case_when(Gender=='M'~1,
                          TRUE~-1)) #FUNCIONOU XD
head(data)
levels(data$Age)
data$Age <- factor(data$Age,levels=unique(data$Age),ordered=TRUE)
# Plotting
ggplot2::ggplot(data)+
  geom_bar(aes(x=Age,y=PopPerc,fill=Gender),stat='identity')+
  geom_text(aes(x=Age,y=PopPerc+signal*.5,label=abs(PopPerc)))+
  coord_flip()+
    scale_fill_manual(name='',values=c('darkred','steelblue'))+
  scale_y_continuous(breaks=seq(-12,12,1),
                     labels=function(x){paste(abs(x),'%')})+
  labs(x='Faixa Etária',y='População',
       title='Pirâmide populacional do Amapá em 2030')+
  #cowplot::theme_cowplot()+
  theme(axis.text.x=element_text(vjust=.5),
        panel.grid.major.y = element_line(color='lightgray',linetype='dashed'),
        legend.position = 'top',
        legend.justification = 'center')


#Indicadores foda-se:










#Razão de Sexo  n-masc/n-fem


data_rz = data.frame(unique(data[1]))
soma = data.frame(seq(1:11))
cont = 1
for(i in 1:(length(data$Age)-1)){
  
  
  if (i%%2 == 1){
    rz = data.frame(c(data[i,3]/data[(i+1),3]))*100
    soma[cont,] = rz
    cont = cont+1
    
    
    
    print(rz)
    
  }

}

colnames(soma) = 'Razão de Sexo'

#razao_2030 = data.frame(data_rz,soma)

razao_2010 = data.frame(data_rz,soma)


razaox = data.frame(razao_2010,razao_2030)
#razaox[3] = NULL


colnames(razaox) = c('FaixaEtária', '2010','2030')


rz_xd = pivot_longer(razaox,c('2010','2030'), names_to = "ANO", values_to = "R0")

ggplot(rz_xd, aes(x = FaixaEtária,y = R0,group =ANO,colour = ANO))+
  geom_line(size = 1) +geom_point(size = 3)+
  labs(x = 'Faixa Etária', y = 'Razão de sexo (*100)',
       title = 'Comparação entre as razões de sexo de 2010 e 2030')+
  theme(panel.grid.major.y = element_line(color='lightgray',linetype='dashed'),
        legend.position = 'top',
        legend.justification = 'center',
        panel.border = element_blank(),
        axis.title.y =element_text(colour='grey1' , size=12),
        axis.title.x =element_text(colour='grey1' , size=12),
        axis.text =element_text(colour='black',size=9.5),
        axis.line =element_line(colour='black'))+
  scale_colour_manual(name='ANO' , values = c('grey1','turquoise2'))+
  ylim(c(70,110))
