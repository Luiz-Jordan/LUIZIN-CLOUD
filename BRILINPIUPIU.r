pacman::p_load(tidyr,tidyverse,dplyr,stringr,writexl,rvest,ggplot2,reshape)

setwd('C:/Users/luiz.graciano/Desktop/Relatório_Revalida')

d_rev = readxl::read_xlsx('./INEP_REV_MED_21 - Notas e Usuários da Avaliação e da Análise de Recursos.xlsx')

d_rev[1,4]



##################################################
###### plot - xlab ######


d_rev$Estação = str_trim(str_sub(d_rev$Estação, end = 10))
d_rev$Estação = factor(d_rev$Estação, levels = c('Estação 1','Estação 2','Estação 3','Estação 4','Estação 5','Estação 6','Estação 7','Estação 8','Estação 9','Estação 10'))

##provisória

ggplot(d_rev, aes(x = Estação, y = `Nota Estação Provisória`))+
  geom_boxplot(fill ='turquoise4', width = 0.5)+
  labs(x = '',y = 'Distribuição das notas Provisórias')+
  theme_bw()+
  stat_summary(fun = "mean", fill = 'white', geom = 'point',shape = 23,size = 3, col = 'White')+
  theme(axis.title.y =element_text(colour='Black' , size=12),
        axis.title.x =element_text(colour='Black' , size=12),
        axis.text =element_text(colour='black',size=13),
        panel.border = element_blank(),
        axis.line =element_line(colour='black'))

#Definitiva

ggplot(d_rev, aes(x = Estação, y = `Nota Estação Definitiva`))+
  geom_boxplot(fill ='turquoise4', width = 0.5)+
  labs(x = '',y = 'Distribuição das notas Definitivas')+
  theme_bw()+
  stat_summary(fun = "mean", fill = 'white', geom = 'point',shape = 23,size = 3, col = 'White')+
  theme(axis.title.y =element_text(colour='Black' , size=12),
        axis.title.x =element_text(colour='Black' , size=12),
        axis.text =element_text(colour='black',size=13),
        panel.border = element_blank(),
        axis.line =element_line(colour='black'))



###### plot 2 lines together ----> res provisório e final por estação ######

test_data = d_rev[,c(3,4,5)]

colnames(test_data) = c('Estação','Provisório','Definitivo')

table_notas = aggregate(test_data, list(test_data$Estação), 'mean')

table_notas_mean = table_notas[,c(1,3,4)]


colnames(table_notas_mean) = c('Estação','Provisório','Definitivo')

table_notas_mean = table_notas_mean %>%
  pivot_longer(c(Provisório,Definitivo), names_to = 'Resultado') 
  
label = (table_notas$Definitivo-table_notas$Provisório)
lab_xd = table_notas$Definitivo-table_notas$Provisório


write_xlsx(table_notas,'table_1_not_mean.xlsx')


ggplot(table_notas_mean, aes(x = Estação, y = value, group = Resultado, colour = Resultado))+
  geom_line(size = 1)+ geom_point(size = 2.5)+
  scale_colour_manual(name = 'Resultados:', values = c('turquoise4','#003366'))+
  labs(x = '',y = 'Média das Notas')+
  theme_bw()+
  #stat_summary(fun = "mean", fill = 'turquoise4', geom = 'point',shape = 23,size = 1.3, col = 'White')+
  theme(axis.title.y =element_text(colour='Black' , size=12),
        axis.title.x =element_text(colour='Black' , size=12),
        axis.text =element_text(colour='black',size=13),
        panel.border = element_blank(),
        axis.line =element_line(colour='black'))+
  ylim(c(4,10))+
  #geom_text(aes(label = labels, vjust = -0.6), size = 4, col = 'Black' )+
  theme(legend.position = 'top')


############################################
############ DEMANDA por data ##############

data_dt = d_rev

data_dt$`Data/Hora Avaliação` = str_sub(data_dt$`Data/Hora Avaliação`,end = 10)
data_dt$`Data/Hora Avaliação` = str_replace_all(data_dt$`Data/Hora Avaliação`,'-','/')

tabela_notas_prov = as.data.frame(table(data_dt$`Data/Hora Avaliação`))

colnames(tabela_notas_prov) = c('Data','Quantidade')

tabela_notas_prov$Data = paste(str_sub(tabela_notas_prov$Data,start = 6),'/2022', sep = '') #900qi

write_xlsx(tabela_notas_prov,'table_data_prov.xlsx')


ggplot(tabela_notas_prov, aes(x = Data, y = Quantidade, group = 1))+
  geom_line(size = 1, colour = 'turquoise4')+ geom_point(size = 3,colour = 'turquoise4')+
  labs(x = 'Data',y = 'Quantidade')+
  theme_bw()+
  #stat_summary(fun = "mean", fill = 'turquoise4', geom = 'point',shape = 23,size = 1.3, col = 'White')+
  theme(axis.title.y =element_text(colour='Black' , size=12),
        axis.title.x =element_text(colour='Black' , size=12),
        axis.text.x =element_text(colour='black',size=13, angle = 45,vjust = 1, hjust = 1 ),
        axis.text.y = element_text(colour='black',size=10),
        panel.border = element_blank(),
        axis.line =element_line(colour='black'))+
  ylim(c(0,6000))
  #geom_text(aes(label = labels, vjust = -0.6), size = 4, col = 'Black' )+
  #theme(legend.position = 'top')

########### Recurso

#data_dt = d_rev

data_dt$`Data/Hora Análise Recurso` = str_sub(data_dt$`Data/Hora Análise Recurso`,end = 10)
data_dt$`Data/Hora Análise Recurso` = str_replace_all(data_dt$`Data/Hora Análise Recurso`,'-','/')

tabela_notas_def = as.data.frame(table(data_dt$`Data/Hora Análise Recurso`))

colnames(tabela_notas_def) = c('Data','Quantidade')

tabela_notas_def$Data = paste(str_sub(tabela_notas_def$Data,start = 6),'/2022', sep = '') #900qi

write_xlsx(tabela_notas_def,'table_data_def.xlsx')

ggplot(tabela_notas_def, aes(x = Data, y = Quantidade, group = 1))+
  geom_line(size = 1, colour = 'turquoise4')+ geom_point(size = 3,colour = 'turquoise4')+
  labs(x = 'Data',y = 'Quantidade')+
  theme_bw()+
  #stat_summary(fun = "mean", fill = 'turquoise4', geom = 'point',shape = 23,size = 1.3, col = 'White')+
  theme(axis.title.y =element_text(colour='Black' , size=12),
        axis.title.x =element_text(colour='Black' , size=12),
        axis.text.x =element_text(colour='black',size=10, angle = 45,vjust = 1, hjust = 1 ),
        axis.text.y = element_text(colour='black',size=10),
        panel.border = element_blank(),
        axis.line =element_line(colour='black'))+
  ylim(c(0,6000))


############################################
########### Notas 0 por estação ############ ##
data_dt = d_rev

data_nota_0 = filter(data_dt, data_dt$`Nota Estação Provisória` == 0)

data_nota_0 = as.data.frame(table(data_nota_0$Estação))

colnames(data_nota_0) = c('Estação', 'Quantidade')


write_xlsx(data_nota_0,'nota_0_table.xlsx')

lab = data_nota_0$Quantidade

ggplot(data_nota_0, aes(x = Estação, y = Quantidade))+
  geom_bar(stat = 'identity', fill = 'turquoise4', colour = 'black')+
  labs(x = '',y = 'Quantidade')+
  theme_bw()+
  geom_text(aes(label = lab), vjust = -0.5)+
  #stat_summary(fun = "mean", fill = 'turquoise4', geom = 'point',shape = 23,size = 1.3, col = 'White')+
  theme(axis.title.y =element_text(colour='Black' , size=12),
        axis.title.x =element_text(colour='Black' , size=12),
        axis.text =element_text(colour='black',size=13),
        panel.border = element_blank(),
        axis.line =element_line(colour='black'))+
  ylim(c(0,210))



##########################################################
######################## table_candidatos ################
