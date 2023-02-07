pacman::p_load(devtools,RColorBrewer,ggplot2,stringr,tidyverse,dplyr,writexl,epitools)

#data

setwd('C:/Users/Lg/Desktop/Pesquisa - Aleida')


dt = readxl::read_xlsx('./Dados_Aleida_Carvalho_Pesquisa_Mestrado.xlsx', col_names = T)

dt = dt[1:71,4:39]  


########## Mudar no Excel, fatores de risco e segurança ##########

dt$`Classificação da pontuação do BDI` = factor(dt$`Classificação da pontuação do BDI`, levels = c('Sem sinais depressivos (0 à 11)','Nível Leve (12 à 19)','Nível Moderado (20 à 35)','Nível Grave (36 à 63)'))
dt$`Como reagiu quando soube que estava grávida?` = factor(dt$`Como reagiu quando soube que estava grávida?`, levels = c('Ficou feliz','Aceitou','Tentou ou desejou fazer um aborto'))
dt$`As mudanças na sua situação financeira durante a pandemia do Covid-19 dificultaram o pagamento de itens básicos, como comida, moradia, remédios?`= factor(dt$`As mudanças na sua situação financeira durante a pandemia do Covid-19 dificultaram o pagamento de itens básicos, como comida, moradia, remédios?`,levels = c('Não dificultou','Um pouco','Muito'))


#### Plot


f_plot = function(data_tidy,name_var2){
  
  data_tidy = na.omit(data_tidy)
  
  colnames(data_tidy) = c('Var1','Var2','Freq')
  
  frequenc = round(data_tidy$Freq/sum(data_tidy$Freq),2)
  
  percent = paste(frequenc*100,'%', sep = '')
  
  ggplot(data_tidy, aes(x = Var1, y = Freq, fill = Var2))+
    geom_bar(stat = 'identity',position = 'dodge', color = 'black')+
    scale_fill_manual(name= name_var2, values = c('cyan','darkblue','darkorchid','pink','darkred'))+
    theme_bw()+
    theme(axis.title.y =element_text(colour='Black' , size=18),
          axis.title.x =element_text(colour='Black' , size=18),
          axis.text =element_text(colour='Black',size=12),
          panel.border = element_blank(),
          axis.line =element_line(colour='black'),
          legend.position = 'top')+
    labs(x = 'Inventário de Depressão de BECK - BDI', y = 'Frequência',size = 20)+
    geom_text(aes(label = percent), position = position_dodge(0.9), vjust = -0.3, size = 8)
  
  
  
}



##############################################
#
#          Tabela de p-valor, para cada fator


f_test_chisq = function(table){
  
  return(chisq.test(table)$p.value)
  
}

f_test_fisher = function(table){
  
  return(fisher.test(table)$p.value)
  
}

data_chisq = rep('-',34)

data_fisher = rep('-',34)

for (i in 3:36){
  
  tabela_dt = table((dt[,c(1,i)]))
  
  x = i-2
  
  data_chisq[x] = round(f_test_chisq(tabela_dt),4)
  
  data_fisher[x] = round(f_test_fisher(tabela_dt),4) 
  
}

data_risco_pvalor = data.frame('Fator de Risco' = colnames(dt[3:28]),
                               'P-valor' = data_fisher[1:26])

write_xlsx(data_risco_pvalor,'risco_pvalor.xlsx')


data_segur_pvalor = data.frame('Fator de Segurança' = colnames(dt[29:36]),
                               'P-valor' = data_fisher[27:34])

write_xlsx(data_segur_pvalor,'segur_pvalor.xlsx')


##########################
###### Plot de variáveis importantes:

f_sav = function(graf,name){
  
  ggsave(filename = paste(name,'.png', sep =''),width=14, height=8,
         plot = graf)
  
}


setwd('C:/Users/Lg/Desktop/Pesquisa - Aleida/graph')


#f_test_fisher(table(dt$`Classificação da pontuação do BDI`,dt$`Primeira gestação:`))


## Histórico de depressão em gestação anterior

dt_hdepre = as.data.frame(table(dt$`Classificação da pontuação do BDI`,dt$`Alguma vez em que você esteve grávida teve depressão?`))

dt_hdepre = dt_hdepre[c(1:4,9:12),]


x = f_plot(dt_hdepre,'Teve depressão em alguma gravidez?')

f_sav(x,'1')


## Histórico de depressão pós-parto

dt_depre = as.data.frame(table(dt[,c(1,8)]))

dt_depre = dt_depre[c(1:4,9:12),]

x = f_plot(dt_depre,'Teve depressão pós-parto?')

f_sav(x,'2')

#

## Histórico depressão fora do ciclo gravídico-puerperal

dt_depre = as.data.frame(table(dt[,c(1,9)]))

x = f_plot(dt_depre,'Teve depressão fora do ciclo gravídico-puerperal?')

f_sav(x,'3')

#

## Perda de ente querido por Covid-19

dt_mort = as.data.frame(table(dt[,c(1,15)]))

dt_mort = dt_mort[c(1:4,9:12),]

fisher.test(table(dt[,c(1,15)]))$p.value

x = f_plot(dt_mort,'Perda de ente querido por Covid-19')

f_sav(x,'4')

#

## Dificuldade física (enjoo prolongado, dores, etc.) na gestação que traz sofrimento

dt_dif = as.data.frame(table(dt[,c(1,20)]))

x = f_plot(dt_dif,'Dificuldades físicas na gestação')

f_sav(x,'5')

#

## Internação de ente querido por COVID-19

dt_inter = as.data.frame(table(dt[,c(1,22)]))

dt_inter = dt_inter[c(1:4,9:12),]

x = f_plot(dt_inter,'Houve internação de ente querido por Covid-19?')

f_sav(x,'6')

#

## Foi responsável por cuidar pessoa significativa com Covid-19

dt_cuid = as.data.frame(table(dt[,c(1,23)]))

dt_cuid = dt_cuid[c(1:4,9:12),]

x = f_plot(dt_cuid,'Responsável por cuidar de alguma pessoa íntima com Covid-19')

f_sav(x,'7')

#

## Perdeu o emprego permanentemente ou não foi paga por causa da pandemia

dt_desem = as.data.frame(table(dt[,c(1,24)]))

x = f_plot(dt_desem,'Teve problemas financeiros devido a pandemia?')

f_sav(x,'8')

#

## As mudanças na sua situação financeira durante a pandemia do Covid-19 dificultaram o pagamento de itens básicos

dt_mudan = as.data.frame(table(dt[,c(1,25)]))

x = f_plot(dt_mudan,'As mudanças financeiras dificultaram o pagamento de itens básicos?')

f_sav(x,'9')

#

## Se sentiu solitária nesse período de isolamento social

dt_solit = as.data.frame(table(dt[,c(1,26)]))

x = f_plot(dt_solit,'Se sentiu solitário nesse período de pandemia?')

f_sav(x,'10')

#

## Deixou de praticar atividade religiosa com o isolamento

dt_noprat = as.data.frame(table(dt[,c(1,27)]))

x = f_plot(dt_noprat,'Deixou de praticar atividades religiosas com o isolamento?')

f_sav(x,'11')

#

## Redução do apoio emocional com isolamento social

dt_redemo = as.data.frame(table(dt[,c(1,28)]))

x = f_plot(dt_redemo,'Houve redução do apoio emocional com o isolamento?')

f_sav(x,'12')

#


#######################################################################################################################################


############################################# Fatores de segurança #############################################


## Ajuda financeira de alguma pessoa ou auxílio emergencial

dt_ajud = as.data.frame(table(dt[,c(1,29)]))

x = f_plot(dt_ajud,'Obteve apoio financeiro durante a pandemia?')

f_sav(x,'seg1')

#

## Apoio emocional do pai do bebê

dt_apoio = as.data.frame(table(dt[,c(1,30)]))

x = f_plot(dt_apoio,'Obteve apoio emocional do pai do bebê?')

f_sav(x,'seg2')

#

## Apoio financeiro do pai do bebê

dt_apoio_f = as.data.frame(table(dt[,c(1,31)]))

x = f_plot(dt_apoio_f,'Obteve apoio financeiro do pai do bebê?')

f_sav(x,'seg3')

#

## Apoio emocional de outras pessoas das suas relações

dt_emocio = as.data.frame(table(dt[,c(1,32)]))

x = f_plot(dt_emocio,'Obteve apoio emocional de outras pessoas?')

f_sav(x,'seg4')

#

## Apoio institucional (igreja, governo, ONGs, trabalho, hospital e etc)

dt_reg = as.data.frame(table(dt[,c(1,33)]))

x = f_plot(dt_reg,'Obteve apoio institucional?')

f_sav(x,'seg5')

#

## Apoio de alguém para te ajudar nas atividades práticas do dia a dia

dt_ap_al = as.data.frame(table(dt[,c(1,34)]))

x = f_plot(dt_ap_al,'Obteve ajuda nas atividades do dia a dia?')

f_sav(x,'seg6')



#

## Apoio de amigos, familiares ou companheiros para conversar sobre seus medos e preocupações durante o período que esteve com sintomas do Covid-19

dt_ap_am = as.data.frame(table(dt[,c(1,35)]))

x = f_plot(dt_ap_am,'Obteve apoio de familiares ou companheiros durante o período que esteve com sintomas do Covid-19?')

f_sav(x,'seg7')

#

## Apoio da equipe de saúde durante o diagnóstico de Covid-19 e tratamento

dt_ap_eq = as.data.frame(table(dt[,c(1,36)]))

x = f_plot(dt_ap_eq,'Obteve apoio da equipe de saúde durante o diagnóstico e tratamento do Covid-19?')

f_sav(x,'seg8')

#


##
####################### Análise por Grupo #########################
##

setwd('C:/Users/Lg/Desktop/Pesquisa - Aleida')

grupo_controle <- readxl::read_excel("./Grupo_controle_e_Intervenção_Tabela_ Final_Aleida_Carvalho.xlsx", sheet = 2)
grupo_controle = grupo_controle[1:8,]
grupo_controle = mutate(grupo_controle, `Grupo de Pré-Natal Psicológico` = 'Controle')

grupo_intervec <- readxl::read_excel("./Grupo_controle_e_Intervenção_Tabela_ Final_Aleida_Carvalho.xlsx", sheet = 1)
grupo_intervec = grupo_intervec[1:7,]
grupo_intervec = mutate(grupo_intervec, `Grupo de Pré-Natal Psicológico` = 'Intervenção')

colnames(grupo_controle) = colnames(grupo_intervec)




### tabela de contigência --> odds ratio

grupos = bind_rows(grupo_controle,grupo_intervec)

grupos$`Grupo de Pré-Natal Psicológico` = factor(grupos$`Grupo de Pré-Natal Psicológico`, levels = c('Intervenção','Controle'))

xd = table(grupos$`Grupo de Pré-Natal Psicológico`,grupos$`Resultado do Instrumento - Risco de desenvolver depressão`)

oddsratio(xd)$data
