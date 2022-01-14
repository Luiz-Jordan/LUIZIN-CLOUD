setwd('C:/Users/luiz.graciano/Desktop/DIR_RSTUDIO')
#setwd('C:/Users/GUSTAVO/Desktop/DEMANDA_CAMILA')

pacman::p_load(tidyr,tidyverse,dplyr,stringr,writexl,rvest,ggplot2)

DADOS = read.csv2('./Panorama Verificação de Aprendizagem - Capacitações - Revalida 21.csv', sep = ';')

DADOS2 = read.csv2('./Completo.csv', sep = ';')
DADOS2 = DADOS2[1:2253,]
library(ggplot2)

PONTUAÇÃO = DADOS$Pontuação %>%
  stringr::str_sub(start = 1,end =2) %>%
  as.numeric()


PONTUAÇÃO2 = DADOS2$Pontuação %>%
  stringr::str_sub(start = 0,end =2) %>%
  str_trim() %>%
  as.numeric()



DADOS$Pontuação = PONTUAÇÃO
DADOS2$Pontuação = PONTUAÇÃO2

DADOS2$Pontuação = as.integer(DADOS2$Pontuação/(10/6))

Dados_revalida = rbind(DADOS,DADOS2, by = c('Evento'))

Dados_revalida$Pontuação = as.numeric(Dados_revalida$Pontuação)

Coordenações = as.vector(Dados_revalida$Coordenação)

unique(Dados_revalida$Coordenação)
unique(Dados_revalida$Função)

NORTE = c('Manaus 1',"Manaus 2")
NORDESTE =c('Salvador 1','Salvador 2','Belém','Fortaleza','Teresina',"João Pessoa 2","Maceió","João Pessoa 1","Aracaju","São Luís","Campina Grande")
CENTRO_OESTE = c("Goiânia","Brasília 2","Brasília 1","Campo Grande 1","Campo Grande 2")
SUL = c('Pelotas',"Porto Alegre 2","Porto Alegre 1","Santa Maria","Florianópolis","Curitiba" )
SUDESTE = c('Belo Horizonte 1',"Belo Horizonte 2","Niterói","São Paulo","Uberlândia")

NORTE = filter(Dados_revalida, Coordenação %in% NORTE)
NORDESTE = filter(Dados_revalida, Coordenação %in% NORDESTE)
CENTRO_OESTE = filter(Dados_revalida, Coordenação %in% CENTRO_OESTE)
SUL = filter(Dados_revalida, Coordenação %in% SUL)
SUDESTE = filter(Dados_revalida, Coordenação %in% SUDESTE)

Dados_revalida$Coordenação = as.character(Dados_revalida$Coordenação)
#Dados_revalida['Coordenação'][Dados_revalida["Coordenação"] == 'Belo Horizonte 1'] <- 'BH 1'
#Dados_revalida['Coordenação'][Dados_revalida["Coordenação"] == 'Belo Horizonte 2'] <- 'BH 2'
#Dados_revalida['Coordenação'][Dados_revalida["Coordenação"] == 'Campina Grande'] <- 'Campina Gr.'
#Dados_revalida['Coordenação'][Dados_revalida["Coordenação"] == 'Campo Grande 1'] <- 'Campo Gr. 1'
#Dados_revalida['Coordenação'][Dados_revalida["Coordenação"] == 'Campo Grande 2'] <- 'Campo Gr. 2'

Dados_revalida = Dados_revalida %>%
  filter(Coordenações !='')


Dados_revalida$Função = as.character(Dados_revalida$Função)

Dados_revalida['Função'][Dados_revalida['Função'] == 'Ator de Encenação'] ='Paciente Simulado'

Dados_revalida['Função'][Dados_revalida['Função'] == 'Assistente do supervisor operador de câmera'] <- 'Supervisor e Assistente de Pacientes Simulados'

Dados_revalida['Função'][Dados_revalida['Função'] == 'Operador de câmera'] <- 'Operador de Câmera'

Dados_revalida['Função'][Dados_revalida['Função'] == 'Operador de câmara'] <- 'Operador de Câmera'

Dados_revalida['Função'][Dados_revalida['Função'] == 'Operador de Camera'] <- 'Operador de Câmera'


Dados_revalida['Função'][Dados_revalida['Função'] == 'ator de encenação '] <- 'Paciente Simulado'

Dados_revalida['Função'][Dados_revalida['Função'] == 'Câmera'] <- 'Operador de Câmera'

Dados_revalida['Função'][Dados_revalida['Função'] == 'Ator'] <- 'Paciente Simulado'

Dados_revalida['Função'][Dados_revalida['Função'] == 'Atriz'] <- 'Paciente Simulado'


Dados_revalida['Função'][Dados_revalida['Função'] == 'Atriz, que vai participar do simulado'] <- 'Ator(riz)'


Dados_revalida['Função'][Dados_revalida['Função'] == 'Assistente do Supervisor de Pacientes Simulados'] <- 'Supervisor e Assistente de Pacientes Simulados'

Dados_revalida['Função'][Dados_revalida['Função'] == 'Atriz, que vai participar do simulado '] <- 'Ator(riz)'
                           


Dados_revalida['Função'][Dados_revalida['Função'] == "Auditor"] <-'Operador de Câmera'

Dados_revalida['Função'][Dados_revalida['Função'] == "Supervisor de Pacientes Simulados"] <-'Supervisor e Assistente de Pacientes Simulados'




Dados_revalida['Função'][Dados_revalida['Função'] == 'Ator de Encenação'] ='Paciente Simulado'

Dados_revalida['Função'][Dados_revalida['Função'] == 'Ator(riz)'] ='Paciente Simulado'
Dados_revalida['Função'][Dados_revalida['Função'] == 'Assistente de Paciente Simulado'] ='Supervisor e Assistente de Pacientes Simulados'



NORTE = c('Manaus 1',"Manaus 2")
NORDESTE =c('Salvador 1','Salvador 2','Belém','Fortaleza','Teresina',"João Pessoa 2","Maceió","João Pessoa 1","Aracaju","São Luís","Campina Grande")
CENTRO_OESTE = c("Goiânia","Brasília 2","Brasília 1","Campo Grande 1","Campo Grande 2")
SUL = c('Pelotas',"Porto Alegre 2","Porto Alegre 1","Santa Maria","Florianópolis","Curitiba" )
SUDESTE = c('Belo Horizonte 1',"Belo Horizonte 2","Niterói","São Paulo","Uberlândia")

NORTE = filter(Dados_revalida, Coordenação %in% NORTE)
NORDESTE = filter(Dados_revalida, Coordenação %in% NORDESTE)
CENTRO_OESTE = filter(Dados_revalida, Coordenação %in% CENTRO_OESTE)
SUL = filter(Dados_revalida, Coordenação %in% SUL)
SUDESTE = filter(Dados_revalida, Coordenação %in% SUDESTE)






############################################################################################################################################

################################FREQUêNCIA#################################################################################################

Fr = table(Dados_revalida$Coordenação)
Pr = as.data.frame(round( prop.table( Fr ) , digits = 4 ) * 100 )

colnames(Pr) = c('Var1', 'Pr')

comp = merge (Fr, Pr, by= 'Var1' )
comp$Pr = paste(gsub('\\.', ',',comp$Pr),'%',sep = '')

comp <-comp[order(comp$Freq),]


ggplot(comp, aes(x = Var1, y = Freq, label = Pr))+
  geom_bar(stat = 'identity' , fill ='turquoise4')+
  geom_text(vjust = -0.5, size = 3.5)+
  labs(x = 'Coordenação',y = 'Frequência')+
  theme_bw()+
  theme(axis.title.y =element_text(colour='Black' , size=12),
        axis.title.x =element_text(colour='Black' , size=12),
        axis.text =element_text(colour='black',size=6),
        panel.border = element_blank(),
        axis.line =element_line(colour='black'))



################################Média das Notas#################################################################################################

ggplot(Dados_revalida, aes(x = Coordenação, y = Pontuação)) + 
  stat_summary(fun = "mean", fill = 'turquoise4', size = 1, col = 'turquoise4')+
  labs(x = 'Coordenação',y = 'Média das Pontuações')+
  theme_bw()+
  theme(axis.title.y =element_text(colour='Black' , size=12),
        axis.title.x =element_text(colour='Black' , size=12),
        axis.text =element_text(colour='black',size=6),
        panel.border = element_blank(),
        axis.line =element_line(colour='black'))+
  ylim(c(5,6))


############################################# 29 GRÁFICOS que apresentem o rendimento médio das funções por coordenação ###################################

Criar_Rend_Med = function(Dados_Coordenação,text){
  
  Data_F= aggregate(Dados_Coordenação[,c(8)], list(Dados_Coordenação$Função), mean)
  
  Labels = round(Data_F$x/6*100 , digits = 2)
  
  Labels_F = paste(as.character(Labels),'%',sep = '')
  
  colnames(Data_F) = c('Função','Média')
  operator = 4
  if( min(Data_F$Média) < 4){
    operator = min(Data_F$Média) -0.5
  }
  ggplot(Data_F, aes(x = Função, y = Média, labels = Labels_F)) + 
    stat_summary(fun = "mean", fill = 'turquoise4', size = 1, col = 'turquoise4')+
    labs(x = 'Função',y = 'Média das Pontuações', title = 'Rendimento Médio por Função', subtitle = text)+
    theme_bw()+
    theme(axis.title.y =element_text(colour='Black' , size=12),
          axis.title.x =element_text(colour='Black' , size=12),
          axis.text =element_text(colour='black',size=10),
          panel.border = element_blank(),
          axis.line =element_line(colour='black'))+
    geom_text(aes(label = Labels_F, vjust = -0.6), size = 4, col = 'Black' )+
    ylim(operator,6)

}








############################# CC2 --->  1) Pacientes Simulados; e 2) Supervisor e Assistente de Pacientes Simulados
########NORTE########## 

Dados_CC2 = filter(Dados_revalida, Função == 'Supervisor e Assistente de Pacientes Simulados' | Função == 'Paciente Simulado')
#Dados_CC2_Error = filter(Dados_revalida, Função == 'Paciente Simulado')

#Dados_cc4 = filter(Dados_revalida, Função == 'Médicos Multiplicadores')
#setwd('C:/Users/luiz.graciano/Desktop/DIR_RSTUDIO/Gráficos')

Criar_Rend_Med(Manaus1, 'Manaus 1')#plot


Criar_Rend_Med(Dados_CC2, 'Revalida 21')



# NORTE = c('Manaus 1',"Manaus 2")

Manaus1 = filter(Dados_CC2, Coordenação == 'Manaus 1')

Criar_Rend_Med(Manaus1, 'Manaus 1')#plot

Manaus2 = filter(Dados_CC2, Coordenação == 'Manaus 2')

Criar_Rend_Med(Manaus2, 'Manaus 2')#plot


#######NORDESTE########

# NORDESTE =c('Salvador 1','Salvador 2','Belém','Fortaleza','Teresina',
#"João Pessoa 2","Maceió","João Pessoa 1","Aracaju","São Luís","Campina Grande")



Criar_Rend_Med(Salvador1, 'Salvador 1')


Salvador1 = filter(Dados_CC2, Coordenação == 'Salvador 1')

Criar_Rend_Med(Salvador1, 'Salvador 1')#plot

Salvador2 = filter(Dados_CC2, Coordenação == 'Salvador 2')

Criar_Rend_Med(Salvador2, 'Salvador 2')#plot

Belém = filter(Dados_CC2, Coordenação == 'Belém')

Criar_Rend_Med(Belém, 'Belém')#plot

Fortaleza = filter(Dados_CC2, Coordenação == 'Fortaleza')

Criar_Rend_Med(Fortaleza, 'Fortaleza')


Teresina = filter(Dados_CC2, Coordenação == 'Teresina')

Criar_Rend_Med(Teresina, 'Teresina')



João_Pessoa2 = filter(Dados_CC2, Coordenação == 'João Pessoa 2')
Criar_Rend_Med(João_Pessoa2, 'João Pessoa 2')



Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Maceió'), 'Maceió')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'João Pessoa 1'), 'João Pessoa 1')


Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Aracaju'), 'Aracaju')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'São Luís'), 'São Luís')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Campina Grande'), 'Campina Grande')


#######CENTRO-OESTE####

# CENTRO_OESTE = c("Goiânia","Brasília 2","Brasília 1","Campo Grande 1","Campo Grande 2")

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Goiânia'), 'Goiânia')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Brasília 2'), 'Brasília 2')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Brasília 1'), 'Brasília 1')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Campo Grande 1'), 'Campo Grande 1')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Campo Grande 2'), 'Campo Grande 2')

#######SUDESTE#########

# SUDESTE = c('Belo Horizonte 1',"Belo Horizonte 2","Niterói","São Paulo","Uberlândia")

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Belo Horizonte 1'), 'Belo Horizonte 1')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Belo Horizonte 2'), 'Belo Horizonte 2')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Niterói'), 'Niterói')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'São Paulo'), 'São Paulo')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Uberlândia'), 'Uberlândia')


#######SUL#############

# SUL = c('Pelotas',"Porto Alegre 2","Porto Alegre 1","Santa Maria","Florianópolis","Curitiba" )

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Pelotas'), 'Pelotas')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Porto Alegre 2'), 'Porto Alegre 2')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Porto Alegre 1'), 'Porto Alegre 1')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Santa Maria'), 'Santa Maria')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Florianópolis'), 'Florianópolis')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Curitiba'), 'Curitiba')






#####################################################################################################################
########################################### CC4 ###################################################################

Dados_revalida['Função'][Dados_revalida['Função'] == 'Chefe de sala de espera inicial'] = 'Médico Multiplicador'

Dados_CC4 = filter(Dados_revalida, Função == 'Médico Multiplicador' | Função == 'Chefe de Estação')


Dados_CC2 = Dados_CC4



Dados_CC4['Função'][Dados_CC4['Função'] == 'Médico Multiplicador'] = 'Médico Multiplicador e Chefe de Estação'

Dados_CC4['Função'][Dados_CC4['Função'] == 'Chefe de Estação'] = 'Médico Multiplicador e Chefe de Estação'

Dados_CC2 = Dados_CC4

setwd('C:/Users/GUSTAVO/Desktop/DEMANDA_CAMILA/Gráficos CC4')


# NORTE = c('Manaus 1',"Manaus 2")

Manaus1 = filter(Dados_CC2, Coordenação == 'Manaus 1')

Criar_Rend_Med(Manaus1, 'Manaus 1')#plot

Manaus2 = filter(Dados_CC2, Coordenação == 'Manaus 2')

Criar_Rend_Med(Manaus2, 'Manaus 2')#plot


#######NORDESTE########

# NORDESTE =c('Salvador 1','Salvador 2','Belém','Fortaleza','Teresina',
#"João Pessoa 2","Maceió","João Pessoa 1","Aracaju","São Luís","Campina Grande")



Salvador1 = filter(Dados_CC2, Coordenação == 'Salvador 1')

Criar_Rend_Med(Salvador1, 'Salvador 1')#plot

Salvador2 = filter(Dados_CC2, Coordenação == 'Salvador 2')

Criar_Rend_Med(Salvador2, 'Salvador 2')#plot

Belém = filter(Dados_CC2, Coordenação == 'Belém')

Criar_Rend_Med(Belém, 'Belém')#plot

Fortaleza = filter(Dados_CC2, Coordenação == 'Fortaleza')

Criar_Rend_Med(Fortaleza, 'Fortaleza')


Teresina = filter(Dados_CC2, Coordenação == 'Teresina')

Criar_Rend_Med(Teresina, 'Teresina')



João_Pessoa2 = filter(Dados_CC2, Coordenação == 'João Pessoa 2')
Criar_Rend_Med(João_Pessoa2, 'João Pessoa 2')



Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Maceió'), 'Maceió')

João_Pessoa1 = filter(Dados_CC2, Coordenação == 'João Pessoa 1')
#João_Pessoa1$Pontuação = 5.2

Criar_Rend_Med(João_Pessoa1, 'João Pessoa 1')


Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Aracaju'), 'Aracaju')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'São Luís'), 'São Luís')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Campina Grande'), 'Campina Grande')


#######CENTRO-OESTE####

# CENTRO_OESTE = c("Goiânia","Brasília 2","Brasília 1","Campo Grande 1","Campo Grande 2")

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Goiânia'), 'Goiânia')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Brasília 2'), 'Brasília 2')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Brasília 1'), 'Brasília 1')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Campo Grande 1'), 'Campo Grande 1')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Campo Grande 2'), 'Campo Grande 2')

#######SUDESTE#########

# SUDESTE = c('Belo Horizonte 1',"Belo Horizonte 2","Niterói","São Paulo","Uberlândia")

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Belo Horizonte 1'), 'Belo Horizonte 1')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Belo Horizonte 2'), 'Belo Horizonte 2')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Niterói'), 'Niterói')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'São Paulo'), 'São Paulo')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Uberlândia'), 'Uberlândia')


#######SUL#############

# SUL = c('Pelotas',"Porto Alegre 2","Porto Alegre 1","Santa Maria","Florianópolis","Curitiba" )

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Pelotas'), 'Pelotas')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Porto Alegre 2'), 'Porto Alegre 2')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Porto Alegre 1'), 'Porto Alegre 1')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Santa Maria'), 'Santa Maria')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Florianópolis'), 'Florianópolis')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Curitiba'), 'Curitiba')


######GERAL ->


Criar_Rend_Med(Dados_CC2,'Revalida 21')




##################################################################################

################################################ CC6 #############################

setwd('C:/Users/GUSTAVO/Desktop/DEMANDA_CAMILA/Gráficos CC6')


########## -------------------> Operadores de câmera e supervisores!

Dados_revalida['Função'][Dados_revalida['Função'] == 'Operador de Câmera'] = 'Operador de Câmera e Supervisor'

Dados_revalida['Função'][Dados_revalida['Função'] == 'Supervisor dos operadores de câmeras'] = 'Operador de Câmera e Supervisor'


########## -------------------> Equipe estruturação (todas as funções desse grupo)!
                            
unique(Dados_revalida$Função)

estruturação = c('Chefe de sala de espera final','Fiscal de Ambiente de Espera (sala de espera final)',
                 'Fiscal de banheiro', 'Apoio Técnico Suporte TI','Apoio limpeza','Manutenção  limpeza',
                 'Apoio operacional (aux. De limpeza)','Chefe/fiscal','Pessoal de apoio - Limpeza',
                 'Segurança de local de aplicação','Fiscal Identificador (sala de espera inicial)',
                 'Segurança noturno','Fiscal de Banheiro','Fiscal de sala','Fiscal de Embalagem')

Dados_revalida['Função'][Dados_revalida['Função'] %in% estruturação] = 'Equipe de Estruturação'

#WHY THAT SHIT DOES NOT WORK??????????????????? Luizin method:

for (i in 1:length(Dados_revalida$Função)){
  if (Dados_revalida[i,3] %in% estruturação == TRUE){
    Dados_revalida[i,3] = 'Equipe de Estruturação'
  }
}

Dados_revalida[1341,3] %in% estruturação


########### ------------------->  Equipe implementação ############

Implementação = c('Paciente Simulado','Chefe de Estação','Chefe de Módulo','Ator simulado ','Fiscal Encaminhador',
                  'Operador de câmera ','Operador de câmara ','Operado de camera','Operador de Câmera',
                  'Operador de camera auditor','Cronometrista')


for (i in 1:length(Dados_revalida$Função)){
  if (Dados_revalida[i,3] %in% Implementação == TRUE){
    Dados_revalida[i,3] = 'Equipe de Implementação'
  }
}


########## ------------------> Equipe de montagem de estação ########

Mont_estação = c('Responsável por equipe de montadores de estação','Montador de Estação')

for (i in 1:length(Dados_revalida$Função)){
  if (Dados_revalida[i,3] %in% Mont_estação == TRUE){
    Dados_revalida[i,3] = 'Equipe de Montagem de Estação e Manutenção'
  }
}


##### CCC6 ####

Dados_cc6 = filter(Dados_revalida,  Função == 'Equipe de Implementação' |Função == 'Equipe de Estruturação' |Função == 'Operador de Câmera e Supervisor')
Dados_cc6['Função'][Dados_cc6['Função'] == 'Equipe de Montagem de Estação e Manutenção'] = 'Equipe de Montagem de Estação'

##### Geral ------>


Criar_Rend_Med(Dados_cc6,'Revalida 21')

setwd('C:/Users/luiz.graciano/Desktop/DIR_RSTUDIO/Gráficos CC6')



################################################ CCC6 - SEM EQ MONTAGEM ESTAÇÃO ##################################################
Dados_CC2 = Dados_cc6

# NORTE = c('Manaus 1',"Manaus 2")

Manaus1 = filter(Dados_CC2, Coordenação == 'Manaus 1')

Criar_Rend_Med(Manaus1, 'Manaus 1')#plot

Manaus2 = filter(Dados_CC2, Coordenação == 'Manaus 2')

Criar_Rend_Med(Manaus2, 'Manaus 2')#plot


#######NORDESTE########

# NORDESTE =c('Salvador 1','Salvador 2','Belém','Fortaleza','Teresina',
#"João Pessoa 2","Maceió","João Pessoa 1","Aracaju","São Luís","Campina Grande")






Salvador1 = filter(Dados_CC2, Coordenação == 'Salvador 1')

Criar_Rend_Med(Salvador1, 'Salvador 1')#plot

Salvador2 = filter(Dados_CC2, Coordenação == 'Salvador 2')

Criar_Rend_Med(Salvador2, 'Salvador 2')#plot

Belém = filter(Dados_CC2, Coordenação == 'Belém')

Criar_Rend_Med(Belém, 'Belém')#plot

Fortaleza = filter(Dados_CC2, Coordenação == 'Fortaleza')

Criar_Rend_Med(Fortaleza, 'Fortaleza')


Teresina = filter(Dados_CC2, Coordenação == 'Teresina')

Criar_Rend_Med(Teresina, 'Teresina')



João_Pessoa2 = filter(Dados_CC2, Coordenação == 'João Pessoa 2')
Criar_Rend_Med(João_Pessoa2, 'João Pessoa 2')



Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Maceió'), 'Maceió')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'João Pessoa 1'), 'João Pessoa 1')


Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Aracaju'), 'Aracaju')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'São Luís'), 'São Luís')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Campina Grande'), 'Campina Grande')


#######CENTRO-OESTE####

# CENTRO_OESTE = c("Goiânia","Brasília 2","Brasília 1","Campo Grande 1","Campo Grande 2")

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Goiânia'), 'Goiânia')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Brasília 2'), 'Brasília 2')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Brasília 1'), 'Brasília 1')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Campo Grande 1'), 'Campo Grande 1')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Campo Grande 2'), 'Campo Grande 2')

#######SUDESTE#########

# SUDESTE = c('Belo Horizonte 1',"Belo Horizonte 2","Niterói","São Paulo","Uberlândia")

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Belo Horizonte 1'), 'Belo Horizonte 1')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Belo Horizonte 2'), 'Belo Horizonte 2')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Niterói'), 'Niterói')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'São Paulo'), 'São Paulo')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Uberlândia'), 'Uberlândia')


#######SUL#############

# SUL = c('Pelotas',"Porto Alegre 2","Porto Alegre 1","Santa Maria","Florianópolis","Curitiba" )

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Pelotas'), 'Pelotas')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Porto Alegre 2'), 'Porto Alegre 2')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Porto Alegre 1'), 'Porto Alegre 1')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Santa Maria'), 'Santa Maria')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Florianópolis'), 'Florianópolis')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Curitiba'), 'Curitiba')



##############################################################################
#############################################################################

#
##############################################################################
#############################################################################

######################################TRANFORMAÇÃO#######################################################

#
#############################################################################
#############################################################################


Dados_iplement = filter(Dados_cc6, Função == 'Equipe de Implementação')

Dados_CC2 = Dados_iplement######


# NORTE = c('Manaus 1',"Manaus 2")

Manaus1 = filter(Dados_CC2, Coordenação == 'Manaus 1')

Criar_Rend_Med(Manaus1, 'Manaus 1')#plot

Manaus2 = filter(Dados_CC2, Coordenação == 'Manaus 2')

Criar_Rend_Med(Manaus2, 'Manaus 2')#plot


#######NORDESTE########

# NORDESTE =c('Salvador 1','Salvador 2','Belém','Fortaleza','Teresina',
#"João Pessoa 2","Maceió","João Pessoa 1","Aracaju","São Luís","Campina Grande")






Salvador1 = filter(Dados_CC2, Coordenação == 'Salvador 1')

Criar_Rend_Med(Salvador1, 'Salvador 1')#plot

Salvador2 = filter(Dados_CC2, Coordenação == 'Salvador 2')

Criar_Rend_Med(Salvador2, 'Salvador 2')#plot

Belém = filter(Dados_CC2, Coordenação == 'Belém')

Criar_Rend_Med(Belém, 'Belém')#plot

Fortaleza = filter(Dados_CC2, Coordenação == 'Fortaleza')

Criar_Rend_Med(Fortaleza, 'Fortaleza')


Teresina = filter(Dados_CC2, Coordenação == 'Teresina')

Criar_Rend_Med(Teresina, 'Teresina')



João_Pessoa2 = filter(Dados_CC2, Coordenação == 'João Pessoa 2')
Criar_Rend_Med(João_Pessoa2, 'João Pessoa 2')



Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Maceió'), 'Maceió')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'João Pessoa 1'), 'João Pessoa 1')


Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Aracaju'), 'Aracaju')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'São Luís'), 'São Luís')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Campina Grande'), 'Campina Grande')


#######CENTRO-OESTE####

# CENTRO_OESTE = c("Goiânia","Brasília 2","Brasília 1","Campo Grande 1","Campo Grande 2")

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Goiânia'), 'Goiânia')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Brasília 2'), 'Brasília 2')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Brasília 1'), 'Brasília 1')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Campo Grande 1'), 'Campo Grande 1')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Campo Grande 2'), 'Campo Grande 2')

#######SUDESTE#########

# SUDESTE = c('Belo Horizonte 1',"Belo Horizonte 2","Niterói","São Paulo","Uberlândia")

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Belo Horizonte 1'), 'Belo Horizonte 1')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Belo Horizonte 2'), 'Belo Horizonte 2')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Niterói'), 'Niterói')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'São Paulo'), 'São Paulo')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Uberlândia'), 'Uberlândia')


#######SUL#############

# SUL = c('Pelotas',"Porto Alegre 2","Porto Alegre 1","Santa Maria","Florianópolis","Curitiba" )

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Pelotas'), 'Pelotas')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Porto Alegre 2'), 'Porto Alegre 2')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Porto Alegre 1'), 'Porto Alegre 1')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Santa Maria'), 'Santa Maria')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Florianópolis'), 'Florianópolis')

Criar_Rend_Med(filter(Dados_CC2, Coordenação == 'Curitiba'), 'Curitiba')


################## GERAL #######
Criar_Rend_Med(Dados_CC2,'Revalida 21')
