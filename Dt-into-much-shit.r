pacman::p_load(tidyr,tidyverse,dplyr,stringr,writexl,rvest,ggplot2,pdftools,tm,officer)

setwd('C:/Users/luiz.graciano/Desktop/Projeto Relatório/DATA')

data_all = readxl::read_xlsx('./glpi.xlsx')

data_all = dplyr::rename(data_all, Prazo = `Tempo para solução`)
data_all = dplyr::rename(data_all, Aberto = `Data de abertura`)

conteudo_filtro = data_all$Descrição

#modificação

data_all$Entidade = NULL
data_all$`Pesquisa de satisfação - Satisfação` = NULL

data_all$`Data de fechamento` = as.character(data_all$`Data de fechamento`)
data_all$Prazo = as.character(data_all$Prazo)
data_all$Aberto = as.character(data_all$Aberto)
data_all$`Data da solução` = as.character(data_all$`Data da solução`)
data_all$`Última atualização` = as.character(data_all$`Última atualização`)

data_all[is.na(data_all)] = '-'

# F

retorno_x = function(pattern){
  
  filtro = regmatches(conteudo_filtro, regexec(pattern,  conteudo_filtro))
  
  filtro = map(filtro,2)
  
  return(filtro)
}

################prazo # O prazo que o hano colocou some nos chamados PENDENTES.

prazo = retorno_x("PRAZO : \\s*(.*?)\\s* ")

###############nome

nome = retorno_x("1) Nome do candidato : \\s*(.*?)\\s*2)")

###############insc

insc = retorno_x('4) Número Inscrição : \\s*(.*?)\\s*5)')

filt_1 = function(carac){
  if(length(carac) !=0){
    if (nchar(carac) != 8 ){
      
      carac = '-'
    }
    return(carac)
  }
    

}

insc = map(insc,filt_1)

###############CPF

CPF = retorno_x('3) CPF : \\s*(.*?)\\s*4)')

filt_2 = function(carac){
  
  if(length(carac) !=0){
    
    while (nchar(carac) < 11){
      
      carac = paste('0',carac, sep = '')
    }
    
    if (carac == '00000000000'){
      
      carac = '-'
    }
    
    return(carac)
  }
  
  
}

CPF = map(CPF, filt_2)

###############cargo

conteudo_filtro = str_replace(conteudo_filtro,'Controle interno', 'Dados internos')

cargo = retorno_x('5) Cargo :\\s*(.*?)\\s*Dados internos')

###############Etapa

etapa = retorno_x('ETAPA : \\s*(.*?)\\s*12)')

###############Concurso

concurso = retorno_x("2) Concurso / ANO : \\s*(.*?)\\s*3)")

##### FIX

data_geral = data_all %>%
  mutate('Nome' = as.character(nome),
         'CPF' = as.character(CPF),
         'Concurso' = as.character(concurso),
         'Inscrição' = as.character(insc),
         'Cargo' = as.character(cargo),
         'Fase' = as.character(etapa),
         'Prazo' = as.character(prazo))


Horário = str_split(data_geral$Prazo, ' ')
Horário = map(Horário,2)

data_geral = mutate(data_geral,
                    'Horário' = as.character(Horário))

#### PRINT !!!!

#data_geral$Requerente = NULL

data_geral$Aberto = str_sub(data_geral$Aberto, end = -9)
data_geral$`Data de fechamento` = str_sub(data_geral$`Data de fechamento`, end = -9)

data_geral$Prazo = str_replace_all(data_geral$Prazo,'-','/')

data_geral = data_geral[order(data_geral$ID),]

concurso = data_geral$Concurso

for(i in 1:length(concurso)){
  if(str_sub(concurso[i], end = 3) == 'htt' ){
    
    lista_ = str_split(concurso[[i]], '/')
    
    concurso[i] =  lista_[[1]][length(lista_[[1]])]
    
  }
}

data_geral$Concurso = concurso

data_geral$Título = NULL




####

data_geral = dplyr::rename(data_geral, GLPI = ID)

dia_hj = Sys.Date() %>%
  str_sub(start = -10) %>%
  str_split('-')

dia_hj = paste(dia_hj[[1]][3],dia_hj[[1]][2],dia_hj[[1]][1], sep = '-')

data_geral$Aberto = str_replace_all(data_geral$Aberto,'-','/')

data_geral$Horário = NULL

colnames(data_geral)

data_geral = data_geral %>%
  select(GLPI,Status,Categoria,Aberto,Prazo,Nome,Inscrição,CPF,Fase,Concurso,Cargo,`Data de fechamento`,`Última atualização`,`Atribuído para - Técnico`,'Requerente - Requerente')

data_geral$`Data de fechamento` = str_replace_all(data_geral$`Data de fechamento`,'-','/')


############# IMPLEMENTAÇÃO BIANCA

### Tempo para Atendimento

tranform_date = function(date){
  date = str_replace_all(date,'-','/')
  date = str_trim(as.character(date))
  date = str_split(date,'/')
  return(paste(date[[1]][3],date[[1]][2],date[[1]][1], sep = '-'))
  
  
}

data_geral$Prazo = as.character(map(data_geral$Prazo,tranform_date))

f_days_make = function(x,y){
  x = str_trim(str_replace_all(x,'/','-'))
  y = str_trim(str_replace_all(y,'/','-'))
  x = as.Date(x)
  y = as.Date(y)
  
  if (x <= y){
    return(length(seq(from = x, to = y, by = 'day'))-1)
  } else {
    return('NULL')
  }
}

ab = data_geral$Aberto[3]
pr = data_geral$Prazo[3]

f_days_make(ab,pr)


#dt = dt[1:465,] #só tinha carac no prazo até a linha 465

for (i in 1:length(data_geral$GLPI)){ ### Faz com que a ausência de dados para o prazo seja supriomida por 0 dias, podendo assim substituir.
  if(data_geral$Prazo[i] == 'NA-NA-NULL'){
    data_geral$Prazo[i] = data_geral$Aberto[i] 
  }
  
}

day_list = as.numeric(map2(data_geral$Aberto,data_geral$Prazo,f_days_make))

data_geral$'Tempo atribuido para conclusão da demanda' = day_list

### Pendência ao comprimento da demanda (+/-)

unique(data_geral$Status)

pendencia = c('Processando (atribuído)','Pendente')

f_prazo_cumprir = function(x,y){
  
  hoje = as.Date(tranform_date(dia_hj)) #para lembrar que rodei isso no inic xd
  
  if(y %in% pendencia){ #elaborar um filtro prévio para coincidir
    
    return(as.numeric(difftime(as.Date(x),hoje)))
    
  }else{
    return('-')
  }
  
}

lista_pend_prazo = map2(data_geral$Prazo,data_geral$Status,f_prazo_cumprir)

data_geral$'Atraso/restante para cumprir demanda' = as.character(lista_pend_prazo)

writexl::write_xlsx(data_geral,paste('Verificacao_GLPIs CAPE_Data ',as.character(dia_hj),'.xlsx'))
