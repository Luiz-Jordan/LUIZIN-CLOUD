pacman::p_load(tidyr,tidyverse,dplyr,stringr,writexl,rvest,ggplot2,pdftools,tm,officer)

setwd('C:/Users/luiz.graciano/Desktop/Central_Project_GLPI')

data_sub = readxl::read_xlsx('./GLPI_Subsídios.xlsx')

data_dec = readxl::read_xlsx('./GLPI_Decisões.xlsx')

data_all = bind_rows(data_sub,data_dec)

####minério

conteudo_filtro = data_all$Conteúdo


# F

retorno_x = function(pattern){
  
  filtro = regmatches(conteudo_filtro, regexec(pattern,  conteudo_filtro))
  
  filtro = map(filtro,2)
  
  return(filtro)
}

################prazo

prazo = retorno_x("PRAZO : \\s*(.*?)\\s*D")

###############nome



nome = retorno_x("1) Nome do candidato : \\s*(.*?)\\s*2)")

###############insc

insc = retorno_x('4) Número Inscrição : \\s*(.*?)\\s*5)')

###############CPF


CPF = retorno_x('3) CPF : \\s*(.*?)\\s*4)')


###############cargo

conteudo_filtro = str_replace(conteudo_filtro,'Controle interno', 'Dados internos')

cargo = retorno_x('5) Cargo :\\s*(.*?)\\s*Dados internos')

###############Etapa

etapa = retorno_x('ETAPA : \\s*(.*?)\\s*12)')

###############Observ

pattern_prazo <- "PRAZO : \\s*(.*?)\\s*D"

obs = retorno_x(' Observação : \\s*(.*?)\\s* Anexo : ') ###check hm

obs = as.list(str_sub(obs, end = -3))

#colnames(obs) = 'Observação'

###############Concurso

concurso = retorno_x("2) Concurso / ANO : \\s*(.*?)\\s*3)")


#################################################################################################################
############# MUTATE -> PRINT


############################ FIX CPF
######################


data_geral = data_all %>%
  mutate('Nome' = as.character(nome),
    'CPF' = as.character(CPF),
    'Concurso' = as.character(concurso),
    'Inscrição' = as.character(insc),
    'Cargo' = as.character(cargo),
    'Fase' = as.character(etapa),
    'Observação' = as.character(obs),
    'Prazo' = str_trim(as.character(prazo)))


Horário = str_split(data_geral$Prazo, ' ')
Horário = map(Horário,2)


data_geral$Prazo = str_sub(data_geral$Prazo, end = -6)

data_geral = mutate(data_geral,
                    'Horário' = as.character(Horário))

#### PRINT !!!!

data_geral[is.na(data_geral)] = 'NULL'

data_geral$Tipo = NULL
data_geral$Fonte = NULL
data_geral$Prioridade = NULL

data_geral$Status = paste('https://atendimento.cebraspe.org.br/glpi/front/ticket.form.php?id=',data_geral$ID, sep = '')

data_geral = data_geral %>%
  rename(Link = Status )

data_geral$Requerente = NULL
data_geral$Fechado = NULL
data_geral$Vencimento = NULL
data_geral$Conteúdo = NULL

data_geral$Aberto = str_sub(data_geral$Aberto, end = -9)

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


unique(concurso)

data_geral$Título = NULL


write_xlsx(data_geral, 'Data_final.xlsx')



#################################################################
###############Teste nomes!

nome_tit = data_geral$Título

nome_tit = as.list(nome_tit)


filt_nome = function(list){
  lista = str_split(list, '-')
  return(lista[[1]][length(lista[[1]])])
}

nome_tit = str_trim(map(nome_tit,filt_nome))


nome_tit == data_geral$Nome

#################################################################

as.data.frame(table(data_geral$Fase))
