###### 

pacman::p_load(tidyr,tidyverse,dplyr,stringr,writexl,rvest,ggplot2,pdftools,tm,officer)

setwd('C:/Users/luiz.graciano/Desktop/DEMANDA GIGAS - Thaiza')

dt = readxl::read_xlsx('Base de Correção da Amostra.xlsx') ##### Componente Específico --- Questões 3,4,5 ! - feito no excel

#### Tranformar não se aplica em 0 para fazer a soma automática para nota dos quesitos (para cada curso tem quesitos únicos)!

dt[dt == 'não se aplica'] = '0'
dt[dt == '-'] = '0'

dt = dt %>%
  mutate_at(colnames(dt)[7:19], as.numeric) %>%
  rename(Questão= Texto)

dt$Questão = str_sub(dt$Questão, end = 20,start = 20)


##

##### Boxplot

f_plot = function(nota){
  
  ggplot(as.data.frame(nota), aes(x = '', y = nota_candidato))+
    geom_boxplot(fill ='darkred', width = 0.5)+
    labs(x = '',y = 'Distribuição das notas')+
    theme_bw()+
    stat_summary(fun = "mean", fill = 'white', geom = 'point',shape = 23,size = 3, col = 'White')+
    theme(axis.title.y =element_text(colour='Black' , size=20),
          axis.title.x =element_text(colour='Black' , size=20),
          axis.text =element_text(colour='black',size=16),
          panel.border = element_blank(),
          axis.line =element_line(colour='black'))
  
}

#Gerar table quantil --> manual creation

create_table = function(data_curso){
  quantil_table = data.frame('Curso' = as.character(nome_curso),
                             'Mín' = round(min(nota_candidato),3),
                             'max' = round(max(nota_candidato),3),
                             '1 Quantil' = round(quantile(nota_candidato,0.25),3),
                             '2 Quantil' = round(quantile(nota_candidato,0.50),3),
                             '3 Quantil' = round(quantile(nota_candidato,0.75),3),
                             'Média' = round(mean(nota_candidato),3),
                             'Desvio Padrão' = round(sd(nota_candidato),3),
                             'Questões em branco' = sum(data_curso$SituacaoCorrecao == 'Não há texto',na.rm=TRUE))
  
  colnames(quantil_table) = c('Curso','Mínimo','Máximo','1ª Quantil','3ª Quantil','2ª Quantil','Média',
                              'Desvio Padrão','Questões em branco')
  
  rownames(quantil_table) = NULL
  
  return(quantil_table)
  
}

# Separar por CURSO

cursos = unique(dt$Curso)

for ( i in 1:length(cursos)){
  
  
  nome_curso = cursos[i]
  
  dt_curso = filter(dt, Curso == cursos[i])
  
  ####
  
  #### Gerar Distribuição de notas por candidatos ####
  
  #Cada candidato possui máscara única ###############
  
  nota_candidato = group_by(dt_curso, Mascara) %>% ## soma por cada máscara
    
    summarise(Nota = sum(Nota)) #temos a distribuição geral
  
  nota_candidato = (nota_candidato$Nota)/3
  
  ################# Distribuição geral ###############
  
  setwd('C:/Users/luiz.graciano/Desktop/DEMANDA GIGAS - Thaiza')
  
  ### Criar Pasta ###
  
  dir.create(paste('./',cursos[i], sep = ''))
  
  ## SET
  
  setwd(paste('./',cursos[i], sep = ''))
  
  ##
  
  #table
  
  create_table(dt_curso)
  
  write_xlsx(create_table(dt_curso),paste('Tabela dos quantis das notas gerais do curso de ',cursos[i],'.xlsx',sep = ''), col_names = T)
  
  
  #plot
  
  ggsave(filename = paste('Boxplot da distribuição das notas gerais do curso de ',cursos[i],'.png',sep = ''),
         width=12, height=8, plot = f_plot(nota_candidato))

  
  
  ##### Questão 3
  
  dt_curso_q3 = filter(dt_curso, Questão == '3')
  
  nota_candidato = dt_curso_q3$Nota
  
  #table
  
  create_table(dt_curso_q3)
  
  write_xlsx(create_table(dt_curso_q3),paste('Tabela dos quantis das notas da Questão 3 do curso de ',cursos[i],'.xlsx',sep = ''), col_names = T)
  
  #plot
  
  ggsave(filename = paste('Boxplot da distribuição das notas da Questão 3 do curso de ',cursos[i],'.png',sep = ''),
         width=12, height=8, plot = f_plot(nota_candidato))
  
  
  ##### Questão 4
  
  dt_curso_q4 = filter(dt_curso, Questão == '4')
  
  nota_candidato = dt_curso_q4$Nota
  
  #table
  
  create_table(dt_curso_q4)
  
  write_xlsx(create_table(dt_curso_q4),paste('Tabela dos quantis das notas da Questão 4 do curso de ',cursos[i],'.xlsx',sep = ''), col_names = T)
  
  #plot
  ggsave(filename = paste('Boxplot da distribuição das notas da Questão 4 do curso de ',cursos[i],'.png',sep = ''),
         width=12, height=8, plot = f_plot(nota_candidato))
  
  ##### Questão 5
  
  dt_curso_q5 = filter(dt_curso, Questão == '5')
  
  nota_candidato = dt_curso_q5$Nota
  
  #table
  
  create_table(dt_curso_q5)
  
  write_xlsx(create_table(dt_curso_q5),paste('Tabela dos quantis das notas da Questão 5 do curso de ',cursos[i],'.xlsx',sep = ''), col_names = T)
  
  #plot
  
  ggsave(filename = paste('Boxplot da distribuição das notas da Questão 5 do curso de ',cursos[i],'.png',sep = ''),
         width=12, height=8, plot = f_plot(nota_candidato))
  
}
