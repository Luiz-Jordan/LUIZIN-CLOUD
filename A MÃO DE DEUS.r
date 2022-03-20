pacman::p_load(tidyr,tidyverse,dplyr,stringr,writexl,rvest,ggplot2,pdftools,tm,officer)

setwd('//arquivos/Eventos/PRF_21/Fases_Especificas/10_Avaliacao_psicologica/Cris/PF_21 Complementar/LAUDO E RESULTADO FINAL')


nomes = list.files('//arquivos/Eventos/PRF_21/Fases_Especificas/10_Avaliacao_psicologica/Cris/PF_21 Complementar/LAUDO E RESULTADO FINAL')

#paste('./',nomes[2],'/Resultado final.docx',sep = '')

Data_geral = data.frame()
D_ins = data.frame()

#####################################################


#############################################################
nome[253]

for(i in 250:length(nomes)){#length(nomes)
  data_1 = read_docx(paste('./',nomes[i],'/LAUDO SÍNTESE.docx',sep = ''))
  
  data_1 = docx_summary(data_1)
  
  tabela = data_1 %>% 
    filter(content_type == "table cell")
  
  table_data1 <- tabela %>% filter(!is_header) %>% select(row_id, cell_id, text)
  
  
  coluna_texto = table_data1[,c('text')]
  
  coluna_texto = data.frame(coluna_texto)
  
  cont_obs = 40
  
  if (coluna_texto[40,] == 'RESULTADOS OBTIDOS'){
    cont_obs = 41
  }
  
  
  dinam_grupo = coluna_texto[cont_obs:(cont_obs+14),]
  cont_obs = cont_obs +25
  
  observação = coluna_texto[cont_obs:(cont_obs+7),]
  ######################### Resultado Final #########################################
  
  data_2 = read_docx(paste('./',nomes[i],'/Resultado final.docx',sep = ''))
  
  data_2 = docx_summary(data_2)
  
  tabela = data_2 %>% 
    filter(content_type == "table cell")
  
  table_data2 <- tabela %>% filter(!is_header) %>% select(row_id, cell_id, text)
  
  coluna_texto2 = table_data2[,c('text')]
  
  coluna_texto2 = data.frame(coluna_texto2)
  
  cont_x = 15
  
  if (coluna_texto2[15,] == 'Decisão'){
    cont_x = cont_x +1
  }
  
  Res_final = coluna_texto2[cont_x:(cont_x+11),]
  
  #############################################################################################
  ################# INSCRIÇÃO
  
  #data_2 = read_docx(paste('./',nomes[i],'/LAUDO SÍNTESE.docx',sep = ''))
  
  #data_2 = docx_summary(data_2)
  
  data_nome = data_1 %>% filter(content_type == "paragraph")
  
  data_nome = data_nome$text
  
  data_nome = data_nome[data_nome != ""]
  
  nome_ins = data_nome[5]
  
  func_x = str_split(nome_ins[1], ' ')
  
  ins = nome_ins[1]
  
  ins
  if (func_x[[1]][1] == 'Nome:'){
    ins = data_nome[6]
  }
  
  ins = sub(".*:", "", ins)
  
  
  #######tranformação para print
  dinam_grupo = data.frame(dinam_grupo)
  observação = data.frame(observação)
  Res_final  = data.frame(Res_final)
  
  colnames(dinam_grupo) = c('Teste')
  colnames(observação) = c('Teste')
  colnames(Res_final) = c('Teste')
  
  
  
  data_print = data.frame(t(data.frame(rbind(dinam_grupo,observação,Res_final))))
  
  Data_geral = bind_rows(Data_geral,data_print)
  
  Dt_inscrição = data.frame(ins)
  
  D_ins = bind_rows(D_ins,Dt_inscrição)
}

Dt_ins = as.data.frame(D_ins)
Dt_ins = str_sub(Dt_ins$ins, start = -8)

DATA_PRINT_GERAL_GOD_CODE = Data_geral %>%
  mutate('Nome' = nomes[250:length(nomes)],
         'Inscrição' = Dt_ins)

#####nomes[250]

setwd('C:/Users/luiz.graciano/Desktop')
write_xlsx(DATA_PRINT_GERAL_GOD_CODE,'Planilha 1v9.xlsx')


