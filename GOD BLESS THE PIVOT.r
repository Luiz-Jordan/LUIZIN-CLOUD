setwd('C:/Users/GUSTAVO/Desktop/trab-demo')

pacman::p_load(read.dbc,viridis,dplyr,zoo,readxl,stringr,foreign,ggplot2,lubridate,RColorBrewer,LexisPlotR)

#DATA_BASE
#QUEREMOS PARA OS ANOS DE 2018-2020


sim = paste('./SIM 2000-2020/',list.files('./SIM 2000-2020/', pattern = "*.dbc"), sep = "")


sim_final = read.dbc(sim[19])

for (i in c(20:21)){
  data_save = read.dbc(sim[i])
  sim_final = dplyr::full_join(sim_final, data_save)
  
}



sim_final$CODMUNRES = as.numeric(as.character(sim_final$CODMUNRES))
sim_final$CODMUNRES = str_sub(sim_final$CODMUNRES, start = 0,end = 2)

sim_final = dplyr::filter(sim_final,sim_final$CODMUNRES == '16')


sim_final = sim_final%>%
  mutate(estado = str_sub(CODMUNRES, end = 2))%>%
  filter(estado == "16")

sinasc = paste('./SINASC 2000-2020/',list.files('./SINASC 2000-2020/', pattern = "*.dbc"), sep = "")


sinasc_final = read.dbc(sinasc[19])


for (i in c(20:21)){
  data_save = read.dbc(sinasc[i])
  sinasc_final = dplyr::full_join(sinasc_final, data_save)
  
}

sinasc_final$CODMUNRES = as.character(sinasc_final$CODMUNRES)
sinasc_final$CODMUNRES = str_sub(sinasc_final$CODMUNRES, start = 0,end = 2)

sinasc_final = dplyr::filter(sinasc_final,sinasc_final$CODMUNRES == '16')



dados_nasc = sinasc_final


#########################COPY SINISTRO###############################################
obitos2018_2020 = sim_final %>%
  filter(TIPOBITO == 2)%>%
  select(DTOBITO, CODMUNRES) %>% #seleciona data de obito
  mutate(DTOBITO = as.character(DTOBITO),
         ano = str_sub(DTOBITO, start = 5, end = 8))%>% #retira apenas ano da data
  group_by(ano)%>%
  summarise(quant = n())

#INDICADORES! POP AMAPA 2019 - 873000
TBM = (mean(obitos2018_2020$quant)/873000)*1000

TBM_SEX






faixas_et <- read_xls("./projecoes_2018_populacao_2010_2060_20200406.xls", sheet = "AP",
                      range = "AP!A5:A25") %>%
  pull()

M_2019 <- read_xls("./projecoes_2018_populacao_2010_2060_20200406.xls", sheet = "AP",
                   range = "AP!J5:L25")%>%
  rowSums()/3

F_2019 <- read_xls("./projecoes_2018_populacao_2010_2060_20200406.xls", sheet = "AP",
                   range = "AP!J28:L48")%>%
  rowSums()/3

pop_ac_sx_etaria = tibble(faixas_et, M_2019, F_2019)[-1,]


dados_sim = sim_final


# Populacao centrada em 2019 pelo IBGE
pop_2019_m <- read_xls("./projecoes_2018_populacao_2010_2060_20200406.xls", sheet = "AP",
                       range = "AP!J5:L6") %>%
  rowSums()/3

pop_2019_f <- read_xls("./projecoes_2018_populacao_2010_2060_20200406.xls", sheet = "AP",
                       range = "AP!J28:L29") %>%
  rowSums()/3

pop_2019 <- pop_2019_m + pop_2019_f

################################################################################################################################
faixas_quinq <- pop_ac_sx_etaria %>%
  select(faixas_et)%>%
  filter(!is.na(faixas_et) & faixas_et!= " ")%>%
  pull()

faixas_quinq <- c("0-1", "1-4", faixas_quinq[-1]) #adiciona faixas iniciais

obit_sx_et <- dados_sim %>%
  mutate(DTOBITO = as.character(DTOBITO),
         ano = str_sub(DTOBITO, start = 5, end = 8))%>% #retira apenas ano da data
  filter(ano == 2019)%>%
  filter(TIPOBITO == 2)%>%
  select(SEXO, IDADE)%>% #puxa apenas essas 2 variaveis
  mutate(
    #ajusta a variavel sexo com label
    SEXO = factor(SEXO, levels = c(0, 1, 2), labels = c('I', 'Masc', 'Fem')),
    #ajustndo a variavel idade
    IDADE = as.character(IDADE),
    IDADE = case_when(
      as.numeric(IDADE) <= 400 ~ "0",
      as.numeric(IDADE) > 400 & as.numeric(IDADE) < 500 ~ str_sub(as.character(IDADE), start = 2),
      as.numeric(IDADE) >= 500 & as.numeric(IDADE) < 515 ~ str_c("1", str_sub(as.character(IDADE), start = 2), sep = "")
    ),
    IDADE = as.numeric(IDADE),
    faixa_et = cut(IDADE, breaks = c(0,1,seq(from = 5, to = 90, by = 5), 115),
                   right = FALSE, include.lowest = TRUE, labels = faixas_quinq)
  ) %>%
  group_by(faixa_et, SEXO) %>%
  summarise(obitos = n())


obit_wide <- obit_sx_et %>% #transforma a ultima tabela em formato wide
  tidyr::pivot_wider(names_from = SEXO, values_from = obitos)%>%
  rename("obit_masc" = Masc, "obit_fem" = Fem)%>%
  filter(!is.na(faixa_et))




#projeção da população inicial, obitos subtraídos ano a ano

zero_1_2019 <- dados_nasc %>%
  select(SEXO, DTNASC)%>%
  mutate(ano = str_sub(DTNASC, start = -4, end = -1))%>%
  filter(ano %in% c("2018", "2019", "2020"))%>%
  group_by(SEXO, ano) %>%
  summarise(n = n())%>%
  group_by(SEXO)%>%
  summarise(media = mean(n))%>%
  filter(SEXO != "0")%>%
  mutate(SEXO = factor(SEXO, labels = c("M_2019", "F_2019")),
         faixas_et = "0-1")%>%
  tidyr::pivot_wider(names_from = SEXO, values_from = media)


pop_ac_sx_etaria <- bind_rows(zero_1_2019, pop_ac_sx_etaria)

pop_ac_sx_etaria[2,] <- c("1-4", pop_ac_sx_etaria[2,-1] - pop_ac_sx_etaria[1,-1])


pop_quinq <- pop_ac_sx_etaria%>%
  select(M_2019, F_2019)%>%
  mutate(faixa_et = faixas_quinq) %>%
  rename("pop_masc" = M_2019, "pop_fem" = F_2019)



nMx <- obit_wide %>%
  filter(!is.na(faixa_et))%>% #removendo a linha com faixa etária NA
  bind_cols(pop_quinq, .name_repair = "universal") %>% #juntando as colunas das 2 tabelas
  select(-c(I, faixa_et...1))%>% #removendo 2 variaveis indesejadas
  rename('faixa_et' = faixa_et...7)%>% #dando nome adequado a faixa etaria
  mutate(nMx_masc = (obit_masc/pop_masc),
         nMx_fem = (obit_fem/pop_fem),
         faixa_et = factor(faixa_et,
                           levels = c("0-1","1-4","5-9","10-14","15-19","20-24","25-29","30-34",
                                      "35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74",
                                      "75-79","80-84","85-89","90+"))
  )


nMx %>%
  select(faixa_et, nMx_masc, nMx_fem)%>%
  tidyr::pivot_longer(cols = -faixa_et, names_to = "Sexo", values_to = "nMx")%>%
  mutate(Sexo = if_else(Sexo == "nMx_masc", "Masculino", "Feminino"))%>%
  ggplot(aes(x = faixa_et, y = nMx, group = Sexo, colour = Sexo))+
  geom_line(size = 1)+
  scale_color_manual(values = c("blue", "black"))+
  scale_y_log10()+
  xlab("Idade em faixas etárias")+
  ylab('NMX')+
  ggtitle('Taxas específicas de mortalidade por sexo e idade')

rm(pop_2019)
rm(pop_quinq)
rm(obit_sx_et)
rm(obit_wide)
rm(zero_1_2019)
rm(pop_ac_sx_etaria)

#########################################################################################################



sim_consolidado$ANO_OBITO <- sim_consolidado$DTOBITO %>% str_extract('(?<=[0-9]{4})[0-9]{4}')
sinasc_consolidado$ANO_NASC <- sinasc_consolidado$DTNASC %>% str_extract('(?<=[0-9]{4})[0-9]{4}')




sim_b <- sim_consolidado %>% filter(ANO_OBITO >=2018  & ANO_OBITO<=2020)

nascimentos_19 <- sinasc_consolidado %>% filter(ANO_NASC == 2019) %>% nrow()

# Menor que 1 ano de idade
mortes_infantil_18_20 <- filter(sim_b,as.numeric(as.character(sim_b$IDADE))<= 400) %>%  nrow()
TMI <- (mortes_infantil_18_20/3)/nascimentos_19

# Menor que 28 dias de idade
mortes_neotal_18_20 <- filter(sim_b,as.numeric(as.character(sim_b$IDADE))<= 227) %>% nrow()
tx_mort_neotal <- (mortes_neotal_18_20/3)/nascimentos_19

# Menor que 1 semana de vida
mortes_neotal_prec_18_20 <- filter(sim_b,as.numeric(as.character(sim_b$IDADE))<= 207) %>% nrow()
tx_mort_neotal_precoce <- (mortes_neotal_prec_18_20/3)/nascimentos_19

# 1 semana a 1 mes de vida
mortes_neotal_tard_18_20 <- filter(sim_b,as.numeric(as.character(sim_b$IDADE))<= 227 & 
                                     as.numeric(as.character(sim_b$IDADE))> 207) %>% nrow()
tx_mort_neotal_tardia <- (mortes_neotal_tard_18_20/3)/nascimentos_19

# 1 mes a 1 ano de vida
mortes_posneotal_18_20 <- filter(sim_b,as.numeric(as.character(sim_b$IDADE))<= 400 & 
                                   as.numeric(as.character(sim_b$IDADE))>= 301) %>% nrow()
tx_mort_posneotal <- (mortes_posneotal_18_20/3)/nascimentos_19

# de 22 semanas de gestação a 7 dias de vida

mortes_neotal_perinatal <- filter(sim_b,as.numeric(as.character(sim_b$IDADE))<= 207) %>% nrow()
mortes_neotal_perinatal <- mortes_neotal_perinatal + filter(sim_b,as.numeric(as.character(sim_b$SEMAGESTAC))>= 22) %>% nrow()

tx_mort_neotal_perinatal <- (mortes_neotal_perinatal/3)/nascimentos_19

# antes do nascimento
obitos_fetais <- filter(sim_b,as.numeric(as.character(sim_b$TIPOBITO))== 1) %>% nrow()

tx_mort_obitos_fetais <- (obitos_fetais/3)/nascimentos_19




tabela3b <- data.frame('Dados' = "AMAPÁ 2018-2020",
                       'TMI (infantil)' = TMI*1000,
                       'Neonatal' = tx_mort_neotal*1000,
                       'Neonatal Precoce' = tx_mort_neotal_precoce*1000,
                       'Neonatal Tardia' = tx_mort_neotal_tardia*1000,
                       'Pós Neonatal' = tx_mort_posneotal*1000,
                       'Neonatal Perinatal' = tx_mort_neotal_perinatal*1000,
                       'Obitos Fetais' = tx_mort_obitos_fetais*1000)

colnames(tabela3b) <- c('Dados', 'Infantil','Neonatal', 'Neonatal Precoce', 'Neonatal Tardia', 'Pós Neonatal', 'Neonatal Perinatal', 'Obitos Fetais')

tabela3b$`Obitos Fetais` = 2.862

#plota o q dé ai fion
tabela_Ripsa = data.frame(c('TMI','','','',''))
