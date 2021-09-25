setwd('C:/Users/GUSTAVO/Desktop/trab-demo')

pacman::p_load(read.dbc,dplyr,stringr,foreign,ggplot2,lubridate,RColorBrewer,LexisPlotR)
install.packages('ggplot2')
library(ggplot2)
#preparando os dados->
#dados sinasc ->

teste2000 = read.dbc('./SINASC 2000-2020/DNAP2000.dbc') 
teste2020 = read.dbc('./SINASC 2000-2020/DNAP2020.dbc') #há municipios indesejados.
rm(teste2000)
rm(teste2020)

sinasc = paste('./SINASC 2000-2020/',list.files('./SINASC 2000-2020/', pattern = "*.dbc"), sep = "")

View(sinasc)


sinasc_final = read.dbc(sinasc[1])


for (i in c(2:length(sinasc))){
  data_save = read.dbc(sinasc[i])
  sinasc_final = dplyr::full_join(sinasc_final, data_save)
  
}


#dados sim ->

teste_sim = read.dbc('./SIm 2000-2020/DOAP2000.dbc')
  
sim = paste('./SIM 2000-2020/',list.files('./SIM 2000-2020/', pattern = "*.dbc"), sep = "")


sim_final = read.dbc(sim[1])

for (i in c(2:length(sinasc))){# 2 dado q o 1 está acoplado no sim[1]
  data_save = read.dbc(sim[i])
  sim_final = dplyr::full_join(sim_final, data_save)
  
}

#Excluindo registros de outros municípios -> UF - 16
#SINASC->
sinasc_final$CODMUNRES = as.character(sinasc_final$CODMUNRES)
sinasc_final$CODMUNRES = str_sub(sinasc_final$CODMUNRES, start = 0,end = 2)

sinasc_final = dplyr::filter(sinasc_final,sinasc_final$CODMUNRES == '16')
#os primeiros 2 digitos sao a uf e queremos a uf de 16!

#SIM->
sim_final$CODMUNRES = as.numeric(as.character(sim_final$CODMUNRES))
sim_final$CODMUNRES = str_sub(sim_final$CODMUNRES, start = 0,end = 2)

sim_final = dplyr::filter(sim_final,sim_final$CODMUNRES == '16')
                          

#QUESTÃO 1 ->

#separando as variaveis que iremos usar -> (BASEADO NO ARQUIVO DO MONITOR LEXIS.HTML)

DATA_SIM = select(sim_final,c(DTOBITO,DTNASC,IDADE))
DATA_SINASC = select(sinasc_final,c(DTNASC))

rm(sim_final) #apagar da memória
rm(sinasc_final)

#A)Queremos o lexis com os dados do SIM ->

plot_lexis = LexisPlotR::lexis_grid(year_start=2000,year_end=2020,age_start=0,age_end=5,delta=1)
plot_lexis

DATA_SIM = mutate(DATA_SIM,DTOBITO = dmy(DTOBITO),DTNASC = dmy(DTNASC))

DATA_SIM$IDADE = as.numeric(as.character(DATA_SIM$IDADE))-400

DATA_SIM$IDADE[DATA_SIM$IDADE < 0] <- 0 

DATA_SIM$IDADE[DATA_SIM$IDADE > 120] <- NA 

#ORGANIZANDO PARA PLOTAR ->

DATA_SIM$DTOBITO = as.Date(DATA_SIM$DTOBITO)

DATA_SIM$DTNASC = as.Date(DATA_SIM$DTNASC)

DATA_SIM$ANONASC = year(DATA_SIM$DTNASC)

DATA_SIM$ANOOBITO = year(DATA_SIM$DTOBITO)

DATA_SIM$ANONASC = as.numeric(DATA_SIM$ANONASC)

DATA_SIM$ANOOBITO = as.numeric(DATA_SIM$ANOOBITO)

#(adaptação do codigo do monitor) [[]]

dados2 <- DATA_SIM %>% #queremos para até 5 anos com inico em 2000
  filter (ANONASC > 1999)

dados2$comparativo <- ymd(paste(dados2$ANONASC,"-06-01"))
dados2$TRI <- ifelse(dados2$DTNASC <= dados2$comparativo,"0","1")          
dados2$TRI <- as.numeric(dados2$TRI)

dados2 = filter(dados2,dados2$IDADE < 21) # tinha cara q nasceu em 2000 com 90 anos xd



as_tibble(dados2)


dados2 <- dados2 %>%
  mutate(
    IDOBITO= as.numeric(difftime(DTOBITO, DTNASC, units = "days")/365),
    ano_nasc=year(DTNASC),
    ano_obito=year(DTOBITO)) %>%
  group_by(ano_nasc, ano_obito, TRI) %>%
  mutate(quantidade=n()) %>%
  ungroup() %>% #?
  distinct(ano_nasc, ano_obito, TRI, .keep_all = T) %>%
  select(ano_nasc,ano_obito, quantidade, TRI) %>%
  arrange(ano_obito, desc(ano_nasc))

dados2$id_ob_anos_comp <- (dados2$ano_obito - dados2$ano_nasc)


plot_lexis = plot_lexis +
  annotate(geom="text", x=as.Date(paste0((dados2$ano_obito)[dados2$TRI==0]
                                         ,"-09-06"))
           ,y=dados2$id_ob_anos_comp[dados2$TRI==0]+0.3,
           label=c(paste0(dados2$quantidade[dados2$TRI==0])),
           color='black')+
  annotate(geom="text", x=as.Date(paste0((dados2$ano_obito + 1)[dados2$TRI==1]
                                         ,"-05-06"))
           ,y=dados2$id_ob_anos_comp[dados2$TRI==1]+0.75,
           label=c(paste0(dados2$quantidade[dados2$TRI==1])),
           color='black')

plot_lexis = plot_lexis + ggtitle("Diagrama de Lexis") +
  xlab("Ano (cortes)") + ylab("Idade")

plot_lexis
