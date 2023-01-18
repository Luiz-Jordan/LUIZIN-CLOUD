#### Documento 1v9:

################## https://www.youtube.com/watch?v=ZAVHbDB5yBQ&ab_channel=LearningSoftware

import os

os.chdir('C:/Users/luiz.graciano/Desktop/Desafio - Gustavo')

#Manusear os DADOS

import pandas as pd

path = 'C:/Users/luiz.graciano/Desktop/Desafio - Gustavo/NOMES BANCA.xlsx'

dt = pd.read_excel(path) #base de dados

nomes = dt.NOME #gerar lista
cpf = dt.CPF

#print(nomes)
#print(cpf)

#print(nomes[0])
#print(cpf[0])

################

from docxtpl import DocxTemplate

from docx2pdf import convert

##salvar arquivos##

doc = DocxTemplate('Declaração - INSS_22 - Heteroidentificação.docx')

for i in range (len(nomes)):

    nome = nomes[i].replace('  ',' ')

    context = {  ## padrões do word, devem esta por {{ x }}
    'nome':nome,
    'cpf':cpf[i]
    }


    doc.render(context)

    os.chdir('C:/Users/luiz.graciano/Desktop/Desafio - Gustavo/declarações')

    doc.save(f'{nome}.docx')

    convert(f'{nome}.docx',f'{nome}.pdf')  #tranformar em pdf sem perder formatação -- Demora pra um caralho - quem fez, fez merda.

    os.chdir('C:/Users/luiz.graciano/Desktop/Desafio - Gustavo')
