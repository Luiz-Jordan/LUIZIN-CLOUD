import datetime

tarefa = int(input())
total = int(input())
alunos = [input().split() for i in range(total)]

domingo, segunda, terca, quarta, quinta, sexta, sabado = 0, 0, 0, 0, 0, 0, 0
hdomingo, hsegunda, hterca, hquarta, hquinta, hsexta, hsabado = 0, 0, 0, 0, 0, 0, 0

regulares = 0
masculino = 0
masculino_e_regular = 0
tempo = 0
tempo_regular = 0
tempo_nregular = 0



def dia(ano, mes, day):
    ano = int(ano) + 2000
    mes = int(mes)
    day = int(day)
    data = datetime.date(ano, mes, day)
    return data.weekday()
    #monday == 0

# no me abra
def el_atrocidad(maldicion):
    global segunda
    global terca
    global quarta
    global quinta
    global sexta
    global sabado
    global domingo

    if maldicion == 0:
        segunda = segunda + 1
    elif maldicion == 1:
        terca = 1 + terca
    elif maldicion == 2:
        quarta = 1 + quarta
    elif maldicion == 3:
        quinta = 1 +quinta
    elif maldicion == 4:
        sexta = 1 + sexta
    elif maldicion == 5:
        sabado = 1 + sabado
    elif maldicion == 6:
        domingo = 1 + domingo
def la_abominacion(el_fim_de_los_tiempos):
    global hsegunda
    global hterca
    global hquarta
    global hquinta
    global hsexta
    global hsabado
    global hdomingo

    if el_fim_de_los_tiempos == 0:
        hsegunda = 1 +hsegunda
    elif el_fim_de_los_tiempos == 1:
        hterca = 1 + hterca
    elif el_fim_de_los_tiempos == 2:
        hquarta = 1 + hquarta
    elif el_fim_de_los_tiempos == 3:
        hquinta = 1 + hquinta
    elif el_fim_de_los_tiempos == 4:
        hsexta = 1 + hsexta
    elif el_fim_de_los_tiempos == 5:
        hsabado = 1 + hsabado
    elif el_fim_de_los_tiempos == 6:
        hdomingo = 1 + hdomingo


for aluno in alunos:
    weekday = dia(aluno[3], aluno[2], aluno[1])

    if aluno[5] == "2" or aluno[5] == "6":
        regulares += 1
        tempo_regular += 19 - int(aluno[0])
    else:

        tempo_nregular += 19 - int(aluno[0])
    if aluno[4] == "2":
        masculino += 1

    if aluno[4] == "2" and aluno[5] == "2" or aluno[5] == "6":
        masculino_e_regular += 1
    
    if aluno[4] == "2":
        la_abominacion(weekday)
    el_atrocidad(weekday)



    
    tempo += 19 - int(aluno[0])

tempo = tempo / total
tempo_nregular = tempo_nregular / (total - regulares)
tempo_regular = tempo_regular / regulares

if tarefa == 1:
    print(f"matriculados ou formados:{regulares / total * 100:5.1f}")
    print(f"alunos em outras situacoes:{(total - regulares) / total * 100:5.1f}")


if tarefa == 2:
    print(f"sexo masculino:{masculino / total * 100:5.1f}")
    print(f"sexo feminino:{(total - masculino ) / total * 100:5.1f}")


if tarefa == 3:
    print(f"media de anos desde ingresso:{tempo + 2000:5.1f}")


if tarefa == 4:
    
    print(f"domingo:{(domingo / total * 100):5.1f}")
    print(f"segunda:{(segunda / total * 100):5.1f}")
    print(f"terca:{(terca / total * 100):5.1f}")
    print(f"quarta:{(quarta / total * 100):5.1f}")
    print(f"quinta:{(quinta / total * 100):5.1f}")
    print(f"sexta:{(sexta / total * 100):5.1f}")
    print(f"sabado:{(sabado / total * 100):5.1f}")


if tarefa == 5:

    print(f"dentre masculinos:")
    print(f"matriculados ou formados:{masculino_e_regular / masculino * 100:5.1f}")
    print(f"alunos em outras situacoes:{(masculino - masculino_e_regular) / masculino * 100:5.1f}")

    print(f"dentre femininos:")
    print(f"matriculados ou formados:{(regulares - masculino_e_regular) / (total - masculino) * 100:5.1f}")
    print(f"alunos em outras situacoes:{((total - masculino) - (regulares - masculino_e_regular)) / (total - masculino) * 100:5.1f}")

if tarefa == 6:
    print(f"entre matriculados ou formados:")
    print(f"media de anos desde ingresso:{tempo_regular:5.2f}")
    print(f"dentre alunos em outras situacoes:")
    print(f"media de anos desde ingresso:{tempo_nregular:5.2f}")

if tarefa == 7:
    print("dentre masculinos:")
    print(f"domingo:{(hdomingo / total * 100):5.1f}")
    print(f"segunda:{(hsegunda / total * 100):5.1f}")
    print(f"terca:{(hterca / total * 100):5.1f}")
    print(f"quarta:{(hquarta / total * 100):5.1f}")
    print(f"quinta:{(hquinta / total * 100):5.1f}")
    print(f"sexta:{(hsexta / total * 100):5.1f}")
    print(f"sabado:{(hsabado / total * 100):5.1f}")

    print("dentre femininos:")
    print(f"domingo:{((domingo - hdomingo) / total * 100):5.1f}")
    print(f"segunda:{((segunda -hsegunda) / total * 100):5.1f}")
    print(f"terca:{((terca - hterca) / total * 100):5.1f}")
    print(f"quarta:{((quarta -hquarta) / total * 100):5.1f}")
    print(f"quinta:{((quinta - hquinta) / total * 100):5.1f}")
    print(f"sexta:{((sexta - hsexta) / total * 100):5.1f}")
    print(f"sabado:{((sabado -hsabado) / total * 100):5.1f}")