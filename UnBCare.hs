module UnBCare where

import ModeloDados

{-
██╗░░░██╗███╗░░██╗██████╗░  ░█████╗░░█████╗░██████╗░██████╗
██║░░░██║████╗░██║██╔══██╗  ██╔══██╗██╔══██╗██╔══██╗██╔════╝
██║░░░██║██╔██╗██║██████╦╝  ██║░░╚═╝███████║██████╔╝█████╗░░
██║░░░██║██║╚████║██╔══██╗  ██║░░██╗██╔══██║██╔══██╗██╔══╝░░
╚██████╔╝██║░╚███║██████╦╝  ╚█████╔╝██║░░██║██║░░██║███████╗
░╚═════╝░╚═╝░░╚══╝╚═════╝░  ░╚════╝░╚═╝░░╚═╝╚═╝░░╚═╝╚══════╝
O objetivo desse trabalho é fornecer apoio ao gerenciamento de cuidados a serem prestados a um paciente.
O paciente tem um receituario médico, que indica os medicamentos a serem tomados com seus respectivos horários durante
um dia.
Esse receituário é organizado em um plano de medicamentos que estabelece, por horário, quais são os remédios a serem
tomados. Cada medicamento tem um nome e uma quantidade de comprimidos que deve ser ministrada.
Um cuidador de plantão é responsável por ministrar os cuidados ao paciente, seja ministrar medicamento, seja comprar
medicamento.
Eventualmente, o cuidador precisará comprar medicamentos para cumprir o plano.
O modelo de dados do problema (definições de tipo) está disponível no arquivo ModeloDados.hs
Defina funções que simulem o comportamento descrito acima e que estejam de acordo com o referido modelo de dados.
-}

{-
  QUESTÃO 1, VALOR: 1,0 ponto
Defina a função "comprarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento, uma quantidade e um
estoque inicial de medicamentos, retorne um novo estoque de medicamentos contendo o medicamento adicionado da referida
quantidade. Se o medicamento já existir na lista de medicamentos, então a sua quantidade deve ser atualizada no novo
estoque.
Caso o remédio ainda não exista no estoque, o novo estoque a ser retornado deve ter o remédio e sua quantidade como
cabeça.
-}

comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento med quant [] = [(med, quant)]
comprarMedicamento med quant ((nome, disponivel) : resto)
  | med == nome = (nome, quant + disponivel) : resto
  | otherwise = (nome, disponivel) : comprarMedicamento med quant resto

{-
  QUESTÃO 2, VALOR: 1,0 ponto
Defina a função "tomarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de
medicamentos, retorna um novo estoque de medicamentos, resultante de 1 comprimido do medicamento ser ministrado ao
paciente.
Se o medicamento não existir no estoque, Nothing deve ser retornado. Caso contrário, deve se retornar Just v, onde v é o
novo estoque.
-}

tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento _ [] = Nothing
tomarMedicamento med ((nome, disponivel) : resto)
  | med == nome = Just ((nome, disponivel - 1) : resto)
  | otherwise = fmap (:) (Just (nome, disponivel)) <*> tomarMedicamento med resto

{-
  QUESTÃO 3  VALOR: 1,0 ponto
Defina a função "consultarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de
medicamentos, retorne a quantidade desse medicamento no estoque.
Se o medicamento não existir, retorne 0.
-}

consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento _ [] = 0
consultarMedicamento med ((nome, disponivel) : resto)
  | med == nome = disponivel
  | otherwise = consultarMedicamento med resto

{-
  QUESTÃO 4  VALOR: 1,0 ponto
Defina a função "demandaMedicamentos", cujo tipo é dado abaixo e que computa a demanda de todos os medicamentos por um
dia a partir do receituario. O retorno é do tipo EstoqueMedicamentos e deve ser ordenado lexicograficamente pelo nome do
medicamento.
Dica: Observe que o receituario lista cada remédio e os horários em que ele deve ser tomado no dia. Assim, a demanda de
cada remédio já está latente no receituario, bastando contar a quantidade de vezes que cada remédio é tomado.
-}

demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos [] = []
demandaMedicamentos ((med, horarios) : resto) = (med, length horarios) : demandaMedicamentos resto

{-
  QUESTÃO 5  VALOR: 1,0 ponto, sendo 0,5 para cada função.
Um receituário é válido se, e somente se, todo os medicamentos são distintos e estão ordenados lexicograficamente e,
para cada medicamento, seus horários também estão ordenados e são distintos.
Inversamente, um plano de medicamentos é válido se, e somente se, todos seus horários também estão ordenados e são
distintos, e para cada horário, os medicamentos são distintos e são ordenados lexicograficamente.
Defina as funções "receituarioValido" e "planoValido" que verifiquem as propriedades acima e cujos tipos são dados
abaixo:
-}

horariosValidos :: [Horario] -> Bool
horariosValidos [_] = True
horariosValidos (horario1 : horario2 : resto)
  | horario1 < horario2 = horariosValidos (horario2 : resto)
  | otherwise = False

receituarioValido :: Receituario -> Bool
receituarioValido [(_, horarios)]
  | horariosValidos horarios = True
  | otherwise = False
receituarioValido ((nome1, horarios1) : (nome2, horarios2) : resto)
  | (nome1 < nome2) && horariosValidos horarios1 = receituarioValido ((nome2, horarios2) : resto)
  | otherwise = False

medicamentosValidos :: [Medicamento] -> Bool
medicamentosValidos [_] = True
medicamentosValidos (med1 : med2 : resto)
  | med1 < med2 = medicamentosValidos (med2 : resto)
  | otherwise = False

planoValido :: PlanoMedicamento -> Bool
planoValido [(_, meds)]
  | medicamentosValidos meds = True
  | otherwise = False
planoValido ((horario1, nomes1) : (horario2, nomes2) : resto)
  | (horario1 < horario2) && medicamentosValidos nomes1 = planoValido ((horario2, nomes2) : resto)
  | otherwise = False

{-
  QUESTÃO 6  VALOR: 1,0 ponto,
Um plantão é válido se, e somente se, todas as seguintes condições são satisfeitas:
1. Os horários da lista são distintos e estão em ordem crescente;
2. Não há, em um mesmo horário, ocorrência de compra e medicagem de um mesmo medicamento
(e.g. `[Comprar m1, Medicar m1 x]`);
3. Para cada horário, as ocorrências de Medicar estão ordenadas lexicograficamente.
Defina a função "plantaoValido" que verifica as propriedades acima e cujo tipo é dado abaixo:
-}

validaHorariosDePlantao :: Plantao -> Bool
validaHorariosDePlantao [_] = True
validaHorariosDePlantao ((horario1, _) : (horario2, cuidado2) : resto)
  | horario1 < horario2 = validaHorariosDePlantao ((horario2, cuidado2) : resto)
  | otherwise = False

comparaDoisCuidados :: Cuidado -> Cuidado -> Bool
comparaDoisCuidados (Comprar medicamento1 _) (Comprar medicamento2 _)
  | medicamento1 /= medicamento2 = True
  | otherwise = False
comparaDoisCuidados (Comprar medicamento1 _) (Medicar medicamento2)
  | medicamento1 /= medicamento2 = True
  | otherwise = False
comparaDoisCuidados (Medicar medicamento1) (Comprar medicamento2 _)
  | medicamento1 /= medicamento2 = True
  | otherwise = False
comparaDoisCuidados (Medicar medicamento1) (Medicar medicamento2)
  | medicamento1 < medicamento2 = True
  | otherwise = False

validaUmCuidadoPorHorario :: Cuidado -> [Cuidado] -> Bool
validaUmCuidadoPorHorario _ [] = True
validaUmCuidadoPorHorario cuidado1 (cuidado2 : resto)
  | comparaDoisCuidados cuidado1 cuidado2 = validaUmCuidadoPorHorario cuidado1 resto
  | otherwise = False

validaCuidadosPorHorario :: [Cuidado] -> Bool
validaCuidadosPorHorario [_] = True
validaCuidadosPorHorario (cuidado1 : resto) = validaUmCuidadoPorHorario cuidado1 resto

plantaoValido :: Plantao -> Bool
plantaoValido [] = True
plantaoValido plantao@((_, cuidado) : resto)
  | validaHorariosDePlantao plantao && validaCuidadosPorHorario cuidado = plantaoValido resto
  | otherwise = False

{-
  QUESTÃO 7  VALOR: 1,0 ponto
Defina a função "geraPlanoReceituario", cujo tipo é dado abaixo e que, a partir de um receituario válido, retorne um
plano de medicamento válido.
Dica: enquanto o receituário lista os horários que cada remédio deve ser tomado, o plano de medicamentos  é uma
disposição ordenada por horário de todos os remédios que devem ser tomados pelo paciente em um certo horário.
-}

mergeMedicamentos :: [Medicamento] -> [Medicamento] -> [Medicamento]
mergeMedicamentos [] [] = []
mergeMedicamentos medicamentos [] = medicamentos
mergeMedicamentos [] medicamentos = medicamentos
mergeMedicamentos (med1 : resto1) (med2 : resto2)
  | med1 == med2 = med1 : mergeMedicamentos resto1 resto2
  | med1 < med2 = med1 : mergeMedicamentos resto1 (med2 : resto2)
  | med2 < med1 = med2 : mergeMedicamentos resto2 (med1 : resto1)

mergePlanosMedicamento :: PlanoMedicamento -> PlanoMedicamento -> PlanoMedicamento
mergePlanosMedicamento [] [] = []
mergePlanosMedicamento plano [] = plano
mergePlanosMedicamento [] plano = plano
mergePlanosMedicamento ((horario1, medicamentos1) : restoPrescricoes1) ((horario2, medicamentos2) : restoPrescricoes2)
  | horario1 < horario2 = (horario1, medicamentos1) : mergePlanosMedicamento restoPrescricoes1 ((horario2, medicamentos2) : restoPrescricoes2)
  | horario2 < horario1 = (horario2, medicamentos2) : mergePlanosMedicamento restoPrescricoes2 ((horario1, medicamentos1) : restoPrescricoes1)
  | horario1 == horario2 = (horario1, mergeMedicamentos medicamentos1 medicamentos2) : mergePlanosMedicamento restoPrescricoes1 restoPrescricoes2

transformaPrescricaoEmPlanoMedicamento :: Prescricao -> PlanoMedicamento
transformaPrescricaoEmPlanoMedicamento (_, []) = []
transformaPrescricaoEmPlanoMedicamento (medicamento, horario1 : resto) = (horario1, [medicamento]) : transformaPrescricaoEmPlanoMedicamento (medicamento, resto)

geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario [] = []
geraPlanoReceituario (prescricao : resto) = mergePlanosMedicamento (transformaPrescricaoEmPlanoMedicamento prescricao) (geraPlanoReceituario resto)

{-
  QUESTÃO 8  VALOR: 1,0 ponto
Defina a função "geraReceituarioPlano", cujo tipo é dado abaixo e que retorna um receituário válido a partir de um plano
de medicamentos válido.
Dica: Existe alguma relação de simetria entre o receituário e o plano de medicamentos? Caso exista, essa simetria
permite compararmos a função geraReceituarioPlano com a função geraPlanoReceituario? Em outras palavras, podemos definir
geraReceituarioPlano com base em geraPlanoReceituario?
-}

mergeHorarios :: [Horario] -> [Horario] -> [Horario]
mergeHorarios [] [] = []
mergeHorarios horarios [] = horarios
mergeHorarios [] horarios = horarios
mergeHorarios (horario1 : resto1) (horario2 : resto2)
  | horario1 == horario2 = horario1 : mergeHorarios resto1 resto2
  | horario1 < horario2 = horario1 : mergeHorarios resto1 (horario2 : resto2)
  | horario2 < horario1 = horario2 : mergeHorarios resto2 (horario1 : resto1)

mergeReceituarios :: Receituario -> Receituario -> Receituario
mergeReceituarios [] [] = []
mergeReceituarios receituario [] = receituario
mergeReceituarios [] receituario = receituario
mergeReceituarios ((medicamento1, horarios1) : restoHorarioComMeds1) ((medicamento2, horarios2) : restoHorarioComMeds2)
  | medicamento1 < medicamento2 = (medicamento1, horarios1) : mergeReceituarios restoHorarioComMeds1 ((medicamento2, horarios2) : restoHorarioComMeds2)
  | medicamento2 < medicamento1 = (medicamento2, horarios2) : mergeReceituarios restoHorarioComMeds2 ((medicamento1, horarios1) : restoHorarioComMeds1)
  | medicamento1 == medicamento2 = (medicamento1, mergeHorarios horarios1 horarios2) : mergeReceituarios restoHorarioComMeds1 restoHorarioComMeds2

transformaHorarioComMedsEmReceituario :: Horario -> [Medicamento] -> Receituario
transformaHorarioComMedsEmReceituario _ [] = []
transformaHorarioComMedsEmReceituario horario (medicamento1 : resto) = (medicamento1, [horario]) : transformaHorarioComMedsEmReceituario horario resto

geraReceituarioPlano :: PlanoMedicamento -> Receituario
geraReceituarioPlano [] = []
geraReceituarioPlano ((horario, meds) : resto) = mergeReceituarios (transformaHorarioComMedsEmReceituario horario meds) (geraReceituarioPlano resto)

{-
  QUESTÃO 9 VALOR: 1,0 ponto
Defina a função "executaPlantao", cujo tipo é dado abaixo e que executa um plantão válido a partir de um estoque de
medicamentos, resultando em novo estoque. A execução consiste em desempenhar, sequencialmente, todos os cuidados para
cada horário do plantão. Caso o estoque acabe antes de terminar a execução do plantão, o resultado da função deve ser
Nothing. Caso contrário, o resultado deve ser Just v, onde v é o valor final do estoque de medicamentos.
-}

executaPlantao :: Plantao -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaPlantao = undefined

{-
  QUESTÃO 10 VALOR: 1,0 ponto
Defina uma função "satisfaz", cujo tipo é dado abaixo e que verifica se um plantão válido satisfaz um plano de
medicamento válido para um certo estoque, ou seja, a função "satisfaz" deve verificar se a execução do plantão implica
terminar com estoque diferente de Nothing e administrar os medicamentos prescritos no plano.
Dica: fazer correspondencia entre os remédios previstos no plano e os ministrados pela execução do plantão. Note que
alguns cuidados podem ser comprar medicamento e que eles podem ocorrer sozinhos em certo horário ou juntamente com
ministrar medicamento.
-}

satisfaz :: Plantao -> PlanoMedicamento -> EstoqueMedicamentos -> Bool
satisfaz = undefined

{-
  QUESTÃO 11 (EXTRA) VALOR: 1,0 ponto
Defina a função "plantaoCorreto", cujo tipo é dado abaixo e que gera um plantão válido que satisfaz um plano de
medicamentos válido e um estoque de medicamentos.
Dica: a execução do plantão deve atender ao plano de medicamentos e ao estoque.
-}

plantaoCorreto :: PlanoMedicamento -> EstoqueMedicamentos -> Plantao
plantaoCorreto = undefined
