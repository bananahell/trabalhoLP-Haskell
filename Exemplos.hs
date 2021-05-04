module Exemplos where

import ModeloDados
import UnBCare

-- Declarações

m1 :: Medicamento
m1 = "Lactulona"

m2 :: Medicamento
m2 = "Pantoprazol"

m3 :: Medicamento
m3 = "Patz"

m4 :: Medicamento
m4 = "Quetiapina"

m5 :: Medicamento
m5 = "Mirtazapina"

m6 :: Medicamento
m6 = "Adera"

m7 :: Medicamento
m7 = "Donepezila"

m8 :: Medicamento
m8 = "Xarelto"

m9 :: Medicamento
m9 = "Alprazolam"

estoque1 :: EstoqueMedicamentos
estoque1 = [(m1, 10), (m2, 5), (m3, 0)]

estoque2 :: EstoqueMedicamentos
estoque2 = [(m1, 10), (m2, 5), (m3, 10)]

estoque3 :: EstoqueMedicamentos
estoque3 = [(m1, 10), (m2, 50), (m3, 10), (m4, 20)]

receituario1 :: Receituario
receituario1 = [(m1, [8, 17]), (m2, [6]), (m3, [22])]

receituario2 :: Receituario
receituario2 = [(m1, [8, 17]), (m2, [6]), (m3, [22]), (m4, [8, 22, 23])]

receituario3 :: Receituario
receituario3 = [(m1, [8, 17]), (m3, [6]), (m2, [22])]

receituario4 :: Receituario
receituario4 = [(m1, [8, 17]), (m2, [6]), (m3, [22]), (m4, [8, 23, 22])]

plano1 :: PlanoMedicamento
plano1 = [(6, [m2]), (8, [m1]), (17, [m1]), (22, [m3])]

plano2 :: PlanoMedicamento
plano2 = [(6, [m2]), (8, [m1, m4]), (17, [m1]), (22, [m3, m4]), (23, [m4])] :: [(Int, [String])]

plano3 :: PlanoMedicamento
plano3 = [(6, [m2]), (17, [m1]), (8, [m1]), (22, [m3])]

plano4 :: PlanoMedicamento
plano4 = [(6, [m2]), (8, [m1, m4]), (17, [m1]), (22, [m4]), (23, [m4, m3])] :: [(Int, [String])]

plantao1 :: Plantao
plantao1 =
  [ (6, [Medicar m2]),
    (8, [Medicar m1]),
    (17, [Medicar m1]),
    (22, [Medicar m3])
  ]

plantao2 :: Plantao
plantao2 =
  [ (6, [Medicar m2]),
    (8, [Medicar m1]),
    (17, [Medicar m1, Comprar m3 30]),
    (22, [Medicar m3])
  ]

plantaoValido0 :: Plantao
plantaoValido0 =
  [ (6, [Medicar m2, Medicar m8]),
    (8, [Medicar m9, Medicar m1]),
    (17, [Medicar m1, Comprar m3 30]),
    (22, [Medicar m3])
  ]

plantaoInvalido1 :: Plantao
plantaoInvalido1 =
  [ (6, [Medicar m2, Medicar m8]),
    (8, [Medicar m9, Medicar m1]),
    (22, [Medicar m3]),
    (17, [Medicar m1, Comprar m3 30])
  ]

plantaoInvalido2 :: Plantao
plantaoInvalido2 =
  [ (6, [Medicar m2, Medicar m8]),
    (8, [Medicar m9, Medicar m1]),
    (17, [Medicar m1, Comprar m1 30]),
    (22, [Medicar m3])
  ]

plantaoInvalido3 :: Plantao
plantaoInvalido3 =
  [ (6, [Medicar m8, Medicar m2]),
    (8, [Medicar m9, Medicar m1]),
    (17, [Medicar m1, Comprar m1 30]),
    (22, [Medicar m3])
  ]

plantaoInvalido4 :: Plantao
plantaoInvalido4 =
  [ (6, [Medicar m2]),
    (8, [Comprar m1 20, Medicar m1, Medicar m4]),
    (17, [Medicar m1]),
    (22, [Medicar m3, Medicar m4]),
    (23, [Medicar m4])
  ]
