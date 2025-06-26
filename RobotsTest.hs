module Main where

import Test.HUnit
import System.Exit
import Robots

-- Variables para las pruebas:
atlas :: Robot
atlas = Robot {
    nombre = "Atlas",
    nivelExperiencia = 100,
    energia = 1000,
    programas = []
}

titan :: Robot
titan = Robot {
    nombre = "Titan",
    nivelExperiencia = 999999,
    energia = 999999,
    programas = [ descargaElectrica, autoAtaque ]
}

robertito :: Robot
robertito = Robot {
    nombre = "Robertito",
    nivelExperiencia = 9999999,
    energia = 9999999,
    programas = [ descargaElectrica ]
}

academia :: Academia
academia = [ atlas, titan, robertito ]

-- TESTS PUNTO 1

-- Verifica que recargaBateria sume energía correctamente
p1_testRecarga :: Test
p1_testRecarga = TestCase $
  assertEqual "recargaBateria suma energía" 150 (energia (recargaBateria 50 robotA))

-- Verifica que descargaElectrica resta correctamente si hay energía > 10
p1_testDescarga :: Test
p1_testDescarga = TestCase $
  assertEqual "descargaElectrica resta 10 si energía > 10" 90 (energia (descargaElectrica robotA))

-- Verifica que autoAtaque en robot sin programas lo deja en ERROR
p1_testAutoAtaque :: Test
p1_testAutoAtaque = TestCase $
  assertEqual "autoAtaque en robot sin programas lo deja en ERROR" "ERROR" (nombre (autoAtaque robotA))

-- TESTS PUNTO 2

-- Verifica poder de un robot
p2_testPoder :: Test
p2_testPoder = TestCase $
  assertEqual "poder de robotB" (200 + (2 * 2)) (poder robotB)

-- Verifica danio con descargaElectrica
p2_testDanio :: Test
p2_testDanio = TestCase $
  assertEqual "danio por descargaElectrica" (-10) (danio robotA descargaElectrica)

-- Verifica diferencia de poder
p2_testDiferenciaPoder :: Test
p2_testDiferenciaPoder = TestCase $
  assertEqual "diferencia de poder entre robotA y robotB" (abs (poder robotA - poder robotB)) (diferenciaDePoder robotA robotB)

-- Tests de punto 4:

-- 4 - Verifica quien tiene mas energía
p4_testSeleccionarMayor :: Test
p4_testSeleccionarMayor = TestCase $
  assertEqual "seleccionarMayor energía" robertito (seleccionarMayor energia academia)

-- 4a - Verifica al mejor programa de Titan contra Atlas
p4_testMejorProgramaContra :: Test
p4_testMejorProgramaContra = TestCase $
  assertEqual "El mejor programa contra devuelve un programa válido"
  (energia atlas - energia (mejorProgramaContra atlas titan atlas))
  (maximum [energia atlas - energia (prog atlas) | prog <- programas titan])

-- 4b - Verifica el mejor oponente para Titan
p4_testMejorOponente :: Test
p4_testMejorOponente = TestCase $
  assertEqual "El mejor oponente para Titan es Robertito" robertito (mejorOponente titan academia)


-- Test para el punto 5:

p5_testNoPuedeDerrotarle :: Test
p5_testNoPuedeDerrotarle = TestCase $
  assertBool "Atlas no puede derrotar a Robertito" (noPuedeDerrotarle atlas robertito)

p5_testPuedeDerrotarle :: Test
p5_testPuedeDerrotarle = TestCase $
  assertBool "Robertito sí puede dañar a Titan" (not (noPuedeDerrotarle robertito titan))

p5_testNoPuedeDerrotarleSinProgramas :: Test
p5_testNoPuedeDerrotarleSinProgramas = TestCase $
  assertBool "Robot sin programas no puede derrotar" (noPuedeDerrotarle atlas titan)

tests :: Test
tests =  test [ 
        TestLabel "P4 SeleccionarMayor" p4_testSeleccionarMayor,
        TestLabel "P4 MejorProgramaContra" p4_testMejorProgramaContra,
        TestLabel "P4 MejorOponente" p4_testMejorOponente,
        TestLabel "P5 NoPuedeDerrotarle" p5_testNoPuedeDerrotarle,
        TestLabel "P5 PuedeDerrotarle" p5_testPuedeDerrotarle,
        TestLabel "P5 NoPuedeDerrotarle Sin Programas" p5_testNoPuedeDerrotarleSinProgramas
    ]

main :: IO ()
main = do
    results <- runTestTT tests
    if failures results > 0 || errors results > 0
        then exitFailure
        else exitSuccess