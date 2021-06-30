                                                      {-Funciones auxiliares para ejecutar los ejercicios.-}

--Funcion que devuelve el menor divisor de un numero n 
menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde a b | b == a = b
                      | mod b a == 0 = a
                      | otherwise = menorDivisorDesde (a+1) b

--Funcion que devuelve el menor divisor natural mayor a 1 de un numero n
menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde 2 n

--Funcion que devuelve el primo anteiror al n introducido
primoAnterior :: Integer -> Integer
primoAnterior n | esPrimo (n-1) == True = n-1  
                | esPrimo (n-1) == False = primoAnterior (n-1)

--Funcion que devuelve True si el numero introducido es primo y False si el numero no es primo
esPrimo :: Integer -> Bool
esPrimo n | n == 1 = False
          | menorDivisor n == n = True
          | otherwise = False

--Funcion que busca si la resta de un n con el primoAnterior es igual a un primo, y devuele el primoAnterior si es true
-- sino busca con el primoAnterior-anterior hasta que la resta sea un primo
esPrimaLaResta :: Integer -> Integer -> Integer
esPrimaLaResta i m | esPrimo (i - primoAnterior m) == True = primoAnterior m
                   | otherwise = esPrimaLaResta i (primoAnterior m)

--Funcion esPrimaLaResta simplificada
esPrimaLaResta2 :: Integer -> Integer
esPrimaLaResta2 n = esPrimaLaResta n n

--Funcion que se fija si el primo anterior a n + (n - primoAnterior) == n
esSumaDePrimos :: Integer -> Bool
esSumaDePrimos n | fst(descomposicionEnPrimos n) + snd(descomposicionEnPrimos n) == n = True
                 | otherwise = False

--Funcion que recibe un número natural n par mayor que 2 y devuelve la cantidad de pares ordenados (a, b) de números primos tales que a + b == n.
cantidadDeParesOrdenados :: Integer -> Integer -> Integer -> Integer
cantidadDeParesOrdenados i m n | m == 2 = n
                               | esPrimo (i - primoAnterior m) == True = cantidadDeParesOrdenados i (primoAnterior m) (n+1)
                               | esPrimo (i - primoAnterior m) == False = cantidadDeParesOrdenados i (primoAnterior m) n


                                                                         {-Ejercicios-}
{-Ejercicio 1. -}
satisfaceGoldbach :: Integer -> Bool
satisfaceGoldbach n | (even n) && (n > 2) && (esSumaDePrimos n == True) = True
                    | otherwise = False

{-Ejercicio 2. -}
verificarConjeturaHasta :: Integer -> Bool
verificarConjeturaHasta n | n == 2 = True  
                          | satisfaceGoldbach n == True = verificarConjeturaHasta (n-2)
{-Ejercicio 3. -}
descomposicionEnPrimos :: Integer -> (Integer, Integer)
descomposicionEnPrimos n = (esPrimaLaResta2 n, n - esPrimaLaResta2 n) 

{-Ejercicio 4. -}
numeroDeDescomposiciones :: Integer -> Integer
numeroDeDescomposiciones n = cantidadDeParesOrdenados n n 0 
              
