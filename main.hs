-- 1. Escreva  uma  função  para  o  cálculo  dos  números  da  sequência  de  Fibonacci,  utilizando Haskell.

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- 2. Um dos primeiros algoritmos documentados é o algoritmo para o cálculo do Maior Divisor Comum  (MDC)  de  Euclides  publicado  por  volta do  ano 300  AC.  Podemos simplificar  este algoritmo  dizendo  que  dados  dois  inteiros  A  e  B,  o  MDC  entre  eles  será dado  pelo  valor absoluto de A se B=0 e pelo MDC entre B e o resto da divisão de A por B se B>0. Escreva uma  função  para  o  cálculo  do  MDC  entre  dois  números  inteiros  positivos,  usando  o algoritmo de Euclides conforme apresentado aqui, utilizando Haskell.

mdc :: Int -> Int -> Int
mdc a b
    | b == 0 = abs a
    | otherwise = mdc b (a `mod` b)

-- 3. Escreva uma função recursiva que dado um número inteiro n, devolva a soma dos dígitos deste  número.  Exemplo:  dado  1234  a  função  deverá  devolver  10.  Utilizando  Haskell  e recursividade.

somaDigitos :: Int -> Int
somaDigitos 0 = 0
somaDigitos n = (n `mod` 10) + somaDigitos (n `div` 10)

-- 4. Escreva  uma  função  que  devolva  a  soma  de  todos  os  números  menores  que  10000  que sejam múltiplos de 3 ou 5.

somaMultiplos :: Int -> Int
somaMultiplos n
    | n == 0 = 0
    | n `mod` 3 == 0 || n `mod` 5 == 0 = n + somaMultiplos (n-1)
    | otherwise = somaMultiplos (n-1)

-- 5. Escreva  uma  função que,  recebendo  uma  lista  de  inteiros,  apresente  a  diferença  entre a soma dos quadrados e o quadrado da soma destes inteiros, usando recursividade. 

somaQuadrados :: [Int] -> Int
somaQuadrados [] = 0
somaQuadrados (x:xs) = (x^2) + somaQuadrados xs

-- 6. O Crivo de Eratóstenes não é o melhor algoritmo para encontrar números primos. Crie uma função que implemente o Crivo de Euler (Euler’s Sieve) para encontrar todos os números primos menores que um determinado inteiro dado. 

crivoDeEuler :: Int -> [Int]
crivoDeEuler n = [x | x <- [2..n], ehPrimo x]


ehPrimo :: Int -> Bool
ehPrimo n = ehPrimoAux n (n-1)


ehPrimoAux :: Int -> Int -> Bool
ehPrimoAux n 1 = True
ehPrimoAux n i
    | n `mod` i == 0 = False
    | otherwise = ehPrimoAux n (i-1)

-- 7. Nem  só  de  Fibonacci  vivem  os  exemplos  de  recursão.  Escreva  uma  função  que  devolva todos os números de uma sequência de Lucas (2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123) menores que um inteiro dado.

lucas :: Int -> [Int]
lucas n = [(axLucas x) | x <- [0..n], n>=(axLucas x)]

axLucas :: Int -> Int
axLucas 0 = 2
axLucas 1 = 1
axLucas n = axLucas (n-1) + axLucas (n-2)

-- 8. Escreva uma função, chamada aoContrario em Haskel para reverter uma lista. Dado [1,2,3] devolva [3,2,1].

aoContrario :: [Int] -> [Int]
aoContrario [] = []
aoContrario (x:xs) = aoContrario xs ++ [x]

-- 9. Escreva uma função chamada somaRecursiva que recebe dois valores inteiros e devolve o produto destes valores sem usar o operador de multiplicação

somaRecursiva :: Int -> Int -> Int
somaRecursiva 0 0 = 0
somaRecursiva 0 n = n
somaRecursiva n 0 = n
somaRecursiva n m = somaRecursiva (n-1) (m-1) + n + m

-- 10. Escreva uma função chamada comprimento que receba uma lista de  inteiros e devolva o comprimento desta lista. Observe que você não pode usar nenhuma função que já calcule o comprimento de uma lista.

comprimento :: [Int] -> Int
comprimento [] = 0
comprimento (x:xs) = 1 + comprimento xs

main = do
    putStr "fibonacci: entrada 3; resultado: "
    print (fibonacci 3)
    putStr "mdc: entrada 68 25; resultado: "
    print (mdc 68 25)
    putStr "somaDigitos: entrada 1234; resultado: "
    print (somaDigitos 1234)
    putStr "somaMultiplos: entrada 10; resultado: "
    print (somaMultiplos 10)
    putStr "somaQuadrados: entrada [1,2,3]; resultado: "
    print (somaQuadrados [1,2,3])
    putStr "crivoDeEuler: entrada 42; resultado: "
    print (crivoDeEuler 42)
    putStr "lucas: entrada 10; resultado: "
    print (lucas 10)
    putStr "aoContrario: entrada [1,2,3]; resultado: "
    print (aoContrario [1,2,3])
    putStr "somaRecursiva: entrada 10 10; resultado: "
    print (somaRecursiva 10 10)
    putStr "comprimento: entrada [1,2,3]; resultado: "
    print (comprimento [1,2,3])


