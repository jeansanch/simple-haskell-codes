import Text.Show.Functions
import Data.Char

-- Remove o maior da lista de números
remove_maior :: [Int]->[Int]
remove_maior [a] = []
remove_maior [] = []
remove_maior (a:x)
 |a==(maior(a:x))=x
 |otherwise =a:(remove_maior x)

maior [a] = a
maior (a:x)
 |a>(maior x)=a
 |otherwise = maior x

-- Verifica se o número pertence a um conjunto de números

pertence :: Int->[Int]->Bool
pertence e [] = False
pertence e (a:x)
 |e == a = True
 |otherwise = pertence e x

-- Soma todos os valores contidos na lista

soma_lista :: [Int]->Int
soma_lista [] = 0
soma_lista (a:x) = a+soma_lista x

-- Remover o ultimo elemento da lista

remove_ultimo :: [Int]->[Int]
remove_ultimo [] = []
remove_ultimo [a] = []
remove_ultimo (a:x) = a:remove_ultimo x

--Adiciona o elemento na posicao N

insereN :: Int->Int->[Int]->[Int]
insereN i a x
 |a==0 = i:x
 |x==[] && a>0 = i:x
insereN i a (b:x)
 |a>0 = b:insereN i (a-1) x

-- InsereN vProfessor

insereNPf :: Int->Int->[Int]->[Int]
insereNPf e _ [] = [e]
insereNPf e 0 lista = e:lista
insereNPf e n (a:x) = a:(insereNPf e (n-1) x)

-- Verifica o valor do leite

vLeite :: [(String,Float)]->Float
vLeite [] = 0
vLeite (("leite",b):x) = b
vLeite ((a,b):x)=vLeite x

-- Arvore Binaria

data ArvoreBin = NodoNull | NodoInt Int ArvoreBin ArvoreBin
 deriving(Show)

passeio :: ArvoreBin->[Int]
passeio NodoNull = []
passeio (NodoInt a NodoNull NodoNull) = [a]
passeio (NodoInt a esq dir) = [a]++passeio esq++passeio dir

criaNodo :: Int->ArvoreBin
criaNodo a = (NodoInt a NodoNull NodoNull)

insere :: Int->ArvoreBin->ArvoreBin
insere i NodoNull = criaNodo i
insere i (NodoInt b esq dir)
 |i < b = insere i esq
 |otherwise = insere i dir

-- Dobro

dobro :: [Int]->[Int]
dobro [] = []
dobro (a:x) = (2*a):(dobro x)

-- Minusculo para Maiusculo

maiusc :: String->String
maiusc [] = []
maiusc (a:x) = toUpper(a):maiusc x

-- Retorna os pares

retPar :: [Int]->[Int]
retPar [] = []
retPar (a:x)
 |a `mod` 2 == 0 = [a]++retPar x
 |otherwise = []++retPar x

-- Concatena palavras

geraFrase :: [String]->String
geraFrase (a:[]) = a
geraFrase (a:x) = a++" "++geraFrase x

-- Funcoes como parametro

mapear :: (a->b)->[a]->[b]
mapear f [] = []
mapear f (a:x) = (f a):mapear f x

------------ Trabalho -----------

-- Exercicio 1

inverte :: Int->Int
inverte 0 = 0
inverte a = (a `mod` 10) * 10^iLog a 10 + inverte (a `div` 10)

iLog :: Int->Int->Int
iLog a b = intLog b 0 a

intLog :: Int->Int->Int->Int
intLog a n x
 |(a `mod` x == a) = n
intLog a n x= intLog (a `div` x) (n+1) x

-- Exercicio 2

somaVet :: [Int]->Int
somaVet [] = 0
somaVet (a:x) = a+somaVet x

-- Exercicio 3

somaFib :: Int->Int
somaFib 0 = 0
somaFib a = a+somaFib(a-1)

-- Exercicio 4

inverteF :: [Float]->[Float]
inverteF [] = []
inverteF (a:[]) = [a]
inverteF (a:x) = inverteF x++[a]

-- Exercicio 5

contaInt :: Int->Int->Int
contaInt _ 0 = 0
contaInt n x
 |(x `mod` 10) == n = 1 + contaInt n (x `div` 10)

-- Exercicio 6

multiplicacao :: Int->Int->Int
multiplicacao a 0 = 0
multiplicacao a b = multiplicacao a (b-1) + a

-- Exercicio 7

imprimeAteAux :: Int->Int->[Int]
imprimeAteAux a 0 = [a]
imprimeAteAux a b = [a-b]++imprimeAteAux a (b-1)

imprimeAteCres :: Int->[Int]
imprimeAteCres a = imprimeAteAux a a

-- Exercicio 8

imprimeAteDec :: Int->[Int]
imprimeAteDec 0 = [0]
imprimeAteDec a = [a]++imprimeAteDec (a-1)

-- Exercicio 9

imprimeParCresAux :: Int->Int->[Int]
imprimeParCresAux a 0
 |a `mod` 2 == 0 = [a]
 |otherwise = []
imprimeParCresAux a b
 |b < 0 = []
 |(a-b) `mod` 2 == 0 = [a-b]++imprimeParCresAux a (b-2)
 |otherwise = imprimeParCresAux a (b-1)

imprimeParCres :: Int->[Int]
imprimeParCres a = imprimeParCresAux a a

-- Exercicio 10

imprimeParDec :: Int->[Int]
imprimeParDec 0 = [0]
imprimeParDec a
 |a `mod` 2 == 0 = [a]++imprimeParDec (a-2)
 |otherwise = imprimeParDec (a-1)

-- Exercicio 11

fatorialDuplo :: Int->Int
fatorialDuplo a 
 |a `mod` 2 == 0 = 0
fatorialDuplo 1 = 1
fatorialDuplo a = a*fatorialDuplo (a-2)

-- Exercicio 12
fatorial :: Int->Int
fatorial 0 = 1
fatorial a = a*fatorial (a-1)

superFatorial :: Int->Int
superFatorial 0 = 1
superFatorial a = fatorial a*superFatorial (a-1)

-- Exercicio 13

menorNoVetorAux :: [Int]->Int->Int
menorNoVetorAux [] b = b
menorNoVetorAux (a:x) b
 |a < b = menorNoVetorAux x a
 |otherwise = menorNoVetorAux x b

menorNoVetor :: [Int]->Int
menorNoVetor (a:x) = menorNoVetorAux (a:x) a

-- Exercicio 14

imprimeSerie :: Int->Int->Int->[Int]
imprimeSerie a b n
 |a > b = [] 
 |otherwise = [a]++imprimeSerie (a+n) b n

-- Exercicio 15

decToBin :: Int->Int
decToBin 0 = 0
decToBin a
 |a `mod` 2 == 0 = 1*10^((iLog a 2)+1) + decToBin (a-(2^iLog a 2))

-- Exercicio 16

somaIncremento :: Int->Int->Int
somaIncremento a 0 = a
somaIncremento a b = 1+somaIncremento a (b-1)

-- Exercicio 17

somaNum :: Int->Int
somaNum 0 = 0
somaNum a = a `mod` 10 + somaNum (a `div` 10)