
-- | Este módulo define funções comuns da Tarefa 3 do trabalho prático.
module Tarefa3_2018li1g107 where

import LI11819
import Data.Char


---------------------------------------------------Relatorio-----------------------------------------------------------------------


--
-- * Introduçao:
--
-- $intro
--
--A ideia nesta fase de desenvolvimento anda em torno de tornar o jogo em si mais eficiente, ou seja,
--nesta tarefa, através de funções como a função comprime e descomprime, foi nos possivel poupar espaço em disco quando 
--o estado do jogo for gravado.

-- * Objetivo:
--
-- $desenv
--
-- O objetivo desta tarefa é, dada uma descrição do estado do jogo implementar um mecanismo de compressão / descompressão que 
--permita poupar caracteres e consequentemente espaço em jogo.
--Foi aqui que criamos funções capazes de comprimir tanto pecas de um mapa como jogadores e os disparos.
--Sendo que para facilitar mais tarde a descompressão utilizamos caracteres de separação dados ao acaso para separar 
--num estado cada componente os seus elementos.
--Para a descompressão fez-se exatamente o oposto da função anterior sendo que descomprime o estado anteriormente comprimido.
--Tal como a comprime, esta função encontra-se dividida por outras funções auxiliares que individualmente descomprimem ora 
--pecas do mapa ora jogadores e disparos.
--Nesta função cada caracter anteriormente atribuído é descomprimido para o seu estado normal, sendo que os caracteres de separação usados na compressão são tidos em conta.

-- * Conclusao:
--
-- $conc
--
--No fim desta tarefa foi-nos possível comprimir um estado do jogo em pouco mais que alguns caracteres. 
--Consequentemente, foi-nos possível também a descompressão desse mesmo estado, anteriormente comprimido, 
--devolvendo-o ao seu estado inicial.
--Ou seja fomos capazes de, usando a funçao comprime transformar um 'Estado' em nada mais que uma 'String', e usando a funçao
--descomprime transformar essa mesma 'String 'de novo num 'Estado' igual ao fornecido inicialmente premitindo-nos assim poupar 
--espaço no disco aquando de ser gravado. 
--
--Module : Tarefa5_2018li1g107
--
--ID : Maria Sofia Martinho Gonçalves Jordao Marques (a87963);
--     Jose Nuno Baptista Martins (a90122)
--
--Data : 28.12.2018
--
--Universidade do Minho



-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Estado'.
testesT3 :: [Estado]
testesT3 = [(Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [(Jogador (2,11) D 2 11 0),(Jogador (8,18) E 0 2 0),(Jogador (16,9) D 1 0 4),(Jogador (9,17) B 2 1 1)] [(DisparoChoque 0 4),(DisparoLaser 2 (16,12) D),(DisparoCanhao 3 (10,17) B)])]  

--[(Estado [[ Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [(Jogador (1,1) B 1 2 3),(Jogador (5,1) C 9 8 7)] [(DisparoCanhao 0 (1,1) C)])]
--testesT3 = [ (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [(Jogador (1,1) B 1 1 1),(Jogador (3,12) E 1 2 3),(Jogador (12,2) C 1 2 3),(Jogador (12,12) E 9 9 9)] [(DisparoCanhao 1 (3,12) C),(DisparoLaser 3 (12,12) E),(DisparoChoque 2 3)])]
--testesT3 = [ ( Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [(Jogador (1,1) C 2 3 3),(Jogador (1,7) D 5 6 7),(Jogador (7,2) E 2 3 3),(Jogador (7,7) B 1 1 1)] [(DisparoCanhao 1 (3,7) C),(DisparoChoque 2 5])]
--testesT3 = [ ( Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [(Jogador (1,1) C 5 6 3),(Jogador (3,7) B 2 1 2),(Jogador (7,2) E 2 5 9),(Jogador (7,7) D 1 1 1)] [(DisparoCanhao 0 (1,1) B),(DisparoLaser 1 (3,7) E),(DisparoChoque 2 5)])]
--testesT3 = [ (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel, Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel, Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel, Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel, Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel,Bloco Indestrutivel]][(Jogador (2,2) B 9 9 9)] [(DisparoCanhao 0 (2,2) D)])] 
-- * Funções principais da Tarefa 3.

-- | Comprime um 'Estado' para formato textual.
--
-- __NB:__ A função 'show' representa um 'Estado' num formato textual facilmente legível mas extenso.
--
-- __NB:__ Uma boa solução deve representar o 'Estado' dado no mínimo número de caracteres possível.
comprime :: Estado 
         -> String
comprime (Estado mapa jgds disp) = auxCMap mapa ++ ".." ++ auxCJdsStr jgds ++ "|." ++ auxCDisp disp 

-- | Dado um mapa, este separa as varias listas de 'Pecas' com um caracter especifico que lhe fora atribuido.
auxCMap ::[[Peca]]
        ->String
auxCMap []=[]
auxCMap [x] = auxM x
auxCMap (h:t) = auxCMap [h] ++ "/" ++ auxCMap t 

-- | Dada um lista de 'Pecas', esta funcao comprime numa determinada string cada uma das pecas ai presentes.
auxM :: [Peca] 
     -> String
auxM []=  []
auxM [x] | x== Bloco Destrutivel = "*"
         | x== Bloco Indestrutivel = "#"
         | x== Vazia = "$"
auxM (h: t) = auxM [h] ++ auxM t

-- | Uma lista de jogadores sera comprimida a uma string, em que cada character representara um dado 'Jogador'.
-- Nao esquecendo tambem que a separacao entre cada jogador da lista é tambem representado por um caracter especifico (primitindo mais tarde uma descompressao mais "fácil").
auxCJdsStr :: [Jogador]
           -> String
auxCJdsStr []= " "
auxCJdsStr [Jogador (x,y) dir vd lsr chq]= (show x)++ "," ++(show y)++ "&"++ auxtd dir ++ (show vd )++"&"++ (show lsr)++"&"++(show chq)
auxCJdsStr (h:t)= auxCJdsStr [h] ++ "|"++ auxCJdsStr t 


-- | Esta auxiliar premite nos do mesmo modo, comprimir a uma string, uma dada lista de 'Disparos' sendo que é tambem apresentada nessa string caracteres especificos representantes da separacao entre jogadores.
auxCDisp ::[Disparo] 
         -> String
auxCDisp [] = " "
auxCDisp [DisparoCanhao n (a,b) dir] = "C" ++ (show n) ++ "&" ++ (show a) ++ "," ++ (show b)++ "&" ++ auxtd dir 
auxCDisp [DisparoLaser n (a,b) dir] = "L" ++ (show n) ++ "&" ++ (show a) ++ "," ++ (show b)++ "&" ++ auxtd dir 
auxCDisp [DisparoChoque n t] = "H" ++ (show n) ++ "&" ++ (show t) 
auxCDisp (h:t)= auxCDisp [h] ++ "J" ++ auxCDisp t

-- | Esta funçao premite-nos nada mais do que comprimir as diferentes direçoes em strings proprias as mesmas.
auxtd :: Direcao
      -> String
auxtd x | x== D = "D"
        | x== E = "E"
        | x== C = "C"
        | x== B = "B"




-- | Descomprime um 'Estado' no formato textual utilizado pela função 'comprime'.
--
-- __NB:__ A função 'comprime' é válida de for possível recuperar o 'Estado' utilizando a função 'descomprime', i.e.:
--
-- prop> descomprime . comprime = id
--
-- __NB:__ Esta propriedade é particularmente válida para a solução pré-definida:
--
-- prop> read . show = id
descomprime :: String 
            -> Estado
descomprime listaa = Estado mapa jogs disparos
    where
      mapa = auxDMapa listaa []
      newListaa = drop( auxJo listaa '.' + 1) listaa
      jogs = auxDJg newListaa []
      newListaa2 = drop( auxJo newListaa '.') newListaa
      disparos = auxDF newListaa2 []




-- | É nesta funçao que ocorre a descompressao da string relativa ao mapa do jogo (resultante da anterior compressao do mapa ).
auxDMapa :: String -- ^ lista de 'Char' (representa pecas do mapa)
         -> Mapa -- ^ lista de lista de Pecas 
         -> Mapa -- ^ 'Mapa' resultante da descompressao dessa string
auxDMapa [] mapa = mapa
auxDMapa lista mapa | head lista == '.' = mapa 
                    | otherwise = auxDMapa l newMapa
                  where 
                    linha = auxDescLinha lista
                    l = drop (length linha +1) lista                    
                    newMapa = mapa ++ [linha]


-- | Esta funçao auxilia excluisavemente a funçao anterior sendo que o seu proposito é descomprimir a string em 'Pecas'.
auxDescLinha :: String 
             -> [Peca]
auxDescLinha (h:t) | h== '.' = []
                   | h=='/' = []
                   | h== '*'= Bloco Destrutivel : auxDescLinha t
                   | h== '#'= Bloco Indestrutivel : auxDescLinha t 
                   | h== '$' = Vazia : auxDescLinha t 

-- | Auxiliar que premite ler uma determinada string ate um certo ponto de paragem, algo extremamente util para a funçao descomprimir em si.
auxJo :: String 
      -> Char 
      -> Int ---- tamanho de uma lista até condiçao de paragem ch, inclusive
auxJo [] ch = 0 
auxJo (h:t) ch | h/=ch = 1 + auxJo t ch
               | otherwise= 1 

-- | Lê a string correspondente à lista dos jogadores 
auxDJg :: String -- ^string resultante do 1º drop da string anterior
       -> [Jogador] 
       -> [Jogador] ---nao esquecer que nesta funcao a lista l ja vem a parte do mapa ( iria dar erro no caso de ainda ter o mapa pois so ia ler ate ao 1 '.' que seria precisamente essa parte correspondente ao mapa) 
auxDJg [] l = l
auxDJg l msl 
             | head l == '.' = msl
             | otherwise= auxDJg newList (msl++[auxJog l]) 
    where
        newList= drop (auxJo l '|') l 


-- | Descomprime uma parte da string relativa a um só 'Jogador'
auxJog ::String 
       -> Jogador 
auxJog ms = Jogador (x,y) dir vd lsr chq
      where
        x=auxENum ms []
        newMS = drop (auxJo ms ',') ms
        y= auxENum newMS []
        newMS2 = drop (auxJo newMS '&') newMS
        dir = auxD newMS2
        newMS3 = drop 1 newMS2
        vd = auxENum newMS3 []
        newMS4 = drop (auxJo newMS3 '&') newMS3
        lsr= auxENum newMS4 []
        newMS5 = drop (auxJo newMS4 '&') newMS4
        chq= auxENum newMS5 []

-- | Esta funçao verifica se um dado 'Char' é ou nao um algarismo ( tendo em conta a numenclatura da Tabela ASCII)
isnumero::Char 
        -> Bool
isnumero ch = if ord ch>=48 && ord ch<=57 then True
              else False

-- | Lê ate onde é que a string é um algarismo, tendo como auxiliar a funçao definida anteriormente.
auxENum::String 
       -> String 
       -> Int
auxENum [] numerro = read numerro
auxENum (h:t) numerro | isnumero h = auxENum t (numerro++[h])
                      | otherwise = read numerro   

-- | Ao contrario da funçao auxtd, esta faz o oposto, descomprimindo cada string numa direcao correspondente.
auxD :: String 
     -> Direcao
auxD (x:t) | x == 'D' = D
           | x == 'C' = C
           | x == 'B' = B
           | x == 'E' = E

-- | Descomprime a string relativa a um disparo no próprio 'Disparo'.
auxDDisp :: String
         -> Disparo
auxDDisp l | head l == 'C' = DisparoCanhao id (x,y) dir
           | head l == 'L'= DisparoLaser id (x,y) dir
           | head l == 'H' = DisparoChoque id tempo
     where 
      id = auxENum (drop 1 l) []
      newl= drop ( auxJo l '&') l
      x= auxENum newl []
      newl1= drop (auxJo newl ',') newl 
      y= auxENum newl1 []
      newl2= drop (auxJo newl1 '&') newl1
      dir= auxD newl2
      tempo= auxENum newl []


-- | Descomprime a string direcionada para o lista dos disparos na propria lista de disparos que lhe deu origem.
auxDF:: String
     -> [Disparo] 
     -> [Disparo]
auxDF [] l = l
auxDF l mj = auxDF newList (mj++disparo)
    where
      newList = drop (auxJo l 'J') l
      disparo= [auxDDisp l] 



