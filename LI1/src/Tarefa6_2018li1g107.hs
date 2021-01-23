


-- | Este módulo define funções comuns da Tarefa 6 do trabalho prático.
module Tarefa6_2018li1g107 where

import LI11819
import Tarefa0_2018li1g107

---------------------------------------------------Relatorio-----------------------------------------------------------------------
--
-- * Introduçao:
--
-- $intro
--
-- O objectivo desta tarefa é implementar um robô capaz de jogar automaticamente tendo em base uma certa estratégia.

-- * Objetivo:
--
-- $desen
--
--Procurando tornar o nosso bot o mais autónomo que conseguimos, começamos por definir uma função capaz de avaliar a melhor
--jogada a fazer tendo em conta as outras posições dos jogadores.
--O mesmo fizemos para ganhar pontos através da destruição de blocos destrutíveis, ou seja, através de uma outra
--função conseguimos fazer com que um bot de acordo com a sua posição e o mapa em questão, tomasse a decisão de contornar
--blocos indestrutíveis, se perante os mesmos, e destruir os restantes.

-- * Conclusao:
--
-- $conc
--
--Nesta tarefa ficamos aquem daquilo que pretendiamos, que seria a autonomizaçao do nosso bot
--No entanto, apesar das suas falhas, concluimos o trabalho que consistiu em criar um jogo 2D de combate.
--
--Module : Tarefa5_2018li1g107
--
--ID : Maria Sofia Martinho Gonçalves Jordao Marques (a87963);
--     Jose Nuno Baptista Martins (a90122)
--
--Data : 28.12.2018
--
--Universidade do Minho







data Deiricao = Acima | Abaixo | Aiesquerda | Aidireita | Aladonenhum deriving (Show,Eq)
 
-- * Funções principais da Tarefa 6.
-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot n (Estado map89 jds disp) =  atingeJogadores n jds map89
	                                               


map89=[[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
jds :: [Jogador]
jds = [(Jogador (1,1) D 8 7 2),(Jogador (1,8) E 3 7 3),(Jogador (5,1) D 2 2 2),(Jogador (12,8) E 1 1 1)]
  --[(Jogador (1,1) D 8 7 2),(Jogador (1,8) E 2 2 2)]
disp :: [Disparo]
disp = [(DisparoCanhao 0 (1,2) D ),(DisparoLaser 1 (1,8) E),(DisparoCanhao 2 (5,5) D)]


-- | Avalia,com ajuda da funçao auxiliar jogadoresEmLinha, a melhor 'Jogada' do tipo 'Dispara Arma' a fazer , por um determinado bot, tendo em conta
-- as posicoes dos outros jogadores e a situacao em que o proprio bot assim se encontra.

atingeJogadores:: Int 
               -> [Jogador] 
               -> Mapa
               -> Maybe Jogada 
atingeJogadores n jgds mapa | jogadoresEmLinha n dirRobalo jgds == converteDD dirRobalo = Just (Dispara Canhao)
                            | jogadoresEmLinha n C jgds==Acima = Just (Movimenta C)
                            | jogadoresEmLinha n B jgds==Abaixo = Just (Movimenta B)
                            | jogadoresEmLinha n D jgds==Aidireita= Just (Movimenta D)
                            | jogadoresEmLinha n E jgds==Aiesquerda= Just (Movimenta E)
                            | otherwise = desviaBlocos n jgds mapa
  where
  roubeite=encontraIndiceLista n jgds
  dirRobalo=direcaoJogador roubeite

  converteDD :: Direcao -> Deiricao
  converteDD x | x==C = Acima
               | x==B = Abaixo
               | x==D = Aidireita
               | x==E = Aiesquerda

-- | Funcao que retorna se existem tanks numa direcao escolhida.
jogadoresEmLinha n dir jgds = bull
  where
  roubeite=encontraIndiceLista n jgds

  aux:: Posicao -> Direcao -> Jogador -> Deiricao
  aux (x,y) dir (Jogador (x1,y1) _ _ _ _) = bulla
    where
      bulla=if dir==C && (x1<x) && (y1==y || y1==y+1 || y1==y-1) then Acima -- existe tank em cima
              else if dir==B && (x1>x) && (y1==y || y1==y+1 || y1==y-1) then Abaixo -- existe tank em baixo
                else if dir==D && (y1>y) && (x1==x || x1== x+1 || x1== x-1) then Aidireita
                  else if dir==E && (y1<y) && (x1==x+1 || x1==x-1 || x1==x) then Aiesquerda
                    else Aladonenhum -- nao existe tank na direcao escolhida 
  --  Recursiva da aux
  aux2:: Posicao 
      -> Direcao
      -> [Jogador] 
      -> Deiricao 
  aux2 pos dir [] = Aladonenhum
  aux2 pos dir jgds | aux pos dir (head jgds)==Aladonenhum = aux2 pos dir (tail jgds)
                    | otherwise = aux pos dir (head jgds)

  bull=aux2 (posicaoJogador roubeite) dir jgds


defendeAtaque:: Jogador 
             -> [Disparo] 
             ->  Maybe Jogada
defendeAtaque (Jogador (x,y) dir vd lsr chq) ((DisparoCanhao n (x1,y1) dir1):t) 
  |(x1<x) && (y1>=(y+1)) && (y1<(y-1)) = if (dir==C) then  (if (lsr>0) then Just (Dispara Laser) else Just (Dispara Canhao))
                                                  else Just (Movimenta D)                                                                                                                                    
  |(y1>y) && (x1>=(x+1)) && (x1<(x-1)) = if (dir==D) then (if (lsr>0) then Just (Dispara Laser)  else Just (Dispara Canhao))
                                                  else Just (Movimenta C)
  |(x1>x) && (y1>=(y+1)) && (y1<(y-1)) = if (dir==B) then (if (lsr>0) then Just (Dispara Laser)  else Just (Dispara Canhao))
                                                  else Just (Movimenta D)
  |(y1<y) && (x1>=(x+1)) && (x1<(x-1)) = if (dir==E) then (if (lsr>0) then Just (Dispara Laser)  else Just (Dispara Canhao))
                                                  else Just (Movimenta D) 
  | otherwise = (defendeAtaque (Jogador (x,y) dir vd lsr chq) (t))

-- | Funçao onde o bot de acordo com a sua posição e o mapa em questão, toma a decisão de contornar
-- blocos indestrutíveis, se perante os mesmos, e destruir os blocos Destrutiveis.
desviaBlocos:: Int 
            -> [Jogador] 
            -> Mapa 
            -> Maybe Jogada 
desviaBlocos n jgds mapa = movimentaRoubeite
  where
    roubeite=encontraIndiceLista n jgds
    (robaloX,robaloY)=posicaoJogador roubeite
    
    cimaEsquerda=encontraPosicaoMatriz (robaloX-1,robaloY) mapa
    cimaDireita=encontraPosicaoMatriz (robaloX-1,robaloY+1) mapa
    baixoEsquerda=encontraPosicaoMatriz (robaloX+2,robaloY) mapa
    baixoDireita=encontraPosicaoMatriz (robaloX+2,robaloY+1) mapa
    direitaCima=encontraPosicaoMatriz (robaloX,robaloY+2) mapa
    direitaEsquerda=encontraPosicaoMatriz (robaloX+1,robaloY+2) mapa
    esquerdaCima=encontraPosicaoMatriz (robaloX,robaloY-1) mapa
    esquerdaBaixo=encontraPosicaoMatriz (robaloX+1,robaloY-1) mapa

    paredesVizinhas = [[cimaEsquerda,cimaDireita   ],
                       [baixoEsquerda,baixoDireita ],
                       [direitaCima,direitaEsquerda],
                       [esquerdaCima,esquerdaBaixo ]
                      ] 
    movimentaRoubeite | elem (Bloco Destrutivel) (encontraIndiceLista 0 paredesVizinhas) = Just (Dispara Canhao)
                      | elem (Bloco Destrutivel) (encontraIndiceLista 1 paredesVizinhas) = Just (Dispara Canhao)
                      | elem (Bloco Destrutivel) (encontraIndiceLista 2 paredesVizinhas) = Just (Dispara Canhao)
                      | elem (Bloco Destrutivel) (encontraIndiceLista 3 paredesVizinhas) = Just (Dispara Canhao)    


                      | cimaEsquerda==Vazia && cimaDireita==Vazia    = Just (Movimenta C)
                      | baixoEsquerda==Vazia && baixoDireita==Vazia  = Just (Movimenta B)
                      | direitaCima==Vazia && direitaEsquerda==Vazia = Just (Movimenta D)
                      | esquerdaCima==Vazia && esquerdaBaixo==Vazia  = Just (Movimenta E)

