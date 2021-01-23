-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2018li1g107 where

import LI11819
import Tarefa0_2018li1g107


-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um 'Estado'.
testesT4 :: [Estado]
testesT4 = [(Estado map89 jds disp)]

-- * Funções principais da Tarefa 4.
-- | Mapa escolhido para constituir o 'Estado' do teste. 
map89=[[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
-- | Lista de Jogadores ecolhida para constituir o 'Estado' do teste.
jds :: [Jogador]
jds = [(Jogador (1,1) D 8 7 2),(Jogador (1,8) E 3 7 3),(Jogador (5,1) D 2 2 2),(Jogador (12,8) E 1 1 1)]
  --[(Jogador (1,1) D 8 7 2),(Jogador (1,8) E 2 2 2)]
-- | Lista de Disparos escolhida para constituir o 'Estado' do teste
disp :: [Disparo]
disp = [(DisparoLaser 0 (1,1) D ),(DisparoCanhao 2 (5,2) D)]
--disp = [(DisparoLaser 2 (5,1) D),(DisparoLaser 3 (12,8) C)]
-- | Avança o 'Estado' do jogo um 'Tick' de tempo.
--
-- __NB:__ Apenas os 'Disparo's afetam o 'Estado' do jogo com o passar do tempo.
--
-- __NB:__ Deve chamar as funções 'tickChoques', 'tickCanhoes' e 'tickLasers' pela ordem definida.
tick :: Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' após um 'Tick'.
tick (Estado mapa jds disp) = tickLasers (tickCanhoes (tickChoques (Estado mapa jds disp)))       
--tick (Estado mapa jds disp) = tickChoques (Estado mapa jds disp)
--tick (Estado mapa jds disp) = tickCanhoes (Estado mapa jds disp)
--tick (Estado mapa jds disp) = tickLasers (Estado mapa jds disp)
--tick = tickChoques . tickCanhoes . tickLasers
--tick ( Estado mapa jds disp) = undefined
--tick (Estado mapa jds disp) = tickChoques (Estado mapa jds disp).tickCanhoes (Estado mapa jds disp).tickLasers (Estado mapa jds disp)

-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos tiros de 'Laser' disparados.
tickLasers :: Estado -> Estado
tickLasers (Estado mapa jds [] ) =  (Estado mapa jds []) 
tickLasers (Estado mapa jds ((DisparoLaser n pos dir):t)) = tickLasers (Estado (mapaatualizado ((DisparoLaser n pos dir):t) pos mapa)(atualizajds ((DisparoLaser n pos dir):t) (jds)) (destroiTiroLaser (t)))
tickLasers (Estado mapa jds (x:t)) =  (Estado mapa jds (x:t))


-- | É atraves desta funçao que o mapa do jogo vai sendo atualizado apos jogadas do tipo 'DisparoLaser'
mapaatualizado:: [Disparo] ->  Posicao-> [[Peca]]-> Mapa
mapaatualizado [] (x1,y1) map = map 
mapaatualizado ((DisparoLaser n (x,y) C):xs) (x1,y1) map 
        |(encontraPosicaoMatriz (x1,y1) map /= Bloco Indestrutivel) && (encontraPosicaoMatriz (x1,y1+1) map /= Bloco Indestrutivel) = mapaatualizado ((DisparoLaser n (x,y) C):xs) (x1-1,y1) (atualizaPosicaoMatriz (x1,y1+1) Vazia (atualizaPosicaoMatriz (x1,y1) Vazia map ))
        |(encontraPosicaoMatriz (x1,y1) map /= Bloco Indestrutivel) && (encontraPosicaoMatriz (x1,y1+1) map == Bloco Indestrutivel) = mapaatualizado (xs) (x1,y1) (atualizaPosicaoMatriz (x1,y1) Vazia map) 
        |(encontraPosicaoMatriz (x1,y1) map == Bloco Indestrutivel) && (encontraPosicaoMatriz (x1,y1+1) map /= Bloco Indestrutivel) = mapaatualizado (xs) (x1,y1) (atualizaPosicaoMatriz (x1,y1+1) Vazia map)
        | otherwise = mapaatualizado (xs) (x1,y1) map
mapaatualizado ((DisparoLaser n (x,y) D):xs) (x1,y1) map 
        |(encontraPosicaoMatriz (x1,y1) map /= Bloco Indestrutivel) && (encontraPosicaoMatriz (x1+1,y1) map /= Bloco Indestrutivel) = mapaatualizado ((DisparoLaser n (x,y) D):xs) (x1,y1+1) (atualizaPosicaoMatriz (x1+1,y1) Vazia (atualizaPosicaoMatriz (x1,y1) Vazia map ))
        |(encontraPosicaoMatriz (x1,y1) map /= Bloco Indestrutivel) && (encontraPosicaoMatriz (x1+1,y1) map == Bloco Indestrutivel) = mapaatualizado (xs) (x1,y1) (atualizaPosicaoMatriz (x1,y1) Vazia map ) 
        |(encontraPosicaoMatriz (x1,y1) map == Bloco Indestrutivel) && (encontraPosicaoMatriz (x1+1,y1) map /= Bloco Indestrutivel) = mapaatualizado (xs) (x1,y1) (atualizaPosicaoMatriz (x1+1,y1) Vazia map)
        | otherwise = mapaatualizado (xs) (x1,y1) map
mapaatualizado ((DisparoLaser n (x,y) B):xs) (x1,y1) map 
        |(encontraPosicaoMatriz (x1,y1) map /= Bloco Indestrutivel) && (encontraPosicaoMatriz (x1,y1+1) map /= Bloco Indestrutivel) = mapaatualizado ((DisparoLaser n (x,y) B):xs) (x1+1,y1) (atualizaPosicaoMatriz (x1,y1+1) Vazia (atualizaPosicaoMatriz (x1,y1) Vazia map ))
        |(encontraPosicaoMatriz (x1,y1) map /= Bloco Indestrutivel) && (encontraPosicaoMatriz (x1,y1+1) map == Bloco Indestrutivel) = mapaatualizado (xs) (x1,y1) (atualizaPosicaoMatriz (x1,y1) Vazia map ) 
        |(encontraPosicaoMatriz (x1,y1) map == Bloco Indestrutivel) && (encontraPosicaoMatriz (x1,y1+1) map /= Bloco Indestrutivel) = mapaatualizado (xs) (x1,y1) (atualizaPosicaoMatriz (x1,y1+1) Vazia map)
        | otherwise = mapaatualizado (xs) (x1,y1) map
mapaatualizado ((DisparoLaser n (x,y) E):xs) (x1,y1) map 
        |(encontraPosicaoMatriz (x1,y1) map /= Bloco Indestrutivel) && (encontraPosicaoMatriz (x1+1,y1) map /= Bloco Indestrutivel) = mapaatualizado ((DisparoLaser n (x,y) E):xs) (x1,y1-1) (atualizaPosicaoMatriz (x1+1,y1) Vazia (atualizaPosicaoMatriz (x1,y1) Vazia map ))
        |(encontraPosicaoMatriz (x1,y1) map /= Bloco Indestrutivel) && (encontraPosicaoMatriz (x1+1,y1) map == Bloco Indestrutivel) = mapaatualizado (xs) (x1,y1) (atualizaPosicaoMatriz (x1,y1) Vazia map ) 
        |(encontraPosicaoMatriz (x1,y1) map == Bloco Indestrutivel) && (encontraPosicaoMatriz (x1+1,y1) map /= Bloco Indestrutivel) = mapaatualizado (xs) (x1,y1) (atualizaPosicaoMatriz (x1+1,y1) Vazia map)
        | otherwise = mapaatualizado (xs) (x1,y1) map
mapaatualizado (r:rs) (x1,y1) map = mapaatualizado rs (x1,y1) map  

-- |Esta funcao é utilizada com o objectivo de atualizar cada jogador, quanto ao seu numero de vidas, apos jogadas feitas do tipo 'DisparoLaser' .
atualizajds :: [Disparo] -> [Jogador] -> [Jogador]                                          
atualizajds ((DisparoLaser n (x,y) dir):xs) [] = []
atualizajds [] l = l
atualizajds ((DisparoLaser n (x,y) C):xs) ((Jogador (x1,y1) dir1 vidas laser choques):t) | ((x >= x1) && (y1 > (y-2)) && (y1 < (y+1))) = (Jogador (x1,y1) dir1 (vidas-1) laser choques) : (atualizajds ((DisparoLaser n (x,y) C):xs) t)
                                                                                         | otherwise = (Jogador (x1,y1) dir1 vidas laser choques) : (atualizajds ((DisparoLaser n (x,y) C):xs) t)
atualizajds ((DisparoLaser n (x,y) D):xs) ((Jogador (x1,y1) dir1 vidas laser choques):t) | (y <= y1) && (x1> (x-2)) && (x1 < (x+1)) = (Jogador (x1,y1) dir1 (vidas-1) laser choques): (atualizajds ((DisparoLaser n (x,y) D):xs) t)
                                                                                         | otherwise = (Jogador (x1,y1) dir1 vidas laser choques): (atualizajds ((DisparoLaser n (x,y) D):xs) t)                                                                                 
atualizajds ((DisparoLaser n (x,y) B):xs) ((Jogador (x1,y1) dir1 vidas laser choques):t) | (x <= x1) && (y1> (y-2)) && (y1 < (y+1)) = (Jogador (x1,y1) dir1 (vidas-1) laser choques): (atualizajds ((DisparoLaser n (x,y) B):xs) t) 
                                                                                         | otherwise = (Jogador (x1,y1) dir1 vidas laser choques): (atualizajds ((DisparoLaser n (x,y) B):xs) t) 
atualizajds ((DisparoLaser n (x,y) E):xs) ((Jogador (x1,y1) dir1 vidas laser choques):t) | (y >= y1) && (x1> (x-2)) && (x1 < (x+1)) = (Jogador (x1,y1) dir1 (vidas-1) laser choques): (atualizajds ((DisparoLaser n (x,y) E):xs) t)
                                                                                         | otherwise = (Jogador (x1,y1) dir1 vidas laser choques): (atualizajds ((DisparoLaser n (x,y) E):xs) t)
atualizajds (x:y:xs) ((Jogador (x1,y1) dir1 vidas laser choques):t) = atualizajds (y:xs) ((Jogador (x1,y1) dir1 vidas laser choques):t)

-- | Funçao que simula a destruiçao de disparos quando se deparam com um disparo do tipo Laser.
destroiTiroLaser :: [Disparo] -> [Disparo]
destroiTiroLaser [] = []  
destroiTiroLaser ((DisparoLaser n (x,y) dir):(DisparoLaser n1 (x1,y1) dir1):xs) = ((DisparoLaser n (x,y) dir):(DisparoLaser n1 (x1,y1) dir1): destroiTiroLaser xs) 
destroiTiroLaser ((DisparoLaser n (x,y) dir ):(DisparoChoque n1 t):xs) = ((DisparoLaser n (x,y) dir ):(DisparoChoque n1 t) : destroiTiroLaser ((DisparoChoque n t):xs))
destroiTiroLaser ((DisparoLaser n (x,y) dir ):(DisparoCanhao n1 (x1,y1) dir1):xs) | (dir==C) && (y== y1) = destroiTiroLaser ((DisparoLaser n (x,y) dir):xs)
                                                                                  | (dir==D) && (x== x1) = destroiTiroLaser ((DisparoLaser n (x,y) dir):xs)
                                                                                  | (dir==B) && (y== y1) = destroiTiroLaser ((DisparoLaser n (x,y) dir):xs)
                                                                                  | (dir==E) && (x== x1) = destroiTiroLaser ((DisparoLaser n (x,y) dir):xs)
                                                                                  | otherwise = ((DisparoCanhao n1 (x1,y1) dir1): destroiTiroLaser ((DisparoLaser n (x,y) dir) : xs))
destroiTiroLaser (x:xs) = (x : destroiTiroLaser xs)






-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos das balas de 'Canhao' disparadas.
tickCanhoes :: Estado -> Estado
tickCanhoes (Estado mapa jds [] ) =  (Estado mapa jds []) 
tickCanhoes (Estado mapa jds ((DisparoCanhao n pos dir):t)) = (Estado (mapaatualizado2 ((DisparoCanhao n pos dir):t) (pos) (mapa))  (atualizajds2 ((DisparoCanhao n pos dir):t) (jds))  (destroiTiroCanhao (desapareceTiro2 (desapareceTiro ((DisparoCanhao n pos dir):t) (pos) (mapa)) (jds))) )
tickCanhoes (Estado mapa jds (x:xs)) =  (Estado mapa jds (x:xs))                                                                                                                                                                   


-- | Tal como a funçao mapaatualizado, esta funçao premite-nos obter o mapa do jogo atualizado apos jogadas, neste caso, jogadas do tipo 'Disparo'.
mapaatualizado2:: [Disparo] ->  Posicao-> [[Peca]]-> Mapa
mapaatualizado2 [] (x1,y1) map = map 
mapaatualizado2 ((DisparoCanhao n (x,y) C):xs) (x1,y1) map 
        |(encontraPosicaoMatriz (x1,y1) map == Vazia) && (encontraPosicaoMatriz (x1,y1+1) map == Vazia) = mapaatualizado2 ((DisparoCanhao n (x-1,y) C):xs) (x1-1,y1) map
        |(encontraPosicaoMatriz (x1,y1) map == Vazia) && (encontraPosicaoMatriz (x1,y1+1) map == Bloco Destrutivel) = mapaatualizado2 (xs) (x1,y1) (atualizaPosicaoMatriz (x1,y1+1) Vazia map)
        |(encontraPosicaoMatriz (x1,y1) map == Bloco Destrutivel) && (encontraPosicaoMatriz (x1,y1+1) map == Vazia) =  mapaatualizado2 (xs) (x1,y1) (atualizaPosicaoMatriz (x1,y1) Vazia map )
        |(encontraPosicaoMatriz (x1,y1) map == Bloco Destrutivel) && (encontraPosicaoMatriz (x1,y1+1) map == Bloco Destrutivel) = mapaatualizado2 (xs) (x1,y1) (atualizaPosicaoMatriz (x1,y1+1) Vazia (atualizaPosicaoMatriz (x1,y1) Vazia map ))
        |(encontraPosicaoMatriz (x1,y1) map /= Bloco Indestrutivel) && (encontraPosicaoMatriz (x1,y1+1) map == Bloco Indestrutivel) = mapaatualizado2 (xs) (x1,y1) (atualizaPosicaoMatriz (x1,y1) Vazia map ) 
        |(encontraPosicaoMatriz (x1,y1) map == Bloco Indestrutivel) && (encontraPosicaoMatriz (x1,y1+1) map /= Bloco Indestrutivel) = mapaatualizado2 (xs) (x1,y1) (atualizaPosicaoMatriz (x1,y1+1) Vazia map)
        | otherwise = mapaatualizado2 (xs) (x1,y1) map
mapaatualizado2 ((DisparoCanhao n (x,y) D):xs) (x1,y1) map 
        |(encontraPosicaoMatriz (x1,y1) map == Vazia) && (encontraPosicaoMatriz (x1+1,y1) map == Vazia) = mapaatualizado2 ((DisparoCanhao n (x,y+1) D):xs) (x1,y1+1) map
        |(encontraPosicaoMatriz (x1,y1) map == Vazia) && (encontraPosicaoMatriz (x1+1,y1) map == Bloco Destrutivel) = mapaatualizado2 (xs) (x1,y1) (atualizaPosicaoMatriz (x1+1,y1) Vazia map)
        |(encontraPosicaoMatriz (x1,y1) map == Bloco Destrutivel) && (encontraPosicaoMatriz (x1+1,y1) map == Vazia) = mapaatualizado2 (xs) (x1,y1)  (atualizaPosicaoMatriz (x1,y1) Vazia map )
        |(encontraPosicaoMatriz (x1,y1) map == Bloco Destrutivel) && (encontraPosicaoMatriz (x1+1,y1) map == Bloco Destrutivel) = mapaatualizado2 (xs) (x1,y1) (atualizaPosicaoMatriz (x1+1,y1) Vazia (atualizaPosicaoMatriz (x1,y1) Vazia map ))
        |(encontraPosicaoMatriz (x1,y1) map /= Bloco Indestrutivel) && (encontraPosicaoMatriz (x1+1,y1) map == Bloco Indestrutivel) = mapaatualizado2 (xs) (x1,y1) (atualizaPosicaoMatriz (x1,y1) Vazia map ) 
        |(encontraPosicaoMatriz (x1,y1) map == Bloco Indestrutivel) && (encontraPosicaoMatriz (x1+1,y1) map /= Bloco Indestrutivel) = mapaatualizado2 (xs) (x1,y1) (atualizaPosicaoMatriz (x1+1,y1) Vazia map)
        | otherwise = mapaatualizado2 (xs) (x1,y1) map
mapaatualizado2 ((DisparoCanhao n (x,y) B):xs) (x1,y1) map 
        |(encontraPosicaoMatriz (x1,y1) map == Vazia) && (encontraPosicaoMatriz (x1,y1+1) map == Vazia) = mapaatualizado2 ((DisparoCanhao n (x+1,y) B):xs) (x1+1,y1) map
        |(encontraPosicaoMatriz (x1,y1) map == Vazia) && (encontraPosicaoMatriz (x1,y1+1) map == Bloco Destrutivel) = mapaatualizado2 (xs) (x1,y1) (atualizaPosicaoMatriz (x1,y1+1) Vazia map)
        |(encontraPosicaoMatriz (x1,y1) map == Bloco Destrutivel) && (encontraPosicaoMatriz (x1,y1+1) map == Vazia) = mapaatualizado2 (xs) (x1,y1) (atualizaPosicaoMatriz (x1,y1) Vazia map )
        |(encontraPosicaoMatriz (x1,y1) map == Bloco Destrutivel) && (encontraPosicaoMatriz (x1,y1+1) map == Bloco Destrutivel) = mapaatualizado2 (xs) (x1,y1) (atualizaPosicaoMatriz (x1,y1+1) Vazia (atualizaPosicaoMatriz (x1,y1) Vazia map ))
        |(encontraPosicaoMatriz (x1,y1) map /= Bloco Indestrutivel) && (encontraPosicaoMatriz (x1,y1+1) map == Bloco Indestrutivel) = mapaatualizado2 (xs) (x1,y1) (atualizaPosicaoMatriz (x1,y1) Vazia map ) 
        |(encontraPosicaoMatriz (x1,y1) map == Bloco Indestrutivel) && (encontraPosicaoMatriz (x1,y1+1) map /= Bloco Indestrutivel) = mapaatualizado2 (xs) (x1,y1) (atualizaPosicaoMatriz (x1,y1+1) Vazia map)
        | otherwise = mapaatualizado2 (xs) (x1,y1) map
mapaatualizado2 ((DisparoCanhao n (x,y) E):xs) (x1,y1) map 
        |(encontraPosicaoMatriz (x1,y1) map == Vazia) && (encontraPosicaoMatriz (x1+1,y1) map == Vazia) = mapaatualizado2 ((DisparoCanhao n (x,y-1) E):xs) (x1,y1-1) map
        |(encontraPosicaoMatriz (x1,y1) map == Vazia) && (encontraPosicaoMatriz (x1+1,y1) map == Bloco Destrutivel) = mapaatualizado2 (xs) (x1,y1) (atualizaPosicaoMatriz (x1+1,y1) Vazia map)
        |(encontraPosicaoMatriz (x1,y1) map == Bloco Destrutivel) && (encontraPosicaoMatriz (x1+1,y1) map == Vazia) = mapaatualizado2 (xs) (x1,y1) (atualizaPosicaoMatriz (x1,y1) Vazia map )
        |(encontraPosicaoMatriz (x1,y1) map == Bloco Destrutivel) && (encontraPosicaoMatriz (x1+1,y1) map == Bloco Destrutivel) = mapaatualizado2 (xs) (x1,y1) (atualizaPosicaoMatriz (x1+1,y1) Vazia (atualizaPosicaoMatriz (x1,y1) Vazia map ))
        |(encontraPosicaoMatriz (x1,y1) map /= Bloco Indestrutivel) && (encontraPosicaoMatriz (x1+1,y1) map == Bloco Indestrutivel) = mapaatualizado2 (xs) (x1,y1) (atualizaPosicaoMatriz (x1,y1) Vazia map) 
        |(encontraPosicaoMatriz (x1,y1) map == Bloco Indestrutivel) && (encontraPosicaoMatriz (x1+1,y1) map /= Bloco Indestrutivel) = mapaatualizado2 (xs) (x1,y1) (atualizaPosicaoMatriz (x1+1,y1) Vazia map)
        | otherwise = mapaatualizado2 (xs) (x1,y1) map
mapaatualizado2 (r:rs) (x1,y1) map = mapaatualizado2 (rs) (x1,y1) map


-- |Funçao que provoca o desaparecimento de um tiro do tipo 'DisparoCanhao' quando colide com 'Blocos Destrutiveis' ou 'Blocos Indestrutiveis'.
desapareceTiro:: [Disparo] ->  Posicao-> [[Peca]]-> [Disparo]
desapareceTiro [] (x,y) map = []
desapareceTiro ((DisparoCanhao n (x,y) C):d) (x1,y1) map 
        |((encontraPosicaoMatriz (x1,y1) map) == Vazia) && (encontraPosicaoMatriz (x1,y1+1) map == Vazia) = (DisparoCanhao n (x-1,y) C): (desapareceTiro (d) (x1,y1) map)
        | otherwise = (desapareceTiro (d) (x1,y1) map)
desapareceTiro ((DisparoCanhao n (x,y) D):d) (x1,y1) map 
        |((encontraPosicaoMatriz (x1,y1) map) == Vazia) && (encontraPosicaoMatriz (x1+1,y1) map == Vazia) = (DisparoCanhao n (x,y+1) D): (desapareceTiro (d) (x1,y1) map)
        | otherwise = (desapareceTiro (d) (x1,y1) map)
desapareceTiro ((DisparoCanhao n (x,y) B):d) (x1,y1) map 
        |((encontraPosicaoMatriz (x1,y1) map) == Vazia) && (encontraPosicaoMatriz (x1,y1+1) map == Vazia) = (DisparoCanhao n (x+1,y) B): (desapareceTiro (d) (x1,y1) map)
        | otherwise = (desapareceTiro (d) (x1,y1) map)
desapareceTiro ((DisparoCanhao n (x,y) E):d) (x1,y1) map 
        |((encontraPosicaoMatriz (x1,y1) map) == Vazia) && (encontraPosicaoMatriz (x1+1,y1) map == Vazia) = (DisparoCanhao n (x,y-1) E): (desapareceTiro (d) (x1,y1) map)
        | otherwise = (desapareceTiro (d) (x1,y1) map)
desapareceTiro (z:zs) (x,y) map = z : (desapareceTiro (zs) (x,y) map) 




-- | Funçao auxiliar semelhante a atualizajds, que atualiza o numero de vida de cada jogador, apos jogadas do tipo 'DisparoCanhao'.
atualizajds2::[Disparo]-> [Jogador]-> [Jogador]
atualizajds2 l [] = []
atualizajds2 [] l = l
atualizajds2 ((DisparoCanhao n (x,y) C):w) ((Jogador (x1,y1) dir vidas laser choques):t) | ((x1==(x-1)) && (y1>=(y-1)) && (y1<=(y+1))) = (Jogador (x1,y1) dir (vidas-1) laser choques) : (atualizajds2 w t)
                                                                                         | otherwise = (Jogador (x1,y1) dir vidas laser choques): atualizajds2 ((DisparoCanhao n (x,y) C):w) t     
atualizajds2 ((DisparoCanhao n (x,y) D):w) ((Jogador (x1,y1) dir vidas laser choques):t) | ((x1>=(x-1)) && (x1<=(x+1)) && (y1==(y+1))) = (Jogador (x1,y1) dir (vidas-1) laser choques) : (atualizajds2 w t)
                                                                                         | otherwise = (Jogador (x1,y1) dir vidas laser choques): atualizajds2 ((DisparoCanhao n (x,y) D):w) t 
atualizajds2 ((DisparoCanhao n (x,y) B):w) ((Jogador (x1,y1) dir vidas laser choques):t) | (x1==(x+1) && y1>=(y-1) && y1<=(y+1)) = (Jogador (x1,y1) dir (vidas-1) laser choques) : (atualizajds2 w t)
                                                                                         | otherwise = (Jogador (x1,y1) dir vidas laser choques): atualizajds2 ((DisparoCanhao n (x,y) B):w) t                                                                                
atualizajds2 ((DisparoCanhao n (x,y) E):w) ((Jogador (x1,y1) dir vidas laser choques):t) | (x1>=(x-1) && x1<=(x+1) && y1==(y-1)) = (Jogador (x1,y1) dir (vidas-1) laser choques) : (atualizajds2 w t)
                                                                                         | otherwise = (Jogador (x1,y1) dir vidas laser choques): atualizajds2 ((DisparoCanhao n (x,y) E):w) t 
atualizajds2 (x:y:t) l = l

-- |Funçao que provoca o desaparecimento de um tiro do tipo 'DisparoCanhao' quando atinge um 'Jogador'.
desapareceTiro2:: [Disparo]->[Jogador]->[Disparo]
desapareceTiro2 l [] = l
desapareceTiro2 [] l = []
desapareceTiro2 ((DisparoCanhao n (x,y) C):w) ((Jogador (x1,y1) dir vidas laser choques):t) | (x1==x && y1>=(y-1) && y1<=(y+1)) = desapareceTiro2 (w) ((Jogador (x1,y1) dir vidas laser choques):t)
                                                                                            | otherwise = (desapareceTiro2 ((DisparoCanhao n (x,y) C):w) (t)) ++ (desapareceTiro2 (w) ((Jogador (x1,y1) dir vidas laser choques):t))   
desapareceTiro2 ((DisparoCanhao n (x,y) D):w) ((Jogador (x1,y1) dir vidas laser choques):t) | (x1>=(x-1) && x1<=(x+1) && y1==y) = desapareceTiro2 (w) ((Jogador (x1,y1) dir vidas laser choques):t)
                                                                                            | otherwise = (desapareceTiro2 ((DisparoCanhao n (x,y) D):w) (t)) ++ (desapareceTiro2 (w) ((Jogador (x1,y1) dir vidas laser choques):t)) 
desapareceTiro2 ((DisparoCanhao n (x,y) B):w) ((Jogador (x1,y1) dir vidas laser choques):t) | (x1==x && y1>=(y-1) && y1<=(y+1)) = desapareceTiro2 (w) ((Jogador (x1,y1) dir vidas laser choques):t)
                                                                                            | otherwise = (desapareceTiro2 ((DisparoCanhao n (x,y) B):w) (t)) ++ (desapareceTiro2 (w) ((Jogador (x1,y1) dir vidas laser choques):t))                                                                                
desapareceTiro2 ((DisparoCanhao n (x,y) E):w) ((Jogador (x1,y1) dir vidas laser choques):t) | (x1>=(x-1) && x1<=(x+1) && y1==y) = desapareceTiro2 (w) ((Jogador (x1,y1) dir vidas laser choques):t)
                                                                                            | otherwise = (desapareceTiro2 ((DisparoCanhao n (x,y) E):w) (t)) ++ (desapareceTiro2 (w) ((Jogador (x1,y1) dir vidas laser choques):t))
desapareceTiro2 (z:zs) l = z : (desapareceTiro2 (zs) l )


-- | Funçao que simula a destruiçao de disparos quando se deparam com um disparo do tipo Canhao.
destroiTiroCanhao::[Disparo]->[Disparo]
destroiTiroCanhao [] = []
--destroiTiroCanhao [(DisparoCanhao n (x,y) dir)] = []
destroiTiroCanhao ((DisparoCanhao n (x,y) dir):(DisparoLaser n1 (x1,y1) dir1):xs) | (dir1==C) && (y== y1) = ((DisparoLaser n1 (x1,y1) dir): destroiTiroCanhao xs)
                                                                                  | (dir1==D) && (x== x1) = ((DisparoLaser n1 (x1,y1) dir): destroiTiroCanhao xs)
                                                                                  | (dir1==B) && (y== y1) = ((DisparoLaser n1 (x1,y1) dir): destroiTiroCanhao xs)
                                                                                  | (dir1==E) && (x== x1) = ((DisparoLaser n1 (x1,y1) dir): destroiTiroCanhao xs)
                                                                                  | otherwise = (DisparoLaser n1 (x1,y1) dir1) : destroiTiroCanhao ((DisparoCanhao n (x,y) dir) :xs)
destroiTiroCanhao ((DisparoCanhao n (x,y) dir):(DisparoCanhao n1 (x1,y1) dir1):xs)| (x==x1) && (y==y1) = destroiTiroCanhao (xs)
                                                                                  | (dir==C) && (dir1==B) && (x1==x+1) && (y1==y) = destroiTiroCanhao (xs)
                                                                                  | (dir==D) && (dir1==E) && (x1==x) && (y1==y-1) = destroiTiroCanhao (xs)
                                                                                  | (dir==B) && (dir1==C) && (x1==x-1) && (y1==y) = destroiTiroCanhao (xs)
                                                                                  | (dir==E) && (dir1==D) && (x1==x+1) && (y1==y+1) = destroiTiroCanhao (xs)
                                                                                  | otherwise = ((DisparoCanhao n (x,y) dir):(DisparoCanhao n1 (x1,y1) dir1): destroiTiroCanhao xs)
destroiTiroCanhao ((DisparoCanhao n (x,y) dir):(DisparoChoque n1 t):xs) = ((DisparoCanhao n (x,y) dir):(DisparoChoque n1 t) : destroiTiroCanhao xs)                                                                                   
destroiTiroCanhao (x:xs)= (x : destroiTiroCanhao xs) 





-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos campos de 'Choque' disparados.
tickChoques :: Estado -> Estado
tickChoques (Estado mapa jds disp ) =  (Estado mapa jds (auxTicks disp))

-- | Auxiliar da funçao ticksChoques que decrementa o tempo do choque conforme o passar do tempo do jogo.
auxTicks::[Disparo]->[Disparo]
auxTicks [] = []
auxTicks ((DisparoChoque n t):xs) | t==0 = auxTicks (destroiTiroChoques xs)
                                  | otherwise = (DisparoChoque n (t-1)) : auxTicks (destroiTiroChoques xs)
auxTicks (f:xs) = f:auxTicks xs                                  

-- | Tal como as funcoes 'dispara tiro' anteriores estas incrementa a situaçao onde um outro tiro
-- entra na area abrangente onde um tiro choque esta a decorrer.
destroiTiroChoques :: [Disparo]->[Disparo]
destroiTiroChoques [] = []
destroiTiroChoques ((DisparoChoque n t):(DisparoLaser n1 (x,y) dir):xs) = ((DisparoChoque n t):(DisparoLaser n1 (x,y) dir) : destroiTiroChoques xs)
destroiTiroChoques ((DisparoChoque n t):(DisparoCanhao n1 (x,y) dir):xs) = ((DisparoChoque n t):(DisparoCanhao n1 (x,y) dir): destroiTiroChoques xs)
destroiTiroChoques ((DisparoChoque n t):(DisparoChoque n1 t1):xs) = ((DisparoChoque n t):(DisparoChoque n1 t1) : destroiTiroChoques xs)
destroiTiroChoques (x:xs) = ( x:destroiTiroChoques xs) 
