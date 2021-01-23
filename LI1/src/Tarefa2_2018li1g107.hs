-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2018li1g107 where

import LI11819

import Tarefa0_2018li1g107
-- * Testes

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [ (2 , (Movimenta D), (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [(Jogador (1,1) C 2 3 4),(Jogador (1,7) B 1 2 3),(Jogador (7,1) E 2 4 3),(Jogador (6,7) D 2 4 5)] [(DisparoCanhao 1 (7,1) C),(DisparoLaser 3 (6,7) D),(DisparoCanhao 2 (1,7) B),(DisparoChoque 1 5)]))]
--testesT2 = [(1 , (Dispara Canhao), (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [(Jogador (1,1) C 2 3 4),(Jogador (1,7) B 1 2 3),(Jogador (7,1) E 2 4 3),(Jogador (6,7) D 2 4 5)] [(DisparoCanhao 1 (7,1) C),(DisparoLaser 3 (4,7) D)]))]

--testesT2 = [ (2, (Movimenta B), (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [(Jogador (1,1) C 2 3 4),(Jogador (1,7) B 1 2 3),(Jogador (7,1) E 2 4 3),(Jogador (6,7) D 2 4 5)] []))]


--testesT2 = [ (1 ,(Movimenta C) ,(Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel, Vazia,Vazia,Bloco Indestrutivel], [Bloco Indestrutivel, Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel, Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel, Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel, Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [(Jogador (4,1) B 3 4 4),(Jogador (2,1) C 2 1 3)] []))]

--testesT2 = [ (1 ,(Movimenta C) ,(Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel, Vazia,Vazia,Bloco Indestrutivel], [Bloco Indestrutivel, Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel, Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel, Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel, Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [(Jogador (4,1) B 3 4 4),(Jogador (2,1) C 2 1 3)] []))]

--testesT2 = [ (2, (Movimenta B), (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [(Jogador (1,1) C 2 3 4),(Jogador (1,7) B 1 2 3),(Jogador (7,1) E 2 4 3),(Jogador (6,7) D 2 4 5)] []))]

--testesT2 = [ (1 ,(Dispara Canhao) ,(Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel, Vazia,Vazia,Bloco Indestrutivel], [Bloco Indestrutivel, Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel, Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel, Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel, Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [(Jogador (4,1) B 3 4 4),(Jogador (2,1) C 2 1 3)] [DisparoCanhao 1 (2,1) C]))]

--testesT2 = [3, (Movimenta C) ,(Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel, Bloco Destrutivel, Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel, Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]][(Jogador (1,1) C 2 3 4), (Jogador (3,5) E 1 2 3), (Jogador (6,5) B 6 1 7)] [(DisparoCanhao 1 (1,1) C), (DisparoLaser 2 (3,3) B)])]

---testesT2 = [ (1 ,(Movimenta C) ,(Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel, Vazia,Vazia,Bloco Indestrutivel], [Bloco Indestrutivel, Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel, Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel, Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel, Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [(Jogador (4,1) B 3 4 4),(Jogador (2,1) C 2 1 3)] []))]


-- * Funções principais da Tarefa 2.

-- | Efetua uma jogada.
jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.
jogada x (Movimenta C ) (Estado mapa jgds disp) = (Estado mapa jgds1 disp) 
                                               where
                                               jgds1= (auxListaJog jgds x (Movimenta C) mapa)
jogada x (Movimenta B) (Estado mapa jgds disp)= (Estado mapa jgds2 disp)
                                                where 
                                               jgds2= (auxListaJog jgds x (Movimenta B) mapa)

jogada x (Movimenta D)(Estado mapa jgds disp)= (Estado mapa jgds3 disp)
                                               where 
                                                jgds3= (auxListaJog jgds x (Movimenta D) mapa)
jogada x (Movimenta E)(Estado mapa jgds disp)= (Estado mapa jgds4 disp)
                                                where 
                                                jgds4= (auxListaJog jgds x (Movimenta E) mapa)


jogada x (Dispara Canhao) (Estado mapa jgds disp)= (Estado mapa jgds disp1)
                                                  where                                                    
                                                    disp1 = auxListaDisparo (DisparoCanhao x pos dir ) disp
                                                    pos= auxPosDisparo (auxEscolheJ jgds x )
                                                    dir=(direcaoJogador (auxEscolheJ jgds x ))
jogada x (Dispara Laser) (Estado mapa jgds disp)= (Estado mapa jgdsatualizado2 disp2)
                                                  where 
                                                    jgdsatualizado2= auxAtualizaDisp jgds x (Dispara Laser)
                                                    disp2=  auxListaDisparo (DisparoLaser x pos dir ) disp
                                                    pos= auxPosDisparo (auxEscolheJ jgds x) 
                                                    dir= (direcaoJogador (auxEscolheJ jgds x))

jogada x (Dispara Choque)(Estado mapa jgds disp)=(Estado mapa jgdsatualizado3 disp3)
                                                  where 
                                                    jgdsatualizado3= auxAtualizaDisp jgds x (Dispara Choque)
                                                    disp3= auxListaDisparo (DisparoChoque x 5 ) disp


-- * Funções auxiliares da Tarefa 2.


-- |Esta funçao auxiliar premite-nos atualizar um dado jogador apos uma jogada do tipo Disparo Arma.
-- 
--(Devolve nos a informaçao desse jogador,apos um determinado disparo, no qual gastou uma muniçao.)
auxDisparoJog :: Jogador -- ^ O 'Jogador' que efetua a jogada
              -> Jogada  -- ^ A 'Jogada' a efetuar
              -> Jogador -- ^ O 'Jogador' resultante após jogada
auxDisparoJog (Jogador pos dir vidas laser choques) (Dispara Canhao)= Jogador pos dir vidas laser choques
auxDisparoJog (Jogador pos dir vidas laser choques)(Dispara Laser) = if laser>0 then Jogador pos dir vidas (laser -1) choques
                                                                                else Jogador pos dir vidas laser choques
auxDisparoJog (Jogador pos dir vidas laser choques) (Dispara Choque)= if choques > 0 then Jogador pos dir vidas laser (choques-1)
                                                                   else Jogador pos dir vidas laser choques

 
-- |Funçao que nos premite obter a lista de jogadores atualizada tendo em conta a nova informaçao obtida na funçao anterior.
--
--(Dada uma lista de jogadores, atualizar a informaçao obtida na auxDisparo relativamente a um dado jogador, devolvendo-nos de novo a lista, atualizada.)
auxAtualizaDisp :: [Jogador] -- ^ A lista dos Jogadores
                -> Int -- ^ O identificador do 'Jogador' de acordo com o indice da lista
                -> Jogada -- ^ A 'Jogada' a efetuar
                -> [Jogador] -- ^ A lista de Jogadores resultante após jogada
auxAtualizaDisp (h:t) x y| x== 0 = auxDisparoJog h y : t 
                         | otherwise= h : auxAtualizaDisp t (x-1) y 


-- | Da nos um determinado jogador, da lista de jogadores, conforme a sua posiçao na mesma. 
auxEscolheJ ::[Jogador] -- ^Lista dos Jogadores 
            -> Int -- ^ Identificador do 'Jogador' de acordo com o indice da lista 
            -> Jogador -- ^ 'Jogador' escolhido
auxEscolheJ (h:t) x | x== 0 = h 
                    |otherwise= auxEscolheJ t (x-1)


-- |Dado um disparo, este posiciona-se na posiçao em frente do tanque que o realizou, conforme a sua direçao. 
auxPosDisparo ::Jogador -- ^ O 'Jogador'
              -> Posicao -- ^ 'Posicao' do 'Disparo'
auxPosDisparo (Jogador (x,y) dir vd lsr chq) | dir==C = (x-1,y)
                                             | dir==D = (x,y+1)
                                             | dir==B = (x+1,y)
                                             | dir==E = (x,y-1)

-- |Apos uma jogada do tipo 'Disparo' a lista correspondente aos disparos dos 'Jogadores' é atualizada.
--
-- (É lhe acrescentada o disparo anteriormente efetuada por um 'Jogador')
auxListaDisparo :: Disparo -- ^ 'Disparo' efetuado por um jogador 
                -> [Disparo] -- ^ Lista anterior
                -> [Disparo] -- ^ Lista resultante apos disparo
auxListaDisparo (DisparoCanhao n pos dir) l = l++[DisparoCanhao n pos dir]
auxListaDisparo (DisparoLaser n pos dir ) l =l++ [DisparoLaser n pos dir]
auxListaDisparo (DisparoChoque x tempo) l = l++[DisparoChoque x tempo] 

-- | Funcao que em conjunto com auxPossivelMove e, consquentemente, a auxPossivel2 e coordenadalista, no diz se um dado jogador, dada uma 'Jogada' do tipo Movimenta, pode efetuar tal jogada, e se sim alterando a sua posiçao e atualiznado a nova informacao na lista de jogadores.
auxListaJog :: [Jogador] -- ^Lista dos jogadores
            -> Int -- ^ Identificador do 'Jogador' conforme o indice da lista
            -> Jogada -- ^'Jogada' a efetuar
            ->Mapa -- ^ Mapa atual onde decorre o jogo
            -> [Jogador] -- ^ Lista dos jogadores resultante apos jogada
auxListaJog (h:t) x y map | x==0 = (auxPossivelMove h (h:t) y map) : t 
                          | otherwise = h : auxListaJog t (x-1) y map 



-- | Da nos a nova 'Posiao' de um 'Jogador' apos uma 'Jogada' do tipo Movimenta valida 
auxPossivelMove :: Jogador
                -> [Jogador]
                -> Jogada
                -> Mapa
                -> Jogador
auxPossivelMove (Jogador (x,y) dir 0 lsr chq) l _ map = (Jogador (x,y) dir 0 lsr chq)--CASO ELE ESTEJA MORTO JOGADA NAO TEM EFEITO
auxPossivelMove (Jogador (x,y) dir vd lsr chq ) ((Jogador (x1,y1) dir2 vd2 lsr2 chq2):t) (Movimenta C) map | dir== C && (encontraPosicaoMatriz (x-1,y) map == Vazia) && (encontraPosicaoMatriz(x-1,y+1) map == Vazia) || dir==C && (vd2== 0 && x1==x-2 && y1==y)|| (vd2 == 0 && x1== x-2 && y1==y+1) = (Jogador (x-1,y) dir vd lsr chq)
                                                                             | otherwise= Jogador (x,y) C vd lsr chq
auxPossivelMove (Jogador (x,y) dir vd lsr chq ) ((Jogador (x1,y1) dir2 vd2 lsr2 chq2):t) (Movimenta D) map | dir== D && (encontraPosicaoMatriz (x,y+2) map == Vazia) && (encontraPosicaoMatriz(x+1,y+2) map == Vazia) || dir==C && (vd2== 0 && x1==x-2 && y1==y)|| (vd2 == 0 && x1== x-2 && y1==y+1) = (Jogador (x,y+1) dir vd lsr chq)
                                                                             | otherwise = Jogador (x,y) D vd lsr chq
auxPossivelMove (Jogador (x,y) dir vd lsr chq ) ((Jogador (x1,y1) dir2 vd2 lsr2 chq2):t) (Movimenta B) map | dir == B && (encontraPosicaoMatriz (x+2,y) map == Vazia) && (encontraPosicaoMatriz(x+2,y+1) map == Vazia) || dir==C && (vd2== 0 && x1==x-2 && y1==y)|| (vd2 == 0 && x1== x-2 && y1==y+1) = (Jogador (x+1,y) dir vd lsr chq)
                                                                             |otherwise= Jogador (x,y) B vd lsr chq
auxPossivelMove (Jogador (x,y) dir vd lsr chq ) ((Jogador (x1,y1) dir2 vd2 lsr2 chq2):t) (Movimenta E) map | dir == E && (encontraPosicaoMatriz (x,y-1) map == Vazia) && (encontraPosicaoMatriz(x+1,y-1) map == Vazia) || dir==C && (vd2== 0 && x1==x-2 && y1==y)|| (vd2 == 0 && x1== x-2 && y1==y+1) = (Jogador (x,y-1) dir vd lsr chq)
                                                                             |otherwise = Jogador (x,y) E vd lsr chq
auxPossivelMove x l m map   | (auxPossivel2 x l ==True) = x


-- | Dado um 'Jogador', verifica se existe agum outro 'Jogador' perto da sua area,tendo em conta a direcao em que o mesmo se encontra.
auxPossivel2 :: Jogador
             -> [Jogador]
             -> Bool
auxPossivel2 (Jogador (x,y) dir vidas laser choques ) (h:t)| (dir==C && ( coordenadalista (x-2,y-1) (h:t) || coordenadalista (x-2,y) (h:t) || coordenadalista (x-2,y+1) (h:t) ) == True ) = True
                                                           | (dir==D && ( coordenadalista (x-1,y+2) (h:t) || coordenadalista (x,y+2) (h:t) || coordenadalista (x+1,y+2) (h:t) ) == True ) = True
                                                           | (dir==B && ( coordenadalista (x+2,y-1) (h:t) || coordenadalista (x+2,y) (h:t) || coordenadalista (x+2,y+1) (h:t) ) == True ) = True
                                                           | (dir==E && ( coordenadalista (x-1,y-2) (h:t) || coordenadalista (x,y-2) (h:t) || coordenadalista (x+1,y-2) (h:t) ) == True ) = True 

-- | Verifica se , dada uma posicao, algum 'Jogador' lá se encontra.
coordenadalista :: Posicao -- ^ 'Posicao' a verificar
                -> [Jogador] -- ^ dLista de todos od jogadores
                -> Bool -- ^Verificaçao da posicao como livre ou ocupada
coordenadalista  (x,y) [] = False 
coordenadalista (x,y) ( (Jogador (x1,y1) dir vidas laser choques):t) | x==x1 && y==y1 = True 
                                                                     | otherwise = coordenadalista (x,y) t   

-- | Verifica se um certo Jogador se encontra na area de choque 
auxApanhaChq::Posicao-> [Jogador]-> [Bool]
auxApanhaChq (x,y) []= [False] 
auxApanhaChq (x,y) ((Jogador (x1,y1) dir vd lsr chq):t) | ((x-3) < x1 &&  x1 < (x+3) && (y-3)< y1 && y1<(y+3)) = True : auxApanhaChq (x,y) t
                                                        | otherwise= auxApanhaChq (x,y) t 


---auxMoveChq::Jogador -> [Jogador]-> Jogada -> Jogador
---auxMoveChq  Jogador (x,y) dir vd lsr chq ((Jogador (x1,y1) dir vd lsr chq):t) (Movimenta C) | (auxApanhaChq (x,y) ((Jogador (x1,y1) dir vd lsr chq):t) == True && (x,y) == (x1,y1)) = Jogador (x1,y1) C (vd-1) lsr chq

