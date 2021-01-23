-- | Este módulo define funções comuns da Tarefa 1 do trabalho prático.
module Tarefa1_2018li1g107 where

import LI11819
import Tarefa0_2018li1g107


-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é uma sequência de 'Instrucoes'.
testesT1 :: [Instrucoes]
testesT1 = [ [MudaTetromino,Move C,Desenha,MudaTetromino,Roda,MudaTetromino],[Move D,Move D,Move C,Move C,Move D,MudaTetromino,Desenha],[Move B,Move E,Move E,Move E,MudaTetromino,Move B,Move B,Move B,Move D,Move E,Move B,Desenha,Move D,Move D,Move D,Move D,Move D,Move D,Desenha],[Move C,Move C,Move C,MudaTetromino,Move C,Move C,Move C,Desenha,Move E,Move E,Move E,Move E,Move E,Move C,Move C,Move E,Move D,Move D,Move D,Move D,Move D,Move C,Move E,Move E,Move E,Move E,Move E,Desenha],[Move B,Move B,Move B,Move B,MudaTetromino,Move D,Move B,Move E,Desenha]]
--testesT1 = [[Move D,Move D,Move B,Move B,Move B,Move B,Move B,Move B,Move E,Move C,Move C,Move E,Move E,Move C,Move C,MudaTetromino,MudaTetromino,Move D,Move D,Move D,Desenha,Move B,Move B,Move D,Move D,Move D,Move D,Move B,Move B,Move B,Move B,Move E,Move E,Move E,Move E,Move E,Move C,Move C,Move E,Move C,Move C,Move C,Move D,Desenha,Move B,Move B,Move B,Move D,Move D,Move D,Move D,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move B,Move B,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move C,Move C,Move C,Move C,MudaTetromino,Move D,MudaTetromino,MudaTetromino,Move D,MudaTetromino,Move D,Move C,Move C,Move C,Move C,Move C,Move B,Move C,Move B,Move E,Move E,Move E,MudaTetromino,Move E,Move E,Move E,MudaTetromino,Move B,MudaTetromino,Move B,Move B,Move B,Move B,Move B,MudaTetromino,Move B,Move B,Move B,Move B,Desenha,Move B,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Desenha,Move C,Move C,Move E,Move E,Desenha,Move C,Move C,Move C,Move C,Move C,Move C,Move C,Move C,Move C,Move C,Desenha,Move B,Move B,Move B,Move D,Move D,Move D,Move D,Move E,Move E,MudaTetromino,Move B,Move B,MudaTetromino,Move B,Move B,Move D,Move D,Move D,Move D,Move D,Move D,MudaTetromino,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move C,Move C,Move C,Move C,Desenha,Move B,Move E,Move E,Move E,Move B,Move B,Move B,MudaTetromino,Move B,Move B,Move D,Move D,Move D,Move D,Move B,Move B,Move D,Move D,Move D,Desenha,Move D,Move D,Move D,Move D]]
--testesT1 =  [[Move C,Roda,MudaTetromino,Roda],[MudaTetromino,MudaTetromino],[Roda,MudaTetromino,Move B,Roda,MudaParede,Desenha],[Move E,MudaTetromino,MudaParede,Desenha],[Move C,MudaParede,Desenha],[MudaTetromino,Move C,Move D, MudaParede,Desenha]]

--testesT1 = [[Move C,Roda,MudaTetromino,Roda],[MudaTetromino,MudaTetromino],[Roda,MudaTetromino,Move B,Roda,MudaParede,Desenha],[Move E,MudaTetromino,MudaParede,Desenha],[Move C,MudaParede,Desenha],[MudaTetromino,Move C,Move D, MudaParede,Desenha]]

--testesT1 =  [[Move C,Roda,MudaTetromino,Roda],[MudaTetromino,MudaTetromino],[Roda,MudaTetromino,Move B,Roda,MudaParede,Desenha],[Move E,MudaTetromino,MudaParede,Desenha],[Move C,MudaParede,Desenha],[MudaTetromino,Move C,Move D, MudaParede,Desenha]]



-- * Funções principais da Tarefa 1.

-- | Aplica uma 'Instrucao' num 'Editor'.
--
--    * 'Move' - move numa dada 'Direcao'.
--
--    * 'MudaTetromino' - seleciona a 'Peca' seguinte (usar a ordem léxica na estrutura de dados),
--       sem alterar os outros parâmetros.
--
--    * 'MudaParede' - muda o tipo de 'Parede'.
--
--    * 'Desenha' - altera o 'Mapa' para incluir o 'Tetromino' atual, sem alterar os outros parâmetros.
instrucao :: Instrucao -- ^ A 'Instrucao' a aplicar.
          -> Editor    -- ^ O 'Editor' anterior.
          -> Editor    -- ^ O 'Editor' resultante após aplicar a 'Instrucao'.

instrucao (Move C) (Editor (l,c) dir tetro parede map) = (Editor (l-1,c) dir tetro parede map)
instrucao (Move B) (Editor (l,c) dir tetro parede map) = (Editor (l+1,c) dir tetro parede map) 
instrucao (Move D) (Editor (l,c) dir tetro parede map) = (Editor (l,c+1) dir tetro parede map)
instrucao (Move E) (Editor (l,c) dir tetro parede map) = (Editor (l,c-1) dir tetro parede map)

instrucao Roda (Editor pos C tetro parede map) = Editor pos D tetro parede map
instrucao Roda (Editor pos D tetro parede map) = Editor pos B tetro parede map
instrucao Roda (Editor pos B tetro parede map) = Editor pos E tetro parede map
instrucao Roda (Editor pos E tetro parede map) = Editor pos C tetro parede map

instrucao MudaTetromino (Editor pos dir I parede map) = Editor pos dir J parede map
instrucao MudaTetromino (Editor pos dir J parede map)= Editor pos dir L parede map
instrucao MudaTetromino (Editor pos dir L parede map) = Editor pos dir O parede map
instrucao MudaTetromino (Editor pos dir O parede map) = Editor pos dir S parede map
instrucao MudaTetromino (Editor pos dir S parede map) = Editor pos dir T parede map
instrucao MudaTetromino (Editor pos dir T parede map) = Editor pos dir Z parede map
instrucao MudaTetromino (Editor pos dir Z parede map) = Editor pos dir I parede map

instrucao MudaParede (Editor pos dir tetro Indestrutivel map) =Editor pos dir tetro Destrutivel map
instrucao MudaParede (Editor pos dir tetro Destrutivel map) = Editor pos dir tetro Indestrutivel map

instrucao Desenha (Editor pos dir tetro parede map) = let
    matrizBool=auxDesenha tetro dir
    newMap=auxDesenha2 map pos pos parede matrizBool (0,0) (0,0)

    in Editor pos dir tetro parede newMap   


-- | Fixa um determinado 'Tetronimo' no mapa.
auxDesenha::Tetromino
          -> Direcao
          -> [[Bool]]
auxDesenha x C = tetrominoParaMatriz x
auxDesenha x D = rodaMatriz (tetrominoParaMatriz x) 
auxDesenha x B = rodaMatriz (auxDesenha x D)
auxDesenha x E = rodaMatriz (auxDesenha x B)


auxDesenha2 :: Mapa
            -> Posicao 
            -> Posicao 
            -> Parede 
            -> Matriz Bool 
            -> Posicao 
            -> Posicao 
            -> Mapa

auxDesenha2 mapa1 (xi,yi) (x,y) parede tetromatriz (mX,mY) (tX,tY) 
            | tX== length tetromatriz -1 && tY== length (head tetromatriz) -1 = mapa1
            | mX<x = auxDesenha2 mapa1 (xi,yi) (x,y) parede tetromatriz (mX+1,mY) (tX,tY)
            | mX==x && mY<y = auxDesenha2 mapa1 (xi,yi) (x,y) parede tetromatriz (mX,mY+1) (tX,tY)
            | mX==x && mY==y = 
                        if encontraPosicaoMatriz (tX,tY) tetromatriz == True
                           then auxDesenha2 newMap (xi,yi) (newMX,newMY) parede tetromatriz (newMX,newMY) (newTX,newTY)
                           else auxDesenha2 mapa1 (xi,yi) (newMX,newMY) parede tetromatriz (newMX,newMY) (newTX,newTY)

            where
                newMap = atualizaPosicaoMatriz (mX,mY) (Bloco parede) mapa1

                newMX | tY==length (head tetromatriz) -1 = mX+1
                      | otherwise = mX
                
                newMY | tY /= length (head tetromatriz) -1 = mY+1 
                      | otherwise = yi

                newTY | tY == length (head tetromatriz) -1 = 0
                      | otherwise = tY+1

                newTX | tY == length (head tetromatriz) -1 = tX+1
                      | otherwise = tX


-- | Aplica uma sequência de 'Instrucoes' num 'Editor'.
--
-- __NB:__ Deve chamar a função 'instrucao'.
instrucoes :: Instrucoes -- ^ As 'Instrucoes' a aplicar.
           -> Editor     -- ^ O 'Editor' anterior.
           -> Editor     -- ^ O 'Editor' resultante após aplicar as 'Instrucoes'.
instrucoes (x:xs) l = instrucoes xs ( instrucao x l)
instrucoes [] l = l

-- | Cria um 'Mapa' inicial com 'Parede's nas bordas e o resto vazio.
mapaInicial :: Dimensao -- ^ A 'Dimensao' do 'Mapa' a criar.
            -> Mapa     -- ^ O 'Mapa' resultante com a 'Dimensao' dada.
mapaInicial (l,c)= let

    mapaini= criaMatriz (l,c) (Bloco Indestrutivel)

    mapaFinal :: Mapa -> Posicao -> Mapa
    mapaFinal mapa (l,c) | c==length (head mapa)-1 && l==length mapa -2 = mapa
                         | c==length (head mapa)-1 = mapaFinal mapa (l+1,1)
                         | l>0 && l<(length mapa)-1 && c>0 && c<length (head mapa)-1 = mapaFinal (atualizaPosicaoMatriz (l,c) Vazia mapa) (l,c+1)                        
                                                 
                         
   
   in mapaFinal mapaini (1,1)   

                  


-- | Cria um 'Editor' inicial.
--
-- __NB:__ Deve chamar as funções 'mapaInicial', 'dimensaoInicial', e 'posicaoInicial'.
editorInicial :: Instrucoes  -- ^ Uma sequência de 'Instrucoes' de forma a poder calcular a  'dimensaoInicial' e a 'posicaoInicial'.
              -> Editor      -- ^ O 'Editor' inicial, usando a 'Peca' 'I' 'Indestrutivel' voltada para 'C'.
editorInicial inst = let
    
    dimensao = dimensaoInicial inst
    posIni = posicaoInicial inst
    mapaIni = mapaInicial dimensao

    in Editor posIni C I Indestrutivel mapaIni


-- | Constrói um 'Mapa' dada uma sequência de 'Instrucoes'.
--
-- __NB:__ Deve chamar as funções 'Instrucoes' e 'editorInicial'.
constroi :: Instrucoes -- ^ Uma sequência de 'Instrucoes' dadas a um 'Editor' para construir um 'Mapa'.
         -> Mapa       -- ^ O 'Mapa' resultante.
constroi is = mapaEditor ( instrucoes is (editorInicial is) )