


-- | Este módulo define funções comuns da Tarefa 5 do trabalho prático.
module Main where

import Tarefa0_2018li1g107
import Tarefa2_2018li1g107
import Tarefa1_2018li1g107
import Tarefa4_2018li1g107
import LI11819
import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
import Graphics.Gloss.Data.Color


---------------------------------------------------Relatorio-----------------------------------------------------------------------

--
-- * Introduçao:
--
-- $intro
--
--É nesta tarefa que se trata da parte gráfica do jogo, sendo aqui onde ele ganha vida, tendo como base todas as outras tarefas
--mas também a biblioteca gloss.

-- * Objetivos:
--
-- $desenv
--
--Apos atribuir imagens próprias para cada objeto em jogo, desde as pecas do mapa, aos jogadores e disparos começamos por 
--definir uma janela onde se visualizará o jogo, e um fundo para o mesmo, neste caso de cor cinza.
--Foram criadas tambem funcoes auxliares capazes de “imprimir” corretamente essas mesmas imagens no jogo tendo em conta 
--as novas posições no gloss. 
--Foi também aqui que definimos as funções responsáveis pela simulação do movimento dos jogadores, sendo que a cada um 
--fora atribuído um conjunto de cursores associados as direções e aos disparos. 
--Nesta tarefa tambem criámos o mapa principal onde o jogo ira decorrer.
--Exemplo: <<imagesexemplo.png img>>

-- * Conclusao:
--
-- $conc
--
--Foi nesta tarefa que através da função play podemos observar o funcionamento do nosso jogo pela primeira vez,
--o que nos permitiu reparar em erros que outra hora não identificamos.
--Sendo um deles associado a tarefa dois, o que faz com que o movimento do tanque para baixo e para a direita não esteja 100% 
--correto, uma vez, que para essas direções, ele avança duas posições numa vez só.
--No entanto graças a tarefa 5 conseguimos entender melhor o nosso objetivo e tentamos melhorar a jogabilidade do mesmo.
--
--Module : Tarefa5_2018li1g107
--
--ID : Maria Sofia Martinho Gonçalves Jordao Marques (a87963);
--     Jose Nuno Baptista Martins (a90122)
--
--Data : 28.12.2018
--
--Universidade do Minho






estadoTeste1 = Estado mapaJogo1 [Jogador (1,1) D 5 5 5, Jogador (13,2) C 5 5 5, Jogador (1,20) B 5 5 5, Jogador (5,14) B 5 5 5] [DisparoLaser 0 (3,1) B]
estadoTesteIni = Estado mapaJogo1 [Jogador (1,1) D 5 5 5, Jogador (13,2) C 5 5 5, Jogador (1,20) B 5 5 5, Jogador (5,14) B 5 5 5] []

-- | Função principal da Tarefa 5.
--
-- __NB:__ Esta Tarefa é completamente livre. Deve utilizar a biblioteca <http://hackage.haskell.org/package/gloss gloss> para animar o jogo, e reutilizar __de forma completa__ as funções das tarefas anteriores.
main :: IO ()
main = do
    blocoIndest <- loadBMP "./../indestrutivel2.bmp"
    blocoDest <- loadBMP "./../destrutivel2.bmp"
    vazia <- loadBMP "./../vazia2.bmp"
    Just jgd1 <- loadJuicy "./../tankfinal.png"
    Just jgd2 <- loadJuicy "./../tankfinal2.png"
    Just jgd3 <- loadJuicy "./../tankfinal3.png"
    Just jgd4 <- loadJuicy "./../tankfinal4.png"
    Just tiroCh <- loadJuicy "./../chqmm.png"
    Just tiroLs <- loadJuicy "./../laser.png"
    Just tiroCa <- loadJuicy "./../fgt.png"


    let listaPics = [blocoDest,blocoIndest,vazia]
        listaPics2 = [tiroCa, tiroLs, tiroCh]
        listaPicsTank = [jgd1,jgd2,jgd3,jgd4]


        desenhaTudo:: Estado -> Picture
        desenhaTudo estado = translate (-200) (100) (scale 0.5 0.5 (Pictures ( desenhaMap listaPics (mapaEstado estado) glossinicial (0,0) []
                                        ++desenhaJog 0 (jogadoresEstado estado) listaPicsTank 
                                        ++ desenhaDisp (disparosEstado estado) listaPics2 (mapaEstado estado) (jogadoresEstado estado) [] )))

    --display window2 (greyN 0.5) (desenhaTudo estadoTeste1)
    play window (greyN 0.5) fr estadoTesteIni desenhaTudo movereact reageTp


 ---play window-- janela onde irá correr o jogo
      --  (greyN 0.5)               -- côr do fundo da janela
       -- fr                        -- frame rate
       -- (estadoGlossInicial bola) -- estado inicial
       -- desenhaEstadoGloss        -- desenha o estado do jogo
       -- reageEventoGloss          -- reage a um evento
       -- reageTempoGloss           -- reage ao passar do tempo

-- |Funçao responsavel pelo simulaçao dos movimentos dos jogadores.
movereact:: Event -> Estado -> Estado
movereact (EventKey (SpecialKey KeyUp) Down _ _) estadoreact = jogada 0 (Movimenta C) estadoreact
movereact (EventKey (SpecialKey KeyDown) Down _ _) estadoreact = jogada 0 (Movimenta B) estadoreact
movereact (EventKey (SpecialKey KeyLeft) Down _ _) estadoreact = jogada 0 (Movimenta E) estadoreact
movereact (EventKey (SpecialKey KeyRight) Down _ _) estadoreact = jogada 0 (Movimenta D) estadoreact
movereact (EventKey (Char '1') Down _ _) estadoreact = jogada 0 (Dispara Canhao) estadoreact
movereact (EventKey (Char '2') Down _ _) estadoreact = jogada 0 (Dispara Laser) estadoreact
movereact (EventKey (Char '3') Down _ _) estadoreact = jogada 0 (Dispara Choque) estadoreact

movereact (EventKey (Char 'w') Down _ _) estadoreact = jogada 1 (Movimenta C) estadoreact
movereact (EventKey (Char 's') Down _ _) estadoreact = jogada 1 (Movimenta B) estadoreact
movereact (EventKey (Char 'd') Down _ _) estadoreact = jogada 1 (Movimenta D) estadoreact
movereact (EventKey (Char 'a') Down _ _) estadoreact = jogada 1 (Movimenta E) estadoreact
movereact (EventKey (Char 'z') Down _ _) estadoreact = jogada 1 (Dispara Canhao) estadoreact
movereact (EventKey (Char 'x') Down _ _) estadoreact = jogada 1 (Dispara Laser) estadoreact
movereact (EventKey (Char 'c') Down _ _) estadoreact = jogada 1 (Dispara Choque) estadoreact

movereact (EventKey (Char 't') Down _ _) estadoreact = jogada 2 (Movimenta C) estadoreact
movereact (EventKey (Char 'g') Down _ _) estadoreact = jogada 2 (Movimenta B) estadoreact
movereact (EventKey (Char 'h') Down _ _) estadoreact = jogada 2 (Movimenta D) estadoreact
movereact (EventKey (Char 'f') Down _ _) estadoreact = jogada 2 (Movimenta E) estadoreact
movereact (EventKey (Char 'v') Down _ _) estadoreact = jogada 2 (Dispara Canhao) estadoreact
movereact (EventKey (Char 'b') Down _ _) estadoreact = jogada 2 (Dispara Laser) estadoreact
movereact (EventKey (Char 'n') Down _ _) estadoreact = jogada 2 (Dispara Choque) estadoreact

movereact (EventKey (Char 'i') Down _ _) estadoreact = jogada 3 (Movimenta C) estadoreact
movereact (EventKey (Char 'k') Down _ _) estadoreact = jogada 3 (Movimenta B) estadoreact
movereact (EventKey (Char 'l') Down _ _) estadoreact = jogada 3 (Movimenta D) estadoreact
movereact (EventKey (Char 'j') Down _ _) estadoreact = jogada 3 (Movimenta E) estadoreact
movereact (EventKey (Char ',') Down _ _) estadoreact = jogada 3 (Dispara Canhao) estadoreact
movereact (EventKey (Char '.') Down _ _) estadoreact = jogada 3 (Dispara Laser) estadoreact
movereact (EventKey (Char '-') Down _ _) estadoreact = jogada 3 (Dispara Choque) estadoreact

movereact _ estadoreact = estadoreact


-- | Posiçao inicial do gloss, tendo em conta a resoluçao do ecra.
glossinicial = (-683,384)

-- | Desenha o 'Mapa' do novo jogo.
---- [Destrutivel,Indestrutivel,Vazia] esta e a ordem da lista das imagens 
desenhaMap :: [Picture] -> Mapa -> Posicao -> Posicao -> [Picture] -> [Picture]
desenhaMap l mapa pos1 (x,y) acumuladorMapa
        | x==length mapa = acumuladorMapa                     
        | y< length (head mapa)-1 && (encontraPosicaoMatriz (x,y) mapa == Bloco Indestrutivel) = desenhaMap l mapa pos1 (x,y+1) novoAcumulador1         
        | y== length (head mapa)-1 && (encontraPosicaoMatriz (x,y) mapa == Bloco Indestrutivel) = desenhaMap l mapa pos1 (x+1,0) novoAcumulador1 
        | y< length (head mapa)-1 &&  (encontraPosicaoMatriz (x,y) mapa == Bloco Destrutivel) = desenhaMap l mapa pos1 (x,y+1) novoAcumulador2  
        | y== length (head mapa)-1 &&  (encontraPosicaoMatriz (x,y) mapa == Bloco Destrutivel)= desenhaMap l mapa pos1 (x+1,0) novoAcumulador2  
        | y< length (head mapa)-1 &&  (encontraPosicaoMatriz (x,y) mapa == Vazia) =desenhaMap l mapa pos1 (x,y+1) novoAcumulador3
        | y == length (head mapa)-1 && (encontraPosicaoMatriz (x,y) mapa == Vazia) =desenhaMap l mapa pos1 (x+1,0) novoAcumulador3
        where
        (xgloss,ygloss)=newgloss pos1 (x,y)
        novoAcumulador1 = acumuladorMapa ++ [translate (fromIntegral xgloss) (fromIntegral ygloss) ( encontraIndiceLista 1 l ) ]
        novoAcumulador2 = acumuladorMapa ++ [translate (fromIntegral xgloss) (fromIntegral ygloss) ( encontraIndiceLista 0 l ) ]
        novoAcumulador3 = acumuladorMapa ++ [translate (fromIntegral xgloss) (fromIntegral ygloss) ( encontraIndiceLista 2 l ) ]


-- | Funcao auxiliar da funcao desenhaDisp, que desenha 'Disparo canhao' no mapa do jogo.
--- [Disparo Canhao, Disparo Laser, Disparo Choque]
desenhaDispCa::Disparo 
             -> [Picture] 
             -> [Picture]
desenhaDispCa (DisparoCanhao n pos dir) disp
       | (dir== D)= [translate (fromIntegral xgloss) (fromIntegral ygloss) (rotate 90 disparoImg) ]
       | (dir== B)= [translate (fromIntegral xgloss) (fromIntegral ygloss) (rotate 180 disparoImg) ]
       | (dir== E)= [translate (fromIntegral xgloss) (fromIntegral ygloss) (rotate (-90) disparoImg) ]
       | (dir== C)= [translate (fromIntegral xgloss) (fromIntegral ygloss) disparoImg ]
       where 
       (xgloss,ygloss)= nwglossdispca glossinicial (posicaoDisparo (DisparoCanhao n pos dir)) (direcaoDisparo (DisparoCanhao n pos dir))  
       disparoImg= encontraIndiceLista 0 disp    

-- | Funçao auxiliar da funcao desenhaDisp, que desenha o 'Disparo Laser' no jogo.
desenhaDispLs::Disparo
             -> [Picture]
             -> Mapa
             -> [Picture]
             -> [Picture]
desenhaDispLs (DisparoLaser n (x,y) dir) disp mapa acm
         | (encontraPosicaoMatriz (x,y) mapa /= Bloco Indestrutivel) && (dir== C) = desenhaDispLs (DisparoLaser n (x-1,y) dir) disp mapa novoacm
         | (encontraPosicaoMatriz(x,y) mapa /= Bloco Indestrutivel) && (dir== B) = desenhaDispLs (DisparoLaser n (x+1,y) dir) disp mapa novoacm2
         | (encontraPosicaoMatriz(x,y) mapa /= Bloco Indestrutivel) && (dir== D) = desenhaDispLs (DisparoLaser n (x,y+1) dir) disp mapa novoacm3
         | (encontraPosicaoMatriz(x,y) mapa /= Bloco Indestrutivel) && (dir== E) = desenhaDispLs (DisparoLaser n (x,y-1) dir) disp mapa novoacm4
         | otherwise = acm
         where 
          (xgloss,ygloss)= nwglossdispls glossinicial (posicaoDisparo (DisparoLaser n (x,y) dir)) (direcaoDisparo (DisparoLaser n (x,y) dir))  
          disparoImg= encontraIndiceLista 1 disp
          novoacm= acm ++ [translate (fromIntegral xgloss) (fromIntegral ygloss) (rotate (-90) disparoImg) ]  
          novoacm2 = acm++ [translate (fromIntegral xgloss) (fromIntegral ygloss) (rotate 90 disparoImg) ]
          novoacm3 = acm ++ [translate (fromIntegral xgloss) (fromIntegral ygloss) disparoImg ]
          novoacm4 = acm ++ [translate (fromIntegral xgloss) (fromIntegral ygloss) (rotate 180 disparoImg) ]

-- |  Funçao auxiliar da funcao desenhaDisp, que desenha 'Disparo Choque' no mapa.
desenhaDispCh:: Disparo
             -> [Jogador] 
             -> [Picture]
             -> [Picture]
desenhaDispCh diCh jogs disp
        =  [translate (fromIntegral xgloss) (fromIntegral ygloss) disparoImg]
         where 
          (xgloss,ygloss)= nwglossdispch_Tank glossinicial (posicaoJogador jogChoque)
          jogChoque=jogs!!(jogadorDisparo diCh)  
          disparoImg= encontraIndiceLista 2 disp



-- | Funçao geral que desenha, individualmente, os diferentes disparos no mapa.
desenhaDisp:: [Disparo]
           -> [Picture]
           -> Mapa 
           -> [Jogador]
           -> [Picture] 
           ->[Picture]
desenhaDisp [] _ _ _ _= []
desenhaDisp [DisparoCanhao n pos dir] pic mapa jg i = desenhaDispCa (DisparoCanhao n pos dir) pic 
desenhaDisp [DisparoLaser n pos dir] pic mapa jg i = desenhaDispLs (DisparoLaser n pos dir) pic mapa i
desenhaDisp [DisparoChoque n tempo] pic mapa jg i = desenhaDispCh (DisparoChoque n tempo) jg pic
desenhaDisp ((DisparoCanhao n pos dir):t) pic mapa jg i = (desenhaDispCa (DisparoCanhao n pos dir) pic) ++ desenhaDisp t pic mapa jg i
desenhaDisp ((DisparoLaser n pos dir):t) pic mapa jg i = (desenhaDispLs (DisparoLaser n pos dir) pic mapa i ) ++ desenhaDisp t pic mapa jg i
desenhaDisp ((DisparoChoque n tempo):t) pic mapa jg i = (desenhaDispCh (DisparoChoque n tempo) jg pic) ++ desenhaDisp t pic mapa jg i


-- | Funçao que desenha os 'Jogadores' no 'Mapa' do jogo.
desenhaJog:: Int 
          -> [Jogador] 
          -> [Picture] 
          -> [Picture]
desenhaJog n [] tank = []
desenhaJog n ((Jogador pos dir vd lsr chq): t) tank 
    = if vd < 0 then desenhaJog (n+1) t tank 
        else translate (fromIntegral xgloss) (fromIntegral ygloss) tankImg : desenhaJog (n+1) t tank
    where 
    (xgloss,ygloss)=nwglossdispch_Tank glossinicial pos
    tankImg | dir==C = rotate 180 (encontraIndiceLista n tank)
            | dir==B = encontraIndiceLista n tank
            | dir==D = rotate (-90) (encontraIndiceLista n tank)
            | dir==E = rotate 90 (encontraIndiceLista n tank) 

-- | Simulaçao do tempo de jogo.
reageTp:: Float 
       -> Estado 
       -> Estado
reageTp  n (Estado mapa jgd disp)= tick (Estado mapa jgd disp)

-- | Frame rate do jogo.
fr::Int
fr= 1
-- | Janela de visualizaçao do jogo em FullScreen.
window :: Display
window  = FullScreen

-- | Janela de vizualizaçao.
window2 :: Display
window2 = InWindow "M&M" (1000,600) (10,10)

-- | Funcao que dada uma posicao do mapa retorna a sua posicao na janela gloss
newgloss:: Posicao -- ^ pos glos inicial
        -> Posicao -- ^ pos mapa
        -> Posicao
newgloss (x,y) (x1,y1) = (x+y1*81,y-x1*81) --posicao x.y depende da resolucao de ecra, a pos x1,y1 e a nova posiçao na matriz e o 81 e o tamanho dos blocos em pixeis 


-- | Funçao responsavel por atribuir a nova posiçao do tanque,e neste caso do 'Disparo Choque' tambem, na nova posiçao gloss.
nwglossdispch_Tank:: Posicao
                  -> Posicao
                  -> Posicao
nwglossdispch_Tank (x,y) (x1,y1) = (x+y1*81+ 41 , y-x1*81- 41)


--- 1º posicao corresponde a posicao gloss e a segunda a nova posiçao.
-- | Funçao auxiliar responsalvel por atribuir a nova posiçao do 'Disparo Canhao' em posiçao gloss.
nwglossdispca:: Posicao 
             -> Posicao 
             -> Direcao 
             -> Posicao
nwglossdispca (x,y) (x1,y1) D = (x+y1*81+31 , y-x1*81-41)  
nwglossdispca (x,y) (x1,y1) B = (x+y1*81+41 , y-x1*81-31)
nwglossdispca (x,y) (x1,y1) E = (x+y1*81+51 , y-x1*81-41)
nwglossdispca (x,y) (x1,y1) C = (x+y1*81+41, y-x1*81-51)

-- | Semelhante as funçoes anteriores, esta funçao atribui ao 'Disparo Laser' uma nova possiçao em gloss.
nwglossdispls:: Posicao -> Posicao -> Direcao -> Posicao 
nwglossdispls (x,y) (x1,y1) E = (x+y1*81+81, y-x1*81-41)  
nwglossdispls (x,y) (x1,y1) D = (x+y1*81, y-x1*81-41)  
nwglossdispls (x,y) (x1,y1) C = (x+y1*81+41, y-x1*81-81)
nwglossdispls (x,y) (x1,y1) B = (x+y1*81+41, y-x1*81)    

-- | Define a cor do fundo da janela de jogo.
background ::Color
background = greyN 0.5



-- | Mapa para teste.
mapateste1 = [
                [x,x,x,x,x,x],
                [x,v,v,d,v,x],
                [x,v,v,d,v,x],
                [x,v,v,d,v,x],
                [x,v,v,d,v,x],
                [x,x,x,x,x,x]
             ]
            where
            x=Bloco Indestrutivel
            d=Bloco Destrutivel
            v=Vazia


-- | Funçao que constroi o mapa onde o jogo irá decorrer.
mapaJogo1:: Mapa
mapaJogo1 = constroi [Move E,Move B,Move B,Move B,Move E,Move E,Move E,Move E,
                     Move E,Move B,Move B,Move D,Move B,Move B,Move E,Move E,
                     Move E,Move E,Move D,Move D,Move D,Desenha,Move C,Move E,
                     Move C,Move D,Desenha,Roda,Move E,Move E,MudaParede,Move E,
                     Move B,Move C,Desenha,Move C,Desenha,Move C,Move D,Roda,Move C,
                     Move C,Move D,Move E,Move C,Move D,Desenha,Move B,Move D,Move D,
                     Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move D,
                     Move D,Move D,Move D,Move D,Move D,Move E,Move B,Move B,Move B,
                     Move B,Move B,Move B,Move B,Move B,Move B,Move E,Move E,Move E,
                     Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,
                     Move E,Move E,Move E,Move E,Move E,Move E,Move C,Move C,Move C,
                     Move C,Roda,Roda,Roda,Move C,Desenha,Move C,Desenha,Move B,Move D,
                     Move D,Move D,Move D,Move D,Move D,Move B,Roda,MudaParede,Move D,
                     Move B,Move B,Move B,Move B,Move D,Desenha,Move C,Move C,Move C,
                     Move C,Desenha,Move C,Move C,MudaTetromino,Move D,Desenha,Move D,
                     Move D,Roda,Move D,Move D,Move C,Move E,Roda,MudaTetromino,MudaTetromino,
                     Move D,Move D,Move D,Move D,Roda,Desenha,Move B,Desenha,Move D,Move D,
                     Desenha,Move C,Desenha,Move E,Move E,Move B,Move B,Desenha,Move D,Desenha,
                     Move D,Desenha,Move B,Desenha,Move B,Desenha,Move B,Desenha,Move E,MudaParede,
                     Move E,Move E,Move C,Move D,Move B,Move E,Move E,Move E,Move E,Move E,Move E,
                     Desenha,Move D,Move D,Move D,Move D,Move E,MudaTetromino,Move E,Move C,Move C,
                     Move C,Move E,Move C,MudaTetromino,Move C,Move C,Move C,Move E,Move B,Move D,
                     Move B,Move C,Desenha,MudaTetromino,MudaTetromino,Move C,Move C,MudaTetromino,
                     Move D,Move D,Move D,Move D,Move D,Move B,Desenha,Move B,Move B,Move B,Move B,
                     Move B,Move B,Move B,Move B,Move B,Roda,Move D,Desenha,MudaTetromino,Move D,
                     Move D,Move D,Move D,Roda,Move E,Move C,Roda,MudaTetromino,MudaTetromino,
                     MudaTetromino,MudaTetromino,MudaTetromino,Roda,Desenha,MudaTetromino,Roda,
                     MudaParede,Move C,Move C,Move C,Move C,Move C,Move E,Move E,Move E,Move E,
                     Move E,Move E,Move B,Move B,Move B,Move B,Move B,Move E,Desenha,Move C,Move C,
                     Move C,Move C,Move C,Move C,Move B,Move C,Move C,Move C,Move C,Move C,Move B,
                     MudaTetromino,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move C,Move E,
                     Desenha,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move D,
                     Move D,Move D,Move D,Move D,Move D,Move B,Move B,Move B]       




