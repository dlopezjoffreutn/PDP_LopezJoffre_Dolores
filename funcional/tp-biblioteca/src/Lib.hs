
module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--https://pdep-lunes.github.io/bitacora/2021/funcional/clase-03/

{-
    Terror: Escritos por Stephen King

    Manga: Escritos por un autor Japonés

    Comics: Menos de 50 paginas 
-}

generoLibro :: Libro -> String
generoLibro libro 
    | esStephenKing libro       = "Terror"
    | esAutorJapones libro      = "Manga"
    | esMenorCincuenta libro    = "Comic"

esMenorCincuenta :: Libro -> Bool
esMenorCincuenta libro = (< 50).cantidadDePaginas $ libro

esAutorJapones :: Libro -> Bool
esAutorJapones libro = elem (autor libro) autoresJaponeses


lecturaObligatoria :: Libro -> Bool
lecturaObligatoria libro = esStephenKing libro || esFundacion libro || esSagaEragon libro

esStephenKing :: Libro -> Bool
esStephenKing libro = esDeAutor "Stephen King" libro 

esFundacion :: Libro -> Bool
esFundacion libro = libro == fundacion

esSagaEragon :: Libro -> Bool
esSagaEragon libro = elem libro sagaEragon


promedioDePaginas :: Biblioteca -> Int
promedioDePaginas biblioteca = div (sumaDePaginas biblioteca) (length biblioteca)

sumaDePaginas :: Biblioteca -> Int
sumaDePaginas biblioteca = sum.map cantidadDePaginas $ biblioteca


nombreDeLaBiblioteca :: Biblioteca -> String
nombreDeLaBiblioteca biblioteca = concatMap tituloSinVocales $ biblioteca 

tituloSinVocales :: Libro -> String 
tituloSinVocales (titulo, _, _) = filter noEsVocal titulo

noEsVocal :: Char -> Bool
noEsVocal = not.(`elem` "AEIOUaeiou")


bibliotecaLigera :: Biblioteca -> Bool 
bibliotecaLigera biblioteca = all libroLigero biblioteca

libroLigero :: Libro -> Bool 
libroLigero (_, _, cantidadDePaginas) = cantidadDePaginas <= 40 


bibliotecaEsFantasiosa :: Biblioteca -> Bool 
bibliotecaEsFantasiosa biblioteca = any libroFantasioso biblioteca

libroFantasioso :: Libro -> Bool 
libroFantasioso libro = esDeAutor "Christopher Paolini" libro || esDeAutor "Neil Gaiman" libro

esDeAutor :: Autor -> Libro -> Bool
esDeAutor elAutor libro = (== elAutor) . autor $ libro 


type Titulo = String
type Autor = String
type CantidadDePaginas = Int
type Libro = (Titulo, Autor, CantidadDePaginas)
type Biblioteca = [Libro]
type Saga = [Libro]

cantidadDePaginas :: Libro -> CantidadDePaginas
cantidadDePaginas (_, _, paginas) = paginas 

autor :: Libro -> Autor
autor (_, autor, _) = autor

elVisitante :: Libro
elVisitante = ("El Visitante", "Stephen King", 592)

shingekiNoKyojin1 :: Libro
shingekiNoKyojin1 = ("Shingeki No Kyojin 1", "Hajime Isayama", 40)

shingekiNoKyojin3 :: Libro
shingekiNoKyojin3 = ("Shingeki No Kyojin 3", "Hajime Isayama", 40)

shingekiNoKyojin127 :: Libro
shingekiNoKyojin127 = ("Shingeki No Kyojin 127", "Hajime Isayama", 40)

autoresJaponeses :: [Autor]
autoresJaponeses = ["Hajime Isayama"]

fundacion :: Libro
fundacion = ("Fundacion", "Isaac Asimov", 230)

sandman5 :: Libro
sandman5 = ("Sandman 5", "Neil Gaiman", 35)
 
sandman10 :: Libro
sandman10 = ("Sandman 10", "Neil Gaiman", 35)
 
sandman12 :: Libro
sandman12 = ("Sandman 12", "Neil Gaiman", 35)
 
eragon :: Libro
eragon = ("Eragon", "Christopher Paolini", 544)
 
eldest :: Libro
eldest = ("Eldest", "Christopher Paolini", 704)
 
brisignr :: Libro
brisignr = ("Brisignr", "Christopher Paolini", 700)

legado :: Libro
legado = ("Legado", "Christopher Paolini", 811)

sagaEragon :: Saga
sagaEragon = [eragon, eldest, brisignr, legado]
 
biblioteca :: Biblioteca
biblioteca = [elVisitante, shingekiNoKyojin1, shingekiNoKyojin3, shingekiNoKyojin127, fundacion, sandman5, sandman10, sandman12, eragon, eldest, brisignr, legado]
