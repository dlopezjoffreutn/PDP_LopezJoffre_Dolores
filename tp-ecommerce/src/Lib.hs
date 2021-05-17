module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--Funciones a ordenar y desarrollar. Todas las funciones deben estar tipadas.

productoXL :: String -> String
productoXL producto = producto ++ " XL" 

productoCorriente :: String -> Bool
productoCorriente producto = elem (head producto) "AEIOUaeiou"  

productoDeLujo :: String -> Bool              
productoDeLujo producto = elem 'X' producto  || elem 'x' producto || elem 'Z' producto || elem 'z' producto

productoCodiciado :: String -> Bool
productoCodiciado producto = length producto > 10           

productoDeElite :: String -> Bool     
productoDeElite producto = productoDeLujo producto && productoCodiciado producto && not(productoCorriente producto)

descodiciarProducto :: String -> String 
descodiciarProducto = take 10

versionBarata :: String -> String
versionBarata = reverse.descodiciarProducto

entregaSencilla :: String ->Bool
entregaSencilla = even.length

aplicarCostoDeEnvio :: Float -> Float -> Float
aplicarCostoDeEnvio precio costoEnvio = precio + costoEnvio

aplicarDescuento :: Float -> Float -> Float
aplicarDescuento precio descuento = precio - ((descuento * 100)/precio)

precioTotal :: Float -> Float -> Float -> Float -> Float
precioTotal precio cant descuento costoEnvio = aplicarCostoDeEnvio (aplicarDescuento (precio * cant) descuento) costoEnvio


--Funciones a utilizar o no, que deben estar tipadas.
{-
    take :: Int -> String -> String
    drop :: Int -> String -> String
    head :: String -> Char
    elem :: Char -> String -> Bool
    reverse :: String -> String
-}
