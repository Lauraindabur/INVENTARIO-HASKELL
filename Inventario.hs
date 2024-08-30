import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception
import Control.DeepSeq (deepseq)

--DEFINIMOS TIPO DE DATO ITEM
data Item = Item {
    nombre :: String,
    categoria :: String
} deriving (Show, Read)

--REGISTRAR LA ENTRADA DE UN ARTICULO
registrarEntrada :: String -> String -> [Item] -> [Item]
registrarEntrada nombreItem categoriaItem inventario=
    Item nombreItem categoriaItem : inventario

--BUSCAR ARTICULO POR CATEGORIA
buscarItem :: String -> [Item] -> [Item]
buscarItem categoriaItem inventario =
    filter  (\i -> categoriaItem == categoria i) inventario

--MOSTRAR ARTICULO
mostrarItem :: Item -> String
mostrarItem (Item nombre categoria) =
    "Item {nombre = \"" ++ nombre ++ "\", categoria = \"" ++ categoria ++ "\"}"


-- MOSTRAR LISTA DE ARTICULOS
listarItems :: [Item] -> IO ()
listarItems [] = putStrLn "No hay articulos en esa categoria"
listarItems articulos = do
    putStrLn "Articulos en la categoria:"
    mapM_ (putStrLn . mostrarItem) articulos

-----------------------FUNCIONES TXT--------------------------
--GUARDAR INFO EN TXT BODEGA 
guardarInventario :: [Item] -> IO ()
guardarInventario inventario = do
    withFile "bodega.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map mostrarItem inventario))
    putStrLn "Inventario guardado en el archivo bodega.txt."

--CARGAR LA INFO DEL TXT BODEGA
cargarInventario :: IO [Item]
cargarInventario = do
    contenido <- withFile "bodega.txt" ReadMode $ \h -> do
        contenido <- hGetContents h
        contenido `deepseq` return contenido
    let lineas = lines contenido
    return (map leerItem lineas)
    where
        leerItem linea = read linea :: Item


main :: IO()
main = do
    inventario <- cargarInventario
    putStrLn "Bienvenido a nuestro Sistema de Gestion de Inventario"

    --ciclo prinicipal llamado inventario
    cicloPrincipal inventario

cicloPrincipal :: [Item] -> IO()
cicloPrincipal inventario = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de artículo"
    putStrLn "2. Buscar artículos por categoría"
    putStrLn "3. Listar todos los artículos"
    putStrLn "4. Mostrar cantidad de artículos por categoría"
    putStrLn "5. Salir :("

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el nombre del articulo"
            nombreItem <- getLine
            putStrLn "Ingrese la categoria del articulo"
            categoriaItem <- getLine
            let inventarioActualizar = registrarEntrada nombreItem categoriaItem inventario
            putStrLn $ "Articulo " ++ nombreItem ++ " y categoria " ++ categoriaItem ++ " registrado en el inventario"
            guardarInventario inventarioActualizar
            cicloPrincipal inventarioActualizar 
        "2" -> do
            putStrLn "Ingresa la categoria a buscar:"
            categoriaItem <- getLine
            let listaItem = buscarItem categoriaItem inventario
            listarItems listaItem
            cicloPrincipal inventario
        "3" -> do
            listarItems inventario
            cicloPrincipal inventario
        "4" -> do 
            putStrLn "Ingrese la categoria para mostrar su cantidad de articulos: "
            categoriaItem <- getLine
            let cantidad = length $ buscarItem categoriaItem inventario
            putStrLn $ "Hay " ++ show cantidad ++ " articulos en la categoria " ++ categoriaItem
            cicloPrincipal inventario

        "5"-> putStrLn "Hasta la proxima :)"
        _ -> do
            putStrLn "Opción no válida, por favor selecciona una opción válida"
            cicloPrincipal inventario



