
data ListCss a = Vacio | Elemento a (ListCss a ) | Propiedad a (ListCss a) deriving (Show)

--elemento h1 -> elemento -> elemento -> [font-size: 10px] -> Vacio
-- El tipo se define como ".fondo-azul" Vacio

--Funcion que genera una lista de selectores
generarSelectores :: String -> ListCss String  -> ListCss String 
generarSelectores a Vacio = Elemento a Vacio
generarSelectores b (Elemento a listaCss) = Elemento a (generarSelectores b listaCss)

--Funcion que genera una lista de propiedades
generarPropiedades :: String -> ListCss String  -> ListCss String 
generarPropiedades a Vacio = Propiedad a Vacio
generarPropiedades b (Propiedad a listaCss) = Propiedad a (generarPropiedades b listaCss)

--Funcion que genera una lista que luego se convertira en codigo CSS
crearListaCss :: ListCss String -> ListCss String -> ListCss String
crearListaCss (Elemento a Vacio) (Propiedad x propiedades) = Elemento a (Propiedad x propiedades)
crearListaCss (Elemento a selectores) (Propiedad x propiedades) = Elemento a (crearListaCss selectores (Propiedad x propiedades) )

--Funcion que convierte una lista CSS en codigo CSS
generarCss :: ListCss String -> ListCss String -> IO ()
generarCss Vacio Vacio = putStrLn "El input es vacio, no se puede escribir."
generarCss listaSelectores listaPropiedades = appendFile "style.css" (selectores2String (listaSelectores) ++ " {" ++ "\n" ++ (propiedades2String listaPropiedades) ++ "\n" ++ "}" ++ "\n\n")

--Funciones de apoyo
selectores2String :: ListCss String -> String
selectores2String Vacio = []
selectores2String (Elemento a Vacio) = a
selectores2String (Elemento a l) = a ++ " " ++ (selectores2String l)

propiedades2String :: ListCss String -> String
propiedades2String Vacio = []
propiedades2String (Propiedad a Vacio) = " " ++ a ++ ";"
propiedades2String (Propiedad a l) = " " ++ a ++ ";" ++ "\n" ++ (propiedades2String l)

-- generarLista :: ListCss String -> String
-- generarLista (Propiedad b Vacio) = b
-- generarLista (Propiedad b l) = " "  ++  b ++ (generarLista l)
-- generarLista (Elemento a l) = a ++ (generarLista l)

-- identificarTipo :: ListCss String -> String
-- identificarTipo Vacio = error "debe ingresar un valor"
-- identificarTipo (Elemento a Vacio) 
--     | head( css2String (Elemento a Vacio) ) == '.' = "Clase"
--     | head( css2String (Elemento a Vacio) ) == '#' = "Id"
--     | otherwise = "Etiqueta"

--pruebas
s1 = generarSelectores "h1" Vacio
s2 = generarSelectores "#id" s1
s3 = generarSelectores ".test" s2

p1 = generarPropiedades "color: red" Vacio
p2 = generarPropiedades "padding: 20px" p1
p3 = generarPropiedades "font-size: 12px" p2

-- listaCss1 = crearListaCss s1 p1
-- listaCss2 = crearListaCss s1 p2
-- listaCss3 = crearListaCss s1 p3
-- listaCss4 = crearListaCss s2 p1
-- listaCss5 = crearListaCss s2 p2
-- listaCss6 = crearListaCss s2 p3
-- listaCss7 = crearListaCss s3 p1
-- listaCss8 = crearListaCss s3 p2
-- listaCss9 = crearListaCss s3 p3

menu :: IO()
menu = do 
        -- putStrLn "\n--- --- --- --- --- --- Parser Css --- --- --- --- --- ---\n"
        putStrLn "Ingrese un selector"
        selectorIngresado <- getLine
        putStrLn "Ingrese una propiedad para el selector"
        propiedadIngresada <- getLine
        generarCss (generarSelectores selectorIngresado Vacio) (generarPropiedades propiedadIngresada Vacio)
        putStrLn "¿Desea ingresar otro selector? s/n"
        opcion <- getLine
        decision opcion

decision opcion
     | opcion == "s" = menu
     | opcion == "n" = putStrLn "Programa terminado"