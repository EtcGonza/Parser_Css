
data ListCss a = Vacio |Elemento a (ListCss a ) |Propiedad a (ListCss a) deriving (Show)

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
generarCss :: ListCss String -> IO ()
generarCss Vacio = putStrLn "El input es vacio, no se puede escribir."

--generarCss listaCSS = appendFile "style.css" (css2String (a) ++ "{" ++ propiedades ++ "}" ++ "\n")


--pruebas
s1 = generarSelectores "h1" Vacio
s2 = generarSelectores "#id" s1
s3 = generarSelectores ".test" s2

p1 = generarPropiedades "color: red" Vacio
p2 = generarPropiedades "padding" p1
p3 = generarPropiedades "font-size" p2


--Funciones de apoyo
css2String :: ListCss String -> String
css2String Vacio = []
css2String (Elemento a _) = a

identificarTipo :: ListCss String -> String
identificarTipo Vacio = error "debe ingresar un valor"
identificarTipo (Elemento a Vacio) 
    | head( css2String (Elemento a Vacio) ) == '.' = "Clase"
    | head( css2String (Elemento a Vacio) ) == '#' = "Id"
    | otherwise = "Etiqueta"
