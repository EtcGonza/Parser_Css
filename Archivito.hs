
--data Tipo a = Clase | Id | Etiqueta deriving (Show)
data Css a = Elemento a (Css a ) deriving (Show) | Prop | Vacio

elemento h1 -> elemento -> elemento -> [font-size: 10px] -> Vacio

-- El tipo se define como ".fondo-azul" Vacio
gCss :: String -> Css String  -> Css String 
gCss a Vacio = Elemento a Vacio
gCss b (Elemento a element) = Elemento a (gCss b element)

css2String :: Css String -> String
css2String Vacio = []
css2String (Elemento a _) = a

identificarTipo :: Css String -> String
identificarTipo Vacio = error "debe ingresar un valor"
identificarTipo (Elemento a Vacio) 
    | head( css2String (Elemento a Vacio) ) == '.' = "Clase"
    | head( css2String (Elemento a Vacio) ) == '#' = "Id"
    | otherwise = "Etiqueta"

escribirPropiedad :: [Char] -> Css String

insertarPropiedad :: Css String -> Css String

escribirSelector :: Css String -> IO ()
-- escribirSelector Vacio _ = putStrLn "El input es vacio, no se puede escribir."
-- escribirSelector a propiedades = appendFile "style.css" (css2String (a) ++ "{" ++ propiedades ++ "}" ++ "\n")