
--data Tipo a = Clase | Id | Etiqueta deriving (Show)
data Css a = Vacio | Elemento a (Css a ) deriving (Show)



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
