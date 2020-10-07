
import Data.List

type Prop0 = (String, Int)
type Prop1 = (String, String)
type ListProp = [(String, String)]

--data Prop = Prop1 |ListProp

--Funcion que crea propiedades
crearProp :: String->Int->Prop0
crearProp [] 0 = error "Ingrese valores"
crearProp [] y = error "Ingrese valores"
crearProp x y = (x, y)


--Funcion que añade simbolo al valor de la propiedad
simboloProp :: Prop0->Prop1
--simboloProp null = error "Ingrese una propiedad"
simboloProp (x,y) 
 | x == "Color" = (x,['#']++ show y)
-- |otherwise (x,y) * agregar mas casos *


--Funcion que devuelve una lista de propiedades
insertarProp :: Prop1->ListProp->ListProp
--insertarProp null = error "No se ingresaron propiedades"
insertarProp x [] = [x]
insertarProp x [a] = a:[x]
insertarProp x (a:as) = a: (insertarProp x as)
