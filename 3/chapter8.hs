
type Parser a = String -> [(a, String)]

return :: a -> Parser a
return v = \inp -> [(v, inp)]

failure :: Parser a
failure = \inp -> []

item :: Parser Char
item = \inp -> case inp of
		[] -> []
		(x:xs) -> [(x, xs)]

parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp

(>==) :: Parser a -> (a -> Parser b) -> Parser b
p >== f = \inp -> case parse p inp of
			[] -> []
			[(v, out)] -> parse (f v) out

p :: Parser (Char, Char)
p = do
	x <- item
	item
	y <- item
	Main.return (x, y)
 


