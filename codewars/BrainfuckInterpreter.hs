import Text.Parsec
import Text.Parsec.Char

data BF = Op Char | Loop [BF] deriving Show

main = let list = many cmd
           cmd = Op <$> oneOf "<>+-.," <|> loop 
           loop = char '[' *> (Loop <$> list) <* char ']'
       in putStrLn . show $ runParser list () "" "[->+<]"
