{-# OPTIONS_GHC -Wno-orphans -Wno-unused-do-bind -Wno-name-shadowing #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module LFPL.Parser where 

import Data.Void (Void) 

import Control.Monad (void)

import LFPL.AST

import Text.Megaparsec 
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex
import Control.Monad.Combinators.Expr
import Data.Functor (($>))

type Parser = Parsec Void String 

-- instance CompilerError (ParseErrorBundle String Void) where 
--   errorStage = const "parsing"
--   errorMsg = errorBundlePretty

lfplTerm :: Parser (LFPLTerm String)
-- lfplTerm = positioned (makeExprParser (term >>= postfix) operators) <?> "expression"
lfplTerm = makeExprParser (term >>= postfix) operators <?> "expression"
  where lfplLambda = do 
          (name, t) <- reserved "fn" *> (nameAndType <|> parens nameAndType)
          e <- symbol "=>" *> lfplTerm
          return $ LFPLLambda name t e
        
        nameAndType = (,) <$> identifier <*> (symbol ":" *> lfplType)

        lfplIf = LFPLIf <$> (reserved "if" *> lfplTerm) 
                        <*> (reserved "then" *> lfplTerm) 
                        <*> (reserved "else" *> lfplTerm)
        lfplIntLiteral = LFPLIntLiteral <$> integer
        lfplBoolLiteral = LFPLBoolLiteral <$> ((True <$ reserved "true") <|> (False <$ reserved "false")) 
        lfplUnitLiteral = LFPLUnitLiteral <$ symbol "()"

        lfplIdent = LFPLIdentifier <$> identifier
        lfplLet = do 
          reserved "let"
          (name, t) <- nameAndType <|> parens nameAndType
          symbol "="
          e1 <- lfplTerm 
          reserved "in"
          e2 <- lfplTerm 
          reserved "end"

          return $ LFPLApp (LFPLLambda name t e2) e1

        lfplTuple = do 
          elems <- parens (sepBy1 lfplTerm (symbol ","))
          case elems of 
            [x] -> return x -- Regular parenthesized expression
            [x, y] -> return $ LFPLPair x y 
            _ -> fail "tuples must have exactly two elements"

        lfplNil = LFPLListNil <$ (reserved "nil" <|> void (symbol "[]"))
        lfplCons = do 
          reserved "cons"
          symbol "("
          e1 <- lfplTerm 
          symbol ","
          e2 <- lfplTerm 
          symbol ","
          e3 <- lfplTerm 
          symbol ")"

          return $ LFPLListCons e1 e2 e3

        lfplDiamond = LFPLDiamondLiteral <$ diamond

        lfplIter = do 
          reserved "iter"
          e <- lfplTerm 
          symbol "{"
          
          void lfplNil
          symbol "=>"
          e0 <- lfplTerm 

          symbol "|"
          reserved "cons"
          symbol "("
          x1 <- identifier -- diamond
          symbol ","
          x2 <- identifier -- head
          symbol ","
          symbol "_" -- tail is inaccessible
          symbol ")"
          reserved "with"
          y <- identifier -- recursive result
          symbol "=>"
          e1 <- lfplTerm

          symbol "}"

          return $ LFPLListIter e e0 x1 x2 y e1

        term = positioned $ 
          choice [lfplLambda, lfplIf, lfplLet,
                  lfplNil, lfplCons, lfplIter, lfplDiamond,
                  lfplIdent, lfplTuple,
                  lfplUnitLiteral, lfplIntLiteral, lfplBoolLiteral]
        postfix e = positioned (functionApp e) <|> return e 

        functionApp e = foldl LFPLApp e <$> some term 

        operators = [[--prefixOp "!" Not,
                      Prefix (operator "-" $> (\a -> LFPLIntArithOp Minus (LFPLIntLiteral 0) a))],
                     [arithOp "*" Times,
                      arithOp "/" Divide,
                      arithOp "%" Mod],
                     [arithOp "+" Plus,
                      arithOp "-" Minus],
                     [cmpOp "<" LessThan,
                      cmpOp "<=" LessEq,
                      cmpOp ">" GreaterThan,
                      cmpOp ">=" GreaterEq
                     ],
                     [cmpOp "==" Equals,
                      cmpOp "!=" NotEquals],
                     [InfixR (LFPLApp <$ operator "$")]
                    --  [semicolon]
                     ]
          where arithOp str opConstructor = 
                  InfixL (operator str $> LFPLIntArithOp opConstructor) 
                
                cmpOp str opConstructor = 
                  InfixL (operator str $> LFPLIntCmpOp opConstructor)

        positioned p = LFPLPositionTerm <$> getSourcePos <*> p <*> getSourcePos

-- Really only used when parsing data types
lfplType :: Parser LFPLType
lfplType = makeExprParser (term >>= postfix) operators <?> "type"
  where term =  
              LFPLIntType <$ reserved "int"
          -- <|> F0PrimitiveType F0StringType <$ reserved "string" 
          <|> LFPLBoolType <$ reserved "bool"
          <|> LFPLUnitType <$ reserved "unit"
          <|> LFPLDiamondType <$ diamond
          <|> parens lfplType 
        
        -- A little hacky, but whats happening is that
        -- we need to know if we are parsing a tuple
        -- type or not. In postfixA we aren't necessarily,
        -- but in postfixB we are.
        postfix e = typeApp e postfix <|> return e

        -- typeApp e next = identifier >>= \name -> next (F0TypeCons e name)
        -- Right now the only type app we can do is "list"
        typeApp e next = reserved "list" *> next (LFPLListType e)

        operators = [[InfixL (LFPLPairType <$ symbol "*")],
                     [InfixR (LFPLFunctionType <$ (symbol "->" <|> symbol "-o"))]
                    ]

diamond = void (symbol "♢" <|> symbol "<>")

-- Lexing 
symbol :: String -> Parser String
symbol = Lex.symbol sc

opChar :: Parser Char
opChar = choice (char <$> "!+-*/%@<>=")

operator :: String -> Parser String
operator s = try (symbol s <* notFollowedBy opChar)

stringLiteral :: Parser String
stringLiteral = lexeme (char '"' >> manyTill Lex.charLiteral (char '"')) <?> "string" 
charLiteral :: Parser Char 
charLiteral = lexeme (char '\'' >> (Lex.charLiteral <* char '\'')) <?> "character"

-- | Parses an integer (the integer is not checked for being in range of a 32-bit signed int)
integer :: Parser Integer 
integer = lexeme (try $ char '0' >> char' 'x' >> Lex.hexadecimal) <|> lexeme Lex.decimal <?> "integer"

identifier :: Parser String
identifier = (lexeme . try) (p >>= check) <?> "identifier"
  where p = (:) <$> identStart <*> many identLetter
        identStart = letterChar
        identLetter = alphaNumChar <|> char '_' <|> char '\''

        check x = if x `elem` reservedWords
                    then fail $ "'" ++ x ++ "' is a reserved word and cannot be an identifier"
                    else return x

reserved :: String -> Parser ()
reserved word = (lexeme . try) (string word *> notFollowedBy alphaNumChar)

reservedWords :: [String]
reservedWords = ["val",
                 "fun",
                 "if",
                 "then",
                 "else",
                 "fn",
                 "int",
                 "string",
                 "bool",
                 "unit",
                 "let",
                 "in",
                 "end",
                 "with",
                --  "case",
                --  "of",
                --  "datatype",
                 "♢", "<>",
                 "λ"]

parens, lexeme :: Show a => Parser a -> Parser a 
parens = between (try $ symbol "(" <* notFollowedBy (char '*')) (symbol ")")
lexeme = Lex.lexeme sc 

sc, lineComment, blockComment :: Parser () 
sc = Lex.space space1 lineComment blockComment
lineComment = do 
  void $ string "--"
  void $ manyTill anySingle (char '\n')

blockComment = do 
  void $ string "(*"
  void $ manyTill anySingle (string "*)")

run :: String -> LFPLTerm String
run str = 
  case runParser (sc *> lfplTerm <* eof) "<testing>" str of 
    Left err -> error (errorBundlePretty err) 
    Right r -> r