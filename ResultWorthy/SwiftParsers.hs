module ResultWorthy.SwiftParsers where

import ResultWorthy.DataTypes
import ResultWorthy.Parsers

import Data.Maybe
import Data.Functor
import Data.Either
import Data.Monoid

import Control.Applicative((*>), (<*), (<*>))
import Control.Monad

import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Perm
import Text.Parsec (ParseError)

-- Parses empty sections, whitespace and newlines
emptyness :: Parser String
emptyness = many1 (space <|> newline)

-- Parses comments
comments :: Parser String
comments = singleLine
  where singleLine = spaces *> string "//" *> manyTill anyChar newline

-- Parses any non AST stuff
ignorable :: Parser String
ignorable = fmap concat (many1 $ emptyness <|> comments)

-- Parses the input parser, ignoring emptyness before
trimLeading :: Parser a -> Parser a
trimLeading a = optional ignorable *> a

-- Parses the input parser, ignoring emptyness after
trimTrailing :: Parser a -> Parser a
trimTrailing a = a <* optional ignorable

-- Parses the input parser many times surrounded by emptyness.
trim :: Parser a -> Parser a
trim = trimTrailing . trimLeading

-- Parses any permissable infix characters
-- (According to the Swift Book)
infixIdentifier :: Parser String
infixIdentifier = many1 validChars
  where validChars = oneOf "/=-+!*%<>&|^?~"

-- Parses Acceptable Argument Names
identifierWord :: Parser String
identifierWord = many1 $ choice [upper, lower, digit, punct]
  where punct = oneOf "._#"

-- Parses Regular Swift Types
regularTypeParser :: Parser SwiftType
regularTypeParser = liftM RegularType identifierWord

-- Parses Array Swift Types
arrayTypeParser :: Parser SwiftType
arrayTypeParser = liftM ArrayType valueType
  where valueType = char '[' *> swiftTypeParser <* char ']'

-- Parses Dictionary Types
dictionaryTypeParser :: Parser SwiftType
dictionaryTypeParser = liftM2 DictionaryType keyType valueType
  where keyType   = char '[' *> spaces *> swiftTypeParser
        valueType = spaces *> char ':' *> spaces *> swiftTypeParser <* spaces <* char ']'

-- Parses Swift Tuple 
-- Unit Type is not permitted
tupleTypeParser :: Parser SwiftType
tupleTypeParser = liftM TupleType singleType
  where singleType = parens $ sepBy1 (padded swiftTypeParser) (char ',')

-- Argument Modifiers Parsers
argumentModifiersParser :: Parser [ArgumentModifier]
argumentModifiersParser = matchConst (try (string "inout")) [Inout] []

-- Parses a signle arguments of a Raw Swift Function type
unnamedArgumentParser :: Parser Argument
unnamedArgumentParser = construct $ padded swiftTypeParser
  where construct t = liftM5 Argument (return []) (return Nothing) (return Nothing) t (return Nothing)

-- This represents arguments as they exist inside a function declaration, rather than as a Function type.
-- This means that they may (or may not be named
namedArgumentParser :: Parser Argument
namedArgumentParser = liftM5 Argument modifiers internalName externalName typeP defaultValue
  where modifiers     = padded argumentModifiersParser
        internalName  = Just <$> padded identifierWord
        externalName  = optionMaybe $ padded identifierWord
        typeP         = padded (char ':') *> padded swiftTypeParser
        defaultValue  = optionMaybe $ padded (char '=') *> padded identifierWord

-- Parses Swift Function Types as they may be outside of a function delaration
-- This means that arguments must be named and there must be at least one arrow
-- Valid :    String -> Foo -> Bar
functionTypeParser :: Parser SwiftType
functionTypeParser = liftM2 FunctionType arguments returnType
 where arguments  = parensCommaSep1 unnamedArgumentParser
       returnType = padded (string "->") *> padded swiftTypeParser

-- Parses Swift Functions as they exist inside a function declaration
functionTypeDeclParser :: Parser SwiftType
functionTypeDeclParser = liftM2 FunctionType arguments returnType
  where arguments      = parensCommaSep (try namedArgumentParser <|> namedArgumentParser)
        definedReturn  = padded (string "->") *> padded swiftTypeParser
        curried        = functionTypeDeclParser
        emptyReturn    = parserReturn (RegularType "Void")
        returnType     = choice [try definedReturn, try curried, emptyReturn]

-- Attempts 
optionalTypeParser :: Parser SwiftType -> Parser SwiftType
optionalTypeParser p = followOnOptional mapper p optionalOrForce
  where mapper t Nothing  = t
        mapper t (Just p) = OptionalType t p
        optionalOrForce   = (char '?' $> False) <|> (char '!' $> True)

-- Parses Generic Swift Types
genericTypeParser :: Parser SwiftType -> Parser SwiftType
genericTypeParser p = followOnOptional mapper p generic
  where mapper t Nothing  = t
        mapper t (Just p) = GenericType t p
        generic           = try $ angleBraced swiftTypeParser

-- Parses a Varargs Type
varargsTypeParser :: Parser SwiftType -> Parser SwiftType
varargsTypeParser p = followOnOptional mapper p elipses
  where mapper t Nothing  = t
        mapper t (Just _) = VarargsType t
        elipses           = string "..."

-- Parses Swift Types
swiftTypeParser :: Parser SwiftType
swiftTypeParser = optionalTypeParser $ genericTypeParser $ varargsTypeParser baseType
  where baseType = choice [try functionTypeDeclParser,
                           try tupleTypeParser,
                           try dictionaryTypeParser,
                           try arrayTypeParser,
                           try regularTypeParser]

-- Parses the access modifier
accessModifierParser :: Parser AccessModifier
accessModifierParser = padded $ choice [try public, try private, internal]
  where public   = padded (string "public") $> Public
        private  = padded (string "private") $> Private
        internal = return Internal

-- Parses the Type of a Container
containerTypeParser :: Parser SwiftContainerType
containerTypeParser = structP <|> extensionP <|> classP <|> staticClassP
  where structP      = string "struct" $> SwiftStruct 
        extensionP   = string "extension" $> SwiftExtension
        classP       = string "class" $> SwiftClass False
        staticClassP = string "final class" $> SwiftClass True

-- Parses the annoation of a container
containerAnnotationParser :: Parser ContainerAnnotation
containerAnnotationParser = liftM4 ContainerAnnotation accessModifierParser containerP typeP extendsP
  where containerP = padded containerTypeParser
        typeP      = padded swiftTypeParser
        extendsP   = option [] (spaces *> char ':' *> sepBy (padded swiftTypeParser) (char ','))

-- Parses a Container (struct, class, extension) TODO enum
containerDeclarationParser :: Parser Declaration
containerDeclarationParser = liftM2 ContainerDecl containerAnnotationParser containerBody
  where declarations  = choice [try $ trim $ declBodyParser initializerDeclParser,
                                try $ trim $ declBodyParser functionDeclParser,
                                try $ trim aliasParser,
                                try $ trim memberParser,
                                try $ trim containerDeclarationParser]
                         <?> "any declaration within a container"
        containerBody = braced (ignorable *> many declarations)
                         <?> "the body of a container"

-- Parses the original parser, or decoates it with a BodyDecl parser
declBodyParser :: Parser Declaration -> Parser Declaration
declBodyParser p = followOnOptional mapper p body
  where mapper d Nothing  = d
        mapper d (Just b) = BodyDecl d b
        body              = concat . flattenTree "{" "}" <$> parseTree '{' '}'

-- Parses the annotations for an Initializer
initializerModifiersParser :: Parser [InitializerModifier]
initializerModifiersParser = permute (construct <$?> requiredP <|?> convenienceP) <*> optionalP
  where construct r c o = catMaybes [r, c, o]
        matchPerm p     = (Nothing, fmap Just p)
        requiredP       = matchPerm $ string "required " $> Required
        convenienceP    = matchPerm $ string "convenience " $> Convenience
        optionalP       = string "init" *> matchConst (try (char '?')) (Just Optional) Nothing

-- Parses the Generic Type Constraing of a function declaration
-- TODO: Make this good
genericConstraintParser :: Parser String
genericConstraintParser = angleBraced $ many letters
  where letters = choice [alphaNum, space, oneOf ":.="]

-- Parses an entire initializer
initializerDeclParser :: Parser Declaration
initializerDeclParser = liftM4 InitializerDecl accessModifierParser initializerModifiersParser constraint arguments
  where constraint = optionMaybe $ try genericConstraintParser
        arguments  = parensCommaSep namedArgumentParser

-- Parses the annotations prior to the func keyword
functionModifiersParser :: Parser [FunctionModifier]
functionModifiersParser = permute (construct <$?> finalP <|?> staticP <|?> classP) <* funcP
  where construct f s c = catMaybes [f, s, c]
        matchPerm p     = (Nothing, fmap Just p)
        funcP           = string "func "
        staticP         = matchPerm $ string "static " $> Static
        classP          = matchPerm $ string "class " $> Class
        finalP          = matchPerm $ try (string "final ") $> Final

-- Parses an entire function
functionDeclParser :: Parser Declaration
functionDeclParser = liftM5 FunctionDecl accessModifierParser functionModifiersParser name constraint function
  where name       = padded (try identifierWord <|> infixIdentifier) 
                      <?> "an identifier for a function declaration"
        constraint = optionMaybe (try genericConstraintParser)
                      <?> "a generic constraint for a function declaration"
        function   = padded functionTypeDeclParser
                      <?> "a functiontype at the end of a function declaration"

-- Parses an Alias
aliasParser :: Parser Declaration
aliasParser = liftM2 AliasDecl alias source
  where alias  = padded (string "typealias") *> padded swiftTypeParser
        source = padded (char '=') *> padded swiftTypeParser

-- Parses a Member
memberParser :: Parser Declaration
memberParser = liftM5 constructor accessModifierParser variable name typeP writable
  where constructor a v n t w = MemberDecl a v w n t 
        variable              = padded $ (string "var" $> True) <|> (string "let" $> False)
        name                  = identifierWord
        typeP                 = spaces *> char ':' *> spaces *> swiftTypeParser
        writable              = padded $ doesMatch (string "{ get }") --lazy

-- Parses lines that look like imports, returning the module name
importParser :: Parser String
importParser = padded $ string "import" *> padded identifierWord <* optional newline

-- Parses the imports section
importsParser :: Parser [String]
importsParser = many $ trimLeading importParser

-- Parses all of the module, must end in an eof
swiftModuleParser :: Parser SwiftModule
swiftModuleParser = liftM2 SwiftModule importsParser declarations <* eof
  where declarations = many1 $ declBodyParser body
        body         = choice [try $ trim containerDeclarationParser,
                               try $ trim functionDeclParser,
                               try $ trim aliasParser,
                               try $ trim memberParser]

-- Take a String and Return either a ParseResult, or the Error in Parsing
parseSwiftModule :: String -> Either ParseError SwiftModule
parseSwiftModule = parseString swiftModuleParser
