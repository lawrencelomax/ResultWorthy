module ResultWorthy.Transforms where

import Data.Maybe(mapMaybe)

import Text.PrettyPrint.Free

import ResultWorthy.DataTypes
import ResultWorthy.Renderer

-- Recursively orders the declarations by decl subtype and then by alphabet.
orderDecls :: SwiftModule -> SwiftModule
orderDecls = undefined

-- Extracts out a curried Result type for last arg NSErrorPointer, cocoa-style APIs
extractResult :: SwiftModule -> SwiftModule
extractResult (SwiftModule _ decls) = construct $ mapMaybe resultFunctionDeclExtract decls
  where construct = SwiftModule []

-- Takes a SwiftType and wraps it in a Result.
putInResult :: SwiftType -> SwiftType
putInResult = GenericType (RegularType "Result")

-- Extracts the Function Type of a Resultable Function.
resultFunctionTypeExtract :: SwiftType -> Maybe SwiftType
resultFunctionTypeExtract (FunctionType a r) =
  let construct (args, errorArg) = FunctionType args $ putInResult r
  in  fmap construct (extractErrorPointerArg a)
resultFunctionTypeExtract  _ = Nothing

-- Extracts the Resultable Functions recursively.
resultFunctionDeclExtract :: Declaration -> Maybe Declaration
resultFunctionDeclExtract (FunctionDecl a m n g f) =
  let constructF   = FunctionDecl a m n g
      construct f  = BodyDecl (constructF f) $ show (renderSwiftBody n [] f)
  in  fmap construct (resultFunctionTypeExtract f)
resultFunctionDeclExtract (ContainerDecl a ds) =
  let construct    = ContainerDecl a
      listMaybe [] = Nothing
      listMaybe as = Just as
  in  fmap construct (listMaybe (mapMaybe resultFunctionDeclExtract ds))
resultFunctionDeclExtract _ = Nothing

-- Pulls out the NSErrorPointer argument from the array (if it exists)
extractErrorPointerArg :: [Argument] -> Maybe ([Argument], Argument) 
extractErrorPointerArg [] = Nothing
extractErrorPointerArg as
  | aType (last as) == RegularType "NSErrorPointer" = Just (as, last as)
  | otherwise = Nothing

-- Doc from Function. Function Name, arguments, returnType
renderSwiftBody :: String -> [Argument] -> SwiftType -> Doc e
renderSwiftBody name args ret =
  let errorDecl      = text "var error: NSError?"
      invocationDecl = text "let ret =" <+> text name <> renderArgumentInvocation args
      returnDecl     = text "return Result.construct(ret, error)"
  in vcat [errorDecl, invocationDecl, returnDecl]

-- Renders the invocation of arguments
renderArgumentInvocation :: [Argument] -> Doc e
renderArgumentInvocation = parensCommaSep . mapMaybe renderArg
  where renderArg = fmap text . aExternalName

--    var error: NSError? = nil
--        let string = NSString(contentsOfURL: url, usedEncoding: nil, error: &error)
--            return ResultExt.construct(string)(maybeError: error)
--
