{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverlappingInstances #-}

module ResultWorthy.SwiftRenderer where

import ResultWorthy.Renderer
import ResultWorthy.DataTypes
import ResultWorthy.Util

import Data.Functor.Identity
import Data.Maybe(fromMaybe)
import Data.Functor

import Text.PrettyPrint.Free

instance Renderable GenericConstraint where
  render = fmap (angles . text)

instance Renderable AccessModifier where
  render Public   = Just $ text "public"
  render Private  = Just $ text "private"
  render _        = Nothing

instance Renderable InitializerModifier where
  render Required    = Just $ text "required" 
  render Convenience = Just $ text "convenience"
  render Optional    = Nothing
  renderList         = spaceSepMaybe 1 . map render

instance Renderable FunctionModifier where
  render Final  = Just $ text "final"
  render Static = Just $ text "static"
  render Class  = Just $ text "class"
  renderList    = spaceSepMaybe 1 . map render

instance Renderable ArgumentModifier where
  render Inout = Just $ text "inout"
  renderList = spaceSepMaybe 1 . map render

instance Pretty SwiftModule where
  pretty m = lineSep 4 [imports m, decls m]
    where imports = pretty . mImports
          decls   = pretty . mDeclarations

instance Pretty Imports where
  pretty = lineSep 1 . map lineText 
    where lineText i = text "import" <+> text i

instance Pretty SwiftContainerType where
  pretty SwiftStruct        = text "struct"
  pretty (SwiftClass False) = text "class"
  pretty (SwiftClass True)  = text "final class"
  pretty SwiftExtension     = text "extension"

instance Pretty ContainerAnnotation where
  pretty (ContainerAnnotation a ct t e) =
    let extension = (colon <+>) . commaSep <$> emptyMaybe (map pretty e)
    in  fromMaybe empty $ spaceSepMaybe 1 [render a, Just $ pretty ct, Just $ pretty t, extension]

instance Pretty Argument where
  pretty (Argument modifier iName eName sType dValue) =
    let names      = spaceSepMaybe 1 [renderList modifier, text <$> iName, text <$> eName, return $ text ":"]
        types      = pretty sType
        defaultVal = (\val -> text "=" <+> text val) <$> dValue
    in  fromMaybe empty $ spaceSepMaybe 1 [names, Just types, defaultVal]
  prettyList = parensCommaSep . map pretty

instance Pretty Declaration where
  pretty (MemberDecl a v w n t) =
    let variable = condRender (text "var") (text "let") v
        preamble = render a
        name     = pretty n 
        typeD    = pretty t
    in  fromMaybe empty $ spaceSepMaybe 1 [preamble, Just variable, Just name, Just colon, Just typeD]
  pretty (AliasDecl a s) =
    let typealias = text "typealias"
        equals    = text "="
    in  spaceSep 1 [typealias, pretty a, equals, pretty s]
  pretty (FunctionDecl a m n g t)  =
    let preamble = spaceSepMaybe 1 [render a, renderList m]
        name     = spaceSepMaybe 1 [return $ text "func", return $ text n]
        generic  = render g
        function = pretty t
    in  fromMaybe empty $ spaceSepMaybe 1 [preamble, name, generic, Just function]
  pretty (InitializerDecl a m g ar) =
    let preamble = spaceSepMaybe 1 [render a, renderList m, return $ text "init"]
        generic  = render g
        args     = Just $ pretty ar
    in  fromMaybe empty $ spaceSepMaybe 0 [preamble, generic, args]
  pretty (ContainerDecl a d) = spaceSep 1 [pretty a, blockBody (pretty d)]
  pretty (BodyDecl d b) = spaceSep 1 [pretty d, blockBody (text b)]
  prettyList = lineSep 2 . map pretty 


instance Pretty SwiftType where
  pretty (RegularType n) = text n
  pretty (OptionalType n o) =
    let optionalCharacter = text $ if o then "!" else "?"
    in  pretty n <> optionalCharacter
  pretty (GenericType t g) = spaceSep 0 [pretty t, angles $ pretty g]
  pretty (VarargsType t) = pretty t <> text "..."
  pretty (ArrayType t) = brackets $ pretty t
  pretty (DictionaryType k v) = brackets $ separate space [pretty k, text ":", pretty v]
  pretty (FunctionType a r)   = separate space [pretty a, text "->", pretty r]
  pretty (TupleType t)        = parensCommaSep $ map pretty t
  prettyList                  = commaSep . map pretty
