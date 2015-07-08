module ResultWorthy.DataTypes where

import Data.Maybe
import Data.Traversable
import Control.Applicative

data SwiftContainerType = SwiftStruct
                        | SwiftClass 
                           { ctFinal :: Bool }
                        | SwiftExtension 
  deriving (Eq, Show, Read)

data InitializerModifier = Required | Convenience | Optional deriving (Eq, Show, Read)
data FunctionModifier = Final | Static | Class deriving (Eq, Show, Read)
data ArgumentModifier = Inout deriving (Eq, Show, Read)
data AccessModifier = Public | Private | Internal deriving (Eq, Show, Read)

type Imports = [String]
type GenericConstraint = Maybe String

data Argument = Argument
  { aModifier :: [ArgumentModifier]
  , aInternalName :: Maybe String
  , aExternalName :: Maybe String
  , aType :: SwiftType
  , aDefaultValue :: Maybe String
  } deriving (Eq, Show, Read)

data SwiftType = RegularType
                  { tName :: String }
               | OptionalType
                  { tType :: SwiftType
                  , tForce :: Bool }
               | GenericType
                  { tType :: SwiftType
                  , tGeneric :: SwiftType }
               | VarargsType
                  { tType :: SwiftType }
               | ArrayType
                  { tType :: SwiftType }
               | DictionaryType
                  { tKeyType :: SwiftType
                  , tValueType :: SwiftType }
               | FunctionType
                  { tArguments :: [Argument]
                  , tReturnType :: SwiftType } 
               | TupleType
                  { tTypes :: [SwiftType] }
    deriving (Eq, Show, Read)

data ContainerAnnotation = ContainerAnnotation
  { cAccessModifier :: AccessModifier
  , cContainerType :: SwiftContainerType
  , cType :: SwiftType
  , cExtends :: [SwiftType]
  } deriving (Eq, Show, Read)

data Declaration = MemberDecl
                    { dAccessModifier :: AccessModifier
                    , dVariable :: Bool
                    , dWritable :: Bool
                    , dName :: String
                    , dType:: SwiftType }
                 | AliasDecl
                    { dAlias :: SwiftType
                    , dSource :: SwiftType }
                 | FunctionDecl
                    { dAccessModifier :: AccessModifier
                    , dFunctionModifiers :: [FunctionModifier]
                    , dName :: String
                    , dGenericConstraint :: GenericConstraint
                    , dFunction :: SwiftType }
                 | InitializerDecl
                    { dAccessModifier :: AccessModifier
                    , dInitializerModifiers :: [InitializerModifier]
                    , dGenericConstraint :: GenericConstraint
                    , dArguments :: [Argument] }
                 | ContainerDecl
                    { dContainerAnnotation :: ContainerAnnotation
                    , dDeclarations :: [Declaration] }
                 | BodyDecl
                    { dDecl :: Declaration
                    , dBody :: String }
  deriving (Eq, Show, Read)

data SwiftModule = SwiftModule
  { mImports :: Imports
  , mDeclarations :: [Declaration]
  } deriving (Eq, Show, Read)

