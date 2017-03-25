module Type where


newtype Var
  = TV String
  deriving (Show, Eq, Ord)


data Type
  = Var Var
  | Con String
  | Arrow Type Type
  deriving (Show, Eq, Ord)


data Scheme
  = Forall [Var] Type
  deriving (Show, Eq, Ord)


var :: String -> Type
var = Var . TV


int, float, string, bool :: Type
int    = Con "Int"
float  = Con "Float"
string = Con "String"
bool   = Con "Bool"


primaryTypes :: [Type]
primaryTypes =
  [ int
  , float
  , string
  , bool
  ]
