module Codegen.Javascript where

import           Codegen.Javascript.Merge     (merge)
import           Codegen.Javascript.Pretty    (prettyPrint)
import           Codegen.Javascript.Rename    (rename)
import qualified Codegen.Javascript.Syntax    as Js
import           Codegen.Javascript.Transform (transform)
import qualified Syntax                       as Ch


codegen :: Ch.Module -> Js.Module
codegen = merge . transform . rename
