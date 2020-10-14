module Hasura.RQL.DML.Update.Types where


import           Hasura.Prelude

import qualified Hasura.SQL.DML                 as S

import           Hasura.RQL.DML.Returning.Types
import           Hasura.RQL.Types.BoolExp
import           Hasura.RQL.Types.Column
import           Hasura.SQL.Backend
import           Hasura.SQL.Types


data AnnUpdG (b :: Backend) v
  = AnnUpd
  { uqp1Table   :: !QualifiedTable
  , uqp1OpExps  :: ![(PGCol, UpdOpExpG v)]
  , uqp1Where   :: !(AnnBoolExp v, AnnBoolExp v)
  , uqp1Check   :: !(AnnBoolExp v)
  -- we don't prepare the arguments for returning
  -- however the session variable can still be
  -- converted as desired
  , uqp1Output  :: !(MutationOutputG b v)
  , uqp1AllCols :: ![PGColumnInfo]
  } deriving (Show, Eq)

type AnnUpd b = AnnUpdG b S.SQLExp

data UpdOpExpG v = UpdSet !v
                 | UpdInc !v
                 | UpdAppend !v
                 | UpdPrepend !v
                 | UpdDeleteKey !v
                 | UpdDeleteElem !v
                 | UpdDeleteAtPath ![v]
                 deriving (Eq, Show, Functor, Foldable, Traversable, Generic, Data)
