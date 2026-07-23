module TenForty
  ( -- * DSL
    module TenForty.DSL,

    -- * Compilation
    compileForm,
    compileFormToJSON,
    resolveForms,

    -- * Graph Types (JSON output)
    ComputationGraph (..),
    GraphMeta (..),
    Node (..),
    Op (..),
  )
where

import TenForty.Compile.JSON
import TenForty.DSL
