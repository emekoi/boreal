module Network.API.MAL.Types.Lens
  ( makeFieldsNoPrefix,
  )
where

import Data.Char (toUpper)
import Language.Haskell.TH
import Lens.Micro
import Lens.Micro.TH

makeFieldsNoPrefix :: Name -> DecsQ
makeFieldsNoPrefix =
  makeLensesWith
    ( camelCaseFields & lensField .~ \_ _ field ->
        let capitalize [] = []
            capitalize (x : xs) = toUpper x : xs
            split p s = case dropWhile p s of
              "" -> []
              s' -> w : split p s''
                where
                  (w, s'') = break p s'
            toCamelCase = concatMap capitalize . split (== '_')
            baseField = nameBase field
            className = "Has" ++ toCamelCase baseField
         in [MethodName (mkName className) (mkName $ '_' : baseField)]
    )
