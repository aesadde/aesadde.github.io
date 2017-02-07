{-|
Module      :  Contexts
Description : Contexts
Copyright   : (c) Alberto, 2017
License     : MIT
Maintainer  : albertosadde@gmail.com
Stability   : experimental
-}

module  Contexts where

import Hakyll (dateField, Context, Tags, defaultContext, tagsField)
import Data.Monoid ((<>))

dateCtx :: Context String
dateCtx = dateField "date" "%B %e, %Y" <> defaultContext

yearCtx :: Context String
yearCtx = dateField "date" "%Y" <> defaultContext

postCtx :: Tags -> Context String
postCtx tags = tagsField "tags" tags <> dateCtx