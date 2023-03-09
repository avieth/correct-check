module Pretty
  ( -- * Rendering Tests
    RenderTest (..)
  , noRenderTest
  , renderTestViaShow
  , renderTestViaPretty

    -- * Rendering Domains
  , RenderDomain (..)
  , noRenderDomain
  , renderDomainViaShow
  , renderDomainViaPretty

    -- * Re-exports
  , PP.Pretty (..)
  , fromString
  , PP.vsep
  , PP.hsep
  , PP.indent
  , PP.nest
  , PP.hang
  , (PP.<+>)
  , renderShowS
  , layoutPretty
  , defaultLayoutOptions
  ) where

import Data.String (fromString)
import Prettyprinter as PP
import Prettyprinter.Render.String
import Prettyprinter.Render.Text

-- | Rendering information about the types of a property, useful for showing
-- diagnostic information.
data RenderTest specimen result assertion = RenderTest
  { renderSpecimen :: forall ann . Maybe (specimen -> Doc ann)
  , renderResult :: forall ann . Maybe (result -> Doc ann)
  , renderAssertion :: forall ann . Maybe (assertion -> Doc ann)
  }

noRenderTest :: RenderTest specimen result assertion
noRenderTest = RenderTest
  { renderSpecimen = Nothing
  , renderResult = Nothing
  , renderAssertion = Nothing
  }

renderTestViaShow :: (Show specimen, Show result, Show assertion)
                  => RenderTest specimen result assertion
renderTestViaShow = RenderTest
  { renderSpecimen = Just tshow
  , renderResult = Just tshow
  , renderAssertion = Just tshow
  }

renderTestViaPretty :: (PP.Pretty specimen, PP.Pretty result, PP.Pretty assertion)
                    => RenderTest specimen result assertion
renderTestViaPretty = RenderTest
  { renderSpecimen = Just PP.pretty
  , renderResult = Just PP.pretty
  , renderAssertion = Just PP.pretty
  }


data RenderDomain space = RenderDomain
  { renderSpace :: forall ann . Maybe (space -> Doc ann)
  }

noRenderDomain :: RenderDomain space
noRenderDomain = RenderDomain
  { renderSpace = Nothing
  }

renderDomainViaShow :: Show space => RenderDomain space
renderDomainViaShow = RenderDomain
  { renderSpace = Just tshow
  }

renderDomainViaPretty :: PP.Pretty space => RenderDomain space
renderDomainViaPretty = RenderDomain
  { renderSpace = Just PP.pretty
  }

tshow :: Show t => t -> Doc ann
tshow = PP.viaShow
