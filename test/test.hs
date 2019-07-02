
import           Hedgehog.Main (defaultMain)

import qualified Test.RepoTool.Text

main :: IO ()
main =
  defaultMain
    [ Test.RepoTool.Text.tests
    ]
