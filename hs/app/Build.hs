module Build (main) where

import Data.Aeson (KeyValue ((.=)), Value (Object), encode, object)
import Data.Aeson.KeyMap qualified as KeyMap
import Relude

base :: KeyMap.KeyMap Value
base =
  KeyMap.fromList
    [ "manifest_version" .= (3 :: Int),
      "name" .= ("see" :: Text),
      "version" .= ("0.1.0" :: Text)
    ]

firefox :: KeyMap.KeyMap Value
firefox =
  KeyMap.fromList
    [ "background"
        .= object
          [ "scripts" .= (["js/background.js"] :: [Text]),
            "type" .= ("module" :: Text)
          ],
      "browser_specific_settings"
        .= object
          [ "gecko"
              .= object
                [ "id" .= ("@see" :: Text)
                ]
          ],
      "host_permissions" .= (["<all_urls>"] :: [Text]),
      "permissions" .= (["nativeMessaging", "scripting"] :: [Text])
    ]

chrome :: KeyMap.KeyMap Value
chrome =
  KeyMap.fromList
    [ "background"
        .= object
          [ "service_worker" .= ("js/background.js" :: Text),
            "type" .= ("module" :: Text)
          ]
    ]

main :: IO ()
main = do
  writeFileLBS "../cljs/public/manifest.json" $ encode $ Object $ firefox <> base
  writeFileLBS "../cljs/release/manifest.json" $ encode $ Object $ chrome <> base
