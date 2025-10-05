module Build (main) where

import Data.Aeson (KeyValue ((.=)), Object, Value (Object), encode, object)
import Data.Aeson.KeyMap qualified as KeyMap
import Relude

main :: IO ()
main = do
  writeManifest "../cljs/public/manifest.json" firefox
  writeManifest "../cljs/release/manifest.json" chrome

writeManifest :: (MonadIO m) => FilePath -> Object -> m ()
writeManifest path config = writeFileLBS path $ encode $ Object $ config <> base

base :: Object
base =
  KeyMap.fromList
    [ "host_permissions" .= (["<all_urls>"] :: [Text]),
      "manifest_version" .= (3 :: Int),
      "name" .= ("see" :: Text),
      "permissions" .= (["nativeMessaging", "scripting"] :: [Text]),
      "version" .= ("0.1.0" :: Text)
    ]

firefox :: Object
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
          ]
    ]

chrome :: Object
chrome =
  KeyMap.fromList
    [ "background"
        .= object
          [ "service_worker" .= ("js/background.js" :: Text),
            "type" .= ("module" :: Text)
          ]
    ]
