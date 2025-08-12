-- | Contains additional TOML combinators/codecs
module Nihil.Toml (didefault, utcTime, seq') where

import Data.Sequence qualified as Seq
import Data.Time (UTCTime)
import Data.Time qualified as Time
import Relude
import Toml qualified as Toml

-- | Marks a combinator as optional, providing a default value
didefault ∷ ∀ a. (Eq a) ⇒ a → Toml.TomlCodec a → Toml.TomlCodec a
didefault empty' =
  Toml.dimap
    (\x → if x == empty' then Nothing else Just x)
    (fromMaybe empty')
    . Toml.dioptional

utcTime ∷ Toml.Key → Toml.TomlCodec UTCTime
utcTime at =
  Toml.dimap
    (Time.utcToZonedTime Time.utc)
    Time.zonedTimeToUTC
    $ Toml.zonedTime at

seq' ∷ ∀ a. Toml.TomlCodec a → Toml.Key → Toml.TomlCodec (Seq a)
seq' smol key = Toml.dimap toList Seq.fromList $ Toml.list smol key
