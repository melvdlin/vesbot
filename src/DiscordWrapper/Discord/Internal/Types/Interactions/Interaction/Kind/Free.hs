
module DiscordWrapper.Discord.Internal.Types.Interactions.Interaction.Kind.Free (
    Free (..),
) where

import qualified DiscordWrapper.Discord.Internal.Types.Interactions.Interaction.Kind.Free.Kind as Kind

{-# ANN Free "HLint: ignore Use newtype instead of data" #-}
data Free = Free Kind.Kind