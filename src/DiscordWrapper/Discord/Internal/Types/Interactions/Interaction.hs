{-# LANGUAGE OverloadedRecordDot #-}
module DiscordWrapper.Discord.Internal.Types.Interactions.Interaction (
    Interaction(..), Common(..), fromDiscord
) where

import qualified Data.Text as T
import qualified Discord.Internal.Types as DTypes
import qualified Discord.Internal.Types.Interactions as DInteractions
import qualified DiscordWrapper.Discord.Internal.Types.Interactions.Interaction.Kind as Kind

data Interaction = Interaction Common Kind.Kind

data Common = Common {
    -- | The id of this interaction.
    interactionId :: DTypes.InteractionId,
        -- | The id of the application that this interaction belongs to.
    interactionApplicationId :: DTypes.ApplicationId,
        -- | The unique token that represents this interaction.
    interactionToken :: DTypes.InteractionToken,
        -- | What version of interaction is this (always 1).
    interactionVersion :: Int,
        -- | What permissions does the app or bot have within the sent channel.
    interactionPermissions :: Maybe T.Text
}

fromDiscord :: DInteractions.Interaction -> Interaction
fromDiscord i = Interaction common kind
  where
    common = Common
        i.interactionId
        i.interactionApplicationId
        i.interactionToken
        i.interactionVersion
        i.interactionPermissions
    kind = Kind.fromDiscord i