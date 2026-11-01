module DiscordWrapper.Discord.Internal.Types.Interactions.Interaction.Kind.Channel (
    Channel (..), ChannelData (..)
) where

import qualified Discord.Internal.Types
import qualified Discord.Internal.Types.Interactions
import qualified DiscordWrapper.Discord.Internal.Types.Interactions.Interaction.Kind.Channel.Kind as Kind
import qualified Data.Text as T

data Channel = Channel ChannelData Kind.Kind

data ChannelData = ChannelData {
    -- | What guild this interaction comes from.
    interactionGuildId :: Maybe Discord.Internal.Types.GuildId,
    -- | What channel this interaction comes from.
    interactionChannelId :: Maybe Discord.Internal.Types.ChannelId,
    -- | What user/member this interaction comes from.
    interactionUser :: Discord.Internal.Types.Interactions.MemberOrUser,
    -- | What message is associated with this interaction.
    interactionMessage :: Discord.Internal.Types.Message,
    -- | The invoking user's preferred locale.
    interactionLocale :: T.Text,
    -- | The invoking guild's preferred locale.
    interactionGuildLocale :: Maybe T.Text
}