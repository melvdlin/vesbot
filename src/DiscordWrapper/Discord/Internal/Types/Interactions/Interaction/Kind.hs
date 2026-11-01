{-# LANGUAGE OverloadedRecordDot #-}

module DiscordWrapper.Discord.Internal.Types.Interactions.Interaction.Kind (
    Kind (..),
    fromDiscord
) where

import qualified DiscordWrapper.Discord.Internal.Types.Interactions.Interaction.Kind.Free as Free
import qualified DiscordWrapper.Discord.Internal.Types.Interactions.Interaction.Kind.Free.Kind as Free.Kind
import qualified DiscordWrapper.Discord.Internal.Types.Interactions.Interaction.Kind.Channel as Channel
import qualified DiscordWrapper.Discord.Internal.Types.Interactions.Interaction.Kind.Channel.Kind as Channel.Kind

import qualified Discord.Internal.Types.Interactions as DInteractions
import qualified DiscordWrapper.Discord.Internal.Types.Interactions.Interaction.Kind.Channel as Kind.Channel

data Kind = Free Free.Free | Channel Channel.Channel

fromDiscord :: DInteractions.Interaction -> Kind
fromDiscord i = case i of
    DInteractions.InteractionComponent {}
        -> Channel $ Channel.Channel channelData $ Channel.Kind.Component i.componentData
    DInteractions.InteractionPing {}
        -> Free $ Free.Free Free.Kind.Ping
    DInteractions.InteractionApplicationCommand {}
        -> Channel $ Channel.Channel channelData $ Channel.Kind.ApplicationCommand i.applicationCommandData
    DInteractions.InteractionApplicationCommandAutocomplete {}
        -> Channel $ Channel.Channel channelData $ Channel.Kind.ApplicationCommandAutocomplete i.applicationCommandData
    DInteractions.InteractionModalSubmit {}
        -> Channel $ Channel.Channel channelData $ Channel.Kind.ModalSubmit i.modalData
  where
    channelData = Kind.Channel.ChannelData
        i.interactionGuildId
        i.interactionChannelId
        i.interactionUser
        i.interactionMessage
        i.interactionLocale
        i.interactionGuildLocale