module DiscordWrapper.Discord.Internal.Types.Interactions.Interaction.Kind.Channel.Kind (
    Kind (..)
) where

import qualified Discord.Internal.Types.Interactions as D

data Kind = Component D.ComponentData | ApplicationCommand D.ApplicationCommandData | ApplicationCommandAutocomplete D.ApplicationCommandData | ModalSubmit D.ModalData