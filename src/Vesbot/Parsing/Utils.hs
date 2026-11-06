{-# LANGUAGE NamedFieldPuns #-}

module Vesbot.Parsing.Utils (
    getInteractionUser
) where


import Vesbot.Parsing (getUserData)
import Discord.Interactions
import Discord.Types

getInteractionUser :: Interaction -> Maybe UserId
getInteractionUser (InteractionComponent                      { interactionUser }) = getUserData interactionUser
getInteractionUser (InteractionPing                           {                 }) = Nothing
getInteractionUser (InteractionApplicationCommand             { interactionUser }) = getUserData interactionUser
getInteractionUser (InteractionApplicationCommandAutocomplete { interactionUser }) = getUserData interactionUser
getInteractionUser (InteractionModalSubmit                    { interactionUser }) = getUserData interactionUser