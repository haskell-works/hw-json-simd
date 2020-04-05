module App.Commands where

import App.Commands.Capabilities
import App.Commands.CreateIndex
import Options.Applicative

commands :: Parser (IO ())
commands = commandsGeneral

commandsGeneral :: Parser (IO ())
commandsGeneral = subparser $ mempty
  <>  commandGroup "Commands:"
  <>  cmdCapabilities
  <>  cmdCreateIndex
