{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module provides a GHC plugin that you can use to run HLint on a
--   module
--
--  To do so, add this @hlint-plugin@ package as a build dependency of your
--  Haskell package and then add the GHC options @-fplugin HLint.Plugin@
--  To use this plugin, add this package as a build dependency and then enable
--  the following GHC options (typically in the @ghc-options:@ field of your
--  @.cabal@ file):
--
--  > -fplugin HLint.Plugin
--
--  You can pass command-line options to @hlint@ using @-fplugin-opt@, like
--  this:
--
--  > -fplugin HLint.Plugin -fplugin-opt='HLint.Plugin:--ignore=Redundant guard'
module HLint.Plugin
    ( -- * Plugin
      plugin
    ) where

import GHC.Driver.Flags (WarnReason(..))
import GHC.Hs (HsParsedModule(..))
import GHC.Plugins (Plugin(..))
import GHC.Types.Error (DecoratedSDoc, MsgEnvelope(..))
import Language.Haskell.HLint (Idea(..))

import qualified GHC.Data.Bag as Bag
import qualified GHC.Driver.Errors as Errors
import qualified GHC.Plugins as Plugins
import qualified GHC.Types.Error as Error
import qualified GHC.Utils.Logger as Logger
import qualified GHC.Utils.Outputable as Outputable
import qualified HLint.Plugin.Settings as Settings
import qualified Language.Haskell.HLint as HLint

-- | GHC plugin that runs HLint on a Haskell module after parsing the module
plugin :: Plugin
plugin = Plugins.defaultPlugin
    { parsedResultAction
    , pluginRecompile = Plugins.purePlugin
    }
  where
    parsedResultAction arguments _ hsParsedModule = do
        (_parseFlags, classifies, hint) <- do
            Plugins.liftIO (Settings.argsSettings arguments)

        let HsParsedModule{ hpm_module } = hsParsedModule

        dynFlags <- Plugins.getDynFlags

        logger <- Logger.getLogger

        let moduleEx = HLint.createModuleEx hpm_module

        let ideas = HLint.applyHints classifies hint [moduleEx]

        let msgEnvelopes = map ideaToMsgEnvelope ideas

        let messages = Bag.listToBag msgEnvelopes

        Plugins.liftIO (Errors.printOrThrowWarnings logger dynFlags messages)

        pure hsParsedModule

ideaToMsgEnvelope :: Idea -> MsgEnvelope DecoratedSDoc
ideaToMsgEnvelope Idea{..} = MsgEnvelope{..}
  where
    errMsgDiagnostic =
        Error.mkDecorated [ Plugins.vcat (sdocs0 <> sdocs1 <> sdocs2) ]
      where
        sdocs0 = [ Plugins.text ideaHint ]

        sdocs1 =
            case ideaTo of
                Nothing -> []
                Just "" -> []
                Just to -> [ Plugins.text ("Perhaps: " <> to) ]

        sdocs2 = do
            note <- ideaNote

            pure (Plugins.text ("Note: " <> show note))

    errMsgSpan = ideaSpan

    errMsgContext = Outputable.alwaysQualify

    errMsgSeverity =
        case ideaSeverity of
            HLint.Ignore -> Error.SevDump
            HLint.Suggestion -> Error.SevInfo
            HLint.Warning -> Error.SevWarning
            HLint.Error -> Error.SevError

    errMsgReason = NoReason
