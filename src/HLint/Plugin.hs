{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module provides a GHC plugin that you can use to run HLint on a
--   module
--
--  To do so, add this @hlint-plugin@ package as a build dependency of your
--  Haskell package and then add the GHC options
--  @-plugin-package hlint-plugin -fplugin HLint.Plugin@
module HLint.Plugin
    ( -- * Plugin
      plugin
    ) where

import GHC.Driver.Errors.Types (GhcMessage)
import GHC.Hs (HsParsedModule(..))
import GHC.Plugins (Plugin(..))
import Language.Haskell.HLint (Idea(..))

import GHC.Types.Error
    ( DiagnosticMessage(..)
    , DiagnosticReason(..)
    , MsgEnvelope(..)
    )

import qualified GHC.Data.Bag as Bag
import qualified GHC.Driver.Config.Diagnostic as Diagnostic
import qualified GHC.Driver.Errors as Errors
import qualified GHC.Driver.Errors.Types as Errors.Types
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
    parsedResultAction :: [Plugins.CommandLineOption] -> Plugins.ModSummary -> Plugins.ParsedResult -> Plugins.Hsc Plugins.ParsedResult
    parsedResultAction arguments _ parsedResult = do
        (_parseFlags, classifies, hint) <- do
            Plugins.liftIO (Settings.argsSettings arguments)

        dynFlags <- Plugins.getDynFlags

        logger <- Logger.getLogger

        let HsParsedModule{ hpm_module } =
                Plugins.parsedResultModule parsedResult

        let moduleEx = HLint.createModuleEx hpm_module

        let ideas = HLint.applyHints classifies hint [moduleEx]

        let msgEnvelopes = map ideaToMsgEnvelope ideas

        let messages = Error.mkMessages (Bag.listToBag msgEnvelopes)

        let diagOpts = Diagnostic.initDiagOpts dynFlags

        let ghcMessageOpts = Diagnostic.initPrintConfig dynFlags

        Plugins.liftIO (Errors.printOrThrowDiagnostics logger ghcMessageOpts diagOpts messages)

        pure parsedResult

ideaToMsgEnvelope :: Idea -> MsgEnvelope GhcMessage
ideaToMsgEnvelope Idea{..} = MsgEnvelope{..}
  where
    sdoc = Plugins.vcat (sdocs0 <> sdocs1 <> sdocs2)
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

    diagnosticMessage = DiagnosticMessage{..}
      where
        diagMessage = Error.mkSimpleDecorated sdoc
        diagReason =
            case ideaSeverity of
                HLint.Ignore -> WarningWithoutFlag
                HLint.Suggestion -> WarningWithoutFlag
                HLint.Warning -> WarningWithoutFlag
                HLint.Error -> ErrorWithoutFlag

        diagHints = []

    errMsgDiagnostic =
        Errors.Types.ghcUnknownMessage diagnosticMessage

    errMsgSeverity =
        case ideaSeverity of
            HLint.Ignore -> Error.SevIgnore
            HLint.Suggestion -> Error.SevWarning
            HLint.Warning -> Error.SevWarning
            HLint.Error -> Error.SevError
