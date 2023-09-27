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

import Control.Applicative (empty)
import GHC.Driver.Types (HsParsedModule(..))
import GHC.Plugins (DynFlags, Plugin(..))
import GHC.Utils.Error (WarnMsg)
import Language.Haskell.HLint (Idea(..))

import qualified GHC.Data.Bag as Bag
import qualified GHC.Plugins as Plugins
import qualified GHC.Utils.Error as Error
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

        let HsParsedModule{ hpm_annotations, hpm_module } = hsParsedModule

        dynFlags <- Plugins.getDynFlags

        let moduleEx = HLint.createModuleEx hpm_annotations hpm_module

        let ideas = HLint.applyHints classifies hint [moduleEx]

        let msgEnvelopes = concatMap (ideaToMsgEnvelope dynFlags) ideas

        let messages = Bag.listToBag msgEnvelopes

        Plugins.liftIO (Plugins.printOrThrowWarnings dynFlags messages)

        pure hsParsedModule

ideaToMsgEnvelope :: DynFlags -> Idea -> [WarnMsg]
ideaToMsgEnvelope dynFlags Idea{..} = do
    makeMessage <- case ideaSeverity of
        HLint.Ignore -> empty
        HLint.Suggestion -> pure Error.mkPlainWarnMsg
        HLint.Warning -> pure Error.mkPlainWarnMsg
        HLint.Error -> pure Error.mkPlainErrMsg

    return (makeMessage dynFlags ideaSpan sdoc)
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
