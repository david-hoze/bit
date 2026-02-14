{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Bit.Core
    ( -- Repo initialization
      init
    , initializeRepoAt
    , InitOptions(..)
    , defaultInitOptions

      -- Git passthrough (these take args from the CLI)
    , add
    , commit
    , diff
    , log
    , lsFiles
    , restore
    , checkout
    , status
    , reset
    , rm
    , mv
    , branch
    , merge

      -- Core sync operations
    , push
    , pull
    , fetch

      -- Verification
    , VerifyTarget(..)
    , RepairMode(..)
    , verify
    , repair
    , fsck

      -- Remote management
    , remoteAdd
    , remoteShow

      -- Merge management
    , mergeContinue
    , mergeAbort

      -- Branch management
    , unsetUpstream

      -- Types re-exported for Commands.hs
    , PullMode(..)
    , PullOptions(..)
    , defaultPullOptions
    ) where

import Prelude hiding (init, log)
import Bit.Core.Helpers (PullMode(..), PullOptions(..), defaultPullOptions)
import Bit.Core.Init (init, initializeRepoAt, InitOptions(..), defaultInitOptions)
import Bit.Git.Passthrough
    ( add
    , commit
    , diff
    , log
    , lsFiles
    , restore
    , checkout
    , status
    , reset
    , rm
    , mv
    , branch
    , merge
    , mergeContinue
    , mergeAbort
    , unsetUpstream
    )
import Bit.Core.Push (push)
import Bit.Core.Pull (pull)
import Bit.Core.Fetch (fetch)
import Bit.Core.RemoteManagement (remoteAdd, remoteShow)
import Bit.Core.Verify (VerifyTarget(..), RepairMode(..), verify, repair, fsck)

