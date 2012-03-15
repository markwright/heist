{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-|

This module contains the core Heist data types.  SpliceT intentionally
does not expose any of its functionality via MonadState or MonadReader
functions.  We define passthrough instances for the most common types of
monads.  These instances allow the user to use SpliceT in a monad stack
without needing calls to `lift`.

Edward Kmett wrote most of the SpliceT code and associated instances,
liberating us from the unused writer portion of RWST.

-}

module Text.Templating.Heist.Types where

------------------------------------------------------------------------------
import             Control.Applicative
import             Control.Monad.CatchIO
import             Control.Monad.Cont
import             Control.Monad.Error
import             Control.Monad.Reader
import             Control.Monad.State
import             Data.ByteString.Char8 (ByteString)
import qualified   Data.HashMap.Strict as H
import             Data.HashMap.Strict (HashMap)
import             Data.List
import             Data.Monoid
import             Data.Text (Text)
import             Data.Typeable
import             Prelude hiding (catch)
import qualified   Text.XmlHtml as X


------------------------------------------------------------------------------
-- | A 'Template' is a forest of XML nodes.  Here we deviate from the "single
-- root node" constraint of well-formed XML because we want to allow templates
-- to contain fragments of a document that may not have a single root.
type Template = [X.Node]


------------------------------------------------------------------------------
-- | MIME Type.  The type alias is here to make the API clearer.
type MIMEType = ByteString


------------------------------------------------------------------------------
-- | Reversed list of directories.  This holds the path to the template
-- currently being processed.
type TPath = [ByteString]


data DocumentFile = DocumentFile
    { dfDoc  :: X.Document
    , dfFile :: Maybe FilePath
    } deriving (Eq)

------------------------------------------------------------------------------
-- | All documents representing templates are stored in a map.
type TemplateMap = HashMap TPath DocumentFile


------------------------------------------------------------------------------
-- | A Splice is a SpliceT computation that returns a 'Template'.
type Splice m = SpliceT m Template


------------------------------------------------------------------------------
-- | SpliceMap associates a name and a Splice.
type SpliceMap m = HashMap Text (Splice m)


------------------------------------------------------------------------------
-- | Output comes in interleaving sections of monadic actions and pure chunks.
-- The idea is that adjacent pure chunks can be consolidated. We also want
-- to left fold over a series of 'yield' and 'actLater' actions as efficiently
-- as possible.
data OutputChunk c m = Pure !c
                     | Act !c !(m c) !c
                     -- ^ An action with optional pure prefix and suffix


------------------------------------------------------------------------------
(~>) :: (Monoid c, Monad m) => m c -> m c -> m c
(~>) m n = do
    !a <- m
    !b <- n
    return $! a `mappend` b
infixl 3 ~>
{-# INLINE (~>) #-}


------------------------------------------------------------------------------
instance (Monoid c, Monad m) => Monoid (OutputChunk c m) where
    mempty  = Pure mempty
    mconcat = foldl' mappend mempty

    (Pure a   ) `mappend` (Pure b   ) = Pure (a `mappend` b)
    (Pure a   ) `mappend` (Act b m c) = Act  (a `mappend` b) m c
    (Act a m b) `mappend` (Pure c   ) = Act  a m (b `mappend` c)
    (Act a m b) `mappend` (Act c n d) =
        Act a (m ~> return (b `mappend` c) ~> n) d


------------------------------------------------------------------------------
-- | Holds all the state information needed for template processing.  You will
-- build a @HeistState@ using any of Heist's @HeistState m -> HeistState m@
-- \"filter\" functions.  Then you use the resulting @HeistState@ in calls to
-- @renderTemplate@.
data HeistState m = HeistState {
    -- | A mapping of splice names to splice actions
      _spliceMap       :: SpliceMap m
    -- | A mapping of template names to templates
    , _templateMap     :: TemplateMap
    -- | The doctypes encountered during template processing.
    , _doctypes        :: [X.DocType]

--    -- | A hook run on all templates at load time.
--    , _onLoadHook      :: Template -> IO Template
--    -- | A hook run on all templates just before they are rendered.
--    , _preRunHook      :: Template -> m Template
--    -- | A hook run on all templates just after they are rendered.
--    , _postRunHook     :: Template -> m Template
}


------------------------------------------------------------------------------
-- | SpliceState shouldn't need type parameters because splices have already
-- been expanded or simply exist in the SpliceT monad.
data SpliceState = SpliceState {
    -- | A flag to control splice recursion
    -- FIXME Probably not needed in caper
      _recurse         :: Bool
    , _spliceNode      :: X.Node
    -- | The path to the template currently being processed.
    , _curContext      :: TPath
    -- | A counter keeping track of the current recursion depth to prevent
    -- infinite loops.
    , _recursionDepth  :: Int
    -- | The full path to the current template's file on disk.
    , _curTemplateFile :: Maybe FilePath
}


------------------------------------------------------------------------------
-- | Gets the names of all the templates defined in a HeistState.
templateNames :: HeistState m -> [TPath]
templateNames ts = H.keys $ _templateMap ts


------------------------------------------------------------------------------
-- | Gets the names of all the splices defined in a HeistState.
spliceNames :: HeistState m -> [Text]
spliceNames ts = H.keys $ _spliceMap ts


------------------------------------------------------------------------------
instance (Monad m) => Monoid (HeistState m) where
    mempty = HeistState H.empty H.empty []

    (HeistState s1 t1 dt1) `mappend` (HeistState s2 t2 dt2) =
        HeistState s t (dt1 `mappend` dt2)
      where
        s = s1 `mappend` s2
        t = t1 `mappend` t2


------------------------------------------------------------------------------
-- | The Typeable instance is here so Heist can be dynamically executed with
-- Hint.
templateStateTyCon :: TyCon
templateStateTyCon = mkTyCon "Text.Templating.Heist.HeistState"
{-# NOINLINE templateStateTyCon #-}

instance (Typeable1 m) => Typeable (HeistState m) where
    typeOf _ = mkTyConApp templateStateTyCon [typeOf1 (undefined :: m ())]


------------------------------------------------------------------------------
-- | SpliceT is the monad used for 'Splice' processing.  SpliceT provides
-- \"passthrough\" instances for many of the monads you might use in the inner
-- monad.
newtype SpliceT m a = SpliceT {
    unSpliceT :: StateT SpliceState m a
} deriving ( Functor
           , Monad
           , MonadTrans
           , Applicative
           , Alternative
           , MonadFix
           , MonadPlus
           , MonadIO
           , MonadCatchIO
           , MonadReader r
           , MonadError e
           , MonadCont
           )


runSpliceT :: SpliceT m a -> SpliceState -> m (a, SpliceState)
runSpliceT = runStateT . unSpliceT


------------------------------------------------------------------------------
-- | Evaluates a template monad as a computation in the underlying monad.
evalSpliceT :: Monad m
           => SpliceT m a
           -> SpliceState
           -> m a
evalSpliceT m = evalStateT (unSpliceT m)
{-# INLINE evalSpliceT #-}


------------------------------------------------------------------------------
-- | MonadState passthrough instance
instance MonadState s m => MonadState s (SpliceT m) where
    get = lift get
    {-# INLINE get #-}
    put = lift . put
    {-# INLINE put #-}


------------------------------------------------------------------------------
-- | The Typeable instance is here so Heist can be dynamically executed with
-- Hint.
templateMonadTyCon :: TyCon
templateMonadTyCon = mkTyCon "Text.Templating.Heist.SpliceT"
{-# NOINLINE templateMonadTyCon #-}

instance (Typeable1 m) => Typeable1 (SpliceT m) where
    typeOf1 _ = mkTyConApp templateMonadTyCon [typeOf1 (undefined :: m ())]


------------------------------------------------------------------------------
-- Type class enabling easy Heist support in other monads.
------------------------------------------------------------------------------


class (Monad m, Monad (Under m)) => MonadSplice m where
    type Under m :: * -> *
    liftSplice :: SpliceT (Under m) a -> m a


instance (Monad m) => MonadSplice (SpliceT m) where
    type Under (SpliceT m) = m
    liftSplice = id


------------------------------------------------------------------------------
-- Functions for our monad.
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Gets the node currently being processed.
--
--   > <speech author="Shakespeare">
--   >   To sleep, perchance to dream.
--   > </speech>
--
-- When you call @getParamNode@ inside the code for the @speech@ splice, it
-- returns the Node for the @speech@ tag and its children.  @getParamNode >>=
-- childNodes@ returns a list containing one 'TextNode' containing part of
-- Hamlet's speech.  @liftM (getAttribute \"author\") getParamNode@ would
-- return @Just "Shakespeare"@.
getParamNode :: MonadSplice m => m X.Node
getParamNode = getsTS _spliceNode
{-# INLINE getParamNode #-}


------------------------------------------------------------------------------
-- | SpliceT's 'local'.
localParamNode :: MonadSplice m
               => (X.Node -> X.Node)
               -> m a
               -> m a
localParamNode f = localTS (\s -> s { _spliceNode = f (_spliceNode s) })
{-# INLINE localParamNode #-}


------------------------------------------------------------------------------
-- | SpliceT's 'gets'.
getsTS :: MonadSplice m => (SpliceState -> r) -> m r
getsTS = liftSplice . SpliceT . gets
{-# INLINE getsTS #-}


------------------------------------------------------------------------------
-- | SpliceT's 'get'.
getTS :: MonadSplice m => m (SpliceState)
getTS = liftSplice $ SpliceT get
{-# INLINE getTS #-}


------------------------------------------------------------------------------
-- | SpliceT's 'put'.
putTS :: MonadSplice m => SpliceState -> m ()
putTS = liftSplice . SpliceT . put
{-# INLINE putTS #-}


------------------------------------------------------------------------------
-- | SpliceT's 'modify'.
modifyTS :: MonadSplice m
         => (SpliceState -> SpliceState)
         -> m ()
modifyTS = liftSplice . SpliceT . modify
{-# INLINE modifyTS #-}


------------------------------------------------------------------------------
-- | Restores the SpliceState.  This function is almost like putTS except it
-- preserves the current doctypes.  You should use this function instead of
-- @putTS@ to restore an old state.  This was needed because doctypes needs to
-- be in a "global scope" as opposed to the template call "local scope" of
-- state items such as recursionDepth, curContext, and spliceMap.
restoreTS :: MonadSplice m => SpliceState -> m ()
restoreTS = putTS
--restoreTS old = modifyTS (\cur -> old { _doctypes = _doctypes cur })
{-# INLINE restoreTS #-}


------------------------------------------------------------------------------
-- | Abstracts the common pattern of running a SpliceT computation with
-- a modified heist state.
localTS :: (Under (SpliceT (Under m)) ~ Under m, MonadSplice m)
        => (SpliceState -> SpliceState)
        -> m a
        -> m a
localTS f k = do
    ts <- getTS
    putTS $ f ts
    res <- k
    restoreTS ts
    return res
{-# INLINE localTS #-}

