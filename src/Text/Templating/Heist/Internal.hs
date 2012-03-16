{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Text.Templating.Heist.Internal where

------------------------------------------------------------------------------
import             Blaze.ByteString.Builder
import             Control.Applicative
import             Control.Arrow hiding (loop)
import             Control.Exception (SomeException)
import             Control.Monad
import             Control.Monad.CatchIO
import             Control.Monad.Trans
import qualified   Data.Attoparsec.Text as AP
import             Data.ByteString (ByteString)
import qualified   Data.ByteString as B
import qualified   Data.ByteString.Char8 as BC
import             Data.Either
import qualified   Data.Foldable as F
import             Data.List
import qualified   Data.HashMap.Strict as Map
import             Data.HashMap.Strict (HashMap)
import             Data.Maybe
import             Data.Monoid
import qualified   Data.Text as T
import             Data.Text (Text)
import             Prelude hiding (catch)
import             System.Directory.Tree hiding (name)
import             System.FilePath
import qualified   Text.XmlHtml as X

------------------------------------------------------------------------------
import             Text.Templating.Heist.Types


------------------------------------------------------------------------------
-- | The maximum recursion depth.  (Used to prevent infinite loops.)
mAX_RECURSION_DEPTH :: Int
mAX_RECURSION_DEPTH = 50


-- ------------------------------------------------------------------------------
-- -- | Mappends a doctype to the state.
-- addDoctype :: Monad m => [X.DocType] -> SpliceT m ()
-- addDoctype dt = do
--     modifyTS (\s -> s { _doctypes = _doctypes s `mappend` dt })
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Adds an on-load hook to a `HeistState`.
-- addOnLoadHook :: (Monad m) =>
--                  (Template -> IO Template)
--               -> HeistState m
--               -> HeistState m
-- addOnLoadHook hook ts = ts { _onLoadHook = _onLoadHook ts >=> hook }
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Adds a pre-run hook to a `HeistState`.
-- addPreRunHook :: (Monad m) =>
--                  (Template -> m Template)
--               -> HeistState m
--               -> HeistState m
-- addPreRunHook hook ts = ts { _preRunHook = _preRunHook ts >=> hook }
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Adds a post-run hook to a `HeistState`.
-- addPostRunHook :: (Monad m) =>
--                   (Template -> m Template)
--                -> HeistState m
--                -> HeistState m
-- addPostRunHook hook ts = ts { _postRunHook = _postRunHook ts >=> hook }
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Binds a new splice declaration to a tag name within a 'HeistState'.
-- bindSplice :: Monad m =>
--               Text              -- ^ tag name
--            -> Splice m          -- ^ splice action
--            -> SpliceState      -- ^ source state
--            -> SpliceState
-- bindSplice n v ts = ts {_spliceMap = Map.insert n v (_spliceMap ts)}
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Binds a set of new splice declarations within a 'HeistState'.
-- bindSplices :: Monad m =>
--                [(Text, Splice m)] -- ^ splices to bind
--             -> SpliceState       -- ^ start state
--             -> SpliceState
-- bindSplices ss ts = foldl' (flip id) ts acts
--   where
--     acts = map (uncurry bindSplice) ss
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Sets the current template file.
-- setCurTemplateFile :: Maybe FilePath -> SpliceState -> SpliceState
-- setCurTemplateFile fp ts = ts { _curTemplateFile = fp }
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Converts 'Text' to a splice returning a single 'TextNode'.
-- textSplice :: (Monad m) => Text -> Splice m
-- textSplice t = return [X.TextNode t]
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Runs the parameter node's children and returns the resulting node list.
-- -- By itself this function is a simple passthrough splice that makes the
-- -- spliced node disappear.  In combination with locally bound splices, this
-- -- function makes it easier to pass the desired view into your splices.
-- runChildren :: Monad m => Splice m
-- runChildren = runNodeList . X.childNodes =<< getParamNode
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Binds a list of splices before using the children of the spliced node as
-- -- a view.
-- runChildrenWith :: (Monad m)
--                 => [(Text, Splice m)]
--                 -- ^ List of splices to bind before running the param nodes.
--                 -> Splice m
--                 -- ^ Returns the passed in view.
-- runChildrenWith splices = localTS (bindSplices splices) runChildren
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Wrapper around runChildrenWith that applies a transformation function to
-- -- the second item in each of the tuples before calling runChildrenWith.
-- runChildrenWithTrans :: (Monad m)
--           => (b -> Splice m)
--           -- ^ Splice generating function
--           -> [(Text, b)]
--           -- ^ List of tuples to be bound
--           -> Splice m
-- runChildrenWithTrans f = runChildrenWith . map (second f)
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Like runChildrenWith but using constant templates rather than dynamic
-- -- splices.
-- runChildrenWithTemplates :: (Monad m) => [(Text, Template)] -> Splice m
-- runChildrenWithTemplates = runChildrenWithTrans return
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Like runChildrenWith but using literal text rather than dynamic splices.
-- runChildrenWithText :: (Monad m) => [(Text, Text)] -> Splice m
-- runChildrenWithText = runChildrenWithTrans textSplice
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Maps a splice generating function over a list and concatenates the
-- -- results.
-- mapSplices :: (Monad m)
--         => (a -> Splice m)
--         -- ^ Splice generating function
--         -> [a]
--         -- ^ List of items to generate splices for
--         -> Splice m
--         -- ^ The result of all splices concatenated together.
-- mapSplices f vs = liftM concat $ mapM f vs
-- {-# INLINE mapSplices #-}
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Convenience function for looking up a splice.
-- lookupSplice :: Monad m =>
--                 Text
--              -> SpliceState
--              -> Maybe (Splice m)
-- lookupSplice nm ts = Map.lookup nm $ _spliceMap ts
-- {-# INLINE lookupSplice #-}
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Does a single template lookup without cascading up.
-- singleLookup :: TemplateMap
--              -> TPath
--              -> ByteString
--              -> Maybe (DocumentFile, TPath)
-- singleLookup tm path name = fmap (\a -> (a,path)) $ Map.lookup (name:path) tm
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Searches for a template by looking in the full path then backing up into
-- -- each of the parent directories until the template is found.
-- traversePath :: TemplateMap
--              -> TPath
--              -> ByteString
--              -> Maybe (DocumentFile, TPath)
-- traversePath tm [] name = fmap (\a -> (a,[])) (Map.lookup [name] tm)
-- traversePath tm path name =
--     singleLookup tm path name `mplus`
--     traversePath tm (tail path) name
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Returns 'True' if the given template can be found in the heist state.
-- hasTemplate :: ByteString -> SpliceState -> Bool
-- hasTemplate nameStr ts = isJust $ lookupTemplate nameStr ts
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Convenience function for looking up a template.
-- lookupTemplate :: ByteString
--                -> SpliceState
--                -> Maybe (DocumentFile, TPath)
-- lookupTemplate nameStr ts =
--     f (_templateMap ts) path name
--   where (name:p) = case splitTemplatePath nameStr of
--                        [] -> [""]
--                        ps -> ps
--         ctx = if B.isPrefixOf "/" nameStr then [] else _curContext ts
--         path = p ++ ctx
--         f = if '/' `BC.elem` nameStr
--                 then singleLookup
--                 else traversePath
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Sets the templateMap in a HeistState.
-- setTemplates :: TemplateMap -> SpliceState -> SpliceState
-- setTemplates m ts = ts { _templateMap = m }
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Adds a template to the heist state.
-- insertTemplate :: TPath -> DocumentFile -> SpliceState -> SpliceState
-- insertTemplate p t st =
--     setTemplates (Map.insert p t (_templateMap st)) st
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Adds an HTML format template to the heist state.
-- addTemplate :: ByteString
--             -- ^ Path that the template will be referenced by
--             -> Template
--             -- ^ The template's DOM nodes
--             -> Maybe FilePath
--             -- ^ An optional path to the actual file on disk where the
--             -- template is stored
--             -> SpliceState
--             -> SpliceState
-- addTemplate n t mfp st =
--     insertTemplate (splitTemplatePath n) doc st
--   where
--     doc = DocumentFile (X.HtmlDocument X.UTF8 Nothing t) mfp
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Adds an XML format template to the heist state.
-- addXMLTemplate :: ByteString
--                -- ^ Path that the template will be referenced by
--                -> Template
--                -- ^ The template's DOM nodes
--                -> Maybe FilePath
--                -- ^ An optional path to the actual file on disk where the
--                -- template is stored
--                -> SpliceState
--                -> SpliceState
-- addXMLTemplate n t mfp st =
--     insertTemplate (splitTemplatePath n) doc st
--   where
--     doc = DocumentFile (X.XmlDocument X.UTF8 Nothing t) mfp
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Stops the recursive processing of splices.  Consider the following
-- -- example:
-- --
-- --   > <foo>
-- --   >   <bar>
-- --   >     ...
-- --   >   </bar>
-- --   > </foo>
-- --
-- -- Assume that @\"foo\"@ is bound to a splice procedure. Running the @foo@
-- -- splice will result in a list of nodes @L@.  Normally @foo@ will recursively
-- -- scan @L@ for splices and run them.  If @foo@ calls @stopRecursion@, @L@
-- -- will be included in the output verbatim without running any splices.
-- stopRecursion :: Monad m => SpliceT m ()
-- stopRecursion = modifyTS (\st -> st { _recurse = False })
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Sets the current context
-- setContext :: Monad m => TPath -> SpliceT m ()
-- setContext c = modifyTS (\st -> st { _curContext = c })
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Gets the current context
-- getContext :: Monad m => SpliceT m TPath
-- getContext = getsTS _curContext
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Gets the full path to the file holding the template currently being
-- -- processed.  Returns Nothing if the template is not associated with a file
-- -- on disk or if there is no template being processed.
-- getTemplateFilePath :: Monad m => SpliceT m (Maybe FilePath)
-- getTemplateFilePath = getsTS _curTemplateFile
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Performs splice processing on a list of nodes.
-- runNodeList :: Monad m => [X.Node] -> Splice m
-- runNodeList = mapSplices runNode
-- {-# INLINE runNodeList #-}
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Checks the recursion flag and recurses accordingly.  Does not recurse
-- -- deeper than mAX_RECURSION_DEPTH to avoid infinite loops.
-- recurseSplice :: Monad m => X.Node -> Splice m -> Splice m
-- recurseSplice node splice = do
--     result <- localParamNode (const node) splice
--     ts' <- getTS
--     if _recurse ts' && _recursionDepth ts' < mAX_RECURSION_DEPTH
--         then do modRecursionDepth (+1)
--                 res <- runNodeList result
--                 restoreTS ts'
--                 return res
--         else return result
--   where
--     modRecursionDepth :: Monad m => (Int -> Int) -> SpliceT m ()
--     modRecursionDepth f =
--         modifyTS (\st -> st { _recursionDepth = f (_recursionDepth st) })
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Looks up a template name runs a 'SpliceT' computation on it.
-- lookupAndRun :: Monad m
--              => ByteString
--              -> ((DocumentFile, TPath) -> SpliceT m (Maybe a))
--              -> SpliceT m (Maybe a)
-- lookupAndRun name k = do
--     ts <- getTS
--     let mt = lookupTemplate name ts
--     let curPath = join $ fmap (dfFile . fst) mt
--     modifyTS (setCurTemplateFile curPath)
--     maybe (return Nothing) k mt
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Looks up a template name evaluates it by calling runNodeList.
-- evalTemplate :: Monad m
--             => ByteString
--             -> SpliceT m (Maybe Template)
-- evalTemplate name = lookupAndRun name
--     (\(t,ctx) -> localTS (\ts -> ts {_curContext = ctx})
--                          (liftM Just $ runNodeList $ X.docContent $ dfDoc t))
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Sets the document type of a 'X.Document' based on the 'SpliceT'
-- -- value.
-- fixDocType :: Monad m => X.Document -> SpliceT m X.Document
-- fixDocType d = do
--     dts <- getsTS _doctypes
--     return $ d { X.docType = listToMaybe dts }
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Same as evalWithHooks, but returns the entire 'X.Document' rather than
-- -- just the nodes.  This is the right thing to do if we are starting at the
-- -- top level.
-- evalWithHooksInternal :: Monad m
--                       => ByteString
--                       -> SpliceT m (Maybe X.Document)
-- evalWithHooksInternal name = lookupAndRun name $ \(t,ctx) -> do
--     addDoctype $ maybeToList $ X.docType $ dfDoc t
--     ts <- getTS
--     nodes <- lift $ {-_preRunHook ts $-} X.docContent $ dfDoc t
--     putTS (ts {_curContext = ctx})
--     res <- runNodeList nodes
--     restoreTS ts
-- --    newNodes <- lift (_postRunHook ts res)
-- --    newDoc   <- fixDocType $ (dfDoc t) { X.docContent = newNodes }
--     newDoc   <- fixDocType $ (dfDoc t) { X.docContent = res }
--     return (Just newDoc)
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Looks up a template name evaluates it by calling runNodeList.  This also
-- -- executes pre- and post-run hooks and adds the doctype.
-- evalWithHooks :: Monad m
--             => ByteString
--             -> SpliceT m (Maybe Template)
-- evalWithHooks name = liftM (liftM X.docContent) (evalWithHooksInternal name)
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Binds a list of constant string splices.
-- bindStrings :: [(Text, Text)] -> SpliceState -> SpliceState
-- bindStrings pairs ts = foldr (uncurry bindString) ts pairs
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Binds a single constant string splice.
-- bindString :: Text -> Text -> SpliceState -> SpliceState
-- bindString n = bindSplice n . textSplice
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Renders a template with the specified parameters.  This is the function
-- -- to use when you want to "call" a template and pass in parameters from
-- -- inside a splice.  If the template does not exist, this version simply
-- -- returns an empty list.
-- callTemplate :: Monad m
--              => ByteString         -- ^ The name of the template
--              -> [(Text, Splice m)] -- ^ Association list of
--                                    -- (name,value) parameter pairs
--              -> SpliceT m Template
-- callTemplate name params = do
--     modifyTS $ bindSplices params
--     liftM (maybe [] id) $ evalTemplate name
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Like callTemplate except the splices being bound are constant text
-- -- splices.
-- callTemplateWithText :: Monad m
--                      => ByteString     -- ^ The name of the template
--                      -> [(Text, Text)] -- ^ Association list of
--                                        -- (name,value) parameter pairs
--                      -> SpliceT m Template
-- callTemplateWithText name params = do
--     modifyTS $ bindStrings params
--     liftM (maybe [] id) $ evalTemplate name
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- Gives the MIME type for a 'X.Document'
-- mimeType :: X.Document -> ByteString
-- mimeType d = case d of
--     (X.HtmlDocument e _ _) -> "text/html;charset=" `BC.append` enc e
--     (X.XmlDocument  e _ _) -> "text/xml;charset="  `BC.append` enc e
--   where
--     enc X.UTF8    = "utf-8"
--     -- Should not include byte order designation for UTF-16 since
--     -- rendering will include a byte order mark. (RFC 2781, Sec. 3.3)
--     enc X.UTF16BE = "utf-16"
--     enc X.UTF16LE = "utf-16"
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Renders a template from the specified HeistState to a 'Builder'.  The
-- -- MIME type returned is based on the detected character encoding, and whether
-- -- the root template was an HTML or XML format template.  It will always be
-- -- @text/html@ or @text/xml@.  If a more specific MIME type is needed for a
-- -- particular XML application, it must be provided by the application.
-- renderTemplate :: Monad m
--                => SpliceState
--                -> ByteString
--                -> m (Maybe (Builder, MIMEType))
-- renderTemplate ts name = evalHeistT tpl (X.TextNode "") ts
--   where tpl = do mt <- evalWithHooksInternal name
--                  case mt of
--                     Nothing  -> return Nothing
--                     Just doc -> return $ Just $ (X.render doc, mimeType doc)
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Renders a template with the specified arguments passed to it.  This is a
-- -- convenience function for the common pattern of calling renderTemplate after
-- -- using bindString, bindStrings, or bindSplice to set up the arguments to the
-- -- template.
-- renderWithArgs :: Monad m
--                    => [(Text, Text)]
--                    -> SpliceState
--                    -> ByteString
--                    -> m (Maybe (Builder, MIMEType))
-- renderWithArgs args ts = renderTemplate (bindStrings args ts)


------------------------------------------------------------------------------
-- Template loading
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Converts a path into an array of the elements in reverse order.  If the
-- path is absolute, we need to remove the leading slash so the split doesn't
-- leave @\"\"@ as the last element of the TPath.
--
-- FIXME @\"..\"@ currently doesn't work in paths, the solution is non-trivial
splitPathWith :: Char -> ByteString -> TPath
splitPathWith s p = if BC.null p then [] else (reverse $ BC.split s path)
  where
    path = if BC.head p == s then BC.tail p else p

-- | Converts a path into an array of the elements in reverse order using the
-- path separator of the local operating system. See 'splitPathWith' for more
-- details.
splitLocalPath :: ByteString -> TPath
splitLocalPath = splitPathWith pathSeparator

-- | Converts a path into an array of the elements in reverse order using a
-- forward slash (/) as the path separator. See 'splitPathWith' for more
-- details.
splitTemplatePath :: ByteString -> TPath
splitTemplatePath = splitPathWith '/'


------------------------------------------------------------------------------
-- | Type synonym for parsers.
type ParserFun = String -> ByteString -> Either String X.Document


------------------------------------------------------------------------------
-- | Reads an HTML or XML template from disk.
getDocWith :: ParserFun -> String -> IO (Either String DocumentFile)
getDocWith parser f = do
    bs <- catch (liftM Right $ B.readFile f)
                (\(e::SomeException) -> return $ Left $ show e)

    let eitherDoc = either Left (parser f) bs
    return $ either (\s -> Left $ f ++ " " ++ s)
                    (\d -> Right $ DocumentFile d (Just f)) eitherDoc


------------------------------------------------------------------------------
-- | Reads an HTML template from disk.
getDoc :: String -> IO (Either String DocumentFile)
getDoc = getDocWith X.parseHTML


------------------------------------------------------------------------------
-- | Reads an XML template from disk.
getXMLDoc :: String -> IO (Either String DocumentFile)
getXMLDoc = getDocWith X.parseHTML


-- ------------------------------------------------------------------------------
-- -- | Runs a template modifying function on a DocumentFile.
-- runHook :: Monad m => (Template -> m Template)
--         -> DocumentFile
--         -> m DocumentFile
-- runHook f t = do
--     n <- f $ X.docContent $ dfDoc t
--     return $ t { dfDoc = (dfDoc t) { X.docContent = n } }
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Runs the onLoad hook on the template and returns the 'HeistState'
-- -- with the result inserted.
-- loadHook :: Monad m => HeistState m -> (TPath, DocumentFile)
--          -> IO (HeistState m)
-- loadHook ts (tp, t) = do
--     t' <- runHook (_onLoadHook ts) t
--     return $ insertTemplate tp t' ts


------------------------------------------------------------------------------
-- | Adds a path prefix to all the templates in the 'HeistState'.  If you
-- want to add multiple levels of directories, separate them with slashes as
-- in "foo/bar".  Using an empty string as a path prefix will leave the
-- 'HeistState' unchanged.
addTemplatePathPrefix :: ByteString -> HeistState m a -> HeistState m a
addTemplatePathPrefix dir ts
  | B.null dir = ts
  | otherwise  = ts { _templateMap = Map.fromList $
                                     map (\(x,y) -> (f x, y)) $
                                     Map.toList $
                                     _templateMap ts
                    }
  where
    f ps = ps++splitTemplatePath dir
------------------------------------------------------------------------------
-- | Loads a template with the specified path and filename.  The
-- template is only loaded if it has a ".tpl" or ".xtpl" extension.
loadTemplate :: String -- ^ path of the template root
             -> String -- ^ full file path (includes the template root)
             -> IO [Either String (TPath, DocumentFile)] --TemplateMap
loadTemplate templateRoot fname
    | isHTMLTemplate = do
        c <- getDoc fname
        return [fmap (\t -> (splitLocalPath $ BC.pack tName, t)) c]
    | isXMLTemplate = do
        c <- getXMLDoc fname
        return [fmap (\t -> (splitLocalPath $ BC.pack tName, t)) c]
    | otherwise = return []
  where -- tName is path relative to the template root directory
        isHTMLTemplate = ".tpl"  `isSuffixOf` fname
        isXMLTemplate  = ".xtpl" `isSuffixOf` fname
        correction = if last templateRoot == '/' then 0 else 1
        extLen     = if isHTMLTemplate then 4 else 5
        tName = drop ((length templateRoot)+correction) $
                -- We're only dropping the template root, not the whole path
                take ((length fname) - extLen) fname


------------------------------------------------------------------------------
-- | Traverses the specified directory structure and builds a HeistState by
-- loading all the files with a ".tpl" or ".xtpl" extension.
loadTemplates :: Monad m
              => FilePath
              -- ^ Path to the template directory
              -> HeistState m a
              -- ^ Map of splices
              -> IO (Either String (HeistState m a))
loadTemplates dir ts = do
    d <- readDirectoryWith (loadTemplate dir) dir
    let tlist = F.fold (free d)
        errs = lefts tlist
    case errs of
--        [] -> liftM Right $ foldM loadHook ts $ rights tlist
        [] -> liftM Right $ foldM compileTemplates ts $ rights tlist
        _  -> return $ Left $ unlines errs


type HeistInit m out a = StateT (HeistState m out) IO a

compileTemplate :: (TPath, DocumentFile)
                -> HeistInit m out a
compileTemplates (tp,df) = do
    



------------------------------------------------------------------------------
-- | Performs splice processing on a single node.
runNode :: Monad m => X.Node -> HeistInit m out (m out)
runNode (X.Element nm at ch) = do
--    newAtts <- mapM attSubst at
--    let n = X.Element nm newAtts ch
    s <- liftM (lookupSplice nm) getTS
    maybe (runKids newAtts) (recurseSplice n) s
  where
    runKids newAtts = do
        newKids <- runNodeList ch
        return [X.Element nm newAtts newKids]
runNode n                    = return [n]


