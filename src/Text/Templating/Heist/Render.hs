{-# LANGUAGE OverloadedStrings #-}

module Text.Templating.Heist.Render where

import           Blaze.ByteString.Builder
import           Data.ByteString (ByteString)
import           Data.Char
import qualified Data.HashMap.Strict as H
import           Data.HashMap.Strict (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Text.XmlHtml
import           Text.Templating.Heist.Types

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


------------------------------------------------------------------------------
-- | Gets the encoding function from 'Text' to 'ByteString' for an encoding.
encoder :: Encoding -> Text -> ByteString
encoder UTF8    = T.encodeUtf8
encoder UTF16BE = T.encodeUtf16BE
encoder UTF16LE = T.encodeUtf16LE  


------------------------------------------------------------------------------
fromText :: Encoding -> Text -> Builder
fromText e t = fromByteString (encoder e t)


-- ------------------------------------------------------------------------------
-- render :: Encoding -> Maybe DocType -> [Node] -> Builder
-- render e dt ns = byteOrder
--        `mappend` xmlDecl e
--        `mappend` docTypeDecl e dt
--        `mappend` nodes
--     where byteOrder | isUTF16 e = fromText e "\xFEFF" -- byte order mark
--                     | otherwise = mempty
--           nodes | null ns   = mempty
--                 | otherwise = firstNode e (head ns)
--                     `mappend` (mconcat $ map (node e) (tail ns))
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Function for rendering XML nodes without the overhead of creating a
-- -- Document structure.
-- renderXmlFragment :: Encoding -> [Node] -> Builder
-- renderXmlFragment _ []     = mempty
-- renderXmlFragment e (n:ns) =
--     firstNode e n `mappend` (mconcat $ map (node e) ns)
-- 
-- 
-- ------------------------------------------------------------------------------
-- xmlDecl :: Encoding -> Builder
-- xmlDecl e = fromText e "<?xml version=\"1.0\" encoding=\""
--             `mappend` fromText e (encodingName e)
--             `mappend` fromText e "\"?>\n"
-- 
-- 
-- ------------------------------------------------------------------------------
-- docTypeDecl :: Encoding -> Maybe DocType -> Builder
-- docTypeDecl _ Nothing                      = mempty
-- docTypeDecl e (Just (DocType tag ext int)) = fromText e "<!DOCTYPE "
--                                    `mappend` fromText e tag
--                                    `mappend` externalID e ext
--                                    `mappend` internalSubset e int
--                                    `mappend` fromText e ">\n"
-- 
-- 
-- ------------------------------------------------------------------------------
-- externalID :: Encoding -> ExternalID -> Builder
-- externalID _ NoExternalID     = mempty
-- externalID e (System sid)     = fromText e " SYSTEM "
--                                 `mappend` sysID e sid
-- externalID e (Public pid sid) = fromText e " PUBLIC "
--                                 `mappend` pubID e pid
--                                 `mappend` fromText e " "
--                                 `mappend` sysID e sid
-- 
-- 
-- ------------------------------------------------------------------------------
-- internalSubset :: Encoding -> InternalSubset -> Builder
-- internalSubset _ NoInternalSubset = mempty
-- internalSubset e (InternalText t) = fromText e " " `mappend` fromText e t
-- 
-- 
-- ------------------------------------------------------------------------------
-- sysID :: Encoding -> Text -> Builder
-- sysID e sid | not ("\'" `T.isInfixOf` sid) = fromText e "\'"
--                                              `mappend` fromText e sid
--                                              `mappend` fromText e "\'"
--             | not ("\"" `T.isInfixOf` sid) = fromText e "\""
--                                              `mappend` fromText e sid
--                                              `mappend` fromText e "\""
--             | otherwise               = error "SYSTEM id is invalid"
-- 
-- 
-- ------------------------------------------------------------------------------
-- pubID :: Encoding -> Text -> Builder
-- pubID e sid | not ("\"" `T.isInfixOf` sid) = fromText e "\""
--                                              `mappend` fromText e sid
--                                              `mappend` fromText e "\""
--             | otherwise               = error "PUBLIC id is invalid"
-- 
-- 
------------------------------------------------------------------------------
node :: Monad m => HeistState m Builder
     -> Encoding -> Node -> OutputChunk Builder m
node _ e (TextNode t)                        = Pure $ escaped "<>&" e t
node _ e (Comment t) | "--" `T.isInfixOf` t  = error "Invalid comment"
                     | "-" `T.isSuffixOf` t  = error "Invalid comment"
                     | otherwise             = Pure $ fromText e "<!--"
                                               `mappend` fromText e t
                                               `mappend` fromText e "-->"
node hs e n@(Element t a c)                  =
    case H.lookup t (_spliceMap hs) of
      Nothing -> element hs e t a c
      Just sp -> Act mempty (evalSpliceT sp n) mempty


-- ------------------------------------------------------------------------------
-- -- | Process the first node differently to encode leading whitespace.  This
-- -- lets us be sure that @parseXML@ is a left inverse to @render@.
-- firstNode :: Encoding -> Node -> Builder
-- firstNode e (Comment t)     = node e (Comment t)
-- firstNode e (Element t a c) = node e (Element t a c)
-- firstNode _ (TextNode "")   = mempty
-- firstNode e (TextNode t)    = let (c,t') = fromJust $ T.uncons t
--                               in escaped "<>& \t\r\n" e (T.singleton c)
--                                  `mappend` node e (TextNode t')


------------------------------------------------------------------------------
escaped :: [Char] -> Encoding -> Text -> Builder
escaped _   _ "" = mempty
escaped bad e t  = let (p,s) = T.break (`elem` bad) t
                       r     = T.uncons s
                   in  fromText e p `mappend` case r of
                         Nothing     -> mempty
                         Just (c,ss) -> entity e c `mappend` escaped bad e ss


------------------------------------------------------------------------------
entity :: Encoding -> Char -> Builder
entity e '&'  = fromText e "&amp;"
entity e '<'  = fromText e "&lt;"
entity e '>'  = fromText e "&gt;"
entity e '\"' = fromText e "&quot;"
entity e c    = fromText e "&#"
                `mappend` fromText e (T.pack (show (ord c)))
                `mappend` fromText e ";"


startTag :: Encoding -> Text -> [(Text, Text)] -> Builder
startTag e t a = (fromText e "<" `mappend` fromText e t)
       -- no attribute substitution for now
       `mappend` (mconcat $ map (attribute e) a)


------------------------------------------------------------------------------
element :: Monad m => HeistState m Builder
        -> Encoding -> Text -> [(Text, Text)] -> [Node]
        -> OutputChunk Builder m
element _ e t a [] = Pure $ startTag e t a `mappend` (fromText e "/>")
element hs e t a c =
    (Pure $ startTag e t a `mappend` (fromText e ">")) `mappend`
    (mconcat $ map (node hs e) c) `mappend`
    (Pure $ fromText e "</" `mappend` fromText e t `mappend` fromText e ">")


------------------------------------------------------------------------------
attribute :: Encoding -> (Text, Text) -> Builder
attribute e (n,v) | not ("\'" `T.isInfixOf` v) = fromText e " "
                                       `mappend` fromText e n
                                       `mappend` fromText e "=\'"
                                       `mappend` escaped "<&" e v
                                       `mappend` fromText e "\'"
                  | otherwise                  = fromText e " "
                                       `mappend` fromText e n
                                       `mappend` fromText e "=\""
                                       `mappend` escaped "<&\"" e v
                                       `mappend` fromText e "\""


-- ------------------------------------------------------------------------------
-- -- | Helper function for substituting a parsed attribute into an attribute
-- -- tuple.
-- attSubst :: (Monad m) => SpliceMap m a -> (t, Text) -> (t, Text)
-- attSubst splices (n,v) = do
--     v' <- parseAtt v
--     return (n,v')
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Parses an attribute for any identifier expressions and performs
-- -- appropriate substitution.
-- parseAtt :: (Monad m) => SpliceMap m a -> Text -> SpliceT m Text
-- parseAtt splices bs = do
--     let ast = case AP.feed (AP.parse attParser bs) "" of
--                 (AP.Done _ res) -> res
--                 (AP.Fail _ _ _) -> []
--                 (AP.Partial _)  -> []
--     chunks <- mapM cvt ast
--     return $ T.concat chunks
--   where
--     cvt (Literal x) = return x
--     cvt (Ident x)   =
--         localParamNode (const $ X.Element x [] []) $ getAttributeSplice x
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | AST to hold attribute parsing structure.  This is necessary because
-- -- attoparsec doesn't support parsers running in another monad.
-- data AttAST = Literal Text
--             | Ident   Text
--   deriving (Show)
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Parser for attribute variable substitution.
-- attParser :: AP.Parser [AttAST]
-- attParser = liftM ($! []) (loop id)
--   where
--     append !dl !x = dl . (x:)
-- 
--     loop !dl = go id
--       where
--         finish subDL = let !txt = T.concat $! subDL []
--                            lit  = Literal $! T.concat $! subDL []
--                        in return $! if T.null txt
--                                       then dl
--                                       else append dl lit
-- 
--         go !subDL = (gobbleText >>= go . append subDL)
--                     <|> (AP.endOfInput *> finish subDL)
--                     <|> (escChar >>= go . append subDL)
--                     <|> (do
--                             idp <- identParser
--                             dl' <- finish subDL
--                             loop $! append dl' idp)
-- 
--     gobbleText = AP.takeWhile1 (AP.notInClass "\\$")
-- 
--     escChar = AP.char '\\' *> (T.singleton <$> AP.anyChar)
-- 
--     identParser = AP.char '$' *> (ident <|> return (Literal "$"))
--     ident = (AP.char '{' *> (Ident <$> AP.takeWhile (/='}')) <* AP.string "}")
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Gets the attribute value.  If the splice's result list contains non-text
-- -- nodes, this will translate them into text nodes with nodeText and
-- -- concatenate them together.
-- --
-- -- Originally, this only took the first node from the splices's result list,
-- -- and only if it was a text node. This caused problems when the splice's
-- -- result contained HTML entities, as they would split a text node. This was
-- -- then fixed to take the first consecutive bunch of text nodes, and return
-- -- their concatenation. This was seen as more useful than throwing an error,
-- -- and more intuitive than trying to render all the nodes as text.
-- --
-- -- However, it was decided in the end to render all the nodes as text, and
-- -- then concatenate them. If a splice returned
-- -- \"some \<b\>text\<\/b\> foobar\", the user would almost certainly want
-- -- \"some text foobar\" to be rendered, and Heist would probably seem
-- -- annoyingly limited for not being able to do this. If the user really did
-- -- want it to render \"some \", it would probably be easier for them to
-- -- accept that they were silly to pass more than that to be substituted than
-- -- it would be for the former user to accept that
-- -- \"some \<b\>text\<\/b\> foobar\" is being rendered as \"some \" because
-- -- it's \"more intuitive\".
-- getAttributeSplice :: Monad m => Text -> SpliceT m Text
-- getAttributeSplice name = do
--     s <- liftM (lookupSplice name) getTS
--     nodes <- maybe (return []) id s
--     return $ T.concat $ map X.nodeText nodes

