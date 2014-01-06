module Analyse.ManyFiles
( analyseNodes
) where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.MultiSet as MS
import Data.Graph.Analysis.Reporting
import Control.Monad.Writer
import Control.Monad.Identity

import Parsing.Types
import Analyse.Utils

type Node = Entity
data NodeInfo = NI { calledBy :: S.Set Node
                   , calls :: S.Set Node
                   }

newtype NodesInfo = NsI { nim :: M.Map Node NodeInfo }

nsIToList         :: NodesInfo -> [(Node, NodeInfo)]
nsIToList (NsI m) = M.toList m

instance Monoid NodesInfo where
    mempty = NsI M.empty
    (NsI m1) `mappend` (NsI m2) = NsI $ M.foldlWithKey addNode m1 m2
        where
          addNode m ent ni@(NI cby cs) =
            case M.lookup ent m of
              Nothing -> M.insert ent ni m
              Just (NI cby2 cs2) -> M.insert ent
                                             (NI (cby2 `S.union` cby)
                                                 (cs2 `S.union` cs))
                                             m

analyseNodes          :: FilePath -> String -> ParsedModules -> [Document]
analyseNodes rt t hms = map (niToDocument res rt t) (nsIToList res)
    where
      (_, res) = runIdentity $ runWriterT $ mapM_ singleModule (M.toList hms)

niToDocument                :: NodesInfo -> FilePath -> String
                               -> (Entity, NodeInfo) -> Document
niToDocument nsi rt t (ent, ni) = Doc { rootDirectory  = rt
                                      , fileFront      = entToFilefront nsi ent
                                      , graphDirectory = "graphs"
                                      , title          = Text $ entToTitle ent
                                      , author         = authorInfo
                                      , date           = t
                                      , legend         = []
                                      , content        = niReport nsi ni
                                      }

modFullName            :: ModName -> [Char]
modFullName UnknownMod = "<Unknown>"
modFullName m          = modName m

entFullName     :: Entity -> [Char]
entFullName ent = (modFullName $ inModule ent) ++ "." ++ (name ent)

entToFilefront :: NodesInfo -> Entity -> [Char]
entToFilefront nsi ent = zeroes ++ (show num)
    where
      m = nim nsi
      num = M.findIndex ent m
      logBaseD :: Double -> Double -> Double
      logBaseD = logBase
      len n = if n==0
              then 1
              else ceiling $ logBaseD 10.0 (fromIntegral $ n + 1)
      maxlen = len (M.size m - 1)
      zeroes = replicate (maxlen - (len num)) '0'

entToFilename         :: NodesInfo -> Entity -> [Char]
entToFilename nsi ent = (entToFilefront nsi ent) ++ ".html"

entToTitle     :: Entity -> [Char]
entToTitle ent = (name ent) ++ " from " ++ (modFullName $ inModule ent)

niReport    :: NodesInfo -> NodeInfo -> [DocElement]
niReport nsi ni = [ Section (Text "Called by")
                            [Itemized $ entSetToLinks $ calledBy ni]
                  , Section (Text "Calls")
                            [Itemized $ entSetToLinks $ calls ni]
                  ]
    where
      entSetToLinks s = map entToLink (S.toList s)
      entToLink ent = Paragraph [DocLink (Text $ entFullName ent)
                                         (File $ entToFilename nsi ent) ]

singleModule        :: (ModName, ParsedModule) -> WriterT NodesInfo Identity ()
singleModule (_, m) = mapM_ singleCall (MS.elems $ funcCalls m)
    where
      singleCall fc = tell $ NsI $ M.fromList
                      [ (source, NI { calledBy = S.empty
                                    , calls = S.singleton target
                                    } )
                      , (target, NI { calledBy = S.singleton source
                                    , calls = S.empty
                                    } ) ]
        where
          target = toEntity fc
          source = fromEntity fc
