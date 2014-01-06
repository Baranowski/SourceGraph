module Analyse.ManyFiles
( analyseNodes
) where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.MultiSet as MS
import Data.Graph.Analysis.Reporting
import Data.List(isPrefixOf)
import Control.Monad.Writer
import Control.Monad.Identity

import Parsing.Types
import Analyse.Utils

type Node = Entity
data NodeInfo = NI { calledBy :: S.Set Node
                   , calls :: S.Set Node
                   }

newtype NodesInfo = NsI (M.Map Node NodeInfo)

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
analyseNodes rt t hms = map (niToDocument rt t) (nsIToList res)
    where
      (_, res) = runIdentity $ runWriterT $ mapM_ singleModule (M.toList hms)

niToDocument                :: FilePath -> String -> (Entity, NodeInfo)
                               -> Document
niToDocument rt t (ent, ni) = Doc { rootDirectory  = rt
                                  , fileFront      = entToFilefront ent
                                  , graphDirectory = "graphs"
                                  , title          = Text $ entToTitle ent
                                  , author         = authorInfo
                                  , date           = t
                                  , legend         = []
                                  , content        = niReport ni
                                  }

modFullName            :: ModName -> [Char]
modFullName UnknownMod = "<Unknown>"
modFullName m          = modName m

entFullName     :: Entity -> [Char]
entFullName ent = (modFullName $ inModule ent) ++ "." ++ (name ent)

entToFilefront :: Entity -> [Char]
entToFilefront = replace "/" "_S_" . entFullName

replace           :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ []    = []
replace old new s = if isPrefixOf old s
                      then new ++ (replace old new (drop (length old) s))
                      else (head s):(replace old new (tail s))


entToFilename     :: Entity -> [Char]
entToFilename ent = (entToFilefront ent) ++ ".html"

entToTitle     :: Entity -> [Char]
entToTitle ent = (name ent) ++ " from " ++ (modFullName $ inModule ent)

niReport    :: NodeInfo -> [DocElement]
niReport ni = [ Section (Text "Called by")
                                 [Itemized $ entSetToLinks $ calledBy ni]
                       , Section (Text "Calls")
                                 [Itemized $ entSetToLinks $ calls ni]
                       ]
    where
      entSetToLinks s = map entToLink (S.toList s)
      entToLink ent = Paragraph [DocLink (Text $ entFullName ent)
                                         (File $ entToFilename ent) ]

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
