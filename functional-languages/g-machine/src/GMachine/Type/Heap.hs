{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module GMachine.Type.Heap where

import Data.Word (Word32)

import qualified Data.Map as M (Map, empty, insert)

import Control.Lens (Lens', (^.), (&), (%~), (.~), at, makeLenses)
import Control.Lens.Iso (anon)
import Text.PrettyPrint (text)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint))

import GMachine.Type.Address (Address(Addr), withAddress)



data Heap a = Heap
    { _heapSize :: Word32
    , _freeCells :: [Int]
    , _heapData :: M.Map Word32 a
    }
makeLenses ''Heap

instance Show a => Show (Heap a) where
    show = show . _heapData

hAlloc :: Heap a -> a -> (Address, Heap a)
hAlloc heap@Heap{..} v = (Addr $ heap ^. heapSize, heap
    & heapSize %~ succ
    & freeCells %~ tail
    & heapData %~ (M.insert (fromIntegral $ head _freeCells) v))

hLookup :: Heap a -> Address -> a
hLookup heap addr = withAddress
    (error $ "Cannot access null address")
    (\i -> heap ^. heapData.at i._fromJust)
    addr

hInitial :: Heap a
hInitial = Heap
    { _heapSize = 0
    , _freeCells = [0..]
    , _heapData = M.empty
    }

hSetAt :: Address -> a -> Heap a -> Heap a
hSetAt addr node heap = withAddress
    (error $ "Cannot modify null address")
    (\i -> heap & heapData.at i._fromJust .~ node)
    addr

_fromJust :: Lens' (Maybe a) a
_fromJust = anon (error "_fromJust: Nothing") (\_ -> False)

instance (Show a) => Pretty (Heap a) where
    pPrint = text . show

--prettyHeap :: Heap Node -> Globals -> Doc
--prettyHeap (Heap _ _ m) globals = M.foldlWithKey prettyHeapCell empty m
--  where
--    prettyHeapCell doc addr node = doc $$ int addr <> text ":" <+> case node of
--        NInd (Addr iAddr) -> text "=>" <+> int iAddr
--        NNode number -> printNum number
--        NGlobal _ _ -> printGlobal addr
--        NConstr tag args -> text "Data" <+> integer tag <+> pPrint args
--        ap@(NApp a1 a2) -> prettyApplication addr ap
--
--    prettyApplication addr = \case
--        NInd addr' -> text "=>" <+> prettyApplication addr' (m M.! addr')
--        NGlobal _ _ -> printGlobal addr
--        NNode num -> printNum num
--        NApp a1 a2 -> text "@" <+> nest 4 (vcat
--            [ prettyApplication a2 (m M.! a2)
--            , prettyApplication a1 (m M.! a1)
--            ])
--
--    printGlobal addr =
--        let inverseG = M.fromList $ concatMap ((:[]) . swap) (M.toList globals)
--        in text "SC" <+> quotes (text (inverseG M.! addr))
--
--    printNum num = text "Num" <+> quotes (integer num)
