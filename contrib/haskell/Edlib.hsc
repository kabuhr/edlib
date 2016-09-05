{-# LANGUAGE CPP, ForeignFunctionInterface, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module Edlib (align, defaultAlignConfig, AlignResult(..)) where

import qualified Data.ByteString as B
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)

#include <edlib.h>
#include "edlib_shim.h"

newtype AlignMode_ = AlignMode_ CInt deriving (Storable, Eq)
#{enum AlignMode_, AlignMode_,
  modeNW_  = EDLIB_MODE_NW,
  modeSHW_ = EDLIB_MODE_SHW,
  modeHW_  = EDLIB_MODE_HW
}

newtype AlignTask_ = AlignTask_ CInt deriving (Storable, Eq)
#{enum AlignTask_, AlignTask_,
  taskDistance_ = EDLIB_TASK_DISTANCE,
  taskLoc_      = EDLIB_TASK_LOC,
  taskPath_     = EDLIB_TASK_PATH
}

newtype CigarFormat_ = CigarFormat_ CInt deriving (Eq)
#{enum CigarFormat_, CigarFormat_,
  cigarStandard_ = EDLIB_CIGAR_STANDARD,
  cigarExtended_ = EDLIB_CIGAR_EXTENDED
}

newtype Edop_ = Edop_ CInt deriving (Eq)
#{enum Edop_, Edop_,
  edopMatch_    = EDLIB_EDOP_MATCH,
  edopInsert_   = EDLIB_EDOP_INSERT,
  edopDelete_   = EDLIB_EDOP_DELETE,
  edopMismatch_ = EDLIB_EDOP_MISMATCH
}

data AlignConfig_ = AlignConfig_ {
    k_ :: CInt
  , mode_ :: AlignMode_
  , task_ :: AlignTask_
  }
  
instance Storable AlignConfig_ where
  sizeOf _ = (#size EdlibAlignConfig)
  alignment _ = alignment (undefined :: CInt)
  peek ptr = do
    k    <- (#peek EdlibAlignConfig, k) ptr
    mode <- (#peek EdlibAlignConfig, mode) ptr
    task <- (#peek EdlibAlignConfig, task) ptr
    return $ AlignConfig_ k mode task
  poke ptr (AlignConfig_ k mode task) = do
    (#poke EdlibAlignConfig, k)    ptr k
    (#poke EdlibAlignConfig, mode) ptr mode
    (#poke EdlibAlignConfig, task) ptr task

data AlignResult_ = AlignResult_ {
    editDistance_ :: CInt
  , endLocations_ :: Ptr CInt
  , startLocations_ :: Ptr CInt
  , numLocations_ :: CInt
  , alignment_ :: Ptr CUChar
  , alignmentLength_ :: CInt
  , alphabetLength_ :: CInt
  }

instance Storable AlignResult_ where
  sizeOf _ = (#size EdlibAlignResult)
  -- BUG: potential portability problem: need to align to max of int and pointer?
  alignment _ = alignment (undefined :: CInt)
  peek ptr = do
    editDistance    <- (#peek EdlibAlignResult, editDistance    ) ptr
    endLocations    <- (#peek EdlibAlignResult, endLocations    ) ptr
    startLocations  <- (#peek EdlibAlignResult, startLocations  ) ptr
    numLocations    <- (#peek EdlibAlignResult, numLocations    ) ptr
    alignment       <- (#peek EdlibAlignResult, alignment       ) ptr
    alignmentLength <- (#peek EdlibAlignResult, alignmentLength ) ptr
    alphabetLength  <- (#peek EdlibAlignResult, alphabetLength  ) ptr
    return $ AlignResult_ editDistance endLocations startLocations numLocations
                          alignment alignmentLength alphabetLength
  poke ptr (AlignResult_ editDistance endLocations startLocations numLocations
                         alignment alignmentLength alphabetLength) = do
    (#poke EdlibAlignResult, editDistance    ) ptr editDistance
    (#poke EdlibAlignResult, endLocations    ) ptr endLocations
    (#poke EdlibAlignResult, startLocations  ) ptr startLocations
    (#poke EdlibAlignResult, numLocations    ) ptr numLocations
    (#poke EdlibAlignResult, alignment       ) ptr alignment
    (#poke EdlibAlignResult, alignmentLength ) ptr alignmentLength
    (#poke EdlibAlignResult, alphabetLength  ) ptr alphabetLength

foreign import ccall unsafe "edlib_shim.h edlibAlignP"
  c_align :: Ptr CChar -> CInt -> Ptr CChar -> CInt 
             -> Ptr AlignConfig_ -> Ptr AlignResult_ -> IO ()

foreign import ccall unsafe "edlib.h edlibAlignmentToCigar"
  c_alignmentToCigar :: Ptr CUChar -> CInt -> CigarFormat_ -> Ptr CChar

foreign import ccall unsafe "edlib_shim.h &edlibFreeAlignResultP"
  p_freeAlignResult :: FunPtr (Ptr AlignResult_ -> IO ())

class CEnum a b where
  toC   :: a -> b
  fromC :: b -> a

instance CEnum AlignMode AlignMode_ where
  toC   x | x == ModeNW   = modeNW_
          | x == ModeSHW  = modeSHW_
          | x == ModeHW   = modeHW_
  fromC x | x == modeNW_  = ModeNW
          | x == modeSHW_ = ModeSHW
          | x == modeHW_  = ModeHW
instance CEnum AlignTask AlignTask_ where
  toC   x | x == TaskDistance    = taskDistance_
          | x == TaskLoc         = taskLoc_
          | x == TaskPath        = taskPath_
  fromC x | x == taskDistance_   = TaskDistance
          | x == taskLoc_        = TaskLoc
          | x == taskPath_       = TaskPath
instance CEnum CigarFormat CigarFormat_ where
  toC   x | x == CigarStandard   = cigarStandard_
          | x == CigarExtended   = cigarExtended_
  fromC x | x == cigarStandard_  = CigarStandard
          | x == cigarExtended_  = CigarExtended
instance CEnum Edop Edop_ where
  toC   x | x == EdopMatch       = edopMatch_
          | x == EdopInsert      = edopInsert_
          | x == EdopDelete      = edopDelete_
          | x == EdopMismatch    = edopMismatch_
  fromC x | x == edopMatch_      = EdopMatch
          | x == edopInsert_     = EdopInsert
          | x == edopDelete_     = EdopDelete
          | x == edopMismatch_   = EdopMismatch
instance CEnum AlignConfig AlignConfig_ where
  toC (AlignConfig k mode task) = AlignConfig_ (fromIntegral k) (toC mode) (toC task)
  fromC (AlignConfig_ k_ mode_ task_) = AlignConfig (fromIntegral k_) (fromC mode_) (fromC task_)

fromCResult :: AlignResult_ -> IO AlignResult
fromCResult (AlignResult_ d_ ends_ starts_ nlocs_ aligns_ naligns_ nalph_) = do
  let d       = fromIntegral d_
      nlocs   = fromIntegral nlocs_
      naligns = fromIntegral naligns_
      nalph   = fromIntegral nalph_
  locs <- if (starts_ /= nullPtr && ends_ /= nullPtr) then do
      starts <- map fromIntegral <$> peekArray nlocs starts_
      ends <- map fromIntegral <$> peekArray nlocs ends_
      return $ Just (zip starts ends)
    else return Nothing
  aligns <- if aligns_ /= nullPtr then
              Just . map fromIntegral <$> peekArray naligns aligns_
            else return Nothing
  return $ AlignResult d locs aligns nalph

data AlignMode = ModeNW | ModeSHW | ModeHW deriving (Show, Eq)
data AlignTask = TaskDistance | TaskLoc | TaskPath deriving (Show, Eq)
data CigarFormat = CigarStandard | CigarExtended deriving (Show, Eq)
data Edop = EdopMatch | EdopInsert | EdopDelete | EdopMismatch deriving (Show, Eq)

data AlignConfig = AlignConfig {
    configK :: Int
  , configMode :: AlignMode
  , configTask :: AlignTask
  } deriving (Show, Eq)

data AlignResult = AlignResult {
    resultEditDistance :: Int
  , resultLocations :: Maybe [(Int, Int)]
  , resultAlignment :: Maybe [Int]
  , resultAlphabetLength :: Int
  } deriving (Show)

defaultAlignConfig :: AlignConfig
defaultAlignConfig = AlignConfig (-1) ModeNW TaskDistance

align :: B.ByteString -> B.ByteString -> AlignConfig -> AlignResult
align query target config = unsafePerformIO $
  unsafeUseAsCStringLen query $ \(query_, queryLen_) ->
    unsafeUseAsCStringLen target $ \(target_, targetLen_) ->
      alloca $ \config_ -> do
        result__ <- mallocForeignPtr
        addForeignPtrFinalizer p_freeAlignResult result__
        withForeignPtr result__ $ \result_ -> do
          poke config_ (toC config)
          c_align query_ (fromIntegral queryLen_) 
                  target_ (fromIntegral targetLen_)
                  config_ result_
          peek result_ >>= fromCResult
