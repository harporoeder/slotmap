{-# LANGUAGE RecordWildCards #-}

module Data.SlotMap
  ( Key
  , SlotMap
    -- * Construction
  , empty
  , clone
    -- * Constant Operations
  , Data.SlotMap.lookup
  , delete
  , unsafeDelete
  , insert
  , update
  , unsafeUpdate
  , Data.SlotMap.null
  , capacity
  , size
    -- * Linear Operations
  , Data.SlotMap.foldr
  , Data.SlotMap.map
  , elems
  ) where

import Data.Primitive.MutVar
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as V

-- | Weak reference to a 'SlotMap' item.
data Key = Key
  { keyIndex      :: Int -- Position In MVector
  , keyGeneration :: Int -- For Deletion Check
  }

-- Internal Slot Representation
data Slot a = Slot
  { slotItem       :: Maybe a -- Nothing On Deletion
  , slotGeneration :: Int     -- Incremented On Insert
  , slotFreeIndex  :: Int     -- Inline Free List Ref
  }

-- | Opaque 'SlotMap' structure.
data SlotMap m a = SlotMap
  { slotMapFree  :: MutVar (PrimState m) Int                              -- Head Of Free List
  , slotMapCount :: MutVar (PrimState m) Int                              -- Number Of Filled Slots
  , slotMapItems :: MutVar (PrimState m) (MVector (PrimState m) (Slot a)) -- Actual Storage Vec
  }

-- | Allocate empty 'SlotMap' instance.
empty :: PrimMonad m => m (SlotMap m a)
empty = do
  slotMapFree  <- newMutVar 0
  slotMapCount <- newMutVar 0
  slotMapItems <- V.new 0 >>= newMutVar
  return SlotMap{..}

-- | Allocate new 'SlotMap' and copy. Maintains keys.
clone :: PrimMonad m => (SlotMap m a) -> m (SlotMap m a)
clone map = do
  slotMapFree  <- readMutVar (slotMapFree map) >>= newMutVar
  slotMapCount <- readMutVar (slotMapCount map) >>= newMutVar
  slotMapItems <- readMutVar (slotMapItems map) >>= V.clone >>= newMutVar
  return SlotMap{..}

-- | Return value associated with key or 'Nothing' if the slot is empty O(1).
lookup :: PrimMonad m => Key -> SlotMap m a -> m (Maybe a)
lookup Key{..} SlotMap{..} = do
  -- Get Current Item Set To Check Size
  items <- readMutVar slotMapItems
  -- Ensure Key Within Bounds
  if keyIndex >= V.length items then pure Nothing else do
    -- Ensure Key Is Correct Generation
    slot <- V.unsafeRead items keyIndex
    if slotGeneration slot /= keyGeneration then pure Nothing else
      -- Return Potentially Still Empty Slot Item
      pure $ slotItem slot

-- | Delete value at key and mark slot for reuse O(1).
delete :: PrimMonad m => Key -> SlotMap m a -> m ()
delete key map = do
  -- Safety Check Before Deletion
  item <- Data.SlotMap.lookup key map
  case item of
    -- Attempted Deletion Of Non Existent Value
    Nothing -> pure ()
    -- Now Safe To Delete Cell
    Just _  -> unsafeDelete key map

-- | Same as @delete@ but does not validate the key O(1).
unsafeDelete :: PrimMonad m => Key -> SlotMap m a -> m ()
unsafeDelete Key{..} SlotMap{..} = do
  -- Update Meta Info
  modifyMutVar slotMapCount (flip (-) 1)
  writeMutVar slotMapFree keyIndex
  -- Erase Item Reference For GC
  let eraseItem x = x { slotItem = Nothing }
  items <- readMutVar slotMapItems
  V.unsafeModify items eraseItem keyIndex

-- | Insert a new element into the 'SlotMap' returning a key O(1).
-- This function may cause an allocation if the 'SlotMap' is full.
insert :: PrimMonad m => a -> SlotMap m a -> m Key
insert item SlotMap{..} = do
  -- Get Context
  free <- readMutVar slotMapFree
  items <- readMutVar slotMapItems
  -- Check Head Of Free List
  potentialcurrent <- readMaybe items free
  -- Insertion Cases
  case potentialcurrent of
    -- Allocation Required
    Nothing -> do
      -- Add New Item
      expanded <- V.unsafeGrow items 1
      let slot = Slot { slotItem = Just item, slotGeneration = 1, slotFreeIndex = 0 }
      V.unsafeWrite expanded (V.length expanded - 1) slot
      -- Update Meta Info
      writeMutVar slotMapFree (V.length expanded)
      modifyMutVar slotMapCount (1 +)
      writeMutVar slotMapItems expanded
      -- Fresh Key
      return Key
        { keyIndex      = free
        , keyGeneration = 1
        }
    -- Allocation Not Required
    Just current -> do
      -- Update Existing Slot
      let next = slotGeneration current + 1
      let slot = current { slotItem = Just item, slotGeneration = next }
      V.unsafeWrite items free current
      -- Update Meta Info
      writeMutVar slotMapFree (slotFreeIndex current)
      modifyMutVar slotMapCount (1 +)
      -- Updated Key
      return Key
        { keyIndex      = free
        , keyGeneration = slotGeneration current + 1
        }

-- | Update item at existing slot O(1). Does nothing if the key is invalid.
update :: PrimMonad m => SlotMap m a -> (a -> a) -> Key -> m ()
update map operation key = do
  -- Validate Existence Before Update
  potentialitem <- Data.SlotMap.lookup key map
  case potentialitem of
    -- Skip Not Existent
    Nothing -> pure ()
    -- Update When Exists
    Just _  -> unsafeUpdate map operation key

-- | Same as @update@ but does not validate the key O(1).
unsafeUpdate :: PrimMonad m => SlotMap m a -> (a -> a) -> Key -> m ()
unsafeUpdate SlotMap{..} operation Key{..} = do
  items <- readMutVar slotMapItems
  V.unsafeModify items (fmap operation) keyIndex

-- | Check if 'SlotMap' is empty O(1).
null :: PrimMonad m => SlotMap m a -> m Bool
null map = Data.SlotMap.size map >>= pure . (==) 0

-- | Total number of slots in the 'SlotMap' O(1).
capacity :: PrimMonad m => SlotMap m a -> m Int
capacity SlotMap{..} = readMutVar slotMapItems >>= pure . V.length

-- | Number of elements in the 'SlotMap' O(1).
size :: PrimMonad m => SlotMap m a -> m Int
size SlotMap{..} = readMutVar slotMapCount

-- | Fold over every full slot O(N) where N = capacity.
foldr :: PrimMonad m => (a -> b -> b) -> b -> SlotMap m a -> m b
foldr op initial SlotMap{..} = readMutVar slotMapItems >>= foldrVector (foldOpMaybe op . slotItem) initial

-- | Apply function to every full slot O(N) where N = capacity.
map :: PrimMonad m => (a -> a) -> SlotMap m a -> m ()
map operation SlotMap{..} = readMutVar slotMapItems >>= mapVector (fmap operation)

-- | Get every element of a 'SlotMap' as a list O(N) where N = capacity.
elems :: PrimMonad m => SlotMap m a -> m [a]
elems SlotMap{..} = readMutVar slotMapItems >>= foldrVector (foldOpMaybe (:) . slotItem) []

{- SlotMap Implementation Utilities -}

foldOpMaybe :: (a -> b -> b) -> Maybe a -> b -> b
foldOpMaybe f Nothing x  = x
foldOpMaybe f (Just x) y = f x y

{- Utilities Not In Vector Package -}

-- Utility To Get Vector Value If Inside Index
readMaybe :: PrimMonad m => MVector (PrimState m) a -> Int -> m (Maybe a)
readMaybe vec index = if index >= V.length vec then pure Nothing else do
  V.unsafeRead vec index >>= pure . Just

-- In-Place Map Over MVector
mapVector :: (PrimMonad m) => (a -> a) -> MVector (PrimState m) a -> m ()
mapVector operation vector = go 0 where
  go iter = if iter == V.length vector then pure () else do
    V.unsafeModify vector operation iter
    go $ iter + 1

-- Standard Fold Over MVector
foldrVector :: PrimMonad m => (a -> b -> b) -> b -> MVector (PrimState m) a -> m b
foldrVector operation initial vector = go 0 where
  go iter = if iter == V.length vector then pure initial else do
    item <- V.unsafeRead vector iter
    rest <- go $ iter + 1
    pure $ operation item rest

{- Internal Utility Classes -}

instance Functor Slot where
  fmap operation slot = slot { slotItem = fmap operation (slotItem slot) }
