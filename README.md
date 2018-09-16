# Haskell SlotMap

This package implements the SlotMap data structure. SlotMaps are popular in certain high performance situations. This implementation is generic over ST or IO. More information on SlotMaps can be found [here](http://open-std.org/JTC1/SC22/WG21/docs/papers/2017/p0661r0.pdf).

## Example Usage

```
module Main where

import Data.SlotMap as M

main :: IO ()
main = do
  -- Create SlotMap
  m <- M.empty

  -- Insert Elements
  a <- M.insert 3 m
  b <- M.insert 7 m
  c <- M.insert 9 m

  -- Print Elements
  M.lookup a m >>= putStrLn . show -- Just 3
  M.lookup b m >>= putStrLn . show -- Just 7
  M.lookup c m >>= putStrLn . show -- Just 9

  -- Mutate Elements
  M.delete a m
  M.update m (1+) b
  M.map (*2) m

  -- Print Updated Elements
  M.lookup a m >>= putStrLn . show -- Just Nothing
  M.lookup b m >>= putStrLn . show -- Just 16
  M.lookup c m >>= putStrLn . show -- Just 18
```

## Structure Properties

Keys are a weakly owning reference to the contents of the store. On insertion a unique key is returned. There is a generational index system such that when a storage slot is reused previous keys to the same slot do not incorrectly return a value.

| Operation | Complexity |
|-----------|------------|
| Lookup    | O(1)       |
| Map       | O(N)*1     |
| Update    | O(1)       |
| Delete    | O(1)       |
| Insert    | O(1)*2     |
| Size      | O(1)*3     |

1. Where N = capacity NOT size.
2. May initiate allocation if the capacity is full.
3. Size != capacity. Capacity may never shrink.
