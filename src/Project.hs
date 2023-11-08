{-# LANGUAGE OverloadedStrings #-}

module Project where

import CodeWorld

-- | Default entry point.
run :: IO ()
run =  drawingOf (solidCircle 7)
