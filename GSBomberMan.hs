{-# LANGUAGE TemplateHaskell #-}
module GSBomberMan
( GSBomberMan
, new
) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import qualified Graphics.UI.GLFW as GLFW

import GameState
import Map
import Input

data GSBomberMan = GSBomberMan { _shouldDraw :: Bool
                               , _num :: Int
                               , _gameMap :: Map 
                               , _exitState :: ExitType
                               } deriving (Eq, Show)
makeLenses ''GSBomberMan

new = GSBomberMan { _num = 1
                  , _gameMap = newMap
                  , _shouldDraw = True
                  , _exitState = NoExit
                  }

instance GameState GSBomberMan where
    initialize = return ()

    handleInput input = do
        if isKeyDown input (GLFW.CharKey 'W')
            then shouldDraw .= False
            else shouldDraw .= True
        if isKeyDown input (GLFW.SpecialKey GLFW.ESC)
            then exitState .= Exit
            else return ()

    -- update :: State GSBomberMan ()
    update = return ()

    -- draw :: StateT GSBomberMan IO ()
    draw = do
        self <- get
        lift $ do
            if self^.shouldDraw
                then renderMap (self^.gameMap)
                else return ()

    -- shouldExit :: StateT GSBomberMan IO Bool
    shouldExit = liftM (^.exitState) get

    getNextState = return Nothing
