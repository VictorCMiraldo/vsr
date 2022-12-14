{-# LANGUAGE FlexibleContexts #-}
import XMonad
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.EZConfig (additionalKeys, additionalMouseBindings, removeKeys)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP 
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Actions.MouseGestures
import XMonad.Actions.CycleWS
import qualified XMonad.StackSet as SS
import qualified Reflection as R
import qualified Data.Map as M
import Data.IORef
import Data.List (isPrefixOf, isSuffixOf)
import Control.Concurrent
import Control.Monad

------------------------------------------
-- * My Modifier and Top-Level Config * --
------------------------------------------

myMod = mod1Mask

myConfig = ((def
  { terminal = "mate-terminal"
  , borderWidth = 2
  , focusedBorderColor = R.accent
  , workspaces = myWorkspaces
  , manageHook = manageHook def <+> manageDocks <+> myManageHook
  , layoutHook = smartBorders . avoidStruts $ layoutHook def
  } `removeKeys` myRemovedKeys)
  `additionalKeys` myKeys)

myKeys :: [((KeyMask, KeySym), X ())]
myKeys = [
  -- Launch rofi in all its flavours
    ((myMod, xK_d), spawn "rofi -show drun")
  , ((myMod, xK_q), spawn "rofi -show p -modi \"p:rofi-power-menu --choices=logout/shutdown/reboot/suspend\"")
  , ((myMod, xK_p), spawn "rofi -show p -modi p:rofi-pass")
  , ((myMod, xK_c), spawn "qalculate-gtk")
  -- Swap focused physical screens
  , ((myMod , xK_e),              myCycleScreen (windows . SS.view))
  , ((myMod .|. shiftMask, xK_e), myCycleScreen (windows . SS.shift))
  -- Swap keyboard layouts
  , ((myMod , xK_i) , spawn "kbd-layout-switch")
  ]

myRemovedKeys :: [(KeyMask, KeySym)]
myRemovedKeys = [
  -- I like to use M-n and M-p on my emacs!
    (myMod , xK_n)
  , (myMod , xK_p)
  -- I don't like 'switch to screen id x', I much rather have
  -- switch to the next one.
  , (myMod , xK_w)
  , (myMod , xK_e)
  , (myMod , xK_r)
  ]

-----------------------
-- * My Workspaces * --
-----------------------

myWorkspaces :: [String]
myWorkspaces
  = [ "1"
    , "2"
    , "3"
    , "4"
    , "5"
    , "6"
    , "7"
    , "8"
    , "9"
    ]

ws :: Int -> String
ws i = myWorkspaces !! (i-1)

-------------------------
-- * My Manage Hooks * --
-------------------------

myManageHook = composeAll
    [ manageFF
    , manageBasicFloats
    , manageGimp
    ]
  where
    -- Sometimes these windows are floated annoyingly large,
    -- look into doRectFloat from XMonad.Hooks.ManageHelpers.
    -- 
    -- The class name can be found with the xprop utility
    floats :: [String]
    floats = 
      [ "Blueman-manager"
      , "Pavucontrol"
      , "Mate-volume-control"
      , "Nm-connection-editor"
      , "Qalculate-gtk"
      ]

    -- Which windows to simply float, basec on theur className
    manageBasicFloats :: ManageHook
    manageBasicFloats = (fmap (`elem` floats) className) --> doFloat

    manageFF :: ManageHook
    manageFF = (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat

    manageGimp :: ManageHook
    manageGimp = (fmap ("Gimp" `isPrefixOf`) className <&&> fmap ("dialog" `isSuffixOf`) (stringProperty "WM_WINDOW_ROLE")) --> doFloat


--------------------------------
-- * Fancy Trackpad Actions * --
--------------------------------

-- Why not use left and right scrolling (by default buttons 6 and 7), to change to the previous/next workspace?
-- The trick is to keep a counter and only switch every n clicks; otherwise its way too fast and
-- we can't keep track of it. Counter starts at 0, scrolling left decreases it, scrolling right increases it.
-- As soon as its absolute value reaches cLICKS_TO_CYCLE, we call either prevWS or nextWS.

cLICKS_TO_CYCLE :: Int
cLICKS_TO_CYCLE = 6

horizontalScrollWorkspaces :: (LayoutClass l Window) => XConfig l -> IO (XConfig l)
horizontalScrollWorkspaces conf = do
  goLeft <- asyncNBeforeTimeout cLICKS_TO_CYCLE 0.3
  goRight <- asyncNBeforeTimeout cLICKS_TO_CYCLE 0.3
  let myMouseBindingsIO :: [((ButtonMask, Button), Window -> X ())]
      myMouseBindingsIO = 
        [ ((0, 6), const $ io goLeft >>= \c -> when c prevWS)
        , ((0, 7), const $ io goRight >>= \c -> when c nextWS)
        ]
  return $ conf `additionalMouseBindings` myMouseBindingsIO
  where
    countIORef :: IORef Int -> (Int -> Int) -> (Int -> Bool) -> X () -> X ()
    countIORef r upd8 predi act = do
      doMe <- io $ atomicModifyIORef r (\i -> if predi i then (0, act) else (upd8 i, return ()))
      doMe

--------------
-- * Main * --
--------------

main :: IO ()
main = do
  let pureConfig = myBar $ ewmhFullscreen $ ewmh $ myConfig
  -- Don't touch the horizontalScroll... it can crash xmonad. Probably due to a deadlock or something...
  -- impureConfig <- horizontalScrollWorkspaces pureConfig
  let impureConfig = pureConfig
  xmonad impureConfig

-----------------
-- * Polybar * --
-----------------

-- |Adds a polybar with our hack of writing statuses to
-- a fifo, which can be picked up by polybar.
myBar :: 
  (LayoutClass l Window) =>
  XConfig l -> 
  XConfig (ModifiedLayout AvoidStruts l)
myBar = withLogHook . withEasySB myStatusBar toggle
  where
    withLogHook conf = conf { logHook = logToFifo }

    toggle XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

    cmd = R.polybarCmd

    polybarColor color str
      = concat ["%{F", color, "}", str, "%{F-}"]

    myStatusBar = def
      { sbLogHook = logToFifo
      , sbStartupHook = spawnFifo >> spawnStatusBar cmd
      , sbCleanupHook = killStatusBar cmd
      }

    -- Prettyprinter, relies on polybar formatting.
    myPP = def
      { ppTitle = polybarColor R.accent . shorten 70
      , ppCurrent = polybarColor R.accent . wrap "[" "]"
      }

    xmonadFifo = "/tmp/.xmonad-title-log"

    spawnFifo :: X ()
    spawnFifo = io $ safeSpawn "mkfifo" [xmonadFifo]

    logToFifo :: X ()
    logToFifo = do
      str <- dynamicLogString myPP
      io $ appendFile xmonadFifo (str ++ "\n")

------------------------------
-- * Custom Functionality * --
------------------------------

-- Send focus to the next screen. We do so by grabing the window
-- stack and checking whether there is a next screen to look at.
myCycleScreen :: (WorkspaceId -> X ()) -> X ()
myCycleScreen act = do
  visibleWs <- gets (SS.visible . windowset)
  case visibleWs of
    []       -> return ()
    (next:_) -> act (screenId next)
 where
   screenId :: SS.Screen i l a sid sd -> i
   screenId = SS.tag . SS.workspace

-- | Given an @n@ and a @t@, returns an action @act@ that,
-- when ran @n@ times in less than @t@ seconds, returns true.
-- Otherwise, returns false.
asyncNBeforeTimeout :: Int -> Float -> IO (IO Bool)
asyncNBeforeTimeout n toutF = do
  when (n < 2) $ error "Pick n to be at least 2"

  let toutMicroS = ceiling $ toutF * 1000000
  
  gate <- newEmptyMVar
  ctr <- newIORef 0

  tid <- forkIO $ forever $ do
    takeMVar gate
    threadDelay toutMicroS
    atomicWriteIORef ctr 0

  return $ do
    val <- atomicModifyIORef' ctr (\x -> (x+1, x))
    if val <= 0
    -- The value is (leq) 1, this is the first call to the action. Open the gate for the timeout-thread!
    then putMVar gate () >> return False
    -- It's not the first run, the timeouter is running and will eventally set the ioref to 0, that's fine!
    -- Just return whether we reached the count or not!
    else if val >= n - 1
         then atomicWriteIORef ctr 0 >> return True
         else return False


