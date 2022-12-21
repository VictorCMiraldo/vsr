{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
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
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import qualified XMonad.StackSet as SS
import qualified Reflection as R
import qualified Data.Map as M
import Data.IORef
import Data.List (isPrefixOf, isSuffixOf)
import Data.Monoid (All(..))
import Control.Concurrent
import Control.Monad
import Foreign.C.Types 

------------------------------------------
-- * My Modifier and Top-Level Config * --
------------------------------------------

myMod = mod1Mask

myModCode = 108

myConfig = ((def
  { terminal = "mate-terminal"
  , borderWidth = 2
  , focusedBorderColor = R.accent
  , workspaces = myWorkspaces
  , manageHook = manageHook def <+> manageDocks <+> myManageHook
  , layoutHook = smartBorders . avoidStruts $ layoutHook def
  , startupHook = myStartupHook
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
  , ((myMod , xK_Tab), cycleRecentWS [xK_Alt_L] xK_Tab xK_grave)
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


------------------------------------
-- * My Events && Startup Hooks * --
------------------------------------

-- The block below is used to implement horizontal workspace scrolling 
-- with moving the mouse while @myMod@ is down

-- | For that to even have a chance of working, we need to subscribe xmonad to 
-- listen to the keyevents of @myMod@ within X
myStartupHook :: X ()
myStartupHook = do
  XConf { display = dpy, theRoot = rootw } <- ask
  void $ io $ do
    myKeyCode <- keysymToKeycode dpy xK_Alt_R
    grabKey dpy myKeyCode anyModifier rootw True grabModeAsync grabModeAsync

-- | Utility to subscribe to pointer motion events
myGrabPointer :: X ()
myGrabPointer = do
  XConf { display = dpy, theRoot = rootw } <- ask
  void $ io $ grabPointer dpy rootw False pointerMotionMask 
                grabModeAsync grabModeAsync none none currentTime

-- | Utility to unsubscribe from pointer events
myUngrabPointer :: X ()
myUngrabPointer = do
  XConf { display = dpy, theRoot = rootw } <- ask
  io $ ungrabPointer dpy currentTime

-- | Now, for our next trick, we implement a simple automaton that
-- changes state depending on the rules we implemented.
myEventHook :: IORef MyHorizontalDragState -> Event -> X All
myEventHook iost (MotionEvent { ev_x = x, ev_y = y }) = do
  st <- io $ readIORef iost
  when (mhdsModIsDown st) $ do
    let st' = myHandleMovementEvent st x y
    st'' <- gaugeMovement st'
    io $ writeIORef iost st''
  return (All False)
myEventHook iost KeyEvent {..} 
  | ev_keycode == myModCode = do
    st <- io $ readIORef iost
    -- Grab the pointer on keydown, ungrab it on keyup
    st' <- case ev_event_type of
              2 -> myGrabPointer >> return (st { mhdsLastX = Just ev_x , mhdsModIsDown = True })
              3 -> myUngrabPointer >> return (st { mhdsLastX = Nothing , mhdsModIsDown = False })
              _ -> return st
    io $ writeIORef iost st'
    return (All True)
myEventHook _ _ = return (All True)


data MyHorizontalDragState = MyHorizontalDragState { 
  -- | Keeps track of whether our mod key is up or down
  mhdsModIsDown :: Bool,

  -- | While our mod key is down, keeps track of how many horizontal
  -- pixels have we scrolled through.
  mhdsAccuX :: CInt,
  mhdsLastX :: Maybe CInt
  } deriving (Show)

newMyHorizontalDragState :: MyHorizontalDragState
newMyHorizontalDragState = MyHorizontalDragState False 0 Nothing

-- This is how many horizontal pixels do we have to move the mouse to
-- trigger a workspace switch
x_THRESHOLD :: CInt
x_THRESHOLD = 100

myHandleMovementEvent :: MyHorizontalDragState -> CInt -> CInt -> MyHorizontalDragState
myHandleMovementEvent st x y
  | not (mhdsModIsDown st) = st
  | Just lx <- mhdsLastX st = st { mhdsLastX = Just x , mhdsAccuX = mhdsAccuX st + (lx - x) }
  | otherwise = st { mhdsLastX = Just x , mhdsAccuX = 0 }

gaugeMovement :: MyHorizontalDragState -> X MyHorizontalDragState
gaugeMovement st
  | mhdsAccuX st > x_THRESHOLD = do
    moveTo Prev (Not emptyWS) 
    return (st { mhdsAccuX = 0 })
  | mhdsAccuX st < -x_THRESHOLD = do
    moveTo Next (Not emptyWS) 
    return (st { mhdsAccuX = 0 })
  | otherwise = return st

horizontalScrollWorkspaces :: (LayoutClass l Window) => XConfig l -> IO (XConfig l)
horizontalScrollWorkspaces conf = do
  iost <- newIORef newMyHorizontalDragState
  return $ conf { 
    handleEventHook = myEventHookWrapper iost <> handleEventHook conf 
  }
  where
    -- convenience wrapper to easily install a logging function for when we need to debug this.
    myEventHookWrapper :: IORef MyHorizontalDragState -> Event -> X All
    myEventHookWrapper iost ev = do
      -- io $ appendFile "/home/victor/my-event-hook" (show ev ++ "\n")
      myEventHook iost ev

--------------
-- * Main * --
--------------

main :: IO ()
main = do
  let pureConfig = myBar $ ewmhFullscreen $ ewmh $ myConfig
  impureConfig <- horizontalScrollWorkspaces pureConfig
  -- let impureConfig = pureConfig
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


