{-# LANGUAGE FlexibleContexts #-}
import XMonad
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.EZConfig (additionalKeys, removeKeys)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP 
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import qualified XMonad.StackSet as SS
import qualified Reflection as R

------------------------------------------
-- * My Modifier and Top-Level Config * --
------------------------------------------

myMod = mod1Mask

myConfig = (def
  { terminal = "mate-terminal"
  , borderWidth = 2
  , focusedBorderColor = R.accent
  , workspaces = myWorkspaces
  , manageHook = manageHook def <+> manageDocks <+> myManageHook
  , layoutHook = smartBorders . avoidStruts $ layoutHook def
  } `removeKeys` myRemovedKeys)
  `additionalKeys` myKeys

myKeys :: [((KeyMask, KeySym), X ())]
myKeys = [
  -- Launch rofi in all its flavours
    ((myMod, xK_d), spawn "rofi -show drun")
  , ((myMod, xK_q), spawn "rofi -show p -modi \"p:rofi-power-menu --choices=logout/shutdown/reboot/suspend\"")
  , ((myMod, xK_p), spawn "rofi -show p -modi p:rofi-pass")
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
    ]
  where
    -- Sometimes these windows are floated annoyingly large,
    -- llok into doRectFloat from XMonad.Hooks.ManageHelpers
    floats :: [String]
    floats = 
      [ "Blueman-manager"
      , "Pavucontrol"
      , "Nm-connection-editor"
      ]

    -- Which windows to simply float, basec on theur className
    manageBasicFloats :: ManageHook
    manageBasicFloats = (fmap (`elem` floats) className) --> doFloat

    manageFF :: ManageHook
    manageFF = (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat

--------------
-- * Main * --
--------------

main :: IO ()
main = xmonad $ myBar $ ewmhFullscreen $ ewmh $ myConfig

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
