-- Imports {{{
import XMonad
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import qualified XMonad.Hooks.ManageHelpers as ManageHelpers

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Loggers
import XMonad.Util.WorkspaceCompare (getSortByIndex)
import XMonad.Actions.CycleWS
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Magnifier
import XMonad.Layout.ImageButtonDecoration
import XMonad.Layout.BorderResize
import XMonad.Layout.WindowArranger
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Spacing
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
-- import XMonad.Layout.Gaps

import qualified XMonad.StackSet as W
import qualified Data.Map as M
-- END Imports }}}

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
     . fullscreenSupportBorder
     $ myConfig


myConfig = def
    { 
      modMask    = mod4Mask -- Rebind Mod to the GUI key
    , terminal = "alacritty"
    , layoutHook = myLayout -- Use custom layouts
    , handleEventHook = fullscreenEventHook
    , manageHook = myManageHook -- Match on certain windows
    , keys = myKeys
    , focusFollowsMouse = False
    , borderWidth = 2
    , clickJustFocuses = False
    , workspaces = myWorkspaces
    }

-- myWorkspaces = withScreen 1 (map show [1..6 :: Int])
            -- ++ withScreen 2 (map show [1..6 :: Int])
myWorkspaces = ["web", "discord", "term1", "term2", "spotify"]

myLayout = borderResize
         . windowArrange
         . spacingWithEdge 4

         $ tiled
       ||| Mirror tiled
       ||| Full 
       ||| threeCol
  where
    tiled    = Tall nmaster delta ratio
    nmaster  = 1       -- Default number of windows in the master pane
    ratio    = 1/2     -- Default proportion of screen occupied by master pane
    delta    = 3/100   -- Percent of screen to increment by when resizing panes

    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio

-- Key bindings {{{
myKeys :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
myKeys = \c -> mkKeymap c $
    [
    -- Base important keybinds
      ("M-<Return>",      spawn "alacritty")
    , ("M-p",             spawn "dmenu_run")
    , ("M-q",             spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
    , ("M-c",             kill)
    , ("M-f",             withFocused $ toggleFloat)

    -- Layout stuff
    , ("M-<Space>",       sendMessage NextLayout)

    -- Focus windows
    , ("M-n",             windows W.focusDown )
    , ("M-e",             windows W.focusUp )
    , ("M-<Return>",      windows W.focusMaster )

    -- Swap windows
    , ("M-C-n",           windows W.swapDown )
    , ("M-C-e",           windows W.swapUp )
    , ("M-C-<Return>",    windows W.swapMaster )

    -- Focus workspaces & screens
    -- Forever remember the time when Elk said this "should be reasonably easy to do"
    -- then proceeded to completely rewrite an entire module to make it work.
    -- , ("M-m",             moveTo Prev spacesOnCurrentScreen)
    -- , ("M-i",             moveTo Next spacesOnCurrentScreen)
    -- , ("M-C-m",           sequence_ [ shiftTo Prev spacesOnCurrentScreen
                                    -- , moveTo Prev spacesOnCurrentScreen] )
    -- , ("M-C-i",           sequence_ [ shiftTo Next spacesOnCurrentScreen
                                    -- , moveTo Next spacesOnCurrentScreen] )
    , ("M-m",             moveTo Prev hiddenWS)
    , ("M-i",             moveTo Next hiddenWS)
    , ("M-C-m",           sequence_ [shiftToPrev, prevWS])
    , ("M-C-i",           sequence_ [shiftToNext, nextWS])
    , ("M-o",             nextScreen)
    , ("M-C-o",           sequence_ [shiftNextScreen, nextScreen])
    , ("M-M1-o",           swapNextScreen)

    -- Increase or decrease number of windows in the master area
    , ("M-,",            sendMessage (IncMasterN 1))
    , ("M-.",            sendMessage (IncMasterN (-1)))

    -- Volume keys
    -- , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
    , ("<XF86AudioMute>", spawn "amixer set Master toggle")
    ]
      where
        toggleFloat w = windows (\s -> if M.member w (W.floating s)
                                       then W.sink w s
                                       else (W.float w (W.RationalRect (1/6) (1/6) (4/6) (4/6)) s))

isOnScreen :: ScreenId -> WindowSpace -> Bool
isOnScreen s ws = s == unmarshallS (W.tag ws)

currentScreen :: X ScreenId
currentScreen = gets (W.screen . W.current . windowset)

spacesOnCurrentScreen :: WSType
spacesOnCurrentScreen = WSIs (isOnScreen `fmap` currentScreen)

-- END Key bindings }}}

-- Xmobar {{{
myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
-- END Xmobar }}}

-- Exception rules {{{
myManageHook :: ManageHook
myManageHook = composeAll
    [ resource  =? "Dialog"             --> ManageHelpers.doCenterFloat
    , className =? "1Password"          --> doFloat
    , fullscreenManageHook
    ]
-- END Exception rules }}}
