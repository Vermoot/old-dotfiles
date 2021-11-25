-- Imports {{{
    -- System
import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map.Strict as M
import XMonad.Prompt
import XMonad.Prompt.Theme

    -- Layouts
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Fullscreen
import XMonad.Layout.ResizableTile
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.BinarySpacePartition

    -- Layout modifiers
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.Magnifier
import XMonad.Layout.ImageButtonDecoration
import XMonad.Layout.WindowArranger
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Spacing
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.NoBorders
import XMonad.Layout.DecorationAddons
import XMonad.Layout.ButtonDecoration

    -- Hooks
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.DebugEvents
import XMonad.Hooks.ManageDocks
import XMonad.ManageHook
import qualified XMonad.Hooks.ManageHelpers as ManageHelpers

    -- Utilities
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Loggers
import XMonad.Util.WorkspaceCompare (getSortByIndex)

    -- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.TiledWindowDragging
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.PerWindowKeys
-- END Imports }}}

-- Defaults {{{
myModMask            = mod4Mask
myTerminal           = "alacritty"

myFocusFollowsMouse  = False
myClickJustFocuses   = False

myBorderWidth        = 1
myNormalBorderColor  = "#665c54"
myFocusedBorderColor = "#ebdbb2"

myWorkspaces         = withScreen 1 (map show [1..6 :: Int])
                    ++ withScreen 2 (map show [1..6 :: Int])
-- myWorkspaces = ["web", "discord", "misc", "misc2","misc3", "misc4", "spotify"]

-- END Defaults}}}

-- Layouts {{{
myLayout = windowArrange
         . avoidStruts
         . draggingVisualizer
         . spacingRaw False (Border 4 4 4 4) True (Border 4 4 4 4) True
         . buttonDeco shrinkText defaultThemeWithButtons

         $ tiled
       ||| Full 
       ||| threeColMid
       ||| emptyBSP
  where
    tiled    = mouseResizableTile {
                                    nmaster       = nmaster
                                  , fracIncrement = delta
                                  , masterFrac    = ratio
                                  , draggerType   = FixedDragger 4 8
                                  }
    nmaster  = 1        -- Default number of windows in the master pane
    ratio    = 1/2      -- Default proportion of screen occupied by master pane
    delta    = 6/100    -- Percent of screen to increment by when resizing panes

    threeColMid = magnifiercz' 2 $ ThreeColMid nmaster delta ratio
-- END Layouts }}}

-- ManageHook {{{
myManageHook :: ManageHook 
myManageHook = composeAll
    [ resource                        =? "Dialog"               --> ManageHelpers.doCenterFloat
    , className                       =? "1password"            --> ManageHelpers.doCenterFloat
    , stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog" --> ManageHelpers.doCenterFloat
    , className                       =? "tint2"                --> hasBorder False
    , ManageHelpers.isFullscreen                                --> ManageHelpers.doFullFloat
    , fullscreenManageHook
    ] 
-- END ManageHook }}}

-- Key bindings {{{
myKeys :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
myKeys = \c -> mkKeymap c $
    [

    -- Base important keybinds
      ("M-<Return>",      spawn "alacritty")
    , ("M-<Space>",       spawn "dmenu_run")
    , ("C-<Space>",       spawn "rofi -combi-modi 'window,drun' -show combi -modi combi")
    , ("M-q",             spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
    , ("M-c",             kill)
    , ("M-f",             withFocused $ toggleFloat)
    , ("M-h",             themePrompt def)

    -- Layout stuff
    , ("M-d",             sendMessage NextLayout)

    -- Focus windows
    , ("M-n",             windows W.focusDown )
    , ("M-e",             windows W.focusUp )
    , ("M-l",             windows W.focusMaster )

    -- Manipulate windows
    , ("M-C-n",           windows W.swapDown )
    , ("M-C-e",           windows W.swapUp )
    , ("M-C-l",           windows W.swapMaster )

    -- Resize tiled windows
    , ("M-<Up>",          sendMessage ShrinkSlave)
    , ("M-<Down>",        sendMessage ExpandSlave)
    , ("M-<Left>",        sendMessage Shrink)
    , ("M-<Right>",       sendMessage Expand)

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
    , ("M-M1-m",          swapTo Prev)
    , ("M-M1-i",          swapTo Next)
    , ("M-o",             nextScreen)
    , ("M-C-o",           sequence_ [shiftNextScreen, nextScreen])
    , ("M-M1-o",          sequence_ [swapNextScreen, nextScreen] )

    -- Increase or decrease number of windows in the master area
    , ("M-,",            sendMessage (IncMasterN 1))
    , ("M-.",            sendMessage (IncMasterN (-1)))

    -- Volume keys
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
    , ("<XF86AudioMute>",        spawn "amixer set Master toggle")


    -- Screenshots
    , ("C-S-4",                 spawn "scrot -s ~/scrot.png && xclip -selection clipboard -t image/png ~/scrot.png && rm ~/scrot.png")
    , ("C-S-3",                 spawn "scrot -s -b ~/scrot.png && xclip -selection clipboard -t image/png ~/scrot.png && rm ~/scrot.png")

    -- Discord
    , ("C-S-M1-e",              bindFirst [(className =? "Discord", spawn "xdotool key alt+Up")])
    ]
      where
        toggleFloat w = windows (\s -> if M.member w (W.floating s)
                                       then W.sink w s
                                       else (W.float w (W.RationalRect (1/6) (1/6) (4/6) (4/6)) s))

-- Cycle onScreen stuff {{{
isOnScreen :: ScreenId -> WindowSpace -> Bool
isOnScreen s ws = s == unmarshallS (W.tag ws)

currentScreen :: X ScreenId
currentScreen = gets (W.screen . W.current . windowset)

spacesOnCurrentScreen :: WSType
spacesOnCurrentScreen = WSIs $ do
    s <- currentScreen
    return $ \x -> W.tag x /= "NSP" && isOnScreen s x
-- }}}

-- END Key bindings }}}

-- Mouse Bindings {{{
myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList
    -- mod-button1 %! Set the window to floating mode and move by dragging
    [ ((modMask, button1), dragWindow)
    -- [ ((modMask, button1), \w -> ifM (withWindowSet (M.member w . W.floating) (mouseMoveWindow w) (dragWindow w)))
    -- mod-button2 %! Raise the window to the top of the stack
    , ((modMask, button2), windows . (W.shiftMaster .) . W.focusWindow)
    -- mod-button3 %! Set the window to floating mode and resize by dragging
    , ((modMask, button3), \w -> focus w >> mouseResizeWindow w
                                         >> windows W.shiftMaster)
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
-- END Mouse Bindings }}}

-- Main Event {{{
main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
     . fullscreenSupportBorder
     $ myConfig

myConfig = def
    { 
      modMask            = myModMask
    , terminal           = myTerminal
    , layoutHook         = myLayout
    , handleEventHook    = fullscreenEventHook <+> debugEventsHook
    , manageHook         = myManageHook
    , keys               = myKeys
    , mouseBindings      = myMouseBindings
    , focusFollowsMouse  = myFocusFollowsMouse
    , clickJustFocuses   = myClickJustFocuses
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , workspaces         = myWorkspaces
    }
-- END Main Event }}}

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
