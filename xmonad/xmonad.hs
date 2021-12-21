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
import XMonad.Layout.SimpleFloat

    -- Layout modifiers
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.Magnifier
import XMonad.Layout.Maximize
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
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Loggers
import XMonad.Util.Paste
import XMonad.Util.WorkspaceCompare (getSortByIndex)

    -- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.TiledWindowDragging
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.PerWindowKeys

    -- Haskell
import Text.ParserCombinators.ReadP
-- END Imports }}}

-- Defaults {{{
myModMask            = mod4Mask
myTerminal           = "alacritty"

myFocusFollowsMouse  = False
myClickJustFocuses   = False

myBorderWidth        = 1
myNormalBorderColor  = "#665c54"
myFocusedBorderColor = "#ebdbb2"

-- myWorkspaces         = withScreen 1 (map show [1..6 :: Int])
                    -- ++ withScreen 2 (map show [1..6 :: Int])
myWorkspaces :: [[Char]]
myWorkspaces = ["1","2","3","4","5","6"]

-- myWorkspaces = ["web", "discord", "misc", "misc2","misc3", "misc4", "spotify"]

-- END Defaults}}}

-- Layouts {{{
myLayout = windowArrange
         . avoidStruts
         . draggingVisualizer
         . spacingRaw False (Border 4 4 4 4) True (Border 4 4 4 4) True
         . maximize
         -- . imageButtonDeco shrinkText defaultThemeWithImageButtons

         $ tiled
       ||| Full 
       ||| threeColMid
       ||| emptyBSP
       ||| simpleFloat
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

-- StartupHook {{{
myStartupHook :: X () 
myStartupHook = do
    spawnOnce "feh --bg-fill ./Pictures/Wallpapers/Paris_4k_Gruvbox.png"
    spawnOnce "xset r rate 150 60"
    spawnOnce "picom -b --experimental-backends"
-- END StartupHook }}}

-- ManageHook {{{
myManageHook :: ManageHook 
myManageHook = composeAll
    [ resource                        =? "Dialog"               --> ManageHelpers.doCenterFloat
    , className                       =? "1Password"            --> ManageHelpers.doCenterFloat
    , stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog" --> ManageHelpers.doCenterFloat
    , className                       =? "tint2"                --> hasBorder False
    , ManageHelpers.isFullscreen                                --> ManageHelpers.doFullFloat
    , fullscreenManageHook
    , manageDocks
    ] 
-- END ManageHook }}}

-- Key bindings {{{
myKeys :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
myKeys = \c -> mkKeymap c $
    [

    -- Base important keybinds
      ("M-<Return>",      spawn "alacritty")
    , ("M-<Space>",       spawn "dmenu_run")
    , ("C-<Space>",       spawn "rofi -m -4 -combi-modi 'window,drun' -show combi -modi combi")
    , ("M-q",             spawn ("if type xmonad; then xmonad --recompile && xmonad --restart;"
                              ++ "else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"))
    , ("M-c",             kill)
    , ("M-f",             withFocused $ toggleFloat)

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
    , ("M-x",             withFocused (sendMessage . maximizeRestore))

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
    , ("M-,",             sendMessage (IncMasterN 1))
    , ("M-.",             sendMessage (IncMasterN (-1)))

    -- Volume & Media keys
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
    , ("<XF86AudioMute>",        spawn "amixer set Master toggle")
    , ("<XF86AudioPlay>",        spawn "playerctl -p spotify play-pause")
    , ("<XF86AudioRewind>",      spawn "playerctl -p spotify previous")
    , ("<XF86AudioForward>",     spawn "playerctl -p spotify next")


    -- Screenshots
    , ("C-S-4",                 spawn ("scrot -s -b ~/scrot.png &&"
                                    ++ "xclip -selection clipboard -t image/png ~/scrot.png &&"
                                    ++ "rm ~/scrot.png"))
    , ("C-S-3",                 spawn ("scrot -s -b ~/scrot.png &&"
                                    ++ "xclip -selection clipboard -t image/png ~/scrot.png &&"
                                    ++ "rm ~/scrot.png"))

    -- App-specific remaps
    , ("C-S-M1-n",              bindFirst [ (className =? "discord", sendKey mod1Mask xK_Down)
                                          , (className =? "firefox", sendKey controlMask xK_Tab)
                                          , (pure True             , sendKey mehMask xK_n)])
    , ("C-S-M1-e",              bindFirst [ (className =? "discord", sendKey mod1Mask xK_Up)
                                          , (className =? "firefox", sendKey (controlMask .|. shiftMask) xK_Tab)
                                          , (pure True             , sendKey mehMask xK_e)])
    , ("C-S-M1-m",              bindFirst [ (className =? "discord", sendKey (controlMask .|. mod1Mask) xK_Down)
                                          , (pure True             , sendKey mehMask xK_m)])
    , ("C-S-M1-i",              bindFirst [ (className =? "discord", sendKey (controlMask .|. mod1Mask) xK_Up)
                                          , (pure True             , sendKey mehMask xK_i)])
    , ("C-S-M1-<Up>",           bindFirst [ (className =? "discord", sendKey (shiftMask .|. mod1Mask) xK_Up)
                                          , (pure True             , sendKey mehMask xK_Up)])
    , ("C-S-M1-<Down>",         bindFirst [ (className =? "discord", sendKey (shiftMask .|. mod1Mask) xK_Down)
                                          , (pure True             , sendKey mehMask xK_Down)])
    ]
      where
        mehMask = controlMask .|. mod1Mask .|. shiftMask
        toggleFloat w = windows (\s -> if M.member w (W.floating s)
                                       then W.sink w s
                                       else (W.float w (W.RationalRect (1/6) (1/6) (4/6) (4/6)) s))

        -- specificRemaps :: [(Query Bool, String] -> X ()
        -- specificRemaps cond keybind = 

        -- Cycle onScreen stuff {{{
        isOnScreen :: ScreenId -> WindowSpace -> Bool
        -- isOnScreen s = (s ==) . unmarshallS . W.tag
        isOnScreen s ws = s == unmarshallS (W.tag ws)

        currentScreen :: X ScreenId
        currentScreen = gets (W.screen . W.current . windowset)

        spacesOnCurrentScreen :: WSType
        spacesOnCurrentScreen = WSIs $ do
            s <- currentScreen
            return $ \x -> isOnScreen s x
        -- }}}

-- END Key bindings }}}

-- Mouse Bindings {{{
myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList
    -- mod-button1 %! Set the window to floating mode and move by dragging
    [ ((modMask, button1), dragWindow)
    -- [ ((modMask, button1), \w -> ifM (withWindowSet (M.member w . W.floating) (mouseMoveWindow w) (dragWindow w)))
    -- mod-button2 %! Raise the window to the top of the stack
    , ((modMask, button2), \w -> focus w >> mouseMoveWindow w
                                          >> windows W.shiftMaster)
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
     . docks
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
     . fullscreenSupportBorder
     $ myConfig

myConfig = def
    { 
      modMask            = myModMask
    , terminal           = myTerminal
    , layoutHook         = myLayout
    , handleEventHook    = fullscreenEventHook <+> debugEventsHook
    , startupHook        = myStartupHook
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
