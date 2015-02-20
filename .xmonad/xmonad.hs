{-# OPTIONS_GHC -cpp #-}

    -- imports
import XMonad hiding ( (|||) )
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit
import Data.List

-- utils
import XMonad.Util.Run
import XMonad.Util.Scratchpad

-- actions
import XMonad.Actions.CycleWS
import XMonad.Actions.OnScreen
import XMonad.Actions.WindowGo
import XMonad.Actions.DwmPromote
import XMonad.Actions.GridSelect
import XMonad.Actions.UpdatePointer

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.EwmhDesktops

-- layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Named
import XMonad.Layout.Spacing
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimplestFloat
import XMonad.Layout.LayoutCombinators


    -------------------------------------------------------------------------------
    -- Main --
main :: IO ()
main = xmonad   =<< statusBar cmd pp kb conf 
    where 
    uhook = withUrgencyHook NoUrgencyHook
    cmd = "xmobar"
    pp = customPP
    kb = toggleStrutsKey
    conf = uhook $ ewmh myConfig 

    myConfig = defaultConfig { 
        workspaces = workspaces'
        , modMask = modMask'
        , borderWidth = borderWidth'
        , normalBorderColor = normalBorderColor'
        , focusedBorderColor = focusedBorderColor'
        , terminal = terminal'
        , keys = keys'
        , layoutHook = layoutHook'
        , manageHook = manageHook' <+> manageScratchPad
        , mouseBindings = myMouseBindings
        , logHook = updatePointer (Relative 0.5 0.5)
    }
-------------------------------------------------------------------------------
-- Window Management --

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RaionalRect l t w h)
    where
        h = 0.1
        w = 1
        t = 1 - h
        l = 1 - w

manageHook' = mainManageHook <+> manageDocks
    where
        -- the main managehook
        mainManageHook = composeAll $ concat
            [ [ resource  =? r --> doIgnore						| r <- myIgnores]
            , [ className =? t --> doShift	(workspaces' !! 0)	| t <- myWebs   ]
            , [ className =? c --> doShift	(workspaces' !! 1)	| c <- myChats  ]
            , [ className =? t --> doShift	(workspaces' !! 2)	| t <- myFiles  ]
            , [ resource  =? c --> doShift	(workspaces' !! 3)	| c <- myWorks  ]
            , [ resource  =? t --> doShift	(workspaces' !! 4)	| t <- myTerms  ]
            , [ className =? c --> doShift	(workspaces' !! 5)	| c <- myMiscs  ]
            , [ className =? c --> doFloat						| c <- myFloats ]
            , [ className =? c --> doCenterFloat				| c <- myCFloats]
            , [ name      =? n --> doCenterFloat				| n <- myCNames ]

            , [ isDialog       --> doCenterFloat								]
            , [ isFullscreen   --> doFullFloat ]
            ]

        role = stringProperty "WM_WINDOW_ROLE"
        name = stringProperty "WM_NAME"

        myIgnores = ["desktop","desktop_window"]
        myChats   = ["SkypeTab"]
        myTerms   = ["tmux"]
        myMiscs   = ["Wine", "VBoxSDL", "Eiskaltdcpp-gtk", "Steam"]
        myWebs    = ["Chromium", "Iron", "Firefox", "Luakit", "mcabber"]
        myFloats  = ["mpv", "Zenity", "VirtualBox", "Viewnior"]
        myCFloats = ["Xmessage","Save As...","XFontSel","Nitrogen"]
        myCNames  = ["bashrun"]
        myFiles   = ["ranger","Nautilus","Lanikai", "Thunderbird"]
        myWorks   = ["vim"]

-- colors and font

myFont = "-*-terminus-medium-*-*-*-12-*-*-*-*-*-*-u"
{- myFont = "xft:Icons-8" -}

myDmenu = "exec `dmenu_path_c | yeganesh -- -nb '" ++ nBgColor ++ "' -nf '" ++ nFgColor ++ "' -sb '" ++ aBgColor ++"' -sf '" ++ aFgColor ++ "' -fn '" ++ myFont ++ "'`"
wMenu = "~/bin/wdmenu"

-- Inactive --
nBgColor  = "#1d1f21"
nFgColor  = "#707880"

-- Active --
aBgColor = "#1d1f21"
aFgColor = "#c5c8c6"

-- Ocupied --
oBgColor = "#1d1f21"
oFgColor = "#AAAAAA"

bBgColor = "#262626"

-- Urgent --
uBgColor = "#a54242"
uFgColor = "#FFFFFF"

-- Borders --
nBorderColor = "#aaaaaa"
aBorderColor = "#1d1f21"

customPP = defaultPP 
    { ppCurrent = xmobarColor aFgColor aBgColor
        , ppHidden = xmobarColor oFgColor oBgColor . xmobarStrip
            , ppHiddenNoWindows = xmobarColor nFgColor nBgColor
            , ppUrgent = xmobarColor uFgColor uBgColor
            , ppLayout = xmobarColor nBgColor nBgColor
            , ppTitle = xmobarColor nBgColor nBgColor
            , ppSep = xmobarColor nFgColor nBgColor " "
            , ppWsSep = ""
    }

-- urgent notification
{- urgentConfig = UrgencyConfig { suppressWhen = Focused } -}

-- borders
borderWidth' = 0
normalBorderColor'  = nBorderColor
focusedBorderColor' = aBorderColor

-- workspaces
workspaces'             :: [String]
workspaces'             = [" www ", " im ", " file ", " work ", " term ", " misc "]

-- layouts
layoutHook' = smartBorders
    $ avoidStruts
    $ onWorkspace (workspaces' !! 1) bFirst
    $ onWorkspace (workspaces' !! 2) tFirst
    $ onWorkspace (workspaces' !! 5) fFirst
    $ standartLayouts
    where
        rt    = ResizableTall 1 (2/100) (5.5/10) [] 
        tile  = named "t" $ spacing 4 $ rt
        mtile = named "b" $ spacing 4 $ Mirror rt
        full  = named "m" $ noBorders Full
        float = named "f" $ simplestFloat

        fFirst = float ||| full ||| tile ||| mtile
        tFirst = tile ||| full ||| mtile ||| float
        bFirst = mtile ||| full ||| tile ||| float
        standartLayouts = full ||| tile ||| mtile ||| float

-- Terminal --
terminal' = "urxvt"

-- Keys/Button bindings --

modMask' = mod4Mask

    -- keys
toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- launching and killing programs
    [ ((modMask,               xK_Return), dwmpromote) 
    , ((modMask,               xK_grave ), spawn myDmenu) 
    , ((modMask,               xK_Tab ), spawn wMenu) 
    , ((modMask .|. shiftMask, xK_c     ), kill)

    -- layouts
    , ((modMask,               xK_a     ), sendMessage $ JumpToLayout "m")
    , ((modMask,               xK_s     ), sendMessage $ JumpToLayout "t")
    , ((modMask,               xK_d     ), sendMessage $ JumpToLayout "b")
    , ((modMask,               xK_f     ), sendMessage $ JumpToLayout "f")

    -- refresh
    , ((modMask,               xK_n     ), refresh)
    , ((modMask,               xK_space ), withFocused $ windows . W.sink)

    -- focus
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp)

    -- swapping
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- resizing
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)

    -- restart
    , ((modMask .|. shiftMask, xK_r     ), spawn "xmonad --recompile; xmonad --restart") 

    -- applications
    , ((modMask .|. shiftMask, xK_f     ), runOrRaise "iron" (className =? "Iron") )
    , ((modMask .|. shiftMask, xK_p     ), raiseMaybe (runInTerm "-name ranger" "ranger") (resource =? "ranger"))
    , ((modMask .|. shiftMask, xK_g     ), raiseMaybe (runInTerm "-name vim" "vim --servername main") (resource =? "vim"))
    , ((modMask .|. shiftMask, xK_Return), raiseMaybe (runInTerm "-name tmux" "/home/cf8/bin/runtmux") (resource =? "tmux"))
    , ((modMask .|. shiftMask, xK_m     ), raiseMaybe (runInTerm "-name mcabber" "dtach -A /tmp/mcabber mcabber") (resource =? "mcabber"))
    {- , ((0 , xK_F2     ), spawn "sh /home/cf8/bin/wiz.sh") -}
    , ((0                    , xK_Scroll_Lock ), spawn "mpc pause ; sflock")
    , ((0                    , xK_Print ), spawn "~/bin/screenshot.sh") 

    -- volume and mpd
    , ((0, xF86XK_AudioLowerVolume),        spawn "~/bin/vol down")
    , ((0, xF86XK_AudioRaiseVolume),        spawn "~/bin/vol up")
    , ((0, xF86XK_AudioMute),               spawn "~/bin/vol mute")
    , ((0, xF86XK_Forward),                 spawn "mpc next")
    , ((0, xF86XK_Back),                    spawn "mpc prev")
    , ((0, xF86XK_AudioPlay),               spawn "mpc toggle")

    , ((modMask, xk_Tab), scratchpadSpawnActionTerminal terminal'

    -- tagkeys
    , ((modMask,               xK_1     ), windows (viewOnScreen 0 ( workspaces' !! 0 )))
    , ((modMask,               xK_2     ), windows (viewOnScreen 0 ( workspaces' !! 1 )))
    , ((modMask,               xK_3     ), windows (viewOnScreen 0 ( workspaces' !! 2 )))
    , ((modMask,               xK_q     ), windows (viewOnScreen 0 ( workspaces' !! 3 )))
    , ((modMask,               xK_w     ), windows (viewOnScreen 0 ( workspaces' !! 4 )))
    , ((modMask,               xK_e     ), windows (viewOnScreen 0 ( workspaces' !! 5 )))
    ]
    -- ++
    -- [((m .|. modMask, k), windows $ f i)
    --     | (i, k) <- zip (XMonad.workspaces conf) [xK_1,xK_2,xK_3,xK_q,xK_w,xK_e]
    --     , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

    ++
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1,xK_2,xK_3,xK_q,xK_w,xK_e]
        , (f, m) <- [(W.shift, shiftMask)]]


    -------------------------------------------------------------------------------

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> (withFocused $ windows . W.sink)))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    ]
