{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction, MultiParamTypeClasses, ImplicitParams #-}
import XMonad
    ( Typeable,
      X,
      KeyMask,
      Window,
      XConfig(borderWidth, focusedBorderColor, handleEventHook,
              layoutHook, logHook, manageHook, modMask, normalBorderColor,
              startupHook, terminal, workspaces),
      spawn,
      xK_x,
      xK_w,
      xK_v,
      xK_u,
      xK_t,
      xK_s,
      xK_r,
      xK_q,
      xK_period,
      xK_p,
      xK_n,
      xK_m,
      xK_l,
      xK_k,
      xK_j,
      xK_i,
      xK_h,
      xK_f,
      xK_d,
      xK_comma,
      xK_c,
      xK_b,
      xK_a,
      xK_Print,
      shiftMask,
      mod4Mask,
      mod1Mask,
      controlMask,
      Resize(Expand, Shrink),
      IncMasterN(IncMasterN),
      (.|.),
      withFocused,
      sendMessage,
      kill,
      resource,
      doIgnore,
      doFloat,
      composeAll,
      (=?),
      (<+>),
      (-->),
      xmonad,
      (|||),
      defaultConfig )
import XMonad.Layout.Fullscreen ( fullscreenEventHook )
import XMonad.Layout.NoBorders ( smartBorders )
import XMonad.Layout.PerWorkspace ( onWorkspace )
import XMonad.Layout.SimplestFloat ( simplestFloat )
import XMonad.Layout.ResizableTile
    ( ResizableTall(ResizableTall),
      MirrorResize(MirrorExpand, MirrorShrink) )
import XMonad.Layout.Circle ( Circle(Circle) )
import XMonad.Layout.ThreeColumns ( ThreeCol(ThreeColMid) )
import XMonad.Layout.Grid ( Grid(Grid) )
import XMonad.Layout.WindowNavigation
    ( Direction2D(D, L, R, U), Navigate(Go, Swap), windowNavigation )
import XMonad.Layout.Reflect
    ( REFLECTY(REFLECTY), REFLECTX(REFLECTX) )
import XMonad.Layout.MultiToggle
    ( Transformer(..), Toggle(Toggle), mkToggle1 )
import XMonad.Layout.MultiToggle.Instances
    ( StdTransformers(MIRROR, NBFULL) )
import XMonad.Hooks.ManageHelpers ( isFullscreen, doFullFloat )
import XMonad.Prompt ( Direction1D(Next, Prev), defaultXPConfig )
import XMonad.Prompt.Shell ( shellPrompt )
import XMonad.Prompt.XMonad ( xmonadPrompt )
import XMonad.Util.EZConfig
    ( additionalMouseBindings, additionalKeys )
import XMonad.Actions.FloatKeys
    ( keysResizeWindow, keysMoveWindow )
import Graphics.X11.ExtraTypes.XF86
    ( xF86XK_Sleep,
      xF86XK_AudioRaiseVolume,
      xF86XK_AudioPrev,
      xF86XK_AudioPlay,
      xF86XK_AudioNext,
      xF86XK_AudioMute,
      xF86XK_AudioLowerVolume )
import XMonad.Hooks.DynamicLog
    ( PP(ppCurrent, ppHidden, ppHiddenNoWindows, ppLayout, ppOrder,
         ppOutput, ppSep, ppTitle, ppUrgent, ppVisible, ppWsSep, ppExtras),
      shorten,
      trim,
      pad,
      dynamicLogWithPP,
      defaultPP )
import XMonad.Hooks.ManageDocks
    ( avoidStruts,
      ToggleStruts(ToggleStruts),
      manageDocks,
      docksEventHook )
import XMonad.Hooks.SetWMName ( setWMName )
import XMonad.Hooks.Place
    ( underMouse, placeHook, inBounds, fixed )
import XMonad.Hooks.EwmhDesktops ( ewmh )
import System.IO.UTF8 ( hPutStrLn )
import XMonad.Actions.GridSelect ()
import XMonad.Util.Run ( spawnPipe )
import XMonad.Actions.CycleWS
    ( WSType(EmptyWS, NonEmptyWS), moveTo )
import Control.Arrow ()
import XMonad.Util.Font ()
import XMonad.Layout.LayoutModifier ()
import XMonad.Core ()
import XMonad.StackSet ()
import qualified XMonad.Util.ExtensibleState as XS ()
import Data.List ()
import XMonad.Prompt.Directory ( directoryPrompt )
import qualified XMonad.Layout.BinarySpacePartition as BSP
import Bar
    ( ColorControl(B, F),
      Alignment(ACenter, ARight, ALeft),
      clickable2,
      clickable,
      changeColor,
      align )
import Spacing ( SPACING(SPACING), spacing )
import PerWorkspaceDirs ( getDir, currentWorkspace, changeDir )
import XMonad.Util.Loggers
import XMonad.Layout.BorderResize

data FLOATED = FLOATED deriving (Read, Show, Eq, Typeable)
instance Transformer FLOATED Window where
  transform FLOATED x k = k myFloaU (const x)

myFloaU = simplestFloat

--------------------------------------------------------------------------------------------------------------------
-- DECLARE WORKSPACES RULES
--------------------------------------------------------------------------------------------------------------------
myLayout = windowNavigation
         $ borderResize
         $ onWorkspace (myWorkspaces !! 6) (avoidStruts simplestFloat)
         $ mkToggle1 NBFULL                                  -- (14)
         $ mkToggle1 REFLECTX                                -- (14,13)
         $ mkToggle1 REFLECTY                                -- (14,13)
         $ mkToggle1 MIRROR                                  --  "
         $ mkToggle1 FLOATED
         $ avoidStruts all
          where
            all            = spacing 30 BSP.emptyBSP
                          ||| bigMonitor
            bigMonitor     = spacing 10 $ ThreeColMid   nmaster delta ratio
            -- Default number of windows in master pane
            nmaster = 1
            --1 Percent of the screen to increment when resizing
            delta = 5/100
            -- Default proportion of the screen taken up by main pane
            ratio = toRational (2/(1 + sqrt 5 :: Double))

-------------------------------------------------------------------------------------------------------
-- WORKSPACE DEFINITIONS
--------------------------------------------------------------------------------------------------------------------
myWorkspaces = ["1. Main"
               ,"2. Web"
               ,"3. Dev"
               ,"4. Play"
               ,"5. IRC"
               ,"6. Music"
               ,"7. Float"
               ,"8. Misc"
               ,"9. Misc2"
               ]
--------------------------------------------------------------------------------------------------------------------
-- APPLICATION SPECIFIC RULES
--------------------------------------------------------------------------------------------------------------------
myManageHook = composeAll
        [ resource =? "dmenu"              --> doFloat
        , resource =? "gsimplecal"         --> placeHook (fixed (1,20/1080))
        , resource =? "alsamixer"          --> placeHook ( fixed (1,35/1080) ) <+> doFloat
        , resource =? "feh"                --> doIgnore
        , resource =? "dzen2"              --> doIgnore
        , resource =? "bar-aint-recursive" --> doIgnore
        , isFullscreen                     --> doFullFloat
        , manageDocks
        ]
newManageHook = myManageHook <+> placeHook (inBounds (underMouse (0, 0))) <+> manageHook defaultConfig

--------------------------------------------------------------------------------------------------------------------
-- DZEN LOG RULES for workspace names, layout image, current program title
--------------------------------------------------------------------------------------------------------------------
myLogHook h = dynamicLogWithPP ( defaultPP
    { ppCurrent = changeColor F (toBarC color13) . makeClickable
    , ppVisible = pad . makeClickable
    , ppHidden  = makeClickable
    , ppUrgent  = changeColor B (toBarC color3)  . makeClickable
    , ppHiddenNoWindows = makeClickable . takeWhile (/= '.')
    , ppWsSep   = " | "
    , ppSep     = "   "
    , ppLayout  = changeColor F (toBarC color10) . clickable "xdotool key super+space" . pad
    , ppTitle   = align ACenter . changeColor F (toBarC color10) . clickable2 (3::Int) "xdotool key super+shift+x" .  shorten 80 . pad
    , ppOrder   = \(ws:l:t:xs) -> [trim ws, trim l, trim t ]
    , ppOutput  = hPutStrLn h
    } )
    where makeClickable ws = let n = take 1 ws in clickable ("xdotool key super+"++n) ws

--------------------------------------------------------------------------------------------------------------------
-- Spawn pipes and menus on boot, set default settings
--------------------------------------------------------------------------------------------------------------------
myXmonadBar :: String
myXmonadBar = "bar -f \"-benis-uushi-medium-r-normal--11-90-75-75-p-58-iso10646-1\" -B \"#FF2B2B2B\" | zsh "

spawnTerminalInDir :: String -> X ()
spawnTerminalInDir s = spawn $ "cd " ++ s ++ "; " ++ myTerminal

main :: IO ()
main = do
  bar 	<- spawnPipe myXmonadBar
  xmonad $ ewmh defaultConfig
    { terminal           = myTerminal
    , borderWidth        = 2
    , normalBorderColor  = "#2B2B2B"
    , focusedBorderColor = color5
    , modMask            = myModMask
    , layoutHook         = myLayout
    , workspaces         = myWorkspaces
    , manageHook         = newManageHook
    , handleEventHook    = fullscreenEventHook <+> docksEventHook
    , startupHook        = setWMName "LG3D" >> spawn "~/bar_start.sh"
    , logHook            = myLogHook bar -- >> fadeInactiveLogHook 0xdddddddd
    }
--------------------------------------------------------------------------------------------------------------------
-- Keyboard options
--------------------------------------------------------------------------------------------------------------------
    `additionalKeys`
    [((myModMask .|. shiftMask , xK_b     ), spawn "firefox")
    ,((myModMask .|. shiftMask , xK_t     ), currentWorkspace >>= getDir >>= spawnTerminalInDir)
    ,((myModMask               , xK_j     ), sendMessage $ Go D)
    ,((myModMask               , xK_k     ), sendMessage $ Go U)
    ,((myModMask               , xK_h     ), sendMessage $ Go L)
    ,((myModMask               , xK_l     ), sendMessage $ Go R)
    ,((myModMask .|. shiftMask , xK_j     ), sendMessage $ Swap D)
    ,((myModMask .|. shiftMask , xK_k     ), sendMessage $ Swap U)
    ,((myModMask .|. shiftMask , xK_h     ), sendMessage $ Swap L)
    ,((myModMask .|. shiftMask , xK_l     ), sendMessage $ Swap R)
    ,((myModMask               , xK_s     ), sendMessage $ BSP.Swap)
    ,((myModMask .|. altMask   , xK_r     ), sendMessage BSP.Rotate)
    ,((myModMask .|. altMask , xK_l     ), sendMessage $ BSP.MoveSplit BSP.East)
    ,((myModMask .|. altMask , xK_h     ), sendMessage $ BSP.MoveSplit BSP.West)
    ,((myModMask .|. altMask , xK_j     ), sendMessage $ BSP.MoveSplit BSP.South)
    ,((myModMask .|. altMask , xK_k     ), sendMessage $ BSP.MoveSplit BSP.North)
    ,((myModMask .|. altMask , xK_r     ), sendMessage BSP.Rotate)
    ,((myModMask               , xK_r     ), shellPrompt defaultXPConfig)
    ,((myModMask               , xK_m     ), spawn "~/.xmonad/scripts/dzen_music.sh")
    ,((myModMask .|. shiftMask , xK_r     ), spawn "~/scripts/dmenu/spotlight")
    ,((myModMask               , xK_q     ), spawn "killall bar; cd ~/.xmonad; ghc -fcontext-stack=50  -threaded xmonad.hs; mv xmonad xmonad-x86_64-linux; xmonad --restart;" )
    ,((myModMask .|. shiftMask , xK_i     ), spawn "xcalib -invert -alter")
    ,((myModMask .|. shiftMask , xK_x     ), kill)
    ,((myModMask .|. shiftMask , xK_c     ), return ())
    ,((myModMask               , xK_p     ), moveTo Prev NonEmptyWS)
    ,((myModMask               , xK_n     ), moveTo Next NonEmptyWS)
    ,((myModMask               , xK_c     ), moveTo Next EmptyWS)
    ,((myModMask .|. controlMask , xK_j   ), sendMessage MirrorShrink)
    ,((myModMask .|. controlMask , xK_k   ), sendMessage MirrorExpand)
    ,((myModMask .|. controlMask , xK_h   ), sendMessage Shrink)
    ,((myModMask .|. controlMask , xK_l   ), sendMessage Expand)
    ,((myModMask               , xK_comma ), sendMessage (IncMasterN   1 )) -- %! Increment the number of windows in the master area
    ,((myModMask               , xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area
    ,((myModMask                , xK_a     ), withFocused (keysMoveWindow   (-20,  0)))
    ,((myModMask                , xK_w     ), withFocused (keysMoveWindow   (0  ,-20)))
    ,((myModMask                , xK_s     ), withFocused (keysMoveWindow   (0  , 20)))
    ,((myModMask                , xK_d     ), withFocused (keysMoveWindow   (20 ,  0)))
    ,((myModMask  .|. shiftMask , xK_a     ), withFocused (keysResizeWindow (-20,  0) (0,0)))
    ,((myModMask  .|. shiftMask , xK_w     ), withFocused (keysResizeWindow (0  ,-20) (0,0)))
    ,((myModMask  .|. shiftMask , xK_s     ), withFocused (keysResizeWindow (0  , 20) (0,0)))
    ,((myModMask  .|. shiftMask , xK_d     ), withFocused (keysResizeWindow (20 ,  0) (0,0)))
    ,((0                       , xK_Print ), spawn "scrot & mplayer /usr/share/sounds/freedesktop/stereo/screen-capture.oga")
    ,((myModMask               , xK_Print ), spawn "scrot -s & mplayer /usr/share/sounds/freedesktop/stereo/screen-capture.oga")
    ,((0                       , xF86XK_AudioLowerVolume), spawn "~/scripts/dvol2 -d 2 & mplayer /usr/share/sounds/freedesktop/stereo/audio-volume-change.oga")
    ,((0                       , xF86XK_AudioRaiseVolume ), spawn "~/scripts/dvol2 -i 2 & mplayer /usr/share/sounds/freedesktop/stereo/audio-volume-change.oga")
    ,((0                       , xF86XK_AudioMute), spawn "~/scripts/dvol2 -t")
    ,((0                       , xF86XK_Sleep), spawn "pm-suspend")
    ,((0                       , xF86XK_AudioPlay), spawn "mpc toggle")
    ,((0                       , xF86XK_AudioNext), spawn "mpc next")
    ,((0                       , xF86XK_AudioPrev), spawn "mpc prev")
    ,((myModMask, xK_b), sendMessage ToggleStruts) -- toggle the statusbar gap
    ,((myModMask .|. controlMask, xK_f), sendMessage $ Toggle NBFULL)
    ,((myModMask .|. controlMask, xK_v), sendMessage $ Toggle REFLECTX)
    ,((myModMask .|. controlMask, xK_m), sendMessage $ Toggle MIRROR)
    ,((myModMask .|. controlMask, xK_u), sendMessage $ Toggle FLOATED)
    ,((myModMask .|. controlMask, xK_i), sendMessage $ SPACING 10)
    ,((myModMask .|. controlMask, xK_d), sendMessage $ SPACING (negate 10))
    ,((myModMask .|. controlMask, xK_a), xmonadPrompt defaultXPConfig)
    ,((myModMask .|. shiftMask  , xK_d), directoryPrompt defaultXPConfig "Set working directory: " (\d -> currentWorkspace >>= changeDir d))
    ]
    `additionalMouseBindings`
    [((myModMask, 6), \_ -> moveTo Next NonEmptyWS)
    ,((myModMask, 7), \_ -> moveTo Prev NonEmptyWS)
    ,((myModMask, 5), \_ -> moveTo Prev NonEmptyWS)
    ,((myModMask, 4), \_ -> moveTo Next NonEmptyWS)
    ]


-- Define constants
toBarC :: String -> String
toBarC (x:xs)  = x : "FF" ++ xs
toBarC ""      = ""

altMask = mod1Mask

myTerminal :: String
myTerminal     = "urxvtc"

myBitmapsDir :: String
myBitmapsDir   = "~/.xmonad/dzen2/"

myModMask :: KeyMask
myModMask      = mod4Mask

myFont :: String
myFont         = "-*-tamsyn-medium-r-normal-*-12-87-*-*-*-*-*-*"
--myFont = "-*-terminus-medium-*-normal-*-9-*-*-*-*-*-*-*"
--myFont = "-*-nu-*-*-*-*-*-*-*-*-*-*-*-*"
--myFont = "-artwiz-lime-medium-r-normal-*-10-110-75-75-m-50-iso8859-*"
--myFont = "-artwiz-limey-medium-r-normal-*-10-110-75-75-m-50-iso8859-*"
--myFont = "-benis-lemon-medium-r-normal-*-10-110-75-75-m-50-iso8859-*"
--myFont = "'sans:italic:bold:underline'"
--myFont = "xft:droid sans mono:size=9"
--myFont = "xft:Droxd Sans:size=12"
--myFont = "-*-cure-*-*-*-*-*-*-*-*-*-*-*-*"

--background = "#140c0b"
--foreground = "#877a70"
--color0     = "#403f3e"
--color8     = "#666362"
--color1     = "#91444d"
--color9     = "#c78186"
--color2     = "#6b853d"
--color10    = "#abbd80"
--color3     = "#916949"
--color11    = "#cca88b"
--color4     = "#557282"
--color12    = "#8eabbd"
--color5     = "#78516d"
--color13    = "#a8879f"
--color6     = "#58756c"
--color14    = "#8ca8a2"
--color7     = "#94928f"
--color15    = "#cdcdcd"

-- CRYPT
--background="#000000"
--foreground="#D3D3D3"
--color0=  "#181818"
--color8=  "#181818"
--color1=  "#D7D7D7"
--color9=  "#D7D7D7"
--color2=  "#AADB0F"
--color10= "#AADB0F"
--color3=  "#666666
--color11= "#666666"
--color4=  "#FFFFFF"
--color12= "#FFFFFF"
--color5=  "#91BA0D"
--color13= "#91BA0D"
--color6=  "#D4D4D4"
--color14= "#D4D4D4"
--color7=  "#D3D3D3"
--color15= "#D3D3D3"

--CLOUDS
--background= "#0E0E0E"
--foreground= "#fefefe"
--
--color0= "#454545"
--color8= "#666666"
--color1=  "#CC4747"
--color9=  "#BF5858"
--color2=  "#A0CF5D"
--color10= "#B8D68C"
--color3=  "#FF9B52"
--color11= "#FFB680"
--color4=  "#5FA69B"
--color12= "#99C7BF"
--color5=  "#A966CC"
--color13= "#BD9FCC"
--color6=  "#6CAB79"
--color14= "#95BA9C"
--color7=  "#d3d3d3"
--color15= "#fefefe"

-- EROSION EDIT
background = "#181512"
foreground = "#D6C3B6"
color0     = "#332d29"
color8     = "#817267"
color1     = "#8c644c"
color9     = "#9f7155"
color2     = "#746C48"
color10    = "#9f7155"
color3     = "#bfba92"
color11    = "#E0DAAC"
color4     = "#646a6d"
color12    = "#777E82"
color5     = "#766782"
color13    = "#897796"
color6     = "#4B5C5E"
color14    = "#556D70"
color7     = "#504339"
color15    = "#9a875f"
-- EROSION
--background= "#181512"
--foreground= "#bea492"
--
--color0= "#332d29"
--color8= "#817267"
--
--color1= "#8c644c"
--color9= "#9f7155"
--
--color2= "#c4be90"
--color10= "#bec17e"
--
--color3= "#bfba92"
--color11= "#fafac0"
--
--color4= "#646a6d"
--color12= "#626e74"
--
--color5= "#6d6871"
--color13= "#756f7b"
--
--color6= "#3b484a"
--color14= "#444d4e"
--
--color7= "#504339"
--color15= "#9a875f"

-- PAPEY
--foreground= "#e5e5e5"
--background= "#1d1d1d"
--color0=  "#121212"
--color8=  "#5f5f5F"
--color1=  "#a35b66"
--color9=  "#ab6b74"
--color2=  "#99ab6f"
--color10= "#acb792"
--color3=  "#ca9733"
--color11= "#ccaa69"
--color4=  "#495d6e"
--color12= "#687987"
--color5=  "#825969"
--color13= "#977381"
--color6=  "#839191"
--color14= "#98A4A4"
--color7=  "#E0E0E0"
--color15= "#e5e5e5"
