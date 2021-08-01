-- xmonad config file (modified default).
--
-- 
-- 
--
-- 
--
-- Import modules
-- We need these for the core system
import XMonad hiding ( (|||) ) -- don't use the normal |||
                               -- operator. It conflicts with
                               -- LayoutCombinators.
import Data.Monoid
import System.Exit
-- We need these for the status bar, xmobar
import XMonad.Hooks.DynamicLog -- system state
import XMonad.Hooks.SetWMName -- Window name in status bar
import XMonad.Hooks.ManageDocks -- Status bar not tiled over
import XMonad.Util.Run(spawnPipe) -- Send data to status bar
import System.IO -- send data to status bar
-- For keybindings
import XMonad.Util.EZConfig -- Easier keybinding names
import Graphics.X11.ExtraTypes.XF86 -- Special keybindings
-- For prompts
import XMonad.Prompt -- Core library
import XMonad.Prompt.Window -- Lets you see a list of windows and
                           -- summon them or go to them.
import XMonad.Prompt.AppendFile -- Appends a single line to a file
import XMonad.Prompt.Man -- Displays the man page of something
import XMonad.Prompt.Shell -- Displays a shell prompt
-- Miscellaneous window/workstation management
import XMonad.Actions.CycleWS -- cycle through virtual desktops
import XMonad.Layout.NoBorders -- Full screen windows have no borders

-- Maximize and minimize windows
-- Doesn't seem to work anymore?? Deprecated maybe.
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
-- import Xmonad.Actions.Minimize
-- import Xmonad.Actions.Maximize

-- Jump to specific layouts for windows
import XMonad.Layout.LayoutCombinators
-- Grid layout
import XMonad.Layout.Grid
-- Three Columns Layout
import XMonad.Layout.ThreeColumns
-- IM layout
import XMonad.Layout.IM
import Data.Ratio ((%))
-- Talks to DBUS and tells me when a program wants my attention
import XMonad.Hooks.UrgencyHook

-- Stuff I stole from Isaac

-- For GridSelect
import XMonad.Actions.GridSelect
-- Window mover popup
import XMonad.Actions.WindowMenu
-- For custom layout names
import XMonad.Layout.Named
-- For the spiral layout
--import XMonad.Layout.Spiral
-- For stacked cycling layout
import XMonad.Layout.Roledex
-- For being able to resize non-master windows
import XMonad.Layout.ResizableTile

-- For controlling windows
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Tabbed layout
import XMonad.Layout.Tabbed
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "rxvt-unicode"
-- myTerminal      = "xterm"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"

------------------------------------------------------------------------
-- xpConfig 
-- This configures the xmonad prompt look and feel
myXPConfig :: XPConfig
myXPConfig = defaultXPConfig { autoComplete = Just 500000
                             , position = Top
                             , promptBorderWidth = 0
                             , height = 20
                             , bgColor = "grey" 
                             , fgColor = "blue"
                             , font = "xft:DejaVuSansMono:size=10"
}

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.  Syntax:
-- xK_<KEY> for windows keys, xF86XK_<KEY> for special laptop
-- keys. modm for modmask.  shiftMask for shift. .|. for "this key and another
-- key." ctrlMask for control. Template:
-- ((key, otherkey), command)
-- 0 in the first key slot indicates "don't wait for a second key."
-- spawn "command" sends a command to the shell (mine is bash).
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "dmenu_run")

    -- launch gmrun
    --, ((modm,         xK_p     ), spawn "gmrun")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Shrink the focused area
    , ((modm,               xK_bracketleft ), sendMessage MirrorShrink)

    -- Expand the focused area
    , ((modm,               xK_bracketright ), sendMessage MirrorExpand)

    -- Push window back into tiling
    , ((modm,               xK_d     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm .|. controlMask, xK_q   ), spawn "xmonad --recompile; xmonad --restart")
    
    -- Screensaver/lock screen
    , ((0, xF86XK_ScreenSaver           ), spawn "xscreensaver-command -lock")

    -- Screenshots: this requires the scrot program for screenshots.
    -- Screenshot of a window  
    , ((controlMask, xK_Print ), spawn "sleep 0.2; scrot -s '%Y-%m-%d_$wx$h.png' -e 'mv $f ~/Pictures/screen_shots/'")
    -- Screenshot of the whole screen  
    , ((0, xK_Print ), spawn "scrot '%Y-%m-%d_$wx$h.png' -e 'mv $f ~/Pictures/screen_shots/'")

    -- Enable laptop volume keys. If you prefer some weird keybinding,
    -- you can do that too.
    , ((0, xF86XK_AudioRaiseVolume ), spawn "amixer sset Master 1+ && amixer -D pulse sset Master unmute")
    , ((0, xF86XK_AudioLowerVolume ), spawn "amixer sset Master 1- && amixer -D pulse sset Master unmute")
    , ((0, xF86XK_AudioMute        ), spawn "amixer -D pulse sset Master toggle")

    , ((0, xF86XK_MonBrightnessUp ),   spawn "xbacklight -inc 5")
    , ((0, xF86XK_MonBrightnessDown ), spawn "xbacklight -dec 5")

    -- Toggle touchpad
    , ((modm, xK_F8 ), spawn "~/myscripts/trackpad-toggle.sh")
    -- Toggle wallpaper
    , ((modm, xK_F7 ), spawn "~/myscripts/toggle_monitor.sh")
      
    -- Cycle left/right through workstations 
    -- mod-RightArrow move to workspace to the right  
    -- mod-LeftArrow move to workspace to the left  
    -- mod-Shift-RightArrow move focussed window to workspace to the right  
    -- mod-Shift-LeftArrow move focussed window to workspace to the left    
    , ((modm, xK_Right               ), nextWS)
    , ((modm, xK_Left                ), prevWS)
    , ((modm .|. shiftMask, xK_Right ), shiftToNext >> nextWS)
    , ((modm .|. shiftMask, xK_Left  ), shiftToPrev >> prevWS)
    , ((modm .|. shiftMask, xK_Tab   ), moveTo Next EmptyWS)

-- mod-o move to workspace to the right
    -- mod-i move to workspace to the left
    -- mod-Shift-O move focussed window to workspace to the right
    -- mod-Shift-I move focussed window to the workspace to the left
    , ((modm, xK_o               ), nextWS)
    , ((modm, xK_i               ), prevWS)
    , ((modm .|. shiftMask, xK_o ), shiftToNext >> nextWS)
    , ((modm .|. shiftMask, xK_i ), shiftToPrev >> prevWS)
    
    -- Toggle to previous workspace (view only)  
    , ((modm, xK_Down                ), toggleWS)
    , ((modm, xK_u                   ), toggleWS)

    -- Toggle to a window that wants your attention
    , ((modm, xK_Up                  ), focusUrgent)
    , ((modm .|. shiftMask, xK_u     ), focusUrgent)

    -- Open graphical emacs client
    , ((modm .|. shiftMask, xK_t ), spawn "emacsclient -c -n")

    -- Open emacs terminal
    , ((modm .|. shiftMask, xK_a ), spawn "emacsclient -c -e '(jmm-goto-term)'")

    -- Call Firefox
    , ((modm .|. shiftMask, xK_f ), spawn "firefox")

    -- Call pidgin
    , ((modm .|. shiftMask, xK_p ), spawn "pidgin")

    -- Get a window list and go to a window you call
    , ((modm .|. shiftMask, xK_g     ), goToSelected defaultGSConfig)
    -- Get a window list and summon a window you call
    , ((modm .|. shiftMask, xK_b     ), bringSelected defaultGSConfig)
    -- Grid-select a workspace
    , ((modm, xK_e                   ), gridselectWorkspace defaultGSConfig (\ws -> W.greedyView ws . W.shift ws))

    -- Append a single line and date to a notes file
    , ((modm .|. shiftMask, xK_n     ), do
         spawn ("date>>"++"~/notes.txt")
         appendFilePrompt myXPConfig "~/notes.txt")
    
    -- Display the man page of something
    , ((modm, xK_F1), manPrompt myXPConfig)
    -- Displays a shell prompt
    , ((modm .|. shiftMask, xK_s    ), shellPrompt myXPConfig {autoComplete = Nothing })

    -- Summons a system monitor
    , ((modm, xK_F2), spawn "rxvt-unicode -e htop")

    -- Spawn a file manager
    , ((modm, xK_F3), spawn "thunar")

    -- Jumps to a new allpaper.
    , ((modm, xK_F11), spawn "python3 ~/myscripts/randomwallpaper.py")

    -- Jump to the fullscreen layout
    , ((modm, xK_f), sendMessage $ JumpToLayout "Full")

    -- Jump to grid layout
    , ((modm, xK_g), sendMessage $ JumpToLayout "Grid")

    -- Jump to cycle layout
    , ((modm, xK_c), sendMessage $ JumpToLayout "Cycle")

    -- Jump to tiled layout
    , ((modm, xK_t), sendMessage $ JumpToLayout "Tiled")

    -- Jump to three colummns
    , ((modm, xK_a), sendMessage $ JumpToLayout "ThreeColumns")

    -- tabbed controls
    , ((modm .|. controlMask, xK_h), sendMessage $ pullGroup L)
    , ((modm .|. controlMask, xK_l), sendMessage $ pullGroup R)
    , ((modm .|. controlMask, xK_k), sendMessage $ pullGroup U)
    , ((modm .|. controlMask, xK_j), sendMessage $ pullGroup D)
    
    , ((modm .|. controlMask, xK_m), withFocused (sendMessage . MergeAll))
    , ((modm .|. controlMask, xK_u), withFocused (sendMessage . UnMerge))
    
    , ((modm .|. controlMask, xK_period), onGroup W.focusUp')
    , ((modm .|. controlMask, xK_comma), onGroup W.focusDown')
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_z, xK_x, xK_v] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.

-- I have added the commands: 
-- avoidStruts, which prevents windows from overlapping with the status bar
-- smartBorders, which removes the border pixels on fullscreen.

    -- $ tiled ||| Mirror tiled ||| noBorders Full
     --     tiled   = Tall nmaster delta ratio
myLayout =  windowNavigation $ subTabbed
            $ avoidStruts $ smartBorders
            $ withIM (5%28) (Role "buddy_list")
            $ tiled ||| threecolumns  ||| cycle ||| grid ||| full 

  where
    -- Three columns 
    threecolumns = named "ThreeColumns" (minimize (maximize (ThreeColMid nmaster delta threecolratio)))

     -- default tiling algorithm partitions the screen into two panes
    tiled   = named "Tiled" (minimize (maximize (ResizableTall nmaster delta ratio [])))

    -- Full
    full = named "Full" (minimize Full)

    -- Grid
    grid = named "Grid" (minimize (maximize Grid))

    -- Roledex cycling algorithm
    cycle = named "Cycle" (minimize (maximize Roledex))

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- portion of the screen occupied by the master pane in threecolumn mode
    threecolratio = 1/3

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

    -- Spiral ratio
    sratio  = 6/7

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
-- The doFloat command is super useful It tells Xmonad to respect the
-- geometry of the window, and ignore it with respect to the
-- tiling. The issue is it is always above the other windows. Useful
-- for some things, I suppose, but I find it pretty annoying.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat -- make mplayers window float
    , className =? "Gimp"           --> doFloat -- Make gimp's windows float
    , resource  =? "desktop_window" --> doIgnore -- There is no desktop
    , resource  =? "kdesktop"       --> doIgnore  -- There is no dpesktop
    , className =? "Firefox" <&&> resource =? "Dialog" --> doFloat 
    , className =? "Terminator" --> doF W.swapDown >> doIgnore -- make terminator a background thing 
    , className =? "Pidgin" --> doShift "9" -- send Pidgin to WS 9
    , className =? "Evolution" --> doShift "8" -- send Thunderbird to WS 8
    ]
 
------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = docksEventHook -- mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = mempty --dynamicLog -- setWMName "LG3D"

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
-- Runs a bunch of programs we want.
-- myStartupHook = return () -- setWMName "LG3D" -- Trick Java into working correctly
myStartupHook :: X()
myStartupHook = do
  -- setWMName "LG3D" -- Tricks Java into working correctly
  spawn "~/.xmonad/autostart" -- runs my startup programs

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- main = xmonad defaults We also call xmobar here. We can't run the
-- loghook until xmobar is running, which is why we call it in main.
main = do
  xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc" --call xmobar
  xmonad $ withUrgencyHook NoUrgencyHook  defaults 
      {
        manageHook = manageDocks <+> myManageHook
      , logHook = dynamicLogWithPP xmobarPP -- make xmobar look nice
                      {
                        ppOutput = hPutStrLn xmproc
                      , ppTitle = xmobarColor "green" ""
                      , ppUrgent = xmobarColor "yellow" "red" . xmobarStrip
                      }
      } 
 


-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }
