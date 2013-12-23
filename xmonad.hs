import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Maybe ( fromJust )

import XMonad
import XMonad.Actions.Plane
import XMonad.Actions.Promote ( promote )
import XMonad.Config.Desktop ( desktopLayoutModifiers )
import XMonad.Config.Kde ( kde4Config )
import XMonad.Hooks.ManageHelpers ( transience' )
import XMonad.Hooks.UrgencyHook ( focusUrgent, withUrgencyHook, NoUrgencyHook ( .. ) )
import XMonad.Layout.Accordion
import XMonad.Layout.ComboP
import XMonad.Layout.FixedColumn ( FixedColumn ( .. ) )
import XMonad.Layout.Fullscreen ( fullscreenEventHook, fullscreenManageHook, fullscreenFull )
import XMonad.Layout.Grid ( Grid ( Grid ) )
import XMonad.Layout.IM ( withIM )
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Named ( named, nameTail )
import XMonad.Layout.NoBorders ( lessBorders, SetsAmbiguous ( hiddens ) )
import XMonad.Layout.PerWorkspace ( onWorkspace, onWorkspaces )
import XMonad.Layout.Reflect ( reflectHoriz, reflectVert )
import XMonad.Layout.Spacing ( spacing )
import XMonad.Layout.TwoPane
import XMonad.Util.CustomKeys ( customKeysFrom )

-- Terminal --
myTerminal = "konsole"

-- ManageHooks --
myManageHook = composeAll $ []
    ++ [ transience' ]
    ++ [ fullscreenManageHook ]
    ++ [ classOrResource name --> doFloat           | name                <- floats ]
    ++ [ classOrResource name --> doIgnore          | name                <- ignores ]
    ++ [ classOrResource name --> doShift workspace | ( name, workspace ) <- shifts ]
    where
        classOrResource x = className =? x <||> resource =? x
        floats  = [ "Wine", "Steam", "plasma-desktop" ]
        ignores = [ "Synapse" ]
        shifts  = [ ( "crx_nckgahadagoaajjgafhacjanaoiihapd", "Comm" ) ]

-- Workspaces & Layouts --
myWorkspaces = [ "Web", "Comm"
               , "Web2", "Code"
               , "Five", "Six"]

myWebLayouts = Tall { tallNMaster = 1
                    , tallRatioIncrement = 0.01
                    , tallRatio = 0.6
                    }

myCommLayouts = named "Comm"
    $ reflectHoriz $ withIM ( 0.20 ) ( ClassName "Google-chrome" `And` Title "Hangouts" )
    $ combineTwoP ( reflectVert $ Mirror $ TwoPane 0.01 0.45) (Mirror Accordion) Grid ( Resource "crx_nckgahadagoaajjgafhacjanaoiihapd" )
--    ( Mirror $ myWebLayouts { tallRatio = 0.55 } )

myCodeLayouts = named "Code" $ reflectHoriz ( FixedColumn 1 1 80 6 )

myMiscLayouts = myWebLayouts ||| Grid

myLayouts = mkToggle (single FULL) $ nameTail $ nameTail $ spacing 3 $
    ( onWorkspaces [ "Web", "Web2" ] myWebLayouts
    $ onWorkspace "Comm" myCommLayouts
    $ onWorkspace "Code" myCodeLayouts
    $ myMiscLayouts
    )

data MyAmbiguity = MyAmbiguity deriving ( Read, Show )

instance SetsAmbiguous MyAmbiguity where
    hiddens _ wset _ wrs = map fst $ filter windowIsFullScreen wrs
        where screens = ( W.current wset ) : ( W.visible wset )
              rectFromScreen = screenRect . W.screenDetail
              windowsFromScreen = W.integrate' . W.stack . W.workspace
              windowtoscreen = concatMap ( \s -> [ ( w, rectFromScreen s ) | w <- windowsFromScreen s ] ) screens
              windowIsFullScreen ( win, rect ) = ( fromJust $ lookup win windowtoscreen ) == rect

myLayoutHook = lessBorders MyAmbiguity $ fullscreenFull $ desktopLayoutModifiers myLayouts

-- Keybindings --
myModMask = mod4Mask

myAdditionalKeys _ =
    [ ( ( myModMask               , xK_Return    ), promote )
    , ( ( myModMask               , xK_BackSpace ), focusUrgent )
    , ( ( myModMask               , xK_s         ), sendMessage $ SwapWindow )
    , ( ( myModMask               , xK_f         ), sendMessage $ Toggle FULL )
    , ( ( myModMask               , xK_n         ), planeMove ( Lines 3 ) Finite ToRight )
    , ( ( myModMask .|. shiftMask , xK_n         ), planeShift ( Lines 3 ) Finite ToRight )
    , ( ( myModMask               , xK_p         ), planeMove ( Lines 3 ) Finite ToLeft )
    , ( ( myModMask .|. shiftMask , xK_p         ), planeShift ( Lines 3 ) Finite ToLeft )
    ] ++ M.assocs ( planeKeys myModMask ( Lines 3 ) Finite )

myRemoveKeys _ =
    [ ( myModMask .|. shiftMask, xK_q ) ]

-- Apply settings --
main = do
    xmonad $ withUrgencyHook NoUrgencyHook $ kde4Config
        { terminal = myTerminal
        , workspaces = myWorkspaces
        , modMask = myModMask
        , keys = customKeysFrom kde4Config myRemoveKeys myAdditionalKeys
        , layoutHook = myLayoutHook
        , manageHook = myManageHook <+> manageHook kde4Config
        , handleEventHook = fullscreenEventHook <+> handleEventHook kde4Config
        }
