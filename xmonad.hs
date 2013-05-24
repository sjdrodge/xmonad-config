import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Control.Monad ( liftM2 )
import Data.Maybe ( fromJust )

import XMonad
import XMonad.Config.Desktop ( desktopLayoutModifiers )
import XMonad.Config.Kde ( kde4Config )
import XMonad.Hooks.ManageHelpers ( isFullscreen )
import XMonad.Hooks.UrgencyHook ( focusUrgent, withUrgencyHook, NoUrgencyHook ( .. ) )
import XMonad.Hooks.FadeInactive ( isUnfocused, fadeOutLogHook )
import XMonad.Layout.NoBorders ( lessBorders, SetsAmbiguous ( hiddens ) )
import XMonad.Layout.Reflect ( reflectHoriz )
import XMonad.Layout.IM ( withIM, Property ( ClassName, And, Title ) )
import XMonad.Layout.PerWorkspace ( onWorkspace, onWorkspaces )
import XMonad.Layout.Named ( named, nameTail )
import XMonad.Layout.Spacing ( spacing )
import XMonad.Layout.FixedColumn ( FixedColumn ( .. ) )
import XMonad.Layout.Grid ( Grid ( Grid ) )
import XMonad.Layout.Fullscreen ( fullscreenEventHook, fullscreenManageHook, fullscreenFull )
import XMonad.Util.CustomKeys ( customKeysFrom )
import XMonad.Actions.Promote ( promote )
import XMonad.Actions.Plane ( planeKeys, Lines ( Lines ), Limits ( Finite ) )
import XMonad.Actions.UpdatePointer ( updatePointer, PointerPosition ( TowardsCentre ) )

-- Terminal --
myTerminal = "~/bin/urxvtcd"

-- ManageHooks --
myManageHook = composeAll $ []
    ++ [ fullscreenManageHook ]
    ++ [ className =? name --> doFloat  | name <- floats ]
    ++ [ resource  =? name --> doFloat  | name <- floats ]
    ++ [ className =? name --> doIgnore | name <- ignores ]
    ++ [ resource  =? name --> doIgnore | name <- ignores ]
    ++ [ className =? name --> doShift workspace | ( name, workspace ) <- shifts ]
    ++ [ resource  =? name --> doShift workspace | ( name, workspace ) <- shifts ]
    where
        floats  = [ "Wine", "Steam", "plasma-desktop" ]
        ignores = [ "Synapse" ]
        shifts  = [ ( "crx_nckgahadagoaajjgafhacjanaoiihapd", "Comm" ) ]

-- LogHook --

fadeWhiteList = [ className =? "URxvt"
                , className =? "Pidgin"
                ]

fadeRules :: Query Rational
fadeRules = do
    whitelisted <- foldl1 ( liftM2 (||) ) fadeWhiteList
    fullscreen <- isFullscreen
    unfocused <- isUnfocused
    return $ case () of _ | not whitelisted || fullscreen -> 1
                          | not unfocused -> 0.95
                          | otherwise -> 0.9

myLogHook = do
    updatePointer (TowardsCentre 0.6 0.6)
    fadeOutLogHook fadeRules

-- Workspaces & Layouts --
myWorkspaces = [ "Web", "Comm"
               , "Web2", "Code"
               , "Five", "Six"]

myWebLayouts = Tall { tallNMaster = 1
                    , tallRatioIncrement = 0.01
                    , tallRatio = 0.7
                    }

myCommLayouts = named "Comm" $ reflectHoriz $
    withIM ( 0.25 ) ( ClassName "Google-chrome" `And` Title "Hangouts" )
           ( Mirror $ myWebLayouts { tallRatio = 0.5 } )

myCodeLayouts = named "Code" $ reflectHoriz ( FixedColumn 1 1 80 6 )

myMiscLayouts = myWebLayouts ||| Grid

myLayouts = nameTail $ nameTail $ spacing 3 $
    ( onWorkspaces [ "Web", "Web2" ] myWebLayouts
    $ onWorkspace "Comm" myCommLayouts
    $ onWorkspace "Code" myCodeLayouts
    $ myMiscLayouts
    ) ||| Full

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
    [ ( ( myModMask, xK_Return ), promote )
    , ( ( myModMask, xK_BackSpace ), focusUrgent )
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
        , logHook = myLogHook >> logHook kde4Config
        , handleEventHook = fullscreenEventHook <+> handleEventHook kde4Config
        }
