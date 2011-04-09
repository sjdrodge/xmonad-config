import qualified Data.Map as M
import Control.Monad(liftM)

import XMonad
import XMonad.Config.Desktop(desktopLayoutModifiers)
import XMonad.Config.Gnome(gnomeConfig)
import XMonad.Hooks.ManageHelpers(isDialog,isFullscreen,doFullFloat)
import XMonad.Hooks.InsertPosition(insertPosition,Focus(Newer),Position(End))
import XMonad.Hooks.Place(placeHook,inBounds,smart,Placement)
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Layout.Reflect(reflectHoriz)
import XMonad.Layout.IM(withIM,Property(ClassName,And,Role))
import XMonad.Layout.PerWorkspace(onWorkspace)
import XMonad.Util.CustomKeys(customKeysFrom)
import XMonad.Actions.Promote(promote)
import XMonad.Actions.Plane(planeKeys,Lines(Lines),Limits(Finite))
import XMonad.Actions.UpdatePointer
    (updatePointer,PointerPosition(TowardsCentre))

myTerminal = "~/bin/urxvtc-wrapper.sh"

myManageHook = composeAll
    [ isFullscreen --> doFullFloat
    , liftM not isDialog --> insertPosition End Newer
    , placeHook $ inBounds $ smart (0.5,0.5)
    , className =? "Pidgin" --> doShift "comm"
    ]

myLogHook = updatePointer (TowardsCentre 0.6 0.6)

-- Workspaces & Layouts --

myWorkspaces = ["web","comm","code","misc"]

myCommLayout = reflectHoriz $
    withIM (0.25) (ClassName "Pidgin" `And` Role "buddy_list")
    (Mirror $ Tall 1 0.01 0.5)

myLayouts = onWorkspace "comm" myCommLayout $
            (Tall 1 0.01 0.7) ||| Full

myLayoutHook = smartBorders $ desktopLayoutModifiers myLayouts

-- Keybindings --

myModMask = mod4Mask

myAdditionalKeys _ =
    [ ((myModMask, xK_Return), promote) ]
    ++ M.assocs (planeKeys myModMask (Lines 2) Finite)

myRemoveKeys _ =
    [ (myModMask .|. shiftMask, xK_q) ]

-- Apply settings --

main = xmonad $ gnomeConfig
    { terminal = myTerminal
    , workspaces = myWorkspaces
    , modMask = myModMask
    , keys = customKeysFrom gnomeConfig myRemoveKeys myAdditionalKeys
    , layoutHook = myLayoutHook
    , manageHook = myManageHook <+> manageHook gnomeConfig
    , logHook = myLogHook >> logHook gnomeConfig
    }
