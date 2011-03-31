import qualified Data.Map as M

import XMonad
import XMonad.Config.Desktop(desktopLayoutModifiers)
import XMonad.Config.Gnome(gnomeConfig)
import XMonad.Hooks.ManageHelpers(isFullscreen,doFullFloat)
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Actions.Promote(promote)
import XMonad.Util.CustomKeys(customKeysFrom)
import XMonad.Actions.Plane(planeKeys,Lines(Lines),Limits(Finite))
import XMonad.Actions.UpdatePointer
    (updatePointer,PointerPosition(TowardsCentre))

myTerminal = "/home/sdrodge/bin/urxvtc-wrapper.sh"

myWorkspaces = ["1:web","2:comm","3:code","4:misc"]

myLayouts = (Tall 1 0.01 0.7) ||| Full

myLayoutHook = smartBorders $ desktopLayoutModifiers myLayouts

myManageHook = isFullscreen --> doFullFloat

myLogHook = updatePointer (TowardsCentre 0.6 0.6)

-- Keybindings --

myModMask = mod4Mask

myAdditionalKeys _ =
    [ ((myModMask, xK_Return), promote) ]
    ++ M.assocs (planeKeys myModMask (Lines 2) Finite)

myRemoveKeys _ =
    [ (myModMask .|. shiftMask, xK_q) ]

-- End Keybindings --

main = xmonad $ gnomeConfig
    { terminal = myTerminal
    , workspaces = myWorkspaces
    , modMask = myModMask
    , keys = customKeysFrom gnomeConfig myRemoveKeys myAdditionalKeys
    , layoutHook = myLayoutHook
    , manageHook = myManageHook <+> manageHook gnomeConfig
    , logHook = myLogHook >> logHook gnomeConfig
    }
