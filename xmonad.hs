import XMonad
import XMonad.Config.Desktop(desktopLayoutModifiers)
import XMonad.Config.Gnome(gnomeConfig)
import XMonad.Util.EZConfig(additionalKeysP,removeKeysP)
import XMonad.Hooks.ManageHelpers(isFullscreen,doFullFloat)
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Actions.CycleWS(prevWS,nextWS,shiftToPrev,shiftToNext)
import XMonad.Actions.UpdatePointer

myTerminal = "xterm"
myModMask = mod4Mask

myLayouts = (Tall 1 0.01 0.7) ||| Full

myLayoutHook = smartBorders $ desktopLayoutModifiers myLayouts

myManageHook = isFullscreen --> doFullFloat

myLogHook = updatePointer (TowardsCentre 0.6 0.6)

myAdditionalKeysP =
	[ ("M-<Left>", prevWS)
        , ("M-<Right>", nextWS)
        , ("M-S-<Left>", shiftToPrev)
        , ("M-S-<Right>", shiftToNext)
	, ("M-M1-<Left>" , shiftToPrev >> prevWS)
	, ("M-M1-<Right>", shiftToNext >> nextWS)
        ]

myRemoveKeysP = ["M-S-q"]


main = xmonad $ gnomeConfig
	{ terminal = myTerminal
	, modMask = myModMask
	, layoutHook = myLayoutHook
	, manageHook = myManageHook <+> manageHook gnomeConfig
	, logHook = myLogHook >> logHook gnomeConfig
	}
	`additionalKeysP` myAdditionalKeysP
	`removeKeysP` myRemoveKeysP
