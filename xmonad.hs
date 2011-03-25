import XMonad
import XMonad.Config.Desktop(desktopLayoutModifiers)
import XMonad.Config.Gnome(gnomeConfig)
import XMonad.Util.EZConfig(additionalKeys,removeKeys)
import XMonad.Hooks.ManageHelpers(isFullscreen,doFullFloat)
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Actions.CycleWS(prevWS,nextWS,shiftToPrev,shiftToNext)
import XMonad.Actions.Promote(promote)
import XMonad.Actions.UpdatePointer

myTerminal = "xterm"

myLayouts = (Tall 1 0.01 0.7) ||| Full

myLayoutHook = smartBorders $ desktopLayoutModifiers myLayouts

myManageHook = isFullscreen --> doFullFloat

myLogHook = updatePointer (TowardsCentre 0.6 0.6)

-- Keybindings --

myModMask = mod4Mask

myAdditionalKeys =
	[ ((myModMask, xK_Return), promote)
	, ((myModMask, xK_Left), prevWS)
	, ((myModMask, xK_Right), nextWS)
	, ((myModMask .|. shiftMask, xK_Left), shiftToPrev)
	, ((myModMask .|. shiftMask, xK_Right), shiftToNext)
	, ((myModMask .|. mod1Mask, xK_Left), shiftToPrev >> prevWS)
	, ((myModMask .|. mod1Mask, xK_Right), shiftToNext >> nextWS)
	]

myRemoveKeys =
	[ (myModMask .|. shiftMask, xK_q)
	]

-- End Keybindings --

main = xmonad $ gnomeConfig
	{ terminal = myTerminal
	, modMask = myModMask
	, layoutHook = myLayoutHook
	, manageHook = myManageHook <+> manageHook gnomeConfig
	, logHook = myLogHook >> logHook gnomeConfig
	}
	`additionalKeys` myAdditionalKeys
	`removeKeys` myRemoveKeys
