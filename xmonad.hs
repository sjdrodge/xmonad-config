import XMonad
import XMonad.Config.Desktop(desktopLayoutModifiers)
import XMonad.Config.Gnome(gnomeConfig)
import XMonad.Util.EZConfig(additionalKeysP,removeKeysP)
import XMonad.Hooks.ManageHelpers(isFullscreen,doFullFloat)
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Actions.CycleWS(prevWS,nextWS,shiftToPrev,shiftToNext)
import XMonad.Actions.UpdatePointer

myLayoutHook =
	smartBorders $ desktopLayoutModifiers $ 
	(Tall 1 0.01 0.7) ||| Full

main = xmonad $ gnomeConfig
	{ terminal = "xterm"
	, modMask = mod4Mask
	, layoutHook = myLayoutHook
	, manageHook = composeAll
		[ manageHook gnomeConfig
		, isFullscreen --> doFullFloat
		]
	, logHook = do
		updatePointer (TowardsCentre 0.6 0.6)
		logHook gnomeConfig
	}
	`additionalKeysP`
	[ ("M-<Left>", prevWS)
	, ("M-<Right>", nextWS)
	, ("M-S-<Left>", shiftToPrev)
	, ("M-S-<Right>", shiftToNext)
	]
	`removeKeysP` ["M-S-q"]
