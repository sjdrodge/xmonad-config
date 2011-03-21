import XMonad
import XMonad.Config.Gnome(gnomeConfig)
import XMonad.Hooks.ManageHelpers(isFullscreen,doFullFloat)
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Actions.UpdatePointer

main = xmonad $ gnomeConfig
	{ terminal = "xterm"
	, modMask = mod4Mask
	, layoutHook = smartBorders (layoutHook gnomeConfig)
	, manageHook = composeAll
		[ manageHook gnomeConfig
		, isFullscreen --> doFullFloat
		]
	, logHook = do
		updatePointer (TowardsCentre 0.6 0.6)
		logHook gnomeConfig
	}
