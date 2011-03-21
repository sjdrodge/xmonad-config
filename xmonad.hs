import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.ManageHelpers(isFullscreen,doFullFloat)

main = xmonad $ gnomeConfig
	{ terminal = "xterm"
	, modMask = mod4Mask
	, manageHook = composeAll
		[ manageHook gnomeConfig
		, isFullscreen --> doFullFloat
		]
	}
