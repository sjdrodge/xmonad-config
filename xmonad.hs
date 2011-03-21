import XMonad
import XMonad.Config.Gnome
import XMonad.Util.EZConfig
import XMonad.Hooks.ManageHelpers

main = xmonad $ gnomeConfig
	{ terminal = "xterm"
	, modMask = mod4Mask
	, manageHook = composeAll
		[ manageHook gnomeConfig
		, isFullscreen --> doFullFloat
		]
	}
