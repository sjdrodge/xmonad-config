import XMonad
import XMonad.Config.Gnome
import XMonad.Util.EZConfig

main = xmonad $ gnomeConfig
	{ terminal = "xterm"
	, modMask = mod4Mask
	}
