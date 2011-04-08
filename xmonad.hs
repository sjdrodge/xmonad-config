-- The DBus-related code, and the xmonad log applet that it's intended to be
-- used with, were originally authored by Adam Wick.
-- (see http://uhsure.com/xmonad-log-applet.html for more details)

-- DBus imports --

import Control.OldException(catchDyn,try)
import DBus
import DBus.Connection
import DBus.Message
import XMonad.Hooks.DynamicLog(wrap,shorten,defaultPP,dynamicLogWithPP,PP(..))

-- Regular imports --

import qualified Data.Map as M
import Control.Monad(liftM)

import XMonad
import XMonad.Config.Desktop(desktopLayoutModifiers)
import XMonad.Config.Gnome(gnomeConfig)
import XMonad.Hooks.ManageHelpers(isDialog,isFullscreen,doFullFloat)
import XMonad.Hooks.InsertPosition(insertPosition,Focus(Newer),Position(End))
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
    , className =? "Pidgin" --> doShift "comm"
    ]

myLogHook :: Connection -> X ()
myLogHook dbus = do
    dynamicLogWithPP (myPrettyPrinter dbus)
    updatePointer (TowardsCentre 0.6 0.6)

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

main = do
    dbus <- busGet Session
    getWellKnownName dbus
    xmonad $ gnomeConfig
        { terminal = myTerminal
        , workspaces = myWorkspaces
        , modMask = myModMask
        , keys = customKeysFrom gnomeConfig myRemoveKeys myAdditionalKeys
        , layoutHook = myLayoutHook
        , manageHook = myManageHook <+> manageHook gnomeConfig
        , logHook = myLogHook dbus >> logHook gnomeConfig
    }

-- DBus stuff --

myPrettyPrinter :: Connection -> PP
myPrettyPrinter dbus = defaultPP {
    ppOutput  = outputThroughDBus dbus
  , ppTitle   = shorten 50 . pangoSanitize
  , ppCurrent = pangoColor "green" . wrap "[" "]" . pangoSanitize
  , ppVisible = wrap "(" ")" . pangoSanitize
  , ppHidden  = wrap " " " "
  , ppLayout  = pangoColor "white" . pangoSanitize
  , ppUrgent  = pangoColor "red"
  }

getWellKnownName :: Connection -> IO ()
getWellKnownName dbus = tryGetName `catchDyn`
    (\ (DBus.Error _ _) -> getWellKnownName dbus)
    where
        tryGetName = do
            namereq <-
                newMethodCall serviceDBus pathDBus interfaceDBus "RequestName"
            addArgs namereq [String "org.xmonad.Log", Word32 5]
            sendWithReplyAndBlock dbus namereq 0
            return ()

outputThroughDBus :: Connection -> String -> IO ()
outputThroughDBus dbus str = do
    msg <- newSignal "/org/xmonad/Log" "org.xmonad.Log" "Update"
    addArgs msg [String str]
    send dbus msg 0 `catchDyn` (\ (DBus.Error _ _ ) -> return 0)
    return ()

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
    where
        left  = "<span font_weight=\"bold\" foreground=\"" ++ fg ++ "\">"
        right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
    where
        sanitize '>'  acc = "&gt;" ++ acc
        sanitize '<'  acc = "&lt;" ++ acc
        sanitize '\"' acc = "&quot;" ++ acc
        sanitize '&'  acc = "&amp;" ++ acc
        sanitize x    acc = x:acc
