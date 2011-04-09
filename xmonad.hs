-- The DBus-related code, and the xmonad log applet that it's intended to be
-- used with, were originally authored by Adam Wick.
-- (see http://uhsure.com/xmonad-log-applet.html for more details)

import qualified Data.Map as M
import Control.Monad(liftM)
import Control.OldException(catchDyn)

import XMonad
import XMonad.Config.Desktop(desktopLayoutModifiers)
import XMonad.Config.Gnome(gnomeConfig)
import XMonad.Hooks.ManageHelpers(isDialog,isFullscreen,doFullFloat)
import XMonad.Hooks.InsertPosition(insertPosition,Focus(Newer),Position(End))
import XMonad.Hooks.DynamicLog(wrap,shorten,defaultPP,dynamicLogWithPP,PP(..))
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Layout.Reflect(reflectHoriz)
import XMonad.Layout.IM(withIM,Property(ClassName,And,Role))
import XMonad.Layout.PerWorkspace(onWorkspace)
import XMonad.Util.CustomKeys(customKeysFrom)
import XMonad.Actions.Promote(promote)
import XMonad.Actions.Plane(planeKeys,Lines(Lines),Limits(Finite))
import XMonad.Actions.UpdatePointer
    (updatePointer,PointerPosition(TowardsCentre))

import DBus(serviceDBus,pathDBus,interfaceDBus,Error(Error))
import DBus.Message(newSignal,newMethodCall,addArgs,Message,Arg(..))
import DBus.Connection
    (busGet,send,sendWithReplyAndBlock,Connection,BusType(Session))


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
    getWellKnownName dbus "org.xmonad.Log"
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
  , ppTitle   = shorten 80 . pangoSanitize
  , ppCurrent = pangoColor "green" . wrap "[" "]" . pangoSanitize
  , ppVisible = const ""
  , ppHidden  = const ""
  , ppLayout  = pangoColor "white" . pangoSanitize
  , ppUrgent  = pangoColor "red" -- is currently useless due to ppHidden
  }


-- see: DBus.Connection.busRequestName as a replacement for this
-- TODO: get rid of catchDyn
getWellKnownName :: Connection -> String -> IO ()
getWellKnownName dbus name = tryGetName `catchDyn`
    (\ (DBus.Error _ _) -> getWellKnownName dbus name)
    where
        tryGetName = do
            namereq <-
                newMethodCall serviceDBus pathDBus interfaceDBus "RequestName"
            addArgs namereq [String name, Word32 5]
            sendWithReplyAndBlock dbus namereq 0
            return ()

-- TODO: get rid of catchDyn
outputThroughDBus :: Connection -> String -> IO ()
outputThroughDBus dbus str = do
    msg <- newSignal "/org/xmonad/Log" "org.xmonad.Log" "Update"
    addArgs msg [String str]
    send dbus msg 0 `catchDyn` (\ (DBus.Error _ _ ) -> return 0)
    return ()


-- Pango stuff --
-- see: Graphics.Rendering.Pango as a replacement
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
