-- The DBus-related code, and the xmonad log applet that it's intended to be
-- used with, were originally authored by Adam Wick.
-- (see http://uhsure.com/xmonad-log-applet.html for more details)

import qualified Data.Map as M
import Control.Monad(liftM)

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
import XMonad.Layout.Named(named)
import XMonad.Util.CustomKeys(customKeysFrom)
import XMonad.Actions.Promote(promote)
import XMonad.Actions.Plane(planeKeys,Lines(Lines),Limits(Finite))
import XMonad.Actions.UpdatePointer
    (updatePointer,PointerPosition(TowardsCentre))

import DBus(serviceDBus,pathDBus,interfaceDBus,Error(Error))
import DBus.Message(newSignal,newMethodCall,addArgs,Message,Arg(..))
import DBus.Connection(busGet,busRequestName,send,Connection,BusType(Session))

import Graphics.Rendering.Pango.Enums(Weight(..))
import Graphics.Rendering.Pango.Markup(markSpan,SpanAttribute(..))
import Graphics.Rendering.Pango.Layout(escapeMarkup)

myTerminal = "~/bin/urxvtc-wrapper.sh"

myManageHook = composeAll
    [ isFullscreen --> doFullFloat
    , liftM not isDialog --> insertPosition End Newer
    , className =? "Pidgin" --> doShift "Comm"
    ]

-- logHook & Pretty Printer --

myLogHook :: Connection -> X ()
myLogHook dbus = do
    dynamicLogWithPP (myPrettyPrinter dbus)
    updatePointer (TowardsCentre 0.6 0.6)

myPrettyPrinter :: Connection -> PP
myPrettyPrinter dbus = defaultPP {
    ppOutput  = outputThroughDBus dbus
  , ppTitle   = escapeMarkup . shorten 80
  , ppCurrent = markSpan
        [ FontWeight WeightBold
        , FontForeground "green"
        ] . escapeMarkup
  , ppVisible = const ""
  , ppHidden  = const ""
  , ppLayout  = markSpan
        [ FontWeight WeightBold
        , FontForeground "white"
        ] . escapeMarkup
  , ppUrgent  = const ""
  }

outputThroughDBus :: Connection -> String -> IO ()
outputThroughDBus dbus str = do
    msg <- newSignal "/org/xmonad/Log" "org.xmonad.Log" "Update"
    addArgs msg [String str]
    send dbus msg 0 --TODO: Exception Handling?
    return ()

-- Workspaces & Layouts --

myWorkspaces = ["Web","Comm","Code","Misc"]

myCommLayout = reflectHoriz $
    withIM (0.25) (ClassName "Pidgin" `And` Role "buddy_list")
    (Mirror $ Tall 1 0.01 0.5)

myLayouts = onWorkspace "Comm" (named "Comm" myCommLayout) $
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
    dbus <- busGet Session --TODO: Exception Handling?
    busRequestName dbus "org.xmonad.Log" [] --TODO: Exception Handling?
    xmonad $ gnomeConfig
        { terminal = myTerminal
        , workspaces = myWorkspaces
        , modMask = myModMask
        , keys = customKeysFrom gnomeConfig myRemoveKeys myAdditionalKeys
        , layoutHook = myLayoutHook
        , manageHook = myManageHook <+> manageHook gnomeConfig
        , logHook = myLogHook dbus >> logHook gnomeConfig
    }
