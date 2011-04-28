-- This configuration is intended to be used with the XMonad Statusbar Applet
-- available at <https://github.com/tehminkeh/xmonad-statusbar-applet>.
-- It also requires the following Haskell packages:
-- dbus-core: <http://hackage.haskell.org/package/dbus-core>
-- dbus-client: <http://hackage.haskell.org/package/dbus-client>
-- pango: <http://hackage.haskell.org/package/pango>

import qualified Data.Map as M(assocs)
import Data.String(fromString)
import qualified Data.Text.Lazy.Encoding as TL(decodeUtf8)
import Control.Monad(liftM)

import XMonad
import XMonad.Config.Desktop(desktopLayoutModifiers)
import XMonad.Config.Gnome(gnomeConfig)
import XMonad.Hooks.ManageHelpers(isDialog,isFullscreen,doFullFloat)
import XMonad.Hooks.InsertPosition(insertPosition,Focus(Newer),Position(End))
import XMonad.Hooks.DynamicLog(defaultPP,dynamicLogWithPP,PP(..))
import XMonad.Hooks.UrgencyHook(focusUrgent,withUrgencyHook,NoUrgencyHook(..))
import XMonad.Hooks.FadeInactive(isUnfocused,fadeOutLogHook)
import XMonad.Layout.NoBorders(lessBorders,Ambiguity(OnlyFloat))
import XMonad.Layout.Reflect(reflectHoriz)
import XMonad.Layout.IM(withIM,Property(ClassName,And,Role))
import XMonad.Layout.PerWorkspace(onWorkspace)
import XMonad.Layout.Named(named,nameTail)
import XMonad.Layout.Spacing(spacing)
import XMonad.Layout.FixedColumn(FixedColumn(..))
import XMonad.Layout.Grid(Grid(Grid))
import XMonad.Util.CustomKeys(customKeysFrom)
import XMonad.Actions.Promote(promote)
import XMonad.Actions.Plane(planeKeys,Lines(Lines),Limits(Finite))
import XMonad.Actions.UpdatePointer
    (updatePointer,PointerPosition(TowardsCentre))

import DBus.Types(toVariant)
import DBus.Bus(getSessionBus)
import DBus.Message(Signal(..))
import DBus.Client(send_,newClient,runDBus,Client,DBus)

import Graphics.Rendering.Pango.Enums(Weight(..))
import Graphics.Rendering.Pango.Markup(markSpan,SpanAttribute(..))
import Graphics.Rendering.Pango.Layout(escapeMarkup)

-- Terminal --
myTerminal = "urxvtcd"

-- ManageHooks --
myManageHook = composeAll
    [ isFullscreen --> doFullFloat
    , liftM not isDialog --> insertPosition End Newer
    , className =? "Pidgin" --> doShift "Comm"
    , className =? "Devhelp" --> doShift "Code"
    , className =? "Gitg" --> doShift "Code"
    ]

-- LogHook --
sendUpdateSignal :: String -> DBus ()
sendUpdateSignal output = send_ Signal
    { signalPath = fromString "/org/xmonad/Log"
    , signalMember = fromString "Update"
    , signalInterface = fromString "org.xmonad.Log"
    , signalDestination = Nothing
    , signalBody = [toVariant (TL.decodeUtf8 (fromString output))]
    }

myPrettyPrinter :: Client -> PP
myPrettyPrinter client = defaultPP
    { ppOutput  = runDBus client . sendUpdateSignal
    , ppTitle   = escapeMarkup
    , ppCurrent = markSpan
        [ FontWeight WeightBold
        , FontForeground "green"
        ] . escapeMarkup
    , ppVisible = const ""
    , ppHidden  = escapeMarkup
    , ppLayout  = markSpan
        [ FontWeight WeightBold
        , FontForeground "white"
        ] . escapeMarkup
    , ppUrgent  = markSpan
        [ FontWeight WeightBold
        , FontForeground "red"
        ]
    }

fadeRules :: Query Rational
fadeRules = do
    b <- isUnfocused
    return $ if b then 0.8 else 0.85

myLogHook :: Client -> X ()
myLogHook client = do
    dynamicLogWithPP (myPrettyPrinter client)
    updatePointer (TowardsCentre 0.6 0.6)
    fadeOutLogHook fadeRules

-- Workspaces & Layouts --
myWorkspaces = [ "Web", "Comm", "Code"
               , "Four", "Five", "Six"]

myWebLayouts = Tall 1 0.01 0.7

myCommLayouts = named "Comm" $ reflectHoriz $
    withIM (0.25) (ClassName "Pidgin" `And` Role "buddy_list")
           (Mirror $ Tall 1 0.01 0.5)

myCodeLayouts = named "Code" $ reflectHoriz (FixedColumn 1 1 80 6)

myMiscLayouts = myWebLayouts ||| Grid

myLayouts = nameTail $ nameTail $ spacing 3 $
    ( onWorkspace "Web" myWebLayouts
    $ onWorkspace "Comm" myCommLayouts
    $ onWorkspace "Code" myCodeLayouts
    $ myMiscLayouts
    ) ||| Full

myLayoutHook = lessBorders OnlyFloat $ desktopLayoutModifiers myLayouts

-- Keybindings --
myModMask = mod4Mask

myAdditionalKeys _ =
    [ ((myModMask, xK_Return), promote)
    , ((myModMask, xK_BackSpace), focusUrgent)
    ] ++ M.assocs (planeKeys myModMask (Lines 2) Finite)

myRemoveKeys _ =
    [ (myModMask .|. shiftMask, xK_q) ]

-- Apply settings --
main = do
    dbusClient <- newClient =<< getSessionBus
    xmonad $ withUrgencyHook NoUrgencyHook $ gnomeConfig
        { terminal = myTerminal
        , workspaces = myWorkspaces
        , modMask = myModMask
        , keys = customKeysFrom gnomeConfig myRemoveKeys myAdditionalKeys
        , layoutHook = myLayoutHook
        , manageHook = myManageHook <+> manageHook gnomeConfig
        , logHook = myLogHook dbusClient >> logHook gnomeConfig
        }
