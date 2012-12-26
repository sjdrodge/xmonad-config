import qualified XMonad.StackSet as W
import qualified Data.Map as M(assocs)
import Control.Monad(liftM)

import XMonad
import XMonad.Config.Desktop(desktopLayoutModifiers)
import XMonad.Config.Gnome(gnomeConfig)
import XMonad.Hooks.ManageHelpers(isFullscreen)
import XMonad.Hooks.UrgencyHook(focusUrgent,withUrgencyHook,NoUrgencyHook(..))
import XMonad.Hooks.FadeInactive(isUnfocused,fadeOutLogHook)
import XMonad.Layout.NoBorders(lessBorders,SetsAmbiguous(hiddens))
import XMonad.Layout.Reflect(reflectHoriz)
import XMonad.Layout.IM(withIM,Property(ClassName,And,Role))
import XMonad.Layout.PerWorkspace(onWorkspace,onWorkspaces)
import XMonad.Layout.Named(named,nameTail)
import XMonad.Layout.Spacing(spacing)
import XMonad.Layout.FixedColumn(FixedColumn(..))
import XMonad.Layout.Grid(Grid(Grid))
import XMonad.Layout.Fullscreen(fullscreenEventHook,fullscreenManageHook,fullscreenFull)
import XMonad.Util.CustomKeys(customKeysFrom)
import XMonad.Actions.Promote(promote)
import XMonad.Actions.Plane(planeKeys,Lines(Lines),Limits(Finite))
import XMonad.Actions.UpdatePointer
    (updatePointer,PointerPosition(TowardsCentre))

-- Terminal --
myTerminal = "~/bin/urxvtcd"

-- ManageHooks --
myManageHook = composeAll
    [ fullscreenManageHook
    , className =? "Synapse" --> doIgnore
    , className =? "Wine" --> doFloat
    , className =? "Pidgin" --> doShift "Comm"
    ]

-- LogHook --
fadeRules :: Query Rational
fadeRules = do
    fullscreen <- isFullscreen
    focused <- liftM not isUnfocused
    return $ case () of _ | fullscreen -> 1
                          | focused -> 0.85
                          | otherwise -> 0.8

myLogHook = do
    updatePointer (TowardsCentre 0.6 0.6)
    fadeOutLogHook fadeRules

-- Workspaces & Layouts --
myWorkspaces = [ "Web", "Comm"
               , "Web2", "Code"
               , "Five", "Six"]

myWebLayouts = Tall 1 0.01 0.7

myCommLayouts = named "Comm" $ reflectHoriz $
    withIM (0.25) (ClassName "Pidgin" `And` Role "buddy_list")
           (Mirror $ Tall 1 0.01 0.5)

myCodeLayouts = named "Code" $ reflectHoriz (FixedColumn 1 1 80 6)

myMiscLayouts = myWebLayouts ||| Grid

myLayouts = nameTail $ nameTail $ spacing 3 $
    ( onWorkspaces ["Web","Web2"] myWebLayouts
    $ onWorkspace "Comm" myCommLayouts
    $ onWorkspace "Code" myCodeLayouts
    $ myMiscLayouts
    ) ||| Full

data MyAmbiguity = MyAmbiguity deriving (Read, Show)

instance SetsAmbiguous MyAmbiguity where
    hiddens _ wset _ wrs = map fst $ filter ((screenrect ==) . snd) wrs
        where screenrect = screenRect $ W.screenDetail $ W.current wset

myLayoutHook =  lessBorders MyAmbiguity $ fullscreenFull $ desktopLayoutModifiers myLayouts

-- Keybindings --
myModMask = mod4Mask

myAdditionalKeys _ =
    [ ((myModMask, xK_Return), promote)
    , ((myModMask, xK_BackSpace), focusUrgent)
    ] ++ M.assocs (planeKeys myModMask (Lines 3) Finite)

myRemoveKeys _ =
    [ (myModMask .|. shiftMask, xK_q) ]

-- Apply settings --
main = do
    xmonad $ withUrgencyHook NoUrgencyHook $ gnomeConfig
        { terminal = myTerminal
        , workspaces = myWorkspaces
        , modMask = myModMask
        , keys = customKeysFrom gnomeConfig myRemoveKeys myAdditionalKeys
        , layoutHook = myLayoutHook
        , manageHook = myManageHook <+> manageHook gnomeConfig
        , logHook = myLogHook >> logHook gnomeConfig
        , handleEventHook = fullscreenEventHook <+> handleEventHook gnomeConfig
        }
