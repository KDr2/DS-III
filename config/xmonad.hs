-- file ~/.xmonad/xmonad.hs
import XMonad

main = xmonad defaultConfig
       { modMask = mod4Mask -- Use Super instead of Alt
         -- more changes
       }
