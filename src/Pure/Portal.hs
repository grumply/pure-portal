{-# LANGUAGE PatternSynonyms, MultiWayIf, DuplicateRecordFields, ViewPatterns, RecordWildCards, MultiParamTypeClasses, TypeFamilies, DeriveGeneric, DeriveAnyClass, OverloadedStrings, CPP #-}
module Pure.Portal where

import Pure hiding (Open,OnClose)
import Pure.Data.Cond
import Pure.Data.Prop
import Pure.Data.Txt as Txt (unwords)
import Pure.Proxy
import Pure.Transition

import Control.Arrow ((&&&))
import Control.Concurrent
import Control.Monad (when,void)
import Data.Coerce (coerce)
import Data.IORef
import Data.Foldable (for_)
import Data.List as List
import Data.Maybe (isJust,fromMaybe,isNothing)
import qualified Data.List as List
import Data.Traversable (for)
import GHC.Generics as G

import Data.Function as Tools ((&))

data Portal = Portal_
    { as                       :: Features -> [View] -> View
    , features                 :: Features
    , children                 :: [View]
    , portal                   :: (Features -> Features) -> View
    , mountNode                :: Maybe Element -- ^ Will not work correctly with pure-managed keyed nodes!
    , closeOnDocumentClick     :: Bool
    , closeOnEscape            :: Bool
    , closeOnPortalMouseLeave  :: Bool
    , closeOnRootNodeClick     :: Bool
    , closeOnTriggerBlur       :: Bool
    , closeOnTriggerClick      :: Bool
    , closeOnTriggerMouseLeave :: Bool
    , defaultOpen              :: Bool
    , mouseEnterDelay          :: Int
    , mouseLeaveDelay          :: Int
    , onClose                  :: IO ()
    , onMount                  :: IO ()
    , onOpen                   :: Evt -> IO ()
    , onUnmounted              :: IO ()
    , open                     :: Bool
    , openOnTriggerClick       :: Bool
    , openOnTriggerFocus       :: Bool
    , openOnTriggerMouseEnter  :: Bool
    } deriving (Generic)

instance Default Portal where
    def = (G.to gdef)
            { as                   = \fs cs -> Div & Features fs & Children cs
            , closeOnDocumentClick = True
            , closeOnEscape        = True
            , openOnTriggerClick   = True
            }

pattern Portal :: Portal -> Portal
pattern Portal p = p

data PortalState = PS
    { active   :: Bool
    , nodes    :: IORef PortalStateNodes
    , timers   :: IORef PortalStateTimers
    , liveView :: IORef (View,View)
    }

data PortalStateTimers = PST
    { mouseEnterTimer :: Maybe ThreadId
    , mouseLeaveTimer :: Maybe ThreadId
    } deriving (Generic,Default)

data PortalStateNodes = PSN
    { portalNode  :: Maybe JSV
    , triggerNode :: Maybe JSV
    } deriving (Generic,Default)

instance Pure Portal where
    view =
        LibraryComponentIO $ \self ->
            let

                toRoot = Just . maybe (coerce body) id

                contained Nothing  _ = return False
                contained (Just s) t = toJSV s `contains` toJSV t

                handleDocumentClick (evtTarget -> t) = do
                    PS      {..} <- get self
                    Portal_ {..} <- ask self
                    PSN     {..} <- readIORef nodes

                    inTrigger <- contained triggerNode t
                    inPortal  <- contained portalNode t
                    inRoot    <- contained (toRoot mountNode) t

                    let
                        -- the conditions for checking if the event should close the portal
                        shouldCheck = alive && handleable
                          where
                            alive      = isJust portalNode
                            handleable = not inTrigger && not inPortal

                        -- the two cases in which the portal should close
                        shouldClose = cond1 || cond2
                          where
                            cond1 = closeOnDocumentClick && not inRoot
                            cond2 = closeOnRootNodeClick &&     inRoot

                        eventShouldClosePortal = shouldCheck && shouldClose

                    when eventShouldClosePortal closePortal

                handleEscape Escape = do
                    Portal_ {..} <- ask self
                    when closeOnEscape closePortal
                handleEscape _ = return ()

                handlePortalMouseLeave _ = do
                    Portal_ {..} <- ask self
                    PS {..} <- get self
                    when closeOnPortalMouseLeave $ do
                        tid <- forkIO $ do
                            threadDelay (mouseLeaveDelay * 1000)
                            closePortal
                        modifyIORef timers $ \PST {..} ->
                            PST { mouseLeaveTimer = Just tid, .. }

                handlePortalMouseEnter _ = do
                    Portal_ {..} <- ask self
                    PS {..} <- get self
                    PST {..} <- readIORef timers
                    when closeOnPortalMouseLeave $
                        for_ mouseLeaveTimer killThread

                handleTriggerBlur (RelatedTarget r) = do
                    Portal_ {..} <- ask self
                    PS {..} <- get self
                    PSN {..} <- readIORef nodes
                    didFocusPortal <- contained (toRoot mountNode) r
                    when (closeOnTriggerBlur && not didFocusPortal)
                        closePortal
                handleTriggerBlur _ = return ()

                handleTriggerClick e = do
                    PS {..} <- get self
                    Portal_ {..} <- ask self
                    if active && closeOnTriggerClick
                        then closePortal
                        else when (not active && openOnTriggerClick)
                                (openPortal e)

                handleTriggerFocus e = do
                    Portal_ {..} <- ask self
                    when openOnTriggerFocus
                        (openPortal e)

                handleTriggerMouseLeave _ = do
                    Portal_ {..} <- ask self
                    PS {..} <- get self
                    when closeOnTriggerMouseLeave $ do
                        tid <- forkIO $ do
                            threadDelay (mouseLeaveDelay * 1000)
                            closePortal
                        modifyIORef timers $ \PST {..} ->
                            PST { mouseLeaveTimer = Just tid, .. }

                handleTriggerMouseEnter e = do
                    Portal_ {..} <- ask self
                    PS {..} <- get self
                    when openOnTriggerMouseEnter $ do
                        tid <- forkIO $ do
                            threadDelay (mouseEnterDelay * 1000)
                            openPortal e
                        modifyIORef timers $ \PST {..} ->
                            PST { mouseEnterTimer = Just tid, .. }

                openPortal e = do
                    Portal_ {..} <- ask self
                    modify self $ \_ PS {..} -> PS { active = True, .. }
                    onOpen e

                closePortal = do
                    Portal_ {..} <- ask self
                    modify self $ \_ PS {..} -> PS { active = False, .. }
                    onClose

                handleTriggerRef (Node r) = do
                    PS {..} <- get self
                    modifyIORef nodes $ \PSN {..} ->
                        PSN { triggerNode = Just r, .. }

                handlePortalRef (Node r) = do
                    PS {..} <- get self
                    modifyIORef nodes $ \PSN {..} ->
                        PSN { portalNode = Just r, .. }

            in
                def
                    { construct = do
                        Portal_ {..} <- ask self
                        PS defaultOpen
                          <$> newIORef def
                          <*> newIORef def
                          <*> newIORef def

                    , receive = \newprops@(Pure.Portal.Portal_ { open = isOpen }) oldstate -> do
                        oldprops@(Pure.Portal.Portal_ { open = wasOpen }) <- ask self
                        let change = isOpen /= wasOpen
                        if | change && isOpen -> do
                            Pure.Portal.onMount newprops
                            return oldstate { active = isOpen }
                           | change -> do
                            Pure.Portal.onUnmounted newprops
                            return oldstate { active = isOpen }
                           | otherwise -> return oldstate

                    , unmounted = void $ do
                        PS  {..} <- get self
                        PST {..} <- readIORef timers
                        for mouseEnterTimer killThread
                        for mouseLeaveTimer killThread

                    , render = \Portal_ {..} ps ->
                        let
                            p | active ps = PortalView Nothing (fromMaybe (coerce body) mountNode) $ portal
                                          $ Lifecycle (HostRef handlePortalRef)
                                          . OnMouseEnter handlePortalMouseEnter
                                          . OnMouseLeave handlePortalMouseLeave
                                          . Listener (OnDoc "click" handleDocumentClick)
                                          . Listener (OnDoc "keydown" handleEscape)
                              | otherwise = Null

                            addTriggerHandlers =
                                Lifecycle (HostRef handleTriggerRef)
                              . OnBlur handleTriggerBlur
                              . OnClick handleTriggerClick
                              . OnFocus handleTriggerFocus
                              . OnMouseLeave handleTriggerMouseLeave
                              . OnMouseEnter handleTriggerMouseEnter

                        in
                            as
                                (features & addTriggerHandlers)
                                (p : children)
                    }

#ifdef __GHCJS__
foreign import javascript unsafe
    "$1.contains($2)" contains_js :: JSV -> JSV -> IO Bool
#endif

contains :: JSV -> JSV -> IO Bool
contains node target =
#ifdef __GHCJS__
    contains_js node target
#else
    return True -- hmm?
#endif

data PortalNode = PortalNode_
pattern PortalNode :: HasProp PortalNode a => Prop PortalNode a -> a -> a
pattern PortalNode p a <- (getProp PortalNode_ &&& id -> (p,a)) where
    PortalNode p a = setProp PortalNode_ p a

data CloseOnDocumentClick = CloseOnDocumentClick_
pattern CloseOnDocumentClick :: HasProp CloseOnDocumentClick a => Prop CloseOnDocumentClick a -> a -> a
pattern CloseOnDocumentClick p a <- (getProp CloseOnDocumentClick_ &&& id -> (p,a)) where
    CloseOnDocumentClick p a = setProp CloseOnDocumentClick_ p a

data CloseOnEscape = CloseOnEscape_
pattern CloseOnEscape :: HasProp CloseOnEscape a => Prop CloseOnEscape a -> a -> a
pattern CloseOnEscape p a <- (getProp CloseOnEscape_ &&& id -> (p,a)) where
    CloseOnEscape p a = setProp CloseOnEscape_ p a

data CloseOnPortalMouseLeave = CloseOnPortalMouseLeave_
pattern CloseOnPortalMouseLeave :: HasProp CloseOnPortalMouseLeave a => Prop CloseOnPortalMouseLeave a -> a -> a
pattern CloseOnPortalMouseLeave p a <- (getProp CloseOnPortalMouseLeave_ &&& id -> (p,a)) where
    CloseOnPortalMouseLeave p a = setProp CloseOnPortalMouseLeave_ p a

data CloseOnRootNodeClick = CloseOnRootNodeClick_
pattern CloseOnRootNodeClick :: HasProp CloseOnRootNodeClick a => Prop CloseOnRootNodeClick a -> a -> a
pattern CloseOnRootNodeClick p a <- (getProp CloseOnRootNodeClick_ &&& id -> (p,a)) where
    CloseOnRootNodeClick p a = setProp CloseOnRootNodeClick_ p a

data CloseOnTriggerBlur = CloseOnTriggerBlur_
pattern CloseOnTriggerBlur :: HasProp CloseOnTriggerBlur a => Prop CloseOnTriggerBlur a -> a -> a
pattern CloseOnTriggerBlur p a <- (getProp CloseOnTriggerBlur_ &&& id -> (p,a)) where
    CloseOnTriggerBlur p a = setProp CloseOnTriggerBlur_ p a

data CloseOnTriggerClick = CloseOnTriggerClick_
pattern CloseOnTriggerClick :: HasProp CloseOnTriggerClick a => Prop CloseOnTriggerClick a -> a -> a
pattern CloseOnTriggerClick p a <- (getProp CloseOnTriggerClick_ &&& id -> (p,a)) where
    CloseOnTriggerClick p a = setProp CloseOnTriggerClick_ p a

data CloseOnTriggerMouseLeave = CloseOnTriggerMouseLeave_
pattern CloseOnTriggerMouseLeave :: HasProp CloseOnTriggerMouseLeave a => Prop CloseOnTriggerMouseLeave a -> a -> a
pattern CloseOnTriggerMouseLeave p a <- (getProp CloseOnTriggerMouseLeave_ &&& id -> (p,a)) where
    CloseOnTriggerMouseLeave p a = setProp CloseOnTriggerMouseLeave_ p a

data DefaultOpen = DefaultOpen_
pattern DefaultOpen :: HasProp DefaultOpen a => Prop DefaultOpen a -> a -> a
pattern DefaultOpen p a <- (getProp DefaultOpen_ &&& id -> (p,a)) where
    DefaultOpen p a = setProp DefaultOpen_ p a

data MountNode = MountNode_
pattern MountNode :: HasProp MountNode a => Prop MountNode a -> a -> a
pattern MountNode p a <- (getProp MountNode_ &&& id -> (p,a)) where
    MountNode p a = setProp MountNode_ p a

data MouseEnterDelay = MouseEnterDelay_
pattern MouseEnterDelay :: HasProp MouseEnterDelay a => Prop MouseEnterDelay a -> a -> a
pattern MouseEnterDelay p a <- (getProp MouseEnterDelay_ &&& id -> (p,a)) where
    MouseEnterDelay p a = setProp MouseEnterDelay_ p a

data MouseLeaveDelay = MouseLeaveDelay_
pattern MouseLeaveDelay :: HasProp MouseLeaveDelay a => Prop MouseLeaveDelay a -> a -> a
pattern MouseLeaveDelay p a <- (getProp MouseLeaveDelay_ &&& id -> (p,a)) where
    MouseLeaveDelay p a = setProp MouseLeaveDelay_ p a

data OnClose = OnClose_
pattern OnClose :: HasProp OnClose a => Prop OnClose a -> a -> a
pattern OnClose p a <- (getProp OnClose_ &&& id -> (p,a)) where
    OnClose p a = setProp OnClose_ p a

data OnOpen = OnOpen_
pattern OnOpen :: HasProp OnOpen a => Prop OnOpen a -> a -> a
pattern OnOpen p a <- (getProp OnOpen_ &&& id -> (p,a)) where
    OnOpen p a = setProp OnOpen_ p a

data Open = Open_
pattern Open :: HasProp Open a => Prop Open a -> a -> a
pattern Open p a <- (getProp Open_ &&& id -> (p,a)) where
    Open p a = setProp Open_ p a

data OpenOnTriggerClick = OpenOnTriggerClick_
pattern OpenOnTriggerClick :: HasProp OpenOnTriggerClick a => Prop OpenOnTriggerClick a -> a -> a
pattern OpenOnTriggerClick p a <- (getProp OpenOnTriggerClick_ &&& id -> (p,a)) where
    OpenOnTriggerClick p a = setProp OpenOnTriggerClick_ p a

data OpenOnTriggerFocus = OpenOnTriggerFocus_
pattern OpenOnTriggerFocus :: HasProp OpenOnTriggerFocus a => Prop OpenOnTriggerFocus a -> a -> a
pattern OpenOnTriggerFocus p a <- (getProp OpenOnTriggerFocus_ &&& id -> (p,a)) where
    OpenOnTriggerFocus p a = setProp OpenOnTriggerFocus_ p a

data OpenOnTriggerMouseEnter = OpenOnTriggerMouseEnter_
pattern OpenOnTriggerMouseEnter :: HasProp OpenOnTriggerMouseEnter a => Prop OpenOnTriggerMouseEnter a -> a -> a
pattern OpenOnTriggerMouseEnter p a <- (getProp OpenOnTriggerMouseEnter_ &&& id -> (p,a)) where
    OpenOnTriggerMouseEnter p a = setProp OpenOnTriggerMouseEnter_ p a

data WithPortal = WithPortal_
pattern WithPortal :: HasProp WithPortal a => Prop WithPortal a -> a -> a
pattern WithPortal p a <- (getProp WithPortal_ &&& id -> (p,a)) where
    WithPortal p a = setProp WithPortal_ p a

data WithTransition = WithTransition_
pattern WithTransition :: HasProp WithTransition a => Prop WithTransition a -> a -> a
pattern WithTransition p a <- (getProp WithTransition_ &&& id -> (p,a)) where
    WithTransition p a = setProp WithTransition_ p a

instance HasProp As Portal where
    type Prop As Portal = Features -> [View] -> View
    getProp _ = as
    setProp _ a p = p { as = a }

instance HasFeatures Portal where
    getFeatures = features
    setFeatures as p = p { features = as }

instance HasChildren Portal where
    getChildren = children
    setChildren cs p = p { children = cs }

instance HasProp PortalNode Portal where
    type Prop PortalNode Portal = (Features -> Features) -> View
    getProp _ = portal
    setProp _ pn p = p { portal = pn }

instance HasProp CloseOnDocumentClick Portal where
    type Prop CloseOnDocumentClick Portal = Bool
    getProp _ = closeOnDocumentClick
    setProp _ codc p = p { closeOnDocumentClick = codc }

instance HasProp CloseOnEscape Portal where
    type Prop CloseOnEscape Portal = Bool
    getProp _ = closeOnEscape
    setProp _ coe p = p { closeOnEscape = coe }

instance HasProp CloseOnPortalMouseLeave Portal where
    type Prop CloseOnPortalMouseLeave Portal = Bool
    getProp _ = closeOnPortalMouseLeave
    setProp _ copml p = p { closeOnPortalMouseLeave = copml }

instance HasProp CloseOnRootNodeClick Portal where
    type Prop CloseOnRootNodeClick Portal = Bool
    getProp _ = closeOnRootNodeClick
    setProp _ cornc p = p { closeOnRootNodeClick = cornc }

instance HasProp CloseOnTriggerBlur Portal where
    type Prop CloseOnTriggerBlur Portal = Bool
    getProp _ = closeOnTriggerBlur
    setProp _ cotb p = p { closeOnTriggerBlur = cotb }

instance HasProp CloseOnTriggerClick Portal where
    type Prop CloseOnTriggerClick Portal = Bool
    getProp _ = closeOnTriggerClick
    setProp _ cotc p = p { closeOnTriggerClick = cotc }

instance HasProp CloseOnTriggerMouseLeave Portal where
    type Prop CloseOnTriggerMouseLeave Portal = Bool
    getProp _ = closeOnTriggerMouseLeave
    setProp _ cotml p = p { closeOnTriggerMouseLeave = cotml }

instance HasProp DefaultOpen Portal where
    type Prop DefaultOpen Portal = Bool
    getProp _ = defaultOpen
    setProp _ o p = p { defaultOpen = o }

instance HasProp MountNode Portal where
    type Prop MountNode Portal = Maybe Element
    getProp _ = mountNode
    setProp _ mn p = p { mountNode = mn }

instance HasProp MouseEnterDelay Portal where
    type Prop MouseEnterDelay Portal = Int
    getProp _ = mouseEnterDelay
    setProp _ med p = p { mouseEnterDelay = med }

instance HasProp MouseLeaveDelay Portal where
    type Prop MouseLeaveDelay Portal = Int
    getProp _ = mouseLeaveDelay
    setProp _ mld p = p { mouseLeaveDelay = mld }

instance HasProp OnClose Portal where
    type Prop OnClose Portal = IO ()
    getProp _ = onClose
    setProp _ oc p = p { onClose = oc }

instance HasProp OnMount Portal where
    type Prop OnMount Portal = IO ()
    getProp _ = onMount
    setProp _ om p = p { onMount = om }

instance HasProp OnOpen Portal where
    type Prop OnOpen Portal = Evt -> IO ()
    getProp _ = onOpen
    setProp _ oo p = p { onOpen = oo }

instance HasProp OnUnmounted Portal where
    type Prop OnUnmounted Portal = IO ()
    getProp _ = onUnmounted
    setProp _ ou p = p { onUnmounted = ou }

instance HasProp Open Portal where
    type Prop Open Portal = Bool
    getProp _ = open
    setProp _ o p = p { open = o }

instance HasProp OpenOnTriggerClick Portal where
    type Prop OpenOnTriggerClick Portal = Bool
    getProp _ = openOnTriggerClick
    setProp _ ootc p = p { openOnTriggerClick = ootc }

instance HasProp OpenOnTriggerFocus Portal where
    type Prop OpenOnTriggerFocus Portal = Bool
    getProp _ = openOnTriggerFocus
    setProp _ ootf p = p { openOnTriggerFocus = ootf }

instance HasProp OpenOnTriggerMouseEnter Portal where
    type Prop OpenOnTriggerMouseEnter Portal = Bool
    getProp _ = openOnTriggerMouseEnter
    setProp _ ootme p = p { openOnTriggerMouseEnter = ootme }

data TransitionablePortal = TransitionablePortal_
    { children       :: [View]
    , onClose        :: IO ()
    , onHide         :: TransitionStatus -> IO ()
    , onOpen         :: IO ()
    , onStart        :: TransitionStatus -> IO ()
    , open           :: Maybe Bool
    , duration       :: AnimationDuration
    , withPortal     :: Portal -> Portal
    , inAnimation    :: SomeInTheme
    , outAnimation   :: SomeOutTheme
    , withTransition :: Transition -> Transition
    } deriving (Generic)

instance Default TransitionablePortal where
    def = (G.to gdef)
        { duration       = Uniform 400
        , withPortal     = id
        , withTransition = id
        , inAnimation    = fadeIn
        , outAnimation   = fadeOut
        }

pattern TransitionablePortal :: TransitionablePortal -> TransitionablePortal
pattern TransitionablePortal tp = tp

data TransitionablePortalState = TPS
    { portalOpen        :: Bool
    , transitionVisible :: Bool
    }

instance Pure TransitionablePortal where
    view =
        LibraryComponentIO $ \self ->
            let
                handlePortalClose = do
                    TransitionablePortal_ {..} <- ask self
                    TPS {..} <- get self
                    (isNothing open) #
                        modify_ self (\_ TPS {..} -> TPS { portalOpen = not portalOpen, .. })

                handlePortalOpen _ = modify_ self $ \_ TPS {..} -> TPS { portalOpen = True, .. }

                handleTransitionHide status = do
                    TransitionablePortal_ {..} <- ask self
                    TPS {..} <- get self
                    modify self $ \_ TPS {..} -> TPS { transitionVisible = False, .. }
                    onClose
                    onHide status

                handleTransitionStart status = do
                    TransitionablePortal_ {..} <- ask self
                    TPS {..} <- get self
                    onStart status
                    (status == Entering) # do
                        modify_ self (\_ TPS {..} -> TPS { transitionVisible = True, .. })
                        onOpen

            in def
                { construct = return (TPS def def)

                , receive = \newprops oldstate -> return $
                    case open (newprops :: TransitionablePortal) of
                        Just o -> oldstate { portalOpen = o }
                        _      -> oldstate

                , render = \TransitionablePortal_ {..} TPS {..} ->
                    View $ Pure.Portal.Portal $ withPortal $ def
                        & Open (portalOpen || transitionVisible)
                        & OnOpen handlePortalOpen
                        & OnClose handlePortalClose
                        & Children
                            [ View $ Pure.Transition.Transition $ withTransition $ def
                                & TransitionOnMount True
                                & OnStart handleTransitionStart
                                & OnHide handleTransitionHide
                                & Visible portalOpen
                                & InAnimation inAnimation
                                & OutAnimation outAnimation
                                & AnimationDuration duration
                                & Children children
                            ]
                }

instance HasChildren TransitionablePortal where
    getChildren = children
    setChildren cs tp = tp { children = cs }

instance HasProp OnClose TransitionablePortal where
    type Prop OnClose TransitionablePortal = IO ()
    getProp _ = onClose
    setProp _ oc tp = tp { onClose = oc }

instance HasProp OnHide TransitionablePortal where
    type Prop OnHide TransitionablePortal = TransitionStatus -> IO ()
    getProp _ = onHide
    setProp _ oh tp = tp { onHide = oh }

instance HasProp OnOpen TransitionablePortal where
    type Prop OnOpen TransitionablePortal = IO ()
    getProp _ = onOpen
    setProp _ oo tp = tp { onOpen = oo }

instance HasProp OnStart TransitionablePortal where
    type Prop OnStart TransitionablePortal = TransitionStatus -> IO ()
    getProp _ = onStart
    setProp _ os tp = tp { onStart = os }

instance HasProp Open TransitionablePortal where
    type Prop Open TransitionablePortal = Maybe Bool
    getProp _ = open
    setProp _ o tp = tp { open = o }

instance HasProp InAnimation TransitionablePortal where
    type Prop InAnimation TransitionablePortal = SomeInTheme
    getProp _ = inAnimation
    setProp _ a tp = tp { inAnimation = a }

instance HasProp OutAnimation TransitionablePortal where
    type Prop OutAnimation TransitionablePortal = SomeOutTheme
    getProp _ = outAnimation
    setProp _ a tp = tp { outAnimation = a }

instance HasProp AnimationDuration TransitionablePortal where
    type Prop AnimationDuration TransitionablePortal = AnimationDuration
    getProp _ = duration
    setProp _ ad tp = tp { duration = ad }

instance HasProp WithPortal TransitionablePortal where
    type Prop WithPortal TransitionablePortal = Portal -> Portal
    getProp _ = withPortal
    setProp _ wp tp = tp { withPortal = wp }

instance HasProp WithTransition TransitionablePortal where
    type Prop WithTransition TransitionablePortal = Transition -> Transition
    getProp _ = withTransition
    setProp _ wt tp = tp { withTransition = wt }
