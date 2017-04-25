{-# LANGUAGE OverloadedStrings #-}
module Foundation where

import Database.Persist.Sql        (ConnectionPool, runSqlPool)
import Import.NoFoundation
import Text.Hamlet                 (hamletFile)
import Yesod.Auth.Email
import Yesod.Auth.Message          (AuthMessage (InvalidLogin))
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Core.Types            (Logger)
import Yesod.Default.Util          (addStaticContentExternal)
import Yesod.Fay
import Network.Mail.Mime
import Text.Shakespeare.Text (stext)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Yesod.Form.Nic (YesodNic)
import System.Log.FastLogger (pushLogStrLn, toLogStr, ToLogStr)

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings          :: AppSettings
    , appStatic            :: Static -- ^ Settings for static file serving.
    , appConnPool          :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager       :: Manager
    , appLogger            :: Logger
    , appFayCommandHandler :: CommandHandler App
    , appData              :: MVar (Maybe DomainData)
    }

data DomainData = 
  DomainData 
    { getMarks :: [Entity Mark]
    , getModels :: [Entity Model]
    , getAges :: [Entity Age]
    , getLkModelAges :: [Entity LkModelAge]
    , getGenerations :: [Entity Generation]
    , getRegions :: [Entity Region]
    , getLkTags :: [Entity LkTag]
    , getTextAdvices :: [Entity TextAdvise]
    , getImages :: [Entity Image]
    }

instance YesodNic App

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        36000    -- timeout in minutes
        "config/client_session_key.aes"

    maximumContentLength _ (Just (FaySiteR _)) = Just (100 * 1024 * 1024) -- 100 mb for photos
    maximumContentLength _ _ = Just (2 * 1024 * 1024)

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            -- addStylesheet $ StaticR css_bootstrap_css
            -- addStylesheet $ StaticR css_bootstrap_combobox_css
            -- addStylesheet $ StaticR css_blocks_css
            -- addStylesheet $ StaticR css_grid_css

            addScript $ StaticR js_jquery_min_js
            addScript $ StaticR js_jquery_parallax_js
            addScript $ StaticR js_bootstrap_min_js
            addScript $ StaticR js_bootstrap_combobox_js
            -- addScript $ StaticR js_blocks_js
            addScript $ StaticR js_blocks2_js
            addScript $ StaticR js_scripts2_js
            addScript $ StaticR js_forms1_js
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    -- Default to Authorized for now.
    isAuthorized HomeR _ = return Authorized
    isAuthorized AboutR _ = return Authorized
    isAuthorized GuideR _ = return Authorized
    isAuthorized TermsR _ = return Authorized
    isAuthorized MarksR _ = return Authorized
    isAuthorized (ModelsR _) _ = return Authorized

    isAuthorized (FaySiteR _) _ = return Authorized
    isAuthorized SearchR _ = return Authorized
    isAuthorized TechR _ = return Authorized
    isAuthorized TaxR _ = return Authorized
    isAuthorized (TechWithIdR _) _ = return Authorized
    isAuthorized _ _ = isAdmin

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            Right
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

isUser :: forall master. YesodAuth master => HandlerT master IO AuthResult
isUser = do
    mu <- maybeAuthId
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized

isAdmin ::
  forall master.
  (YesodPersist master, YesodAuth master, AuthId master ~ Key User,
    YesodPersistBackend master ~ SqlBackend) =>
  HandlerT master IO AuthResult
isAdmin = do
    mu <- maybeAuthId
    ar <- case mu of
      Nothing -> return AuthenticationRequired
      Just uid -> do
          u <- runDB $ selectList [UserId ==. uid] []
          return $ case u of
                     [] -> AuthenticationRequired
                     (Entity _ u':_) -> 
                        case (userRole u') of
                          "admin" -> Authorized
                          _ -> AuthenticationRequired
    return ar

instance YesodJquery App where 
  urlJqueryJs _ = Left (StaticR js_jquery_min_js)

instance YesodFay App where

    fayRoute = FaySiteR

    yesodFayCommand render command = do
        master <- getYesod
        appFayCommandHandler master render command

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    authenticate creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        return $ case x of
            Just (Entity uid _) -> Authenticated uid
            Nothing -> UserError InvalidLogin

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authEmail]

    getAuthId creds = runDB $ do
        x <- insertBy $ User (credsIdent creds) Nothing Nothing False "user"
        return $ Just $ 
            case x of
                Left (Entity userid _) -> userid
                Right userid -> userid


    authHttpManager = error "Email doesn't need an HTTP manager"

instance YesodAuthEmail App where
    type AuthEmailId App = UserId

    -- registerHandler :: YesodAuthEmail master => AuthHandler master Html
    registerHandler = do
      (widget, enctype) <- lift $ generateFormPost registrationForm
      toParentRoute <- getRouteToParent
      lift $ defaultLayout $ do
        setTitle "Регистрация"
        [whamlet|
            <p>Пожалуйста, оставьте свой e-mail для того, чтобы продолжить поиск без ограничений.
            <form method="post" action="@{toParentRoute $ PluginR em reg}" enctype=#{enctype}>
                <div id="registerForm">
                    ^{widget}
                <button .btn>Отправить
        |]
          where
            em = "email"
            reg = ["register"]
            registrationForm extra = do
              let emailSettings = FieldSettings
                    {
                      fsLabel = "Email",
                      fsTooltip = Nothing,
                      fsId = Just "email",
                      fsName = Just "email",
                      fsAttrs = [("autofocus", "")]
                    }

              (emailRes, emailView) <- mreq emailField emailSettings Nothing

              let userRes = UserForm <$> emailRes
              let widget = do
                    [whamlet|
                      #{extra}
                      ^{fvLabel emailView}
                      ^{fvInput emailView}
                    |]

              return (userRes, widget)


    addUnverified email verkey =
        runDB $ insert $ User email Nothing (Just verkey) False "user"

    confirmationEmailSentResponse ident = selectRep $ provideRep $ defaultLayout $ do
      setTitle "Проверьте почту"
      let a = "Письмо с подтверждением было отправлено по адресу " <> ident <> ". Пожалуйста, проверьте почту."
      [whamlet|<h2>#{a}|]

    sendVerifyEmail email _ verurl =
        liftIO $ renderSendMail (emptyMail $ Address Nothing "noreply")
            { mailTo = [Address Nothing email]
            , mailHeaders =
                [ ("Subject", "Подтверждение вашего email адреса")
                ]
            , mailParts = [[textPart, htmlPart']]
            }
      where
        textPart = Part
            { partType = "text/plain; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = Data.Text.Lazy.Encoding.encodeUtf8 [stext|
Пожалуйста, подтвердите ваш email адрес, перейдя по ссылке.

\#{verurl}

Спасибо.
|]
            , partHeaders = []
            }
        htmlPart' = Part
            { partType = "text/html; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = renderHtml [shamlet|
<p>Пожалуйста, подтвердите ваш email адрес, перейдя по ссылке.
<p>
    <a href=#{verurl}>#{verurl}
<p>Спасибо.
|]
            , partHeaders = []
            }
    getVerifyKey = runDB . fmap (join . fmap userVerkey) . get
    setVerifyKey uid key = runDB $ update uid [UserVerkey =. Just key]
    verifyAccount uid = runDB $ do
        mu <- get uid
        case mu of
            Nothing -> return Nothing
            Just _ -> do
                update uid [UserVerified =. True]
                return $ Just uid
    getPassword = runDB . fmap (join . fmap userPassword) . get
    setPassword uid pass = runDB $ update uid [UserPassword =. Just pass]
    getEmailCreds email = runDB $ do
        mu <- getBy $ UniqueUser email
        case mu of
            Nothing -> return Nothing
            Just (Entity uid u) -> return $ Just EmailCreds
                { emailCredsId = uid
                , emailCredsAuthId = Just uid
                , emailCredsStatus = isJust $ userPassword u
                , emailCredsVerkey = userVerkey u
                , emailCredsEmail  = userEmail u
                }
    getEmail = runDB . fmap (fmap userEmail) . get
    
    afterPasswordRoute _ = HomeR

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

logMessage :: forall (m :: * -> *) r.
  (IsString r, MonadHandler m, IsSequence r, ToLogStr r
  , HandlerSite m ~ App, Element r ~ Char) =>
  r -> m ()
logMessage msg = do
  y <- getYesod
  liftIO $ logMessageSite y msg

logMessageSite :: forall r. (IsString r, IsSequence r, ToLogStr r, Element r ~ Char) => 
  App -> r -> IO ()
logMessageSite app msg = do
  t <- getCurrentTime
  let timelog = pack $ formatTime defaultTimeLocale "%d-%b-%Y %T" t
  pushLogStrLn (loggerSet $ appLogger app) (toLogStr $ timelog <> "\t" <> msg)

data UserForm = UserForm { email :: Text }

