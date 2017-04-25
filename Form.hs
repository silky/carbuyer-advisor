{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Form where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, bfs)
import Yesod.Form.Nic (YesodNic, nicHtmlField)
import Web.Cookie
import Cache
import SessionState

import Database.Persist.Sql
import qualified Data.Text as T
import qualified Data.Maybe as M
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import Text.Blaze.Internal (Markup)
import Control.Monad.Trans.RWS.Lazy (RWST)

adviceForm :: forall site.
                  (RenderMessage site FormMessage, YesodNic site) =>
                  Maybe Text
                  -> Maybe Html
                  -> Maybe (Maybe (Key Type))
                  -> [(Text, Key Type)]
                  -> Maybe (Maybe Text)
                  -> Maybe Int
                  -> AForm (HandlerT site IO) Advice
adviceForm t c typ typs g sc = Advice
    <$> areq   textField (bfs ("Title"::Text)) t
    <*> areq   nicHtmlField (bfs ("Content"::Text)) c
    <*> aopt   (selectFieldList typs) "Выберите тип рекомендации" typ
    <*> aopt   textField (bfs ("Group"::Text)) g
    <*> areq   intField (bfs ("Score"::Text)) sc

textAdviseForm :: forall site.
                  (RenderMessage site FormMessage) =>
                  Maybe Text
                  -> Maybe Textarea
                  -> Maybe Textarea
                  -> Maybe Textarea
                  -> Maybe (Maybe Textarea)
                  -> AForm (HandlerT site IO) TextAdvise
textAdviseForm n hap unhap look promo = TextAdvise
    <$> areq   textField (bfs ("Name"::Text)) n
    <*> areq   textField (bfs ("FileName"::Text)) n
    <*> areq   textareaField (bfs ("Happy"::Text)) hap
    <*> areq   textareaField (bfs ("Unhappy"::Text)) unhap
    <*> areq   textareaField (bfs ("Lookup"::Text))  look
    <*> aopt   textareaField (bfs ("promo"::Text))  promo

simpleAdviceForm :: forall site.
  (RenderMessage site FormMessage, YesodNic site)
  => Maybe Html -> AForm (HandlerT site IO) SimpleAdvice
simpleAdviceForm c = SimpleAdvice
    <$> areq   nicHtmlField (bfs ("Content"::Text)) c

data File = File

instance RenderMessage File FormMessage where
    renderMessage _ _ = defaultFormMessage

fileForm :: forall (m :: * -> *).
  (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
  Markup
  -> RWST
  (Maybe (Env, FileEnv), HandlerSite m, [Lang])
  Enctype
  Ints
  m
  (FormResult FileInfo, WidgetT (HandlerSite m) IO ())
fileForm = renderDivs $ fileAFormReq "File" 

typeForm :: forall site. 
               (RenderMessage site FormMessage) =>
               Maybe (Key Level)
               -> Maybe (Key Group)
               -> Maybe (Maybe Int)
               -> Maybe (Maybe Int)
               -> Maybe (Maybe Int)
               -> AForm (HandlerT site IO) Type
typeForm l g t b v = Type
    <$> areq  (selectFieldList [("Возраст" :: Text, (toSqlKey 1)),("Популярность" :: Text, (toSqlKey 2)), ("Цена" :: Text, (toSqlKey 3)),("Регион" :: Text, (toSqlKey 4))]) "Выберите тип рекомендации" l
    <*> areq  (selectFieldList [ ("Диапазон лет" :: Text, (toSqlKey 1))
                               , ("Диапазон чисел объявлений" :: Text, (toSqlKey 2))
                               , ("+%" :: Text, (toSqlKey 3))
                               , ("-%" :: Text, (toSqlKey 4))
                               , ("Крайнее левое" :: Text, (toSqlKey 5))
                               , ("Крайнее правое" :: Text, (toSqlKey 6))
                               , ("Региональное значение" :: Text, (toSqlKey 7))
                               ]) "Выберите группу типа пограничного значения" g
    <*> aopt  intField (bfs ("Bottom"::Text)) b
    <*> aopt  intField (bfs ("Top"::Text)) t
    <*> aopt  intField (bfs ("Constant Value"::Text)) v


emptyAdviceForm :: forall site. (RenderMessage site FormMessage, YesodNic site) =>
                      AForm (HandlerT site IO) Advice
emptyAdviceForm = adviceForm Nothing Nothing Nothing [] Nothing Nothing

emptyTypeForm :: forall site.
                       RenderMessage site FormMessage =>
                       AForm (HandlerT site IO) Type
emptyTypeForm = typeForm Nothing Nothing Nothing Nothing Nothing

emptyTextAdviseForm :: forall site.
                       RenderMessage site FormMessage =>
                       AForm (HandlerT site IO) TextAdvise
emptyTextAdviseForm = textAdviseForm Nothing Nothing Nothing Nothing Nothing

emptySimpleAdviceForm :: forall site.
  (RenderMessage site FormMessage, YesodNic site)
  => AForm (HandlerT site IO) SimpleAdvice
emptySimpleAdviceForm = simpleAdviceForm Nothing

bootstrapAdviceForm :: forall site.
  (RenderMessage site FormMessage, YesodNic site) =>
  Markup
  -> RWST
  (Maybe (Env, FileEnv), site, [Lang])
  Enctype
  Ints
  (HandlerT site IO)
  (FormResult Advice, WidgetT site IO ())
bootstrapAdviceForm = renderBootstrap3 BootstrapBasicForm $ emptyAdviceForm

bootstrapTypeForm :: forall site.
  RenderMessage site FormMessage =>
  Markup
  -> RWST
  (Maybe (Env, FileEnv), site, [Lang])
  Enctype
  Ints
  (HandlerT site IO)
  (FormResult Type, WidgetT site IO ())
bootstrapTypeForm   = renderBootstrap3 BootstrapBasicForm $ emptyTypeForm

bootstrapTextAdviseForm :: forall site.
  RenderMessage site FormMessage =>
  Markup
  -> RWST
  (Maybe (Env, FileEnv), site, [Lang])
  Enctype
  Ints
  (HandlerT site IO)
  (FormResult TextAdvise, WidgetT site IO ())
bootstrapTextAdviseForm   = renderBootstrap3 BootstrapBasicForm $ emptyTextAdviseForm

bootstrapSimpleAdviseForm :: forall site.
  (RenderMessage site FormMessage, YesodNic site) =>
  Markup
  -> RWST
  (Maybe (Env, FileEnv), site, [Lang])
  Enctype
  Ints
  (HandlerT site IO)
  (FormResult SimpleAdvice, WidgetT site IO ())
bootstrapSimpleAdviseForm   = renderBootstrap3 BootstrapBasicForm $ emptySimpleAdviceForm

b2AdviceForm :: forall site.
  (RenderMessage site FormMessage, YesodNic site) =>
  Maybe Text
  -> Maybe Html
  -> Maybe (Maybe (Key Type))
  -> [(Text, Key Type)]
  -> Maybe (Maybe Text)
  -> Maybe Int
  -> Markup
  -> RWST
  (Maybe (Env, FileEnv), site, [Lang])
  Enctype
  Ints
  (HandlerT site IO)
  (FormResult Advice, WidgetT site IO ())
b2AdviceForm x1 x2 x3 x4 x5 x6 = renderBootstrap3 BootstrapBasicForm $ adviceForm x1 x2 x3 x4 x5 x6

b2TypeForm :: forall site.
  RenderMessage site FormMessage =>
  Maybe (Key Level)
  -> Maybe (Key Group)
  -> Maybe (Maybe Int)
  -> Maybe (Maybe Int)
  -> Maybe (Maybe Int)
  -> Markup
  -> RWST
  (Maybe (Env, FileEnv), site, [Lang])
  Enctype
  Ints
  (HandlerT site IO)
  (FormResult Type, WidgetT site IO ())
b2TypeForm x1 x2 x3 x4 x5 = renderBootstrap3 BootstrapBasicForm $ typeForm x1 x2 x3 x4 x5

b2TextAdviseForm :: forall site.
    RenderMessage site FormMessage =>
    Maybe Text
    -> Maybe Textarea
    -> Maybe Textarea
    -> Maybe Textarea
    -> Maybe (Maybe Textarea)
    -> Markup
    -> RWST
    (Maybe (Env, FileEnv), site, [Lang])
    Enctype
    Ints
    (HandlerT site IO)
    (FormResult TextAdvise, WidgetT site IO ())
b2TextAdviseForm x1 x2 x3 x4 x5 = renderBootstrap3 BootstrapBasicForm $ textAdviseForm x1 x2 x3 x4 x5 

b2SimpleAdviceForm :: forall site.
    (RenderMessage site FormMessage, YesodNic site) =>
    Maybe Html
    -> Markup
    -> RWST
    (Maybe (Env, FileEnv), site, [Lang])
    Enctype
    Ints
    (HandlerT site IO)
    (FormResult SimpleAdvice, WidgetT site IO ())
b2SimpleAdviceForm x1 = renderBootstrap3 BootstrapBasicForm $ simpleAdviceForm x1

typeToFormList :: (Entity Type, Entity Group) -> (Text, Key Type)
typeToFormList (typ, group') = 
  let grp = T.concat [ groupName $ entityVal group', ": " ]
      (tn, tid) = typeToName typ
  in (T.concat [grp, tn], tid)

typeToName :: Entity Type -> (Text, Key Type)
typeToName (Entity typeId typ) = 
  case (typeValue typ) of
    Nothing ->
        (T.concat [ T.pack $ show $ M.fromJust $ typeBottom typ
                  ,  " - "
                  , T.pack $ show $ M.fromJust $ typeTop typ 
                  ], typeId)
    Just val -> (T.pack $ show val, typeId)

joinTypeWithGroup :: [Entity Group] -> Entity Type -> (Entity Type, Entity Group)
joinTypeWithGroup grps typ = joinTypeWithEntity grps typeGroupId typ

joinTypeWithLevel :: [Entity Level] -> Entity Type -> (Entity Type, Entity Level)
joinTypeWithLevel lvls typ = joinTypeWithEntity lvls typeLevelId typ
 
joinTypeWithEntity :: forall r record.
                            Eq (Key record) =>
                            [Entity record]
                            -> (r -> Key record) -> Entity r -> (Entity r, Entity record)
joinTypeWithEntity ents fkAttr typ = 
  (typ, L.head $ filter ((==(fkAttr $ entityVal typ)) . entityKey) ents)

tquery :: Text
tquery = "select ??, ??, ?? from type, \"group\", level where type.level_id = level.id and type.group_id = \"group\".id order by level.id"

rquery :: Text
rquery = "select distinct pop from region where pop is not null and pop <> '' order by 1"

summary :: forall (m :: * -> *).
                 MonadIO m =>
                 (Entity Type, Entity Group, Entity Level)
                 -> m (Text, (Text, Key Type))

summary (type', group', (Entity _ level')) =
  liftIO $ return ( levelName level'
         , typeToFormList (type', group') 
         )

grouping :: (Text, Text, Key Type) -> (Text, Text, Key Type) -> Bool
grouping (a1, _, _) (a2, _, _) = a1 == a2

shiftSummary :: (Text, (Text, Key Type)) -> (Text, Text, Key Type)
shiftSummary (a, (b,c)) = (a, b, c)

lvlFromTriple :: forall a b c. (a, b, c) -> a
lvlFromTriple (a, _, _) = a

q2s :: forall t. Single t -> (t, t)
q2s (Single t) = (t, t )

oldCarSearchForm ::
  forall (m :: * -> *) (t :: * -> *) (t1 :: * -> *).
  ( Foldable t, Foldable t1, MonadIO m, MonadBaseControl IO m
  , MonadThrow m
  ) => t (Entity Region) -> t1 (Entity Mark) -> WidgetT App m ()
oldCarSearchForm regions marks = [whamlet|<div #rec2967582 .r style="padding-top: 15px; padding-bottom: 60px; background-color: rgb(255, 233, 173); opacity: 1;" data-record-type="214">
  <!-- T186B -->
  <div .t186B>
      
    <div .container>
      <div .row .hide>
        <div .lr_col_12>
          <label for=progress>Прогресс заполнения
          <div style="width:100%; border:1px solod #000000; ">
            <div #progress .progress style="width: 0%;">
        <br>
        <div .row .hide #generation-help >
          <div .hide .lr_col_10 .prefix_1>
            <div .wrapper style="background-color:#CCCCCC;">
              <div .msg-help #old-generation-help-msg .block .blocks-input>
            <br>
        <div .row>
          <div .lr_col_12>
            <div .wrapper>
              <div .blockinput #region-container>
                <label for=region>
                  Где искать?
                <select #region .combobox .input-large .form-control .blocks-input style="color:#000000!important; border:1px solid #000000;">
                  <option selected=selected value="">Введите регион
                  $forall Entity rkey r <- regions
                    <option value=#{fromSqlKey rkey} east=#{regionEast r}>#{regionName r}
              <div .blockinput #mark-container>
                <label for=mark>
                  Марка
                <select #mark .combobox .input-large .form-control .blocks-input style="color:#000000; border:1px solid #000000;">
                  <option selected=selected value="">Введите марку
                  $forall Entity mi ma <- marks
                    <option value=#{show $ fromSqlKey mi}>#{markName ma}

              <div .blockinput #model-container>
                <label for=model>
                  Модель
                <select #model .combobox .input-large .form-control .blocks-input style="color:#000000; border:1px solid #000000;">
                  <option selected=selected value="">Введите модель

              <div .blockinput #year-container>
                <label for=year>
                  Год
                <select #year .combobox .input-large .form-control .blocks-input style="color:#000000; border:1px solid #000000;">
                  <option selected=selected value="">Введите год

              <div .blockinput #separate-container>
                <div .hide #generation-container .tt >
                  <span .tooltiptext #generation-help-msg >
                  <label for=generation>
                    Поколение
                  <select #generation .combobox .input-large .form-control .blocks-input style="color:#000000; border:1px solid #000000;">
                    <option selected=selected value="">Введите поколение

                <div #budget-container>
                  <label for=budget>
                    Задайте цену
                  <input #budget .input-large .form-control .blocks-input placeholder="Бюджет" style="color:#000000;" type="text" maxlength=10>
      <div .container>
        <div .row>
          <div .lr_col_2 .prefix_5>
            <div .line>
            <br>

        <div .row>
          <div .lr_col_4 .prefix_4>
            <div .wrapper>
              <div .blockinput #btn-wrapper >
                <form #form2967582 name="form2967582" role="form" method=POST action=@{SearchR}>
                  <input type=hidden name=q #q>
                  <button #form-submit .btn-disabled .blocks-submit style="color:#000000; border:1px solid #000000;" type="text">Получить рекомендацию!
        <div .row>
          <div .lr_col_12>
            <div .wrapper>
              <div .blockinput>
                <div #advice .hide style="color:#000000; border:1px solid #000000;">

<style>
  \ #rec2967582 input::-webkit-input-placeholder {color:#000000; opacity: 0.5;}
  \ #rec2967582 input::-moz-placeholder          {color:#000000; opacity: 0.5;}
  \ #rec2967582 input:-moz-placeholder           {color:#000000; opacity: 0.5;}
  \ #rec2967582 input:-ms-input-placeholder      {color:#000000; opacity: 0.5;}
  \ #rec2967582 .hide              {display:none;}
  \ #rec2967582 .show              {display:block;}
  \ #rec2967582 select             {background-color: #FFFFFF;}
  \ #rec2967582 select:disabled    {background-color: #CCCCCC;}
  \ #rec2967582 #advice            {background-color: #CCCCCC; padding: 30px;}
  \ #rec2967582 .msg-help          {height: 80px; padding: 10px; display:inline;}
  \ #rec2967582 .progress          {background-color:rgb(190, 245, 186);border: 1px solid #000000;height: 30px;text-align: right;padding-right: 7px;}
  \ #rec2967582 .btn-disabled      {color: #C0C0C0!important;}
  \ #rec2967582 .btn-enabled       {color: #000000!important;}
  \ #rec2967582 .input-group-addon {display:none;}
  \ #rec2967582 ul                 {padding-left: 0px!important;}
  \ #rec2967582 ul li a            {color: #555!important;}
|]


carSearchForm ::
  forall (m :: * -> *) (t :: * -> *) (t1 :: * -> *).
  ( Foldable t, Foldable t1, MonadIO m, MonadBaseControl IO m
  , MonadThrow m
  ) => t (Entity Region) -> t1 (Entity Mark) -> WidgetT App m ()
carSearchForm regions marks = [whamlet|  <div #rec9267769 .r style="padding-bottom: 30px; opacity: 1;" data-record-type="214">
    <!-- T186B -->
    <div .t186B>
    
      <div .container>
        <div .row .hide >
          <div .lr_col_12>
            <label for=progress>Прогресс заполнения
            <div style="width:100%; border:1px solod #000000; ">
              <div #progress .progress style="width: 0%;">
              <br>

        <div .row .hide #generation-help >
          <div .hide .lr_col_10 .prefix_1>
            <div .wrapper style="background-color:#CCCCCC;">
              <div .msg-help #old-generation-help-msg .block .blocks-input>
        <div .row>
          <div .t186C_block .t-input #region-container .lr_col_3>
            <label for=region>
              Где искать?
            <select #region .combobox .input-large .form-control .t-input style="color:#000000!important; border:1px solid #000000;">
              <option selected=selected value="">Введите регион
              $forall Entity rkey r <- regions
                <option value=#{fromSqlKey rkey} east=#{regionEast r}>#{regionName r}
          <div .t186C_block .t-input #mark-container .lr_col_2>
            <label for=mark>
              Марка
            <select #mark .combobox .t-input .input-large .form-control .blocks-input style="color:#000000; border:1px solid #000000;">
              <option selected=selected value="">Введите марку
                $forall Entity mi ma <- marks
                  <option value=#{show $ fromSqlKey mi}>#{markName ma}

          <div .t186C_block #model-container .lr_col_2>
            <label for=model>
              Модель
            <select #model .combobox .t-input .input-large .form-control .blocks-input style="color:#000000; border:1px solid #000000;">
              <option selected=selected value="">Введите модель

          <div .t186C_block #year-container .lr_col_2>
            <label for=year>
              Год
            <select #year .combobox .t-input .input-large .form-control .blocks-input style="color:#000000; border:1px solid #000000;">
              <option selected=selected value="">Введите год

          <div .t186C_block #separate-container .lr_col_2>
            <div .hide #generation-container .tt>
              <span .tooltiptext #generation-help-msg >
              <label for=generation>
                Поколение
              <select #generation .combobox .t-input .input-large .form-control .blocks-input style="color:#000000; border:1px solid #000000;">
                <option selected=selected value="">Введите поколение

            <div #budget-container .lr_col_3>
              <label for=budget>
                Задайте цену
              <input #budget .t-input .input-large .form-control .blocks-input placeholder="Бюджет" style="color:#000000;" type="text" maxlength=10>

      <div .container>
        <div .row>
          <div .lr_col_2 .prefix_5>
            <div .line>
            <br>

        <div .row>
          <div .lr_col_4 .prefix_4>
            <div #btn-wrapper style="width:30%; padding-left:35%; padding-right: 35%" >
              <form #form9267769 name="form9267769" role="form" method=POST action=@{SearchR}>
                <input type=hidden name=q #q>
                <button .t-btn #form-submit .t-submit .btn-disabled style="display:block; border-radius: 5px; -moz-border-radius: 5px; -webkit-border-radius: 5px; color:#FFFFFF; background-color:#000000;" type="text">Узнать о моих рисках
        <div .row>
          <div .lr_col_12>
            <div .wrapper>
              <div .blockinput>
                <div #advice .hide style="color:#000000; border:1px solid #000000;">

|]

initCarSearchForm :: forall (m :: * -> *) (t :: * -> *) (t1 :: * -> *).
  (Foldable t, Foldable t1, MonadIO m, MonadBaseControl IO m, MonadThrow m)
  => t (Entity Region) -> t1 (Entity Mark) -> WidgetT App m ()
initCarSearchForm region marks = [whamlet|  <div #rec9275974 .r style="padding-top: 60px; padding-bottom: 30px; opacity: 1;" data-record-type="43">
    <!-- T030 -->
    <div .t030 #choice >
      <div .t-container .t-align_center>
        <div .t-col .t-col_10 .t-prefix_1> 
          <div .t030__title .t-title .t-title_xxs field="title">Сомневаетесь в своем выборе?
          <div .t030__descr .t-descr .t-descr_md field="descr">
            Расскажите, какой автомобиль и за сколько вы хотите купить.
            <br>Мы 
              <strong>проанализируем объявления, популярность машин, цены на них
            и скажем, насколько хороша ваша задумка.
  ^{carSearchForm region marks}
|]

retryCarSearchForm :: forall (m :: * -> *) (t :: * -> *) (t1 :: * -> *).
  (Foldable t, Foldable t1, MonadIO m, MonadBaseControl IO m, MonadThrow m)
  => t (Entity Region) -> t1 (Entity Mark) -> WidgetT App m ()
retryCarSearchForm region marks = [whamlet|  <div #rec9342672 .r style="padding-top:60px;padding-bottom:30px; " data-record-type="43">
    <!-- T030 -->
    <div .t030 #choice >
      <div .t-container .t-align_center >
        <div .t-col .t-col_10 .t-prefix_1 >      
          <div .t030__title .t-title .t-title_xxs field="title">Не подходит?
          <div .t030__descr .t-descr .t-descr_md field="descr">Попробуйте изменить цену или выбрать другую модель!
  ^{carSearchForm region marks}
|]

wantByCarSearchForm :: forall (m :: * -> *) (t :: * -> *) (t1 :: * -> *).
  (Foldable t, Foldable t1, MonadIO m, MonadBaseControl IO m, MonadThrow m)
  => t (Entity Region) -> t1 (Entity Mark) -> WidgetT App m ()
wantByCarSearchForm region marks = [whamlet|  <div .r data-record-type="43" #rec9313459 style="padding-top: 60px; padding-bottom: 30px; opacity: 0;">
    $#  T030 
    <div .t030>
      <div .t-container .t-align_center>
        <div .t-col .t-col_10 .t-prefix_1>
          <div .t030__title .t-title .t-title_xxs field="title">
            Собираетесь купить такую машину?
          <div .t030__descr .t-descr .t-descr_md field="descr">
            Проверьте, насколько она популярна на рынке и хватит ли у вас на нее денег. 
            <br>
            Автоскептик умеет автоматически анализировать объявления!
  ^{carSearchForm region marks}
|]

weakPlacesForm :: forall (m :: * -> *) (t :: * -> *).
  (Foldable t, MonadIO m, MonadBaseControl IO m, MonadThrow m)
  => t (Entity Mark) -> WidgetT App m ()  
weakPlacesForm marks = [whamlet|  <div #rec9267769 .r style="padding-top: 90px; padding-bottom: 30px; opacity: 1;" data-record-type="214">
    <!-- T186B -->
    <div .t186B >
      <div .row .hide #tech-generation-help >
        <div .hide .lr_col_10 .prefix_1>
          <div .wrapper style="background-color:#CCCCCC;">
      <div .t-container >
        <div .t-col .t-col_10 .t-prefix_1 >
          <div .t186B__wrapper >
            <div .t186B__blockinput #tech-mark-container >
              <select #tech-mark .combobox .t-input .t186B__input .input-large .form-control .blocks-input style="color:#000000; border:1px solid #000000;">
                <option selected=selected value="">FORD
                  $forall Entity mi ma <- marks
                    <option value=#{show $ fromSqlKey mi}>#{markName ma}

            <div #tech-model-container .t186B__blockinput >
              <select #tech-model .combobox .t186B__input .t-input .input-large .form-control .blocks-input style="color:#000000; border:1px solid #000000;">
                <option selected=selected value="">FOCUS

            <div .t186B__blockinput #tech-separate-container >
              <div #tech-year-container >
                <select #tech-year .combobox .t186B__input .t-input .input-large .form-control .blocks-input style="color:#000000; border:1px solid #000000;">
                  <option selected=selected value="">2010

              <div #tech-generation-container .hide .tt >
                <span .tooltiptext #tech-generation-help-msg >
                <select #tech-generation .combobox .t186B__input .t-input .input-large .form-control .blocks-input style="color:#000000; border:1px solid #000000;">
                  <option selected=selected value="">I

            <div #tech-btn-wrapper .t186B__blockbutton style="width:30%; padding-top:1.5%;" >
              <form #form92677692 name="form92677692" role="form" method=POST action=@{TechR}>
                <input type=hidden name=tech-q #tech-q>
                <button #tech-form-submit type="submit" .t186B__submit .t-submit style="color:#000000; background-color:#b3ff80; ">УЗНАТЬ СЛАБЫЕ МЕСТА
    <style>
      \ #rec9267769 input::-webkit-input-placeholder {color:#000000; opacity: 0.5;}
      \ #rec9267769 input::-moz-placeholder {color:#000000; opacity: 0.5;}
      \ #rec9267769 input:-moz-placeholder {color:#000000; opacity: 0.5;}
      \ #rec9267769 input:-ms-input-placeholder {color:#000000; opacity: 0.5;}
|]  

wantMoreWeakPlaces :: forall (m :: * -> *) (t :: * -> *).
  (Foldable t, MonadIO m, MonadBaseControl IO m, MonadThrow m)
  => t (Entity Mark) -> WidgetT App m ()
wantMoreWeakPlaces marks = [whamlet|  <div #rec9387262 .r style="padding-top: 60px; padding-bottom: 0px; opacity: 0;" data-record-type="43" >
    <!-- T030 -->
    <div .t030>
      <div .t-container .t-align_center>
        <div .t-col .t-col_10 .t-prefix_1>
          <div .t030__title .t-title .t-title_xxs field="title">Хотите больше узнать про надежность?
          <div .t030__descr .t-descr .t-descr_md field="descr">Легко! О чем рассказать?
  ^{weakPlacesForm marks}
|]

retryWeakPlaces :: forall (m :: * -> *) (t :: * -> *).
  (Foldable t, MonadIO m, MonadBaseControl IO m, MonadThrow m)
  => t (Entity Mark) -> WidgetT App m ()
retryWeakPlaces marks = [whamlet|  <div .r data-record-type="43" #rec9313074 style="padding-top: 60px; padding-bottom: 0px; opacity: 0;">
    $#  T030 
    <div .t030>
      <div .t-container .t-align_center>
        <div .t-col .t-col_10 .t-prefix_1>
          <div .t030__title .t-title .t-title_xxs field="title">
            Не подходит? Почитайте про другую модель!
  ^{weakPlacesForm marks}
|]

guideButton :: forall (m :: * -> *).
  (MonadIO m, MonadBaseControl IO m, MonadThrow m)
  => WidgetT App m ()
guideButton = [whamlet|  <div #rec9277374 .r style="padding-top:30px;padding-bottom:30px;background-color:#f3f3f3; " data-record-type="209" data-bg-color="#f3f3f3" data-animationappear="off">
    <!-- T185 -->
    <div .t185>
      <div .t-container .t-container_flex >
        <div .t-col .t-col_flex .t-col_6 .t-prefix_2>
          <div .t-text .t-text_lg field="text">
            <div style="font-size:22px;" data-customstyle="yes">У Автоскептика есть целый 150-страничный гид для тех, кто собрался покупать подержанную машину
        <div .t185__butwrapper .t-col .t-col_2 >
          <a href=@{GuideR} target="" .t-btn style="display:table-cell;vertical-align:middle;color:#000000; background-color:#b3ff80; ">
            Скачать его
|]

simpleAdvice :: forall (m :: * -> *) site.
  (MonadIO m, MonadBaseControl IO m, MonadThrow m)
  => Entity SimpleAdvice -> WidgetT site m ()
simpleAdvice sa = [whamlet|  <div #rec9277367 .r style="padding-top: 75px; padding-bottom: 60px; opacity: 1;" data-record-type="47">
    <!-- T033 -->
    <div .t033>
      <div .t-container>
        <div .t-col .t-col_4 >
          <div .t033__lineTop>
          <div .t033__title .t-title field="title">Просто хороший совет
        <div .t-col .t-col_8 >
          <div .t033__descr .t-descr .t-descr_xl field="text">
            <div style="font-size:18px;" data-customstyle="yes">
              #{simpleAdviceContent $ entityVal sa}
|]

needHelp :: forall (m :: * -> *) site. (MonadIO m, MonadBaseControl IO m, MonadThrow m)
  => WidgetT site m ()  
needHelp = [whamlet|  <div #rec9309376 .r style="padding-top: 60px; padding-bottom: 60px; opacity: 1;" data-record-type="43">
    <!-- T030 -->
    <div .t030 #personal >
      <div .t-container .t-align_center>
        <div .t-col .t-col_10 .t-prefix_1> 
          <div .t030__title .t-title .t-title_xxs field="title">Нужна персональная помощь?
          <div .t030__descr .t-descr .t-descr_md field="descr">- Подберем машину под ваши требования и бюджет<br>- Отберем с вами лучшие предложения на рынке<br>- Выведем обманщиков-продавцов на чистую воду<br>- Осмотрим и протестируем машины, которые вы выберете<br>- Поможем с оформлением всех документов<br><br>Оставляйте заявку, мы с вами свяжемся и все обсудим!
  <div #rec9309299 .r style="padding-top: 0px; padding-bottom: 60px; opacity: 1;" data-record-type="298">
    <!-- T186C -->
    <div .t186C >
      <form #form9309299 name="form9309299" role="form" action="" method="POST" data-formactiontype="0" data-inputbox=".t186C__blockinput" .js-form-proccess> 
        <div .t-container>
          <div .t-col .t-col_8 .t-prefix_2 >
            <div .t186C__wrapper>
              <label .t-descr_md .t186C__blocktitle .t-descr style="" >
                E-mail
              <div .t186C__blockinput>
                <input name="email" .t186C__input .t-input placeholder="Чтобы мы могли написать вам ответ" data-req="1" data-rule="email" style="color:#000000; border:1px solid #000000; border-radius: 0px; -moz-border-radius: 0px; -webkit-border-radius: 0px;" type="text">
              <label .t186C__blocktitle .t-descr .t-descr_md style="" >
                Ваше имя
              <div .t186C__blockinput >
                <input name="name" .t186C__input .t-input placeholder="Как к вам обращаться?" data-req="1" data-rule="none" style="color:#000000; border:1px solid #000000; border-radius: 0px; -moz-border-radius: 0px; -webkit-border-radius: 0px;" type="text">
              <label .t186C__blocktitle .t-descr .t-descr_md style="" >
                Телефон
              <div .t186C__blockinput >
                <input name="phone" .t186C__input .t-input placeholder="Необязательно, но так быстрее" data-rule="phone" style="color:#000000; border:1px solid #000000; border-radius: 0px; -moz-border-radius: 0px; -webkit-border-radius: 0px;" type="text">
              <label .t186C__blocktitle .t-descr .t-descr_md style="" >
                Ваши пожелания
              <div .t186C__blockinput>
                <textarea name="comments" .t186C__input .t-input placeholder="Хочу хорошую машину. Помогите мне!" style="color:#000000; border:1px solid #000000; border-radius: 0px; -moz-border-radius: 0px; -webkit-border-radius: 0px;height:102px" rows="3">
              <div .t186C__blockbutton >
                <button type="submit" .t186C__submit .t-submit style="color:#000000; border:0 px solid #ffffff; background-color:#b3ff80; border-radius: 0px; -moz-border-radius: 0px; -webkit-border-radius: 0px;">
                  Отправить
    <style>
      \ #rec9309299 input::-webkit-input-placeholder {color:#000000; opacity: 0.5;}
      \ #rec9309299 input::-moz-placeholder {color:#000000; opacity: 0.5;}
      \ #rec9309299 input:-moz-placeholder {color:#000000; opacity: 0.5;}
      \ #rec9309299 input:-ms-input-placeholder {color:#000000; opacity: 0.5;} 
      \ #rec9309299 textarea::-webkit-input-placeholder {color:#000000; opacity: 0.5;}
      \ #rec9309299 textarea::-moz-placeholder {color:#000000; opacity: 0.5;}
      \ #rec9309299 textarea:-moz-placeholder {color:#000000; opacity: 0.5;}
      \ #rec9309299 textarea:-ms-input-placeholder {color:#000000; opacity: 0.5;} 
|]

header :: forall (m :: * -> *) a.
  (ToWidget App a, MonadIO m, MonadBaseControl IO m, MonadThrow m)
  => a -> WidgetT App m ()  
header m = [whamlet|  <div #rec9266707 .r data-animationappear="off" >
    <div #nav9266707marker >
      <div #nav9266707 .t461 .t461__positionstatic style="background-color: rgba(255,255,255,1); " data-bgcolor-hex="#ffffff" data-bgcolor-rgba="rgba(255,255,255,1)" data-navmarker="nav9266707marker" data-appearoffset="" data-bgopacity-two="" data-menushadow="" data-bgopacity="1" data-menu-items-align="center" data-menu="yes">
      <div .t461__maincontainer >
        <div .t461__topwrapper style="height:150px;" >
          <div .t461__logowrapper >
             <div .t461__logowrapper2 >
              <div style="display: block;">
                <a href=@{HomeR} style="color:#ffffff;">
                  <img src=@{StaticR img_AS_png} .t461__imglogo .t461__imglogomobile imgfield="img" style="max-width: 400px; width: 400px;" alt="Company">
          <div .t461__listwrapper .t461__mobilelist >
            <ul .t461__list >
              <li .t461__list_item onclick="scroller('.t186B');" >
                <span style="color:#424241;font-weight:600;letter-spacing:1px;text-transform:uppercase;" data-menu-item-number="1">Слабые места более 300 машин
              <li .t461__list_item onclick="scroller('#choice');" >
                <span style="color:#424241;font-weight:600;letter-spacing:1px;text-transform:uppercase;" data-menu-item-number="2">Правильно ли я выбрал авто?
              <li .t461__list_item onclick="scroller('.t033');" >
                <span style="color:#424241;font-weight:600;letter-spacing:1px;text-transform:uppercase;" data-menu-item-number="3">Лайфхаки покупателя
              <li .t461__list_item .hide>
                <span style="color:#424241;font-weight:600;letter-spacing:1px;text-transform:uppercase;" data-menu-item-number="4">Истории
              <li .t461__list_item onclick="scroller('#personal');" >
                <span style="color:#424241;font-weight:600;letter-spacing:1px;text-transform:uppercase;" data-menu-item-number="5">Помогите мне персонально!
          <div .t461__alladditional > 
            <div .t461__leftwrapper style=" padding-right:250px;">
              <div .t461__additionalwrapper >
                <div .t461__additional_social_links .t461__additionalitem >
                  <div .t461__additional_social_links_wrap >
                <div .t461__additional_social_links_item >
                  <a href="https://www.facebook.com/pirunpoika" target="_blank">
                    <svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" width="30px" height="30px" viewBox="0 0 48 48" enable-background="new 0 0 48 48" xml:space="preserve">
                      <desc>Facebook
                      <path d="M47.761,24c0,13.121-10.638,23.76-23.758,23.76C10.877,47.76,0.239,37.121,0.239,24c0-13.124,10.638-23.76,23.764-23.76C37.123,0.24,47.761,10.876,47.761,24M20.033,38.85H26.2V24.01h4.163l0.539-5.242H26.2v-3.083c0-1.156,0.769-1.427,1.308-1.427h3.318V9.168L26.258,9.15c-5.072,0-6.225,3.796-6.225,6.224v3.394H17.1v5.242h2.933V38.85z">
                <div .t461__additional_social_links_item >
                  <a href="https://vk.com/avtosceptic" target="_blank">
                    <svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" width="30px" height="30px" viewBox="0 0 48 48" enable-background="new 0 0 48 48" xml:space="preserve">
                      <desc>VK
                      <path d="M47.761,24c0,13.121-10.639,23.76-23.76,23.76C10.878,47.76,0.239,37.121,0.239,24c0-13.123,10.639-23.76,23.762-23.76C37.122,0.24,47.761,10.877,47.761,24 M35.259,28.999c-2.621-2.433-2.271-2.041,0.89-6.25c1.923-2.562,2.696-4.126,2.45-4.796c-0.227-0.639-1.64-0.469-1.64-0.469l-4.71,0.029c0,0-0.351-0.048-0.609,0.106c-0.249,0.151-0.414,0.505-0.414,0.505s-0.742,1.982-1.734,3.669c-2.094,3.559-2.935,3.747-3.277,3.524c-0.796-0.516-0.597-2.068-0.597-3.171c0-3.449,0.522-4.887-1.02-5.259c-0.511-0.124-0.887-0.205-2.195-0.219c-1.678-0.016-3.101,0.007-3.904,0.398c-0.536,0.263-0.949,0.847-0.697,0.88c0.31,0.041,1.016,0.192,1.388,0.699c0.484,0.656,0.464,2.131,0.464,2.131s0.282,4.056-0.646,4.561c-0.632,0.347-1.503-0.36-3.37-3.588c-0.958-1.652-1.68-3.481-1.68-3.481s-0.14-0.344-0.392-0.527c-0.299-0.222-0.722-0.298-0.722-0.298l-4.469,0.018c0,0-0.674-0.003-0.919,0.289c-0.219,0.259-0.018,0.752-0.018,0.752s3.499,8.104,7.463,12.23c3.638,3.784,7.764,3.36,7.764,3.36h1.867c0,0,0.566,0.113,0.854-0.189c0.265-0.288,0.256-0.646,0.256-0.646s-0.034-2.512,1.129-2.883c1.15-0.36,2.624,2.429,4.188,3.497c1.182,0.812,2.079,0.633,2.079,0.633l4.181-0.056c0,0,2.186-0.136,1.149-1.858C38.281,32.451,37.763,31.321,35.259,28.999">
            <div .t461__rightwrapper style=" padding-left:250px;">
              <div .t461__additionalwrapper >
                <div .t461__additional_descr .t461__additionalitem >
                  Автоскептик честный.
                  <br>
                  Он поможет выбрать автомобиль с пробегом.
                  <br>
                <div .t461__additional_buttons .t461__additionalitem >
                  <div .t461__additional_buttons_wrap >
                    <div .t461__additional_buttons_but >
                      <a href=@{AboutR} .t461__btn .t-btn style="color:#000000; background-color:#b3ff80; border-radius: 20px; -moz-border-radius: 20px; -webkit-border-radius: 20px;">
                        <table style="width:100%; height:100%;">
                          <tbody>
                            <tr>
                              <td>Об Автоскептике!
          <div .t461__middlelinewrapper >
            <div .t461__linewrapper >
              <hr .t461__horizontalline style=" background-color:#000000; opacity:0.1;">
          ^{m}

    <style>
      \ #rec9266707 .t461__list_item .t-active{
      \ color:#96f087 !important;
      \ }
    <style>
      \ @media screen and (max-width: 980px) {
      \ #rec9266707 .t461__leftcontainer{
      \ padding: 20px;
      \ }
      \ }
      \ @media screen and (max-width: 980px) {
      \ #rec9266707 .t461__imglogo{
      \ padding: 20px 0;
      \ }
      \ }
|]

withMenu :: forall (m :: * -> *) site. (MonadIO m, MonadBaseControl IO m, MonadThrow m)
  => WidgetT site m ()  
withMenu = [whamlet|          <div .t461__bottomwrapper .t461__menualign_center style=" height:100px;">
            <div .t461__listwrapper .t461__desktoplist >
              <ul .t461__list >
                <li .t461__list_item onclick="scroller('.t186B');" >
                  <span style="color:#424241;font-weight:600;letter-spacing:1px;text-transform:uppercase;" data-menu-item-number="1">Слабые места более 300 машин
                <li .t461__list_item onclick="scroller('#choice');" >
                  <span style="color:#424241;font-weight:600;letter-spacing:1px;text-transform:uppercase;" data-menu-item-number="2">Правильно ли я выбрал авто?
                <li .t461__list_item onclick="scroller('.t033');" >
                  <span style="color:#424241;font-weight:600;letter-spacing:1px;text-transform:uppercase;" data-menu-item-number="3">Лайфхаки покупателя
                <li .t461__list_item .hide>
                  <span style="color:#424241;font-weight:600;letter-spacing:1px;text-transform:uppercase;" data-menu-item-number="4">Истории
                <li .t461__list_item onclick="scroller('#personal');" >
                  <span style="color:#424241;font-weight:600;letter-spacing:1px;text-transform:uppercase;" data-menu-item-number="5">Помогите мне персонально!
|]

withoutMenu :: forall (m :: * -> *) site.
                  (MonadIO m, MonadBaseControl IO m, MonadThrow m) =>
                  WidgetT site m ()
withoutMenu = [whamlet|
|]

footer :: forall (m :: * -> *) site. (MonadIO m, MonadBaseControl IO m, MonadThrow m)
  => WidgetT site m ()
footer = [whamlet|  <div #rec9309867 .r style="padding-top:0px;padding-bottom:75px;background-color:#171717; " data-animationappear="off" data-record-type="452" data-bg-color="#171717">
    <!-- T452 -->
    <div .t452 #t-footer_9309867>
      <div .t452__maincontainer style="height: 80px;">
        <div .t452__content>
          <div .t452__col .t452__col_hiddenmobile>
            <div .t452__typo .t452__copyright .t-name .t-name_xs field="text" style="color: #ffffff">
              © 2016 Автоскептик. Помощь в выборе подержанных авто
          <div .t452__col .t452__col_center .t-align_center>
            <div .t452__right_social_links>
              <div .t452__right_social_links_wrap>
                <div .t452__right_social_links_item>
                  <a href="https://www.facebook.com/pirunpoika" target="_blank"> 
                    <svg style="fill:#ffffff;" version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" width="30px" height="30px" viewBox="0 0 48 48" enable-background="new 0 0 48 48" xml:space="preserve"> 
                      <desc>Facebook 
                      <path d="M47.761,24c0,13.121-10.638,23.76-23.758,23.76C10.877,47.76,0.239,37.121,0.239,24c0-13.124,10.638-23.76,23.764-23.76C37.123,0.24,47.761,10.876,47.761,24 M20.033,38.85H26.2V24.01h4.163l0.539-5.242H26.2v-3.083c0-1.156,0.769-1.427,1.308-1.427h3.318V9.168L26.258,9.15c-5.072,0-6.225,3.796-6.225,6.224v3.394H17.1v5.242h2.933V38.85z">
                <div .t452__right_social_links_item>
                  <a href="https://vk.com/avtosceptic" target="_blank"> 
                    <svg style="fill:#ffffff;" version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" width="30px" height="30px" viewBox="0 0 48 48" enable-background="new 0 0 48 48" xml:space="preserve"> 
                      <desc>VK
                      <path d="M47.761,24c0,13.121-10.639,23.76-23.76,23.76C10.878,47.76,0.239,37.121,0.239,24c0-13.123,10.639-23.76,23.762-23.76C37.122,0.24,47.761,10.877,47.761,24 M35.259,28.999c-2.621-2.433-2.271-2.041,0.89-6.25c1.923-2.562,2.696-4.126,2.45-4.796c-0.227-0.639-1.64-0.469-1.64-0.469l-4.71,0.029c0,0-0.351-0.048-0.609,0.106c-0.249,0.151-0.414,0.505-0.414,0.505s-0.742,1.982-1.734,3.669c-2.094,3.559-2.935,3.747-3.277,3.524c-0.796-0.516-0.597-2.068-0.597-3.171c0-3.449,0.522-4.887-1.02-5.259c-0.511-0.124-0.887-0.205-2.195-0.219c-1.678-0.016-3.101,0.007-3.904,0.398c-0.536,0.263-0.949,0.847-0.697,0.88c0.31,0.041,1.016,0.192,1.388,0.699c0.484,0.656,0.464,2.131,0.464,2.131s0.282,4.056-0.646,4.561c-0.632,0.347-1.503-0.36-3.37-3.588c-0.958-1.652-1.68-3.481-1.68-3.481s-0.14-0.344-0.392-0.527c-0.299-0.222-0.722-0.298-0.722-0.298l-4.469,0.018c0,0-0.674-0.003-0.919,0.289c-0.219,0.259-0.018,0.752-0.018,0.752s3.499,8.104,7.463,12.23c3.638,3.784,7.764,3.36,7.764,3.36h1.867c0,0,0.566,0.113,0.854-0.189c0.265-0.288,0.256-0.646,0.256-0.646s-0.034-2.512,1.129-2.883c1.15-0.36,2.624,2.429,4.188,3.497c1.182,0.812,2.079,0.633,2.079,0.633l4.181-0.056c0,0,2.186-0.136,1.149-1.858C38.281,32.451,37.763,31.321,35.259,28.999">
          <div .t452__col .t452__col_mobile>
            <div .t452__typo .t452__copyright .t-name .t-name_xs field="text" style="color: #ffffff">
              © 2016 Автоскептик. Помощь в выборе подержанных авто
          <div .t452__col>
            <div .t452__scroll .t-align_right>
              <a .t452__typo .t-name .t-name_xs .t452_scrolltop style="color: #ffffff" href="javascript:t452_scrollToTop();">
                Наверх
                <span .t452__icon>
                  <svg width="5px" viewBox="0 0 6 20" version="1.1">
                    <defs>
                    <g id="Welcome" stroke="none" stroke-width="1" fill="none" fill-rule="evenodd" sketch:type="MSPage">
                    <g id="Desktop-HD-Copy-39" sketch:type="MSArtboardGroup" transform="translate(-569.000000, -1797.000000)" fill="#ffffff">
                    <path d="M565.662286,1804.2076 L562.095536,1806.87166 C561.958036,1807.00916 561.958036,1807.16385 562.095536,1807.30135 L565.662286,1809.96541 C565.799786,1810.10291 565.941411,1810.0431 565.941411,1809.83616 L565.941411,1808.11741 L581.816411,1808.11741 L581.816411,1806.05491 L565.941411,1806.05491 L565.941411,1804.33616 C565.941411,1804.18147 565.866474,1804.1141 565.769536,1804.14297 C565.737224,1804.1526 565.696661,1804.17322 565.662286,1804.2076 Z" id="Shape" sketch:type="MSShapeGroup" transform="translate(571.904411, 1807.088000) rotate(-270.000000) translate(-571.904411, -1807.088000) ">|]

allClear :: forall (m :: * -> *). (MonadIO m, MonadBaseControl IO m, MonadThrow m)
  => WidgetT App m ()
allClear = [whamlet|  <div .r style="padding-top:15px;padding-bottom:15px;">
    <div .t-container>
      <form action=@{HomeR} method=GET>
        <div .t-col .t-col_8 .t-prefix_2 >
            <div .t186C__wrapper>
              <div .t186C__blockbutton >
                <button type=submit .t186C__submit .t-submit style="color:#000000; border:0 px solid #ffffff; background-color:#b3ff80; border-radius: 0px; -moz-border-radius: 0px; -webkit-border-radius: 0px;">
                  ВСЕ ПОНЯТНО! ПРОДОЛЖИТЬ
|]

getRandomSimpleAdvice :: forall site. (YesodPersist site, YesodPersistBackend site ~ SqlBackend)
  => HandlerT site IO [Entity SimpleAdvice]
getRandomSimpleAdvice = do
  let sql = "select ?? from (select * from simple_advice where id in (select id from (select q.cnt, sa.id, row_number() over (order by sa.id)-1 rn from simple_advice sa, (select trunc(random(0,count(1))) cnt from simple_advice) q) simple_advice where cnt = rn)) simple_advice"
  (sas :: [Entity SimpleAdvice]) <- runDB $ rawSql sql []
  return sas  

getRandomPromos :: forall site. (YesodPersist site, YesodPersistBackend site ~ SqlBackend)
  => HandlerT site IO [(Entity TextAdvise, Entity Image)]
getRandomPromos = do
  let sql = "with len as ( select count(1) cnt from text_advise ta where promo is not null and exists (select 1 from image where image.text_advise_id = ta.id) ), rans as ( select trunc(random(0, cnt)) rrn from len union select trunc(random(0, cnt)) rrn from len union select trunc(random(0, cnt)) rrn from len ), ext as ( select ta.id, row_number() over (order by ta.id)-1 rn from text_advise ta where promo is not null ), rants as ( select ext.id from rans, ext where rans.rrn = ext.rn ), imgs as ( select min(id) mid, text_advise_id from image group by text_advise_id ) select ??, ?? from ( select * from text_advise ta where id in (select id from rants) ) text_advise, ( select * from image where id in (select mid from imgs) ) image where text_advise.id = image.text_advise_id"
  (tas :: [(Entity TextAdvise, Entity Image)]) <- runDB $ rawSql sql []
  return tas

repairStatic2 :: forall a. (IsString a, Semigroup a) => a -> a
repairStatic2 x = "../.." <> x

repairStatic :: forall a. (IsString a, Semigroup a) => a -> a
repairStatic x = ".." <> x

repairStatic0 :: forall a. (IsString a, Semigroup a) => a -> a
repairStatic0 x = "." <> x


-----------------------------------------------------------

containedIn :: forall (f :: * -> *) b b1.
               (Eq b1, Functor f, IsSequence b) =>
               f b1 -> (Element b -> b1) -> b -> f b
containedIn xs f y = fmap (\x -> filter ((==x) . f) y) xs

equalTo :: forall seq b.
           (Eq b, IsSequence seq) =>
           b -> (Element seq -> b) -> seq -> seq
equalTo x f y = filter((==x) . f) y

getTagByMark :: Entity Mark -> [Entity Generation] -> [Entity LkTag] -> Maybe (Entity LkTag)
getTagByMark m gs tags =
  let fgs = equalTo (entityKey m) (generationMarkId . entityVal) gs
      ftags = concat $ containedIn (fmap entityKey fgs) (lkTagGeneration . entityVal) tags
  in maybeFirst ftags

getTagByModel :: Entity Model -> [Entity Generation] -> [Entity LkTag] -> Maybe (Entity LkTag)
getTagByModel m gs tags =
  let fgs = equalTo (entityKey m) (generationModelId . entityVal) gs
      ftags = concat $ containedIn (fmap entityKey fgs) (lkTagGeneration . entityVal) tags
  in maybeFirst ftags

getTextAdviseByTag :: [Entity TextAdvise] -> Entity LkTag -> Entity TextAdvise
getTextAdviseByTag tas tag =
  (equalTo (lkTagTextAdviseId $ entityVal tag) (entityKey) tas) L.!! 0

getImageByTextAdvise :: [Entity Image] -> Entity TextAdvise -> Maybe (Entity Image)
getImageByTextAdvise imgs ta =
  let fimgs = equalTo (entityKey ta) (imageTextAdviseId . entityVal) imgs
  in maybeFirst fimgs

maybeFirst :: forall a. [a] -> Maybe a
maybeFirst fs = if null fs then Nothing else Just (fs L.!! 0)

getMarkWithContent :: Entity Mark
  -> [Entity Generation] -> [Entity LkTag] -> [Entity Image]
  -> [Entity TextAdvise]
  -> (Entity Mark, Maybe (Entity Image), Maybe (Entity TextAdvise))
getMarkWithContent ma gs tags imgs tas =
  let mtag = getTagByMark ma gs tags
      mta = fmap (getTextAdviseByTag tas) mtag
      mimg = fmap (getImageByTextAdvise imgs) mta
      mimg2 = case mimg of
                Nothing -> Nothing
                Just m  -> m
  in (ma, mimg2, mta)

getModelWithContent :: Entity Model
  -> [Entity Generation] -> [Entity LkTag] -> [Entity Image]
  -> [Entity TextAdvise]
  -> (Entity Model, Maybe (Entity Image), Maybe (Entity TextAdvise))
getModelWithContent mo gs tags imgs tas =
  let mtag = getTagByModel mo gs tags
      mta = fmap (getTextAdviseByTag tas) mtag
      mimg = fmap (getImageByTextAdvise imgs) mta
      mimg2 = case mimg of
                Nothing -> Nothing
                Just m  -> m
  in (mo, mimg2, mta)

existenceCheck :: (a,Maybe b,Maybe c) -> Bool
existenceCheck (_,Nothing,_) = False
existenceCheck (_,_,Nothing) = False
existenceCheck (_,_,_) = True

mFromMaybe :: forall t t1 t2.
                 (t, Maybe t1, Maybe t2) -> (t, t1, t2)
mFromMaybe (a, Just b, Just c) = (a,b,c)

byKey :: forall record a. (Integral a, ToBackendKey SqlBackend record)
  => a -> Key record -> Bool
byKey x = (==(toSqlKey $ fromIntegral x))

showQueryAge :: Entity Mark -> Entity Model -> Entity Age -> Text
showQueryAge ma mo ag =
  T.concat
    [ markName (entityVal ma)
    , " "
    , modelName (entityVal mo)
    , " "
    , T.pack $ show $ ageAge (entityVal ag)
    , " года"
    ] 

showQueryGen :: Entity Mark -> Entity Model -> Entity Generation -> Text
showQueryGen ma mo ag =
  T.concat
    [ markName (entityVal ma)
    , " "
    , modelName (entityVal mo)
    , " "
    , fromMaybeText $ generationGeneration (entityVal ag)
    ] 

fromMaybeText :: forall a. IsString a => Maybe a -> a
fromMaybeText Nothing = ""
fromMaybeText (Just x) = x


ageToFront :: forall t. Num t => Entity Age -> (Text, t)
ageToFront = toFront (pack . show . ageAge)

modelToFront :: forall t. Num t => Entity Model -> (Text, t)
modelToFront = toFront modelName

generationToFront :: forall t. Num t => Entity Generation -> (Text, t)
generationToFront = toFront (\x -> fromJust (generationGeneration x) (generationBottomAge x) (generationTopAge x))

toFront :: forall t t1 b. (Num t1, ToBackendKey SqlBackend b) => (b -> t) -> Entity b -> (t, t1)
toFront a = \x -> (a . entityVal $ x, fromIntegral . fromSqlKey . entityKey $ x)

fromJust :: forall t t1. (Show t, Show t1) => Maybe Text -> Maybe t -> Maybe t1 -> Text
fromJust Nothing Nothing Nothing = ""
fromJust Nothing Nothing (Just a) = toText a
fromJust Nothing (Just a) Nothing = toText a
fromJust Nothing (Just a) (Just b) = concat [toText a, " - ", toText b]
fromJust (Just a) Nothing Nothing = a
fromJust (Just a) (Just b) Nothing = concat [toText a, ": ", toText b]
fromJust (Just a) Nothing (Just b) = concat [toText a, ": ", toText b]
fromJust (Just a) (Just b) (Just c) = concat [a, ": ", toText b, " - ", toText c]


toText :: forall t. Show t => t -> Text
toText = pack . show


---------------------------------------
-- User checking
--------------------------------------

minusDay :: Int
minusDay = negate $ 1 * 60 * 60 * 24


-- function result is not maybe
frinm :: forall (m :: * -> *) a. MonadIO m
  => m (Maybe a) -> m (Bool, Maybe a)
frinm f = let fun = f in do
  res <- fun
  liftIO $ return $ isNotMaybe res

isNotUser :: forall master. YesodAuth master
             => HandlerT master IO (Bool, Maybe (AuthId master))
isNotUser = frinm (maybeAuthId)

processCookie :: forall (m :: * -> *). MonadHandler m => Text -> m ()
processCookie n = do
  cks <- lookupCookie n
  sendCookie n (fmap (B.pack . T.unpack) cks)

createCookie :: ByteString -> ByteString -> SetCookie
createCookie name val = def {setCookieName = name, setCookieValue = val }

sendCookie :: forall (m :: * -> *). MonadHandler m => Text -> Maybe ByteString -> m ()
sendCookie n Nothing = setCookie $ createCookie (B.pack $ T.unpack n) "1"
sendCookie n (Just "1") = do
  deleteCookie n ""
  setCookie $ createCookie (B.pack $ T.unpack n) "2"
sendCookie n (Just a) = do
  deleteCookie n ""
  setCookie $ createCookie (B.pack $ T.unpack n) a

adminPass :: forall t. t -> Text -> HandlerT App IO Html -> HandlerT App IO Html
adminPass ep url f = do
  ia <- isAdmin
  let execContinue = f
  sess <- getSessionState url
  if (ia == Authorized) then execContinue else (youShallNotPass ep sess f)

youShallNotPass :: forall t. t -> SessionState -> HandlerT App IO Html -> HandlerT App IO Html
youShallNotPass ep sess f = executeIfAction f $ actionTree ep sess 

actionTree :: forall a a1. a1 -> SessionState -> IfActionTree a (EndPoint a1)
actionTree ep = \sess ->
  IfAction
    { ifCond = decide sess IsUser
    , thenBranch =
        IfAction
          { ifCond = decide sess UserDailyLimitReached
          , thenBranch = TreeExit UserDailyLimit
          , elseBranch = TreeExit (Continue ep)
          }
    , elseBranch =
        IfAction
          { ifCond = decide sess IsCookieInstalled
          , thenBranch = guestSubTree ep sess              
          , elseBranch =
              IfAction
                { ifCond = decide sess IsIPUARegistered
                , thenBranch = TreeExit (Captcha ep)
                , elseBranch = guestSubTree ep sess
                }
          }
    }

guestSubTree :: forall a a1. a1 -> SessionState -> IfActionTree a (EndPoint a1)
guestSubTree ep = \sess ->
  IfAction
    { ifCond = decide sess GuestSeenIt
    , thenBranch = TreeExit (Continue ep)
    , elseBranch =
        IfAction
          { ifCond = decide sess GuestFreeLimitReached
          , thenBranch =
              IfAction
                { ifCond = decide sess GuestReposted
                , thenBranch =
                    IfAction
                      { ifCond = decide sess GuestRepostLimitReached
                      , thenBranch = TreeExit (GuestNoMoreReposts)
                      , elseBranch = TreeExit (Continue ep)
                      }
                , elseBranch = TreeExit (GuestPayOrRepost ep)
                }
          , elseBranch = TreeExit (Continue ep)
          }
    }
decide :: SessionState -> Decision -> Bool
decide (SessionState {sUser = SUser _ _}) IsUser = True
decide (SessionState {sUser = SGuest _ _ _ _ _ _п}) IsUser = False
decide (SessionState {dailyLimit = dl, sUser = SUser {sCurrentDaily = cd }})
  UserDailyLimitReached = cd >= dl
decide (SessionState {sUser = SGuest {sSessionInstalled = i}}) IsCookieInstalled = i
decide (SessionState {sUser = SGuest {sCurrentIPUA = i }}) IsIPUARegistered = i > 0
decide (SessionState {sUser = SGuest {sAlreadySeen = i }}) GuestSeenIt = i /= 0
decide (SessionState {sUser = SGuest {sCurrentWatch = i}, watchTechLimit = tl, watchAdviseLimit = al}) GuestFreeLimitReached = or [i >= tl, i >= al]
decide (SessionState {sUser = SGuest {sCurrentRepost = i }}) GuestReposted = i /= 0
decide (SessionState {sUser = SGuest {sCurrentRepost = i }, repostLimit = rl})
  GuestRepostLimitReached = i >= rl


data IfActionTree a b =
  TreeExit b |
  IfAction { ifCond :: Bool
           , thenBranch :: IfActionTree a b
           , elseBranch :: IfActionTree a b
           }

executeIfAction :: forall t a. HandlerT App IO Html -> IfActionTree a (EndPoint t) -> HandlerT App IO Html
executeIfAction f (TreeExit (Continue i)) = f
executeIfAction _ (TreeExit i) = proceedWith i
executeIfAction f i =
  let a = ifCond i
      t = executeIfAction f (thenBranch i)
      e = executeIfAction f (elseBranch i)
  in if a then t else e

captcha :: HandlerT App IO Html
captcha = redirect CaptchaR

proceedWith :: forall t. EndPoint t -> HandlerT App IO Html
proceedWith (Captcha a) = captcha
proceedWith ep = do
  c <- getCache
  (sAdvice':_) <- getRandomSimpleAdvice
  let regions = getRegions c
      marks   = getMarks c

  defaultLayout $ do
    addStylesheet $ StaticR css_bootstrap_css
    setTitle "Рекомендация Автоскептика"
    chooseWidget (sAdvice', regions, marks) ep
    $(fayFile "HomeF")

chooseWidget :: forall t (m :: * -> *) (t1 :: * -> *) (t2 :: * -> *). (Foldable t1, Foldable t2, MonadIO m, MonadBaseControl IO m, MonadThrow m) => (Entity SimpleAdvice, t1 (Entity Region), t2 (Entity Mark)) -> EndPoint t -> WidgetT App m ()
chooseWidget (sAdvice', regions, marks) UserDailyLimit = $(widgetFile "dailylimitpopup")
chooseWidget (sAdvice', regions, marks) (GuestPayOrRepost a) = $(widgetFile "freepopup")
chooseWidget (sAdvice', regions, marks) GuestNoMoreReposts = $(widgetFile "nomorepopup")

divideThree :: forall a. [a] -> [[a]]
divideThree [] = [[]]
divideThree x = [take 3 x] ++ divideThree (drop 3 x) 
