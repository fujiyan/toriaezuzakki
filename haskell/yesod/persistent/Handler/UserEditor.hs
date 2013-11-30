module Handler.UserEditor where

import Import


userEditorForm :: Text -> Maybe UserId -> Maybe User -> Html -> MForm Handler (FormResult (Text, Maybe Text, User), Widget)
userEditorForm verb userId user = renderDivs $ (,,)
    <$> areq hiddenField "" (Just verb)
    <*> aopt hiddenField "" (Just <$> (toPathPiece <$> userId))
    <*> (User
            <$> areq textField "ユーザID" (userIdent <$> user)
            <*> aopt passwordField "パスワード" (userPassword <$> user)
        )


getUserEditorR :: Handler Html
getUserEditorR = do
    maybeUserId <- lookupGetParam "userId"
    case maybeUserId of
        Nothing -> do
            (widget, enctype) <- generateFormPost $ userEditorForm "add" Nothing Nothing
            defaultLayout $(widgetFile "user-editor")
        Just userIdText ->
            case (fromPathPiece userIdText) of
                Just userId -> do
                    user <- runDB $ get404 userId
                    (widget, enctype) <- generateFormPost $ userEditorForm "modify" (Just userId) (Just user)
                    defaultLayout $(widgetFile "user-editor")
                Nothing -> notFound


postUserEditorR :: Handler Html
postUserEditorR = do
    ((result, _), _) <- runFormPost $ userEditorForm "" Nothing Nothing
    case result of
        FormSuccess (verb, maybeUserId, user) ->
            case verb of
                "add" -> do
                    _ <- runDB $ insert user
                    redirect UserListR
                "modify" ->
                    case (fromPathPiece =<< maybeUserId) of
                        Just userId -> do
                            _ <- runDB $ do
                                _ <- get404 userId
                                replace userId user
                            redirect UserListR
                        Nothing -> undefined
                _ -> undefined
        _ -> undefined
