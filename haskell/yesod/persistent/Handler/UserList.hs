module Handler.UserList where

import Import


userListForm :: Text -> Maybe UserId -> Html -> MForm Handler (FormResult (Text, Maybe Text), Widget)
userListForm verb userId = renderDivs $ (,)
    <$> areq hiddenField "" (Just verb)
    <*> aopt hiddenField (FieldSettings "" Nothing Nothing (Just "userId") []) (Just <$> (toPathPiece <$> userId))


getUserListR :: Handler Html
getUserListR = do
    users <- runDB $ selectList [] [Asc UserIdent]
    (widget, enctype) <- generateFormPost $ userListForm "delete" Nothing
    defaultLayout $(widgetFile "user-list")


postUserListR :: Handler Html
postUserListR = do
    ((result, _), _) <- runFormPost $ userListForm "" Nothing
    case result of
        FormSuccess (verb, maybeUserId) ->
            case verb of
                "delete" ->
                    case (fromPathPiece =<< maybeUserId) of
                        Just userId -> do
                            _ <- runDB $ delete (userId :: UserId)
                            redirect UserListR
                        Nothing -> undefined
                _ -> undefined
        _ -> undefined
