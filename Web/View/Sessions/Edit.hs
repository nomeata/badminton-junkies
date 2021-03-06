module Web.View.Sessions.Edit where
import Web.View.Prelude

data EditView = EditView {
    login_data :: LoginData
}

instance View EditView where
    html EditView{..} = case fromFrozenContext :: Maybe SessionData of
        Nothing -> loginView login_data
        Just sd -> editView sd

editView sd = [hsx|
    <div class="h-100" id="sessions-new">
        <div class="d-flex align-items-center">
            <div class="w-100">
                <div style="max-width: 400px" class="mx-auto mb-5">
                    <h3>Hello, {fullName sd}</h3>

                    <p>You can change your nickname on this platform here:</p>

                    {changeNickname (nickname sd)}

                    <hr/>

                    {changeActing (actingFor sd)}

                    <hr/>

                    <p>
                      <a class="btn btn-secondary js-delete js-delete-no-confirm" href={DeleteSessionAction}>Log out</a>
                    </p>
                </div>
            </div>
        </div>
    </div>
 |]

changeNickname nickname = [hsx|
    <form class="form-group" method="POST" action={ChangeNameAction}>
      <div class="input-group">
        <input name="nickname" value={nickname} placeholder="Nickname" required="required" autofocus="autofocus" class="form-control" />
        <div class="input-group-append">
         <button type="submit" class="btn btn-primary">Change</button>
        </div>
     </div>
    </form>

    <p>By using different names, you can register twice. Please do not abuse this.</p>
 |]

changeActing (Just af) = [hsx|
    <p>You are currently acting for {af}.</p>
    <p>
      <a class="btn btn-primary" href={StopActingAction}>Stop doing that</a>
    </p>
 |]

changeActing Nothing = [hsx|
    <p>You can also act in someone else's name, for example to register guests. Please do not abuse this either.</p>

    <form class="form-group" method="POST" action={ChangeActingAction}>
      <div class="input-group">
        <input name="acting_for" placeholder="Someone else" required="required" autofocus="autofocus" class="form-control"/>
        <div class="input-group-append">
         <button type="submit" class="btn btn-primary">Change</button>
        </div>
     </div>
    </form>
 |]



loginView login_data = [hsx|
        <div class="h-100" id="sessions-new">
            <div class="d-flex align-items-center">
                <div class="w-100">
                    <div style="max-width: 400px" class="mx-auto mb-5">
                        <h3>Login</h3>

                        <p>Please use the same email address and password as on <a href="https://web.meinverein.de">meinverein.de</a>. You can register or change your password there.</p>
                        {renderForm login_data}
                    </div>
                </div>
            </div>
        </div>
    |]

renderForm :: LoginData -> Html
renderForm login_data = [hsx|
    <form method="POST" action={CreateSessionAction}>
        <div class="form-group">
            <input name="email" value={get #email login_data} type="email" class="form-control" placeholder="E-Mail" required="required" autofocus="autofocus" />
        </div>
        <div class="form-group">
            <input name="password" type="password" class="form-control" placeholder="Password"/>
        </div>
        <button type="submit" class="btn btn-primary btn-block">Login</button>
    </form>
|]
