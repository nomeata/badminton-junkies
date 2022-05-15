module Web.View.Sessions.New where
import Web.View.Prelude
import IHP.AuthSupport.View.Sessions.New

data LoginView = LoginView { login_data :: LoginData }

instance View LoginView where
    html LoginView { .. } = [hsx|
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
