module Web.View.Registrations.New where
import Web.View.Prelude

data NewView = NewView { registration :: Registration }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New Registration</h1>
        {renderForm registration}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Registrations" RegistrationsAction
                , breadcrumbText "New Registration"
                ]

renderForm :: Registration -> Html
renderForm registration = formFor registration [hsx|
    {(textField #playerName)}
    {(textField #date)}
    {submitButton}

|]