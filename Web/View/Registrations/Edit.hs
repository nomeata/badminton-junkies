module Web.View.Registrations.Edit where
import Web.View.Prelude

data EditView = EditView { registration :: Registration }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit Registration</h1>
        {renderForm registration}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Registrations" RegistrationsAction
                , breadcrumbText "Edit Registration"
                ]

renderForm :: Registration -> Html
renderForm registration = formFor registration [hsx|
    {(textField #playerName)}
    {(textField #date)}
    {submitButton}

|]