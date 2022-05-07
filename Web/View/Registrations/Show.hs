module Web.View.Registrations.Show where
import Web.View.Prelude

data ShowView = ShowView { registration :: Registration }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show Registration</h1>
        <p>{registration}</p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Registrations" RegistrationsAction
                            , breadcrumbText "Show Registration"
                            ]