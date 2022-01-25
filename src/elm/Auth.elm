module Auth exposing (parseFiat, serializeFiat)

import Base64



-- Just for testing: Fiat Authorization
-- With fiat authorization, the client tells the server who they are and the server believes it, no questions asked!


parseFiat headerValue =
    if String.startsWith "Basic " headerValue then
        String.dropLeft 6 headerValue
            |> Base64.decode
            |> Result.toMaybe

    else
        Nothing


serializeFiat username =
    "Basic " ++ Base64.encode (username ++ ":")
