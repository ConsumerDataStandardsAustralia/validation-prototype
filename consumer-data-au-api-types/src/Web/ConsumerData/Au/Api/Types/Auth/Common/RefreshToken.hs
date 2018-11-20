module Web.ConsumerData.Au.Api.Types.Auth.Common.RefreshToken where

-- Refresh tokens are intended only for Auth server, and can also be opaque to the client
data RefreshToken = Text
