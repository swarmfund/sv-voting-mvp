module SecureVote.Utils.Markdown exposing (..)

import Markdown
import Markdown.Config exposing (Options, defaultOptions)

toHtmlWHardLineBreaks = Markdown.toHtml (Just <| { defaultOptions | softAsHardLineBreak = True })

toHtml = Markdown.toHtml Nothing
