module SecureVote.SPAs.SwarmMVP.Views.SetDelegateV exposing (..)

import Html exposing (Html, p, span, text)
import Html.Attributes exposing (class)
import Material.Card as Card
import Material.Color as Color
import Material.Options as Options exposing (cs, css)
import Material.Textfield as Textf
import Material.Typography exposing (display2)
import Maybe.Extra exposing ((?), isNothing)
import Monocle.Common exposing ((=>), dict, maybe)
import SecureVote.Ballots.Types exposing (BallotSpec)
import SecureVote.Components.UI.Btn exposing (BtnProps(..), btn)
import SecureVote.Components.UI.FullPageSlide exposing (fullPageSlide)
import SecureVote.Components.UI.Typo exposing (headline)
import SecureVote.Eth.Utils exposing (isValidEthAddress)
import SecureVote.SPAs.SwarmMVP.Ballots.Types exposing (BallotParams)
import SecureVote.SPAs.SwarmMVP.Helpers exposing (defaultDelegate, dlgtAddrField, getBoolField, getDelegateAddress, getField, setBoolField)
import SecureVote.SPAs.SwarmMVP.Model exposing (..)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(..), ToCurve25519Msg(..))
import SecureVote.SPAs.SwarmMVP.Routes exposing (Route(SwmSubmitR))


delegateExplanationCopy : String -> Model -> List String
delegateExplanationCopy bHash model =
    let
        token =
            (mErc20Abrv bHash).get model
    in
    [ "You may optionally select a delegate, though this isn't required. If you would like to, please enter their " ++ token ++ " token address below. "
    , "If your delegate casts a vote, your votes will be replaced with the votes that your delegate chooses. "
    , "If your delegate does not cast a vote, your vote will be cast with the options you have selected. "
    ]


delegateView : Model -> ( String, BallotSpec ) -> Html Msg
delegateView model ( bHash, currBallot ) =
    let
        btnDisabled =
            if addrErr then
                Disabled
            else
                BtnNop

        ( addrErr, addrErrMsg ) =
            validAddress (getField dlgtAddressK model)

        dlgtSetting =
            "setting.delegate." ++ bHash

        dlgtAddressK =
            dlgtAddrField bHash

        clickMsgs_ extra =
            MultiMsg <|
                extra
                    ++ [ ConstructBallotPlaintext
                       , PageGoForward SwmSubmitR
                       ]

        clickMsgs =
            clickMsgs_ []

        showDlgtFieldMsg =
            setBoolField dlgtSetting True

        showDlgtField =
            getBoolField model dlgtSetting ? False

        dlgtForm =
            if showDlgtField then
                [ Textf.render Mdl
                    [ 7674564333 ]
                    model.mdl
                    [ Options.onInput <| SetField dlgtAddressK
                    , Textf.label "Delegate's Address (optional)"
                    , Textf.floatingLabel
                    , Textf.value <| getField dlgtAddressK model ? ""
                    , Textf.error addrErrMsg |> Options.when addrErr
                    , css "max-width" "400px"
                    , cs "w-100 db center"
                    ]
                    []
                , btn 9458439437 model [ SecBtn, Attr (class "ma2"), Click <| clickMsgs_ [ RemoveFieldVal dlgtAddressK ] ] [ text "Skip" ]
                , btn 5475855442 model [ PriBtn, Attr (class "ma2"), Click clickMsgs, btnDisabled ] [ text "Continue" ]
                ]
            else
                [ btn 394839423 model [ SecBtn, Attr (class "ma2"), Click showDlgtFieldMsg ] [ text "Set a Delegate" ]
                , btn 945839437 model [ PriBtn, Attr (class "ma2"), Click clickMsgs ] [ text "Skip" ]
                ]
    in
    fullPageSlide 3453456456
        model
        "Choose a Delegate"
    <|
        [ p [ class "mw7 center" ] [ text <| String.concat <| delegateExplanationCopy bHash model ]
        ]
            ++ dlgtForm


validAddress : Maybe String -> ( Bool, String )
validAddress delegateAddress =
    case delegateAddress of
        Nothing ->
            ( False, "Please paste in your Delegate's Eth address" )

        Just addr ->
            if isValidEthAddress addr then
                ( False, "Address valid!" )
            else if addr == "" then
                ( False, "You will have no delegate" )
            else
                ( True, "Invalid address" )
