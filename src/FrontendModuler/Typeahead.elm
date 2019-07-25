module FrontendModuler.Typeahead exposing (Suggestion, Typeahead, TypeaheadOptions, toHtml, typeahead, withSuggestions)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Typeahead msg
    = Typeahead
        { label : String
        , msg : String -> msg
        , innhold : String
        , suggestions : List (Suggestion msg)
        }


type alias TypeaheadOptions msg =
    { msg : String -> msg
    , label : String
    }


typeahead : TypeaheadOptions msg -> String -> Typeahead msg
typeahead options innhold =
    Typeahead
        { label = options.label
        , msg = options.msg
        , innhold = innhold
        , suggestions = []
        }


type alias Suggestion msg =
    { innhold : String
    , onActive : msg
    , onClick : msg
    , active : Bool
    }


withSuggestions : List (Suggestion msg) -> Typeahead msg -> Typeahead msg
withSuggestions suggestions (Typeahead options) =
    Typeahead { options | suggestions = suggestions }



--<SkjemaGruppe
--        feil={this.props.feilmelding ? { feilmelding: this.props.feilmelding } : undefined}
--        className={classNames('typeahead', this.props.className)}>
--- SKJEMAGRUPPE
--<div className={cls(className, feil)} {...other}>
--    { title && this.renderTitle() }
--    {children}
--    <SkjemaelementFeilmelding feil={feil} />
--</div>
--<input
--    id={this.props.id}
--    role="combobox"
--    aria-autocomplete="list"
--    aria-controls={`${this.props.id}-suggestions`}
--    aria-owns={`${this.props.id}-suggestions`}
--    aria-expanded={showSuggestions}
--    aria-haspopup={showSuggestions}
--    aria-activedescendant={`${this.props.id}-item-${this.state.activeSuggestionIndex}`}
--    placeholder={this.props.placeholder}
--    value={this.state.value}
--    autoComplete="off"
--    onChange={this.onChange}
--    onBlur={this.onBlur}
--    onKeyDown={this.onKeyDown}
--    onFocus={this.onFocus}
--    ref={input => {
--        this.input = input;
--    }}
--    className="skjemaelement__input input--fullbredde typo-normal"
--/>
--<ul
--    id={`${this.props.id}-suggestions`}
--    role="listbox"
--    className={showSuggestions ? '' : 'typeahead-suggestions-hidden'}
--    onMouseLeave={this.onMouseLeave}
--    onMouseEnter={this.onMouseEnter}>
--    {showSuggestions &&
--        this.props.suggestions.map((li, i) => (
--            <TypeaheadSuggestion
--                id={`${this.props.id}-item-${i}`}
--                key={li}
--                index={i}
--                value={li}
--                match={this.state.value}
--                active={i === this.state.activeSuggestionIndex}
--                onClick={this.selectSuggestion}
--                highlightSuggestion={this.highlightSuggestion}
--                avoidBlur={this.avoidBlur}
--            />
--        ))}
--</ul>


toHtml : Typeahead msg -> Html msg
toHtml (Typeahead options) =
    div [ class "typeahead" ]
        [ input
            [ onInput options.msg
            , value options.innhold
            , class "skjemaelement__input input--fullbredde"
            ]
            []
        , if List.isEmpty options.suggestions then
            text ""

          else
            options.suggestions
                |> List.map viewSuggestion
                |> ul []
        ]


viewSuggestion : Suggestion msg -> Html msg
viewSuggestion suggestion =
    li
        [ onClick suggestion.onClick
        , onMouseEnter suggestion.onActive
        ]
        [ span [ classList [ ( "typetext", True ), ( "active", suggestion.active ) ] ]
            [ text suggestion.innhold ]
        ]
