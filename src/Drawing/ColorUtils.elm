module ColorUtils exposing
  ( hexToColor
  , colorToHex
  )

import Html
import String
import Color exposing (Color)
import Char


hexToColor : String -> Color
hexToColor rgb =
  case (String.split "" rgb) of
    _ :: r1 :: r2 :: g1 :: g2 :: b1 :: b2 :: [] ->
      let
        r = (toNumber r1) * 16 + toNumber r2
        g = (toNumber g1) * 16 + toNumber g2
        b = (toNumber b1) * 16 + toNumber b2
      in
        Color.rgb r g b
    _ ->
      Color.red


colorToHex : Color -> String
colorToHex color =
  let
    { red, green, blue } = Color.toRgb color
  in
    "#"
    ++ toChar (red // 16)
    ++ toChar (red % 16)
    ++ toChar (green // 16)
    ++ toChar (green % 16)
    ++ toChar (blue // 16)
    ++ toChar (blue % 16)


toNumber = toLowerChar >> charToInt


toChar = intToChar >> String.fromChar


aCode = (Char.toCode 'a')


zeroCode = (Char.toCode '0')


toLowerChar : String -> Char
toLowerChar str =
  case String.uncons str of
    Just (result, _) ->
      Char.toLower result
    Nothing ->
      '0'


charToInt : Char -> Int
charToInt char =
  if Char.isDigit char then
    (Char.toCode char) - zeroCode
  else if Char.isHexDigit char then
    (Char.toCode char) - aCode + 10
  else
    0


intToChar : Int -> Char
intToChar num =
  if num <= 15 && num >= 10 then
    Char.fromCode (num - 10 + aCode)
  else if num >= 0 then
    Char.fromCode (num + zeroCode)
  else
    '0'
