type rgba = {
  r: int,
  g: int,
  b: int,
  a: option(float),
};

type hsv = {
  h: int,
  s: int,
  v: int,
};

type hsl = {
  h: int,
  s: int,
  l: int,
};

let string_of_hex = hex => "#" ++ hex;

let string_of_rgba = c => {
  let {r, g, b, a} = c;
  switch (a) {
  | Some(alpha) => {j|rgba($r, $g, $b, $alpha)|j}
  | None => {j|rgba($r, $g, $b, 1)|j}
  };
};

let maxColorSaturation = 100;
let maxColorHue = 359;
let maxColorValue = 100;
let maxColorRgba = 255;

let numberToPaddedHex = n => {
  let hex = Js.Int.toStringWithRadix(n, ~radix=16);
  String.length(hex) == 1 ? "0" ++ hex : hex;
};

let toRgba = (r, g, b, a) => {r, g, b, a};

let toDecimal = hex => Js.Float.fromString("0x" ++ hex) |. int_of_float;

let char2Decimal = c => String.make(1, c) |. toDecimal;

let hex2rgba = (~alpha=None, s) => {
  let l = String.length(s);
  switch (l) {
  | 3 =>
    toRgba(
      char2Decimal(s.[0]),
      char2Decimal(s.[1]),
      char2Decimal(s.[2]),
      alpha,
    )
  | 4 =>
    toRgba(
      char2Decimal(s.[1]),
      char2Decimal(s.[2]),
      char2Decimal(s.[3]),
      alpha,
    )
  | 6 =>
    let ri = char2Decimal(s.[0]) * 16 + char2Decimal(s.[1]);
    let gi = char2Decimal(s.[2]) * 16 + char2Decimal(s.[3]);
    let bi = char2Decimal(s.[4]) * 16 + char2Decimal(s.[5]);
    toRgba(ri, gi, bi, alpha);
  | 7 =>
    let ri = char2Decimal(s.[1]) * 16 + char2Decimal(s.[2]);
    let gi = char2Decimal(s.[3]) * 16 + char2Decimal(s.[4]);
    let bi = char2Decimal(s.[5]) * 16 + char2Decimal(s.[6]);
    toRgba(ri, gi, bi, alpha);
  | _ => toRgba(0, 0, 0, alpha)
  };
};

let floori = n => floor @@ float_of_int(n) |. int_of_float;

let roundi = n => Js.Math.round @@ float_of_int(n) |. int_of_float;

let rgb2hex = c =>
  [numberToPaddedHex(c.r), numberToPaddedHex(c.g), numberToPaddedHex(c.b)]
  |> String.concat("");

let rgb2hsv = c => {
  let h = ref(0);

  let max = max(c.r, c.g) |> max(c.b);
  let min = min(c.r, c.g) |> min(c.b);
  let delta = max - min;

  switch (delta) {
  | 0 => ()
  | _ =>
    h :=
      (
        switch (c.r, c.g, c.b) {
        | (r, g, b) when r == max => (g - b) / delta mod 6
        | (r, g, b) when g == max => (b - r) / delta + 2
        | (r, g, _) => (r - g) / delta + 4
        }
      )
  };

  h := h^ * 60 |> roundi;

  if (h^ < 0) {
    h := h^ + 360;
  };

  {
    h: h^,
    s:
      (max == 0 ? 0 : delta)
      / max
      |> float_of_int
      |> (n => Js.Math.round(n) *. 100. |> int_of_float),
    v:
      max
      / 255
      |> float_of_int
      |> (n => Js.Math.round(n) *. 100. |> int_of_float),
  };
};

let hsl2hsv = c => {
  let s = (c.l < 50 ? c.l : 100 - c.l) / 100;
  {h: c.h, s: 2 * s / (c.l + s) * 100, v: c.l + s};
};

let hsv2hsl = (c: hsv) => {
  let s = float_of_int(c.s) /. float_of_int(maxColorSaturation);
  let v = float_of_int(c.v) /. float_of_int(maxColorValue);
  let l = (2. -. s) *. v |> ref;
  let sl = s *. v /. (l^ <= 1. ? l^ : 2. -. l^);
  l := l^ /. 2.;

  {h: c.h, s: int_of_float(sl *. 100.), l: int_of_float(l^ *. 100.)};
};

let hsv2rgb = (c: hsv) => {
  let s = c.s / 100;
  let v = c.v / 100;

  let c' = v * s;
  let hh = c.h / 60;
  let x = c' * (1 - abs(hh mod 2 - 1));
  let m = v - c';

  let rgb = [|0, 0, 0|];

  switch (floori(hh)) {
  | 0 =>
    rgb[0] = c';
    rgb[1] = x;
  | 1 =>
    rgb[0] = x;
    rgb[1] = c';
  | 2 =>
    rgb[1] = c';
    rgb[2] = x;
  | 3 =>
    rgb[1] = x;
    rgb[2] = c';
  | 4 =>
    rgb[0] = x;
    rgb[2] = c';
  | 5 =>
    rgb[0] = c';
    rgb[2] = x;
  | _ => ()
  };

  {
    r: roundi @@ maxColorRgba * (rgb[0] + m),
    g: roundi @@ maxColorRgba * (rgb[1] + m),
    b: roundi @@ maxColorRgba * (rgb[2] + m),
    a: None,
  };
};

let hsl2rgb = c => hsl2hsv(c) |. hsv2rgb;
