type color = {
  name: string,
  colors: list(component),
}
and component = {
  bright: string,
  hex: string,
  alpha: option(float),
}
and colors = array(color);

type brightness = [ | `number(int) | `bright(int) | `alpha(int)];

let string_of_brightness =
  fun
  | `number(x) => string_of_int(x)
  | `bright(x) => "Bright" ++ string_of_int(x)
  | `alpha(x) => "Alpha" ++ string_of_int(x);

let inspect = a => {
  Js.log(a);
  a;
};

let parse_brightness = s =>
  switch (
    Js.String.startsWith("Bright", s),
    Js.String.startsWith("Alpha", s),
  ) {
  | (true, _) =>
    let n = s |> Js.String.sliceToEnd(~from=6) |> int_of_string;
    `bright(n);
  | (_, true) =>
    let n = s |> Js.String.sliceToEnd(~from=5) |> int_of_string;
    `alpha(n);
  };

let brightness_of_string = s =>
  try (
    {
      let n = int_of_string(s);
      `number(n);
    }
  ) {
  | Failure("int_of_string") => parse_brightness(s)
  };

let int_of_brightness =
  fun
  | `number(x) => x
  | `bright(x) => x
  | `alpha(x) => x;

module Decode = {
  open! Json.Decode;

  let component = json => {
    bright: json |> field("bright", string),
    hex: json |> field("hex", string),
    alpha: json |> optional(field("alpha", float)),
  };

  let color = json => {
    name: json |> field("name", string),
    colors: json |> field("colors", list(component)),
  };

  let colors = json => json |> array(color);
};

module Printer = {
  let component = c => {
    let bright = c.bright;
    let brightness = brightness_of_string(bright);
    let hex = c.hex;
    let alpha = c.alpha;
    let rgba = Color.hex2rgba(~alpha, hex) |> Color.string_of_rgba;
    Js.log(
      {j|bright=$bright hex=$hex rgba=$rgba alpha=$alpha brightness=$brightness|j},
    );
  };

  let color = c => {
    let cname = c.name;

    Js.log({j|name: $cname|j});
    let cs = c.colors;
    Belt.List.forEach(cs, component);
  };

  let colors = cs => Belt.Array.forEach(cs, color);
};
