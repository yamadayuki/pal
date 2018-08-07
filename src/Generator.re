open ChangeCase;
open Struct;

[@bs.send] external trimEnd : string => string = "trimEnd";

module Modifier = {
  let genereatedStringToArray = str => Js.String.split("\n", str);

  let trimWhitespace = str =>
    str
    |. genereatedStringToArray
    |. Belt.Array.map(trimEnd)
    |. Belt.List.fromArray
    |> String.concat("\n");
};

module type Template = {let template: Struct.colors => string;};

module ReasonTemplate: Template = {
  open Modifier;

  let componentTemplate = (name, comp) => {
    let brightnessName = camelCase(name) ++ pascalCase(comp.bright);
    let value =
      switch (comp.alpha) {
      | Some(a) =>
        Color.(hex2rgba(~alpha=Some(a), comp.hex) |. string_of_rgba)
      | None => comp.hex |> Color.string_of_hex
      };
    {j|let $brightnessName = "$value";|j};
  };

  let colorTemplate = c => {
    let name = c.name;
    let moduleName = pascalCase(name);
    let components =
      c.colors
      |. Belt.List.map(componentTemplate(name))
      |> String.concat("\n    ");
    {j|
  module $moduleName = {
    $components
  }
    |j};
  };

  let template = cs => {
    let colors =
      cs
      |. Belt.Array.map(colorTemplate)
      |. Belt.List.fromArray
      |> String.concat("");
    {j|
module Color = {
  $colors
};
    |j} |. trimWhitespace;
  };
};
