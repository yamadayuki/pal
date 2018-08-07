let () = {
  let colorsJson = Node.Fs.readFileSync("./fixtures/colors.json", `utf8);
  let cjson = Json.parseOrRaise(colorsJson);
  let colors = Struct.Decode.colors(cjson);
  Generator.ReasonTemplate.template(colors) |> Js.log;
};
