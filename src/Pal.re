let () = {
  let colorsJson = Node.Fs.readFileSync("./fixtures/colors.json", `utf8);
  let cjson = Json.parseOrRaise(colorsJson);
  let colors = Struct.DecodeJson.colors(cjson);
  Struct.Printer.colors(colors);
};
