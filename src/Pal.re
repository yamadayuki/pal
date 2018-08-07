let () = {
  let colorsJson = Node.Fs.readFileSync("./fixtures/colors.json", `utf8);
  let cjson = Json.parseOrRaise(colorsJson);
  let colors = Struct.Decode.colors(cjson);
  let generated = Generator.ReasonTemplate.template(colors);

  generated
  |. Generator.Modifier.genereatedStringToArray
  |. Array.length
  |. Js.log;

  Node.Fs.writeFileAsUtf8Sync("./fixtures/colors.re", generated);
};
