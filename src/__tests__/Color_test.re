open Jest;
open Expect;

open Color;

describe("toDecimal", () => {
  let suites = [("3", 3), ("10", 16), ("a", 10), ("ff", 255)];

  let runTest = suite => {
    let (str, expected) = suite;
    test(str, () =>
      expect(toDecimal(str)) |> toBe(expected)
    );
  };

  Belt.List.forEach(suites, runTest);
});

describe("char2Decimal", () => {
  let suites = [('3', 3), ('a', 10), ('f', 15)];

  let runTest = suite => {
    let (ch, expected) = suite;
    test(ch |> String.make(1), () =>
      expect(char2Decimal(ch)) |> toBe(expected)
    );
  };

  Belt.List.forEach(suites, runTest);
});
