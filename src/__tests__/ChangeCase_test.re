open Jest;
open Expect;

open ChangeCase;

/*
 ("testString", "TestString"),
 ("Test String", "TestString"),
 ("Test_String", "TestString"),
 ("Test-String", "TestString"),
 ("Facebook API", "FacebookApi"),
 ("a-test-again", "ATestAgain"),
 ("a---better__test", "ABetterTest"),
  */

describe("camelCase", () => {
  let suites = [
    ("TestString", "testString"),
    ("Test String", "testString"),
    ("Test_String", "testString"),
    ("Test-String", "testString"),
    ("Facebook API", "facebookApi"),
    ("-webkit-transform", "webkitTransform"),
    ("fooBarBaz", "fooBarBaz"),
    ("some (things)", "someThings"),
  ];

  let runTest = suite => {
    let (str, expected) = suite;
    test(str, () =>
      expect(camelCase(str)) |> toBe(expected)
    );
  };

  Belt.List.forEach(suites, runTest);
});

describe("pascalCase", () => {
  let suites = [
    ("testString", "TestString"),
    ("Test String", "TestString"),
    ("Test_String", "TestString"),
    ("Test-String", "TestString"),
    ("Facebook API", "FacebookApi"),
    ("a-test-again", "ATestAgain"),
    ("a---better__test", "ABetterTest"),
  ];

  let runTest = suite => {
    let (str, expected) = suite;
    test(str, () =>
      expect(pascalCase(str)) |> toBe(expected)
    );
  };

  Belt.List.forEach(suites, runTest);
});
