open Jest;
open Expect;

let long = "aabbzzabcyayabcdefghjkmnoprqstuvdqyzabcdefe";

describe("src/password.re", () =>
  testAll(
    "validate_pass",
    [
      ("aabbabc", true),
      ("zzdefaa", true),
      ("aabbccghj", true),
      ("abczzaajj", true),
      ("hola", false),
      ("aabbccghjl", false),
      ("aabbabcl", false),
      ("aabbabci", false),
      (long, false),
      ("123abczzaajj", false),
      ("@#!abczzaajj", false),
    ],
    ((s, expected)) =>
    Password.validate_pass(s) |> expect |> toBe(expected)
  )
);