open Jest;

describe("src/index.re", () => {
  open Expect;

  test("hello world", () =>
    expect(true) |> toBe(true))
});
