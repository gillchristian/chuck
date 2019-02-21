let rec to_triples = (r, cs) => {
  switch (cs) {
  | [a, b, c] => [a ++ b ++ c, ...r]
  | [a, b, c, ...x] => to_triples([a ++ b ++ c, ...r], x)
  | _ => r
  };
};
let to_js_string = List.map(Js.String.make);

let chars =
  Data.range(Char.code('a'), Char.code('z') + 1)
  |> List.filter(n => n !== Char.code('i') && n !== Char.code('l'))
  |> List.map(Char.chr)
  |> List.map(Char.escaped);

let pairs = chars |> List.map(s => s ++ s) |> to_js_string;
let triples = to_triples([], chars) |> to_js_string;

let only_chars = Js.Re.fromString("^[a-z]+$");

let validate_pass = s => {
  let includes = str => Js.String.(includes(str, make(s)));

  String.length(s) <= 32
  && Js.Re.test(s, only_chars)
  && triples
  |> List.filter(includes)
  |> List.length
  |> (v => v > 0)
  && pairs
  |> List.filter(includes)
  |> List.length
  |> (v => v >= 2)
  && !String.contains(s, 'i')
  && !String.contains(s, 'l');
};